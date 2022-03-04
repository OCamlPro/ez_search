(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1

module Parmap = UseParmap

module EzSearch = struct

  open EzCompat
  open EzFile.OP

  let version = 1

  module TYPES = struct

    type dbm

    type file = {
      file_name : string ;
      file_entry : string ;
      file_pos : int ;
      file_length : int ;
    }

    type db = {
      mutable db_mapfile : dbm option ;
      db_text : string ;
      db_index : file array ;
    }

    type occurrence = int

    type occurrence_file = {
      occ_file : file ;
      occ_file_pos : int ;
    }

    type occurrence_context = {
      prev_lines : ( int * string ) list ;
      curr_line : string ;
      curr_pos : int ;
      next_lines : ( int * string ) list ;
    }

  end

  open TYPES

let db_name_default = "sources"

  let time msg f x =
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let t1 = Unix.gettimeofday () in
    Printf.eprintf "%s lasted: %.2f s\n%!" msg ( t1 -. t0 ) ;
    y

  let output_index ~db_dir ~db_name ( index : file array ) =
    let index_file = db_dir // db_name ^ ".index" in
    let oc = open_out_bin index_file in
    output_value oc version ;
    output_value oc index ;
    close_out oc

  let input_index ~db_dir ~db_name =
    let index_file = db_dir // db_name ^ ".index" in
    let ic = open_in_bin index_file in
    let v = input_value ic in
    if v <> version then assert false;
    let ( index : file array ) = input_value ic in
    close_in ic ;
    index

  let index_files ~db_dir ?(db_name = db_name_default) f =

    let plain_file = db_dir // db_name ^ ".text" in
    let plain_oc = open_out_bin plain_file in
    (* write a 8 bytes header first, that will be used by OCaml GC *)
    output_bytes plain_oc ( Bytes.create 8 );
    let index = ref [] in

    (* file_pos origin is offsetted by 8 bytes *)
    let pos = ref 0 in

    let index_file ~file_entry ~file_name ~file_content =
      let file_length = String.length file_content in
      Printf.fprintf plain_oc "%s\000\000\000\000" file_content ;
      index := {
        file_name ;
        file_entry ;
        file_pos = !pos ;
        file_length ;
      } :: !index;
      pos := !pos + file_length + 4;
    in
    f index_file;
    output_bytes plain_oc ( Bytes.create 8 );
    close_out plain_oc ;
    let db_index = Array.of_list !index in
    EzArray.rev db_index;
    output_index ~db_dir ~db_name db_index

  let index_directory ~db_dir ?db_name ~select dir =

    index_files ~db_dir ?db_name
      (fun index_file ->

         let curdir = Sys.getcwd () in
         Unix.chdir dir;
         let entries = Sys.readdir "." in
         Array.iter (fun file_entry ->

             let index_file path =
               match EzFile.read_file ( file_entry // path ) with
               | exception exn ->
                   Printf.eprintf "Warning with file %S >> %S:\n%!" file_entry path;
                   Printf.eprintf "  Exception %s\n%!" (Printexc.to_string exn)
               | s ->
                   index_file ~file_entry ~file_name:path ~file_content:s
             in

             EzFile.make_select EzFile.iter_dir ~deep:true
               ~f:(fun path ->
                   if select path then
                     index_file path
                 ) file_entry;
           ) entries;
         Unix.chdir curdir
      )

  external mapfile_openfile : string -> dbm = "ocp_mapfile_openfile_c"
  external mapfile_get_string : dbm -> string = "ocp_mapfile_get_string_c"

  let load_db ~db_dir ?(db_name = db_name_default) ?(use_mapfile = true) () =
    let db_index = input_index ~db_dir ~db_name in
    let source_file = db_dir // db_name ^ ".text" in

    if use_mapfile then
      let db_mapfile = mapfile_openfile source_file in
      let db_text = mapfile_get_string db_mapfile in
      { db_index ; db_text ; db_mapfile = Some db_mapfile }
    else
      let st = Unix.lstat source_file in
      let size = st.Unix.st_size - 16 in
      let s = Bytes.create size in
      let ic = open_in_bin source_file in
      really_input ic s 0 8 ;
      really_input ic s 0 size ;
      close_in ic;
      let db_text = Bytes.unsafe_to_string s in
      { db_index ; db_text ; db_mapfile = None }

  let count_lines_total ~db =
    let s = db.db_text in
    let len = String.length s in
    let rec iter pos n len =
      if pos = len then
        n
      else
        let n =
          match s.[ pos ] with
          | '\n' -> n+1
          | _ -> n
        in
        iter (pos+1) n len
    in
    iter 0 0 len

  let find_file index pos =
    let len = Array.length index in
    let rec iter i1 i2 =
      if i1 = i2 || i1 + 1 = i2  then
        index.(i1)
      else
        let m = ( i1 + i2 ) / 2 in
        let file = index.( m ) in
        if file.file_pos > pos then
          iter i1 m
        else
          iter m i2
    in
    iter 0 len

  let occurrence_file ~db pos =
    let file = find_file db.db_index pos in
    {
      occ_file_pos = pos - file.file_pos ;
      occ_file = file ;
    }

  let search ~db ~f ?pos ?last ?len find =
    let slen = String.length db.db_text in
    let len = match len with
      | None -> slen
      | Some len -> min len slen
    in
    let pos = match pos with
      | Some pos -> pos
      | None ->
          match last with
          | None -> 0
          | Some occ ->
              occ.occ_file.file_pos + occ.occ_file_pos + 1
    in
    let rec iter pos =
      match find ~pos ~len db.db_text with
      | exception _ -> ()
      | pos ->
          if f pos then
            iter (pos+1)
    in
    iter pos


  external memmem : haystack:string -> pos:int -> haystack_len:int ->
    needle:string -> needle_len:int -> int = "memmem_c" [@@noalloc]

  external count_matches :
    needle:string -> haystack:string -> startpos:int -> length:int -> int = "ocaml_countmatch"

  let memmem ~haystack ~pos ~len ~needle =
    let pos = memmem ~haystack ~pos ~haystack_len:len ~needle
        ~needle_len:(String.length needle) in
    if pos = -1 then raise Not_found;
    pos


  let search_and_count ~db
      ?(is_regexp=false)
      ?(is_case_sensitive=true)
      ?(ncores = max_int)
      ?(maxn = 10)
      ?find
      ?(engine=`Re)
      term =
    let maxn = max 1 maxn in
    let find =
      match find with
      | Some find -> find
      | None ->
          match is_regexp, is_case_sensitive, engine with
          | true, true, `Re ->
              let regexp = ReStr.regexp term in
              fun ~pos ~len s ->
                ReStr.search_forward ~len:(len-pos) regexp s pos
          | true, true, `Str ->
              let regexp = Str.regexp term in
              fun ~pos ~len s ->
                Str.search_forward ~len:(len-pos) regexp s pos
          | true, false, `Re ->
              let regexp = ReStr.regexp_case_fold term in
              fun ~pos ~len s ->
                ReStr.search_forward ~len:(len-pos) regexp s pos
          | true, false, `Str ->
              let regexp = Str.regexp_case_fold term in
              fun ~pos ~len s ->
                Str.search_forward ~len:(len-pos) regexp s pos
          | false, true, _ ->
              fun ~pos ~len haystack ->
                memmem ~haystack ~pos ~len ~needle:term
          | false, false, `Re ->
              let regexp = ReStr.regexp_string_case_fold term in
              fun ~pos ~len s ->
                ReStr.search_forward ~len:(len-pos) regexp s pos
          | false, false, `Str ->
              let regexp = Str.regexp_string_case_fold term in
              fun ~pos ~len s ->
                Str.search_forward ~len:(len-pos) regexp s pos
    in
    let ncores = max 0 ( min ( Parmap.get_default_ncores () ) (ncores-1)) in
    let list =
      let maxlen = String.length db.db_text in
      let seglen = maxlen / (ncores+1) in
      let sequence = Array.init (ncores+1) (fun n ->
          max ( n * seglen - 1000 ) 0
        ) in
      if is_case_sensitive && not is_regexp then
        Parmap.parmap ~ncores
          (fun pos ->
             let n = ref 0 in
             let occs = ref [] in
             search ~db find ~pos ~len:(pos+seglen) ~f:(fun occ ->
                 occs := occ :: !occs;
                 incr n;
                 !n < maxn
               );
             let n =
               if maxn > 0 && !n = maxn then
                 let startpos = 1 + List.hd !occs in
                 !n + count_matches ~needle:term
                   ~haystack:(db.db_text) ~startpos
                   ~length:(pos+seglen-startpos)
               else
                 !n
             in
             n, !occs
          ) (A sequence)
      else
      if ncores = 0 then
        let n = ref 0 in
        let occs = ref [] in
        search ~db find ~pos:0 ~len:maxlen ~f:(fun occ ->
            if !n < maxn then
              occs := occ :: !occs;
            incr n;
            true
          );
        [!n, !occs]
      else
        Parmap.parmap ~ncores
          (fun pos ->
             let n = ref 0 in
             let occs = ref [] in
             search ~db find ~pos ~len:(pos+seglen) ~f:(fun occ ->
                 if !n < maxn then
                   occs := occ :: !occs;
                 incr n;
                 true
               );
             !n, !occs
          ) (A sequence)
    in
    let total = ref 0 in
    let total_occs = ref [] in
    List.iter (fun (n, occs) ->
        total := !total + n;
        total_occs := !total_occs @ occs
      ) list;
    !total, !total_occs

  let occurrence_line ~db occ =
    let s = db.db_text in
    let rec iter ~pos ~line ~occ_pos =
      if pos = occ_pos then
        line
      else
        let line =
          match s.[pos] with
          | '\n' -> line+1
          | _ -> line
        in
        iter ~pos:(pos+1) ~line ~occ_pos
    in
    let pos = occ.occ_file.file_pos in
    iter
      ~pos
      ~line:1
      ~occ_pos: ( pos + occ.occ_file_pos )

  let occurrence_context ~db ~line occ ~max =
    let s = db.db_text in
    let file = occ.occ_file in
    let file_pos = file.file_pos in
    let pos = file_pos + occ.occ_file_pos in
    let file_end = file_pos + file.file_length in

    let rec prev ~pos ~line lines ~max =
      if pos = file_pos then
        ( line, pos ) :: lines
      else
        match s.[pos] with
        | '\n' ->
            let lines = ( line, pos+1 ) :: lines in
            let line = line - 1 in
            if max = 0 then
              lines
            else
              prev ~pos:(pos-1) lines ~line ~max:(max-1)
        | _ ->
            prev ~pos:(pos-1) ~line lines ~max
    in
    let prev_lines_rev =
      if pos = file_pos then
        [ line, pos ]
      else
        prev ~pos:(pos-1) ~line [] ~max
    in

    let rec next ~pos ~line ~max lines =
      if pos = file_end then
        match lines with
        | (_, pos) :: _ when pos = file_end -> lines
        | _ ->
            ( line, file_end ) :: lines
      else
        match s.[pos] with
        | '\n' ->
            let lines = ( line, pos+1 ) :: lines in
            let line = line + 1 in
            if max = 0 then
              lines
            else
              next ~pos:(pos+1) ~line lines ~max:(max-1)
        | _ ->
            next ~pos:(pos+1) ~line lines ~max
    in
    let next_lines_rev = next ~pos ~line:(line+1) [] ~max in

    let prev_lines, bol =
      let rec iter lines = function
        | [] -> assert false
        | [ _line, bol ] -> lines, bol
        | ( line, bol1 ) :: ( ( (_,bol2) :: _ ) as rem) ->
            let lines =
              ( line, String.sub s bol1 (bol2 - bol1 -1) ) :: lines
            in
            iter lines rem
      in
      iter [] prev_lines_rev
    in

    let next_lines, eol =
      let rec iter lines = function
        | [] -> assert false
        | [ _line, bol ] -> lines, bol
        | ( _, bol2 ) :: ( ( (line,bol1) :: _ ) as rem) ->
            let lines =
              ( line, String.sub s bol1 (bol2 - bol1 -1) ) :: lines
            in
            iter lines rem
      in
      iter [] next_lines_rev
    in

    let curr_line = String.sub s bol ( eol - bol - 1 ) in

    { prev_lines = List.rev prev_lines ;
      curr_line ;
      curr_pos = pos-bol ;
      next_lines
    }

  let file_content ~db file =
    String.sub db.db_text file.file_pos file.file_length

  let files ~db = db.db_index

  let length ~db = String.length db.db_text

(*
let test s occ_pos =
  let db_text = Printf.sprintf "%s\000\000\000\000" s in
  let db = { db_text ; db_index = [||] } in
  let occ = {
    occ_pos ;
    occ_file = {
      file_name = "FILENAME";
      file_entry = "ENTRY" ;
      file_pos = 0 ;
      file_length = String.length s;
    };
  } in
  let line = occ_line ~db occ in
  let prev_lines, curr_line, pos, next_lines = occ_context ~db ~line occ 3 in
  List.iter (fun ( line, s ) ->
      Printf.eprintf "%4d  %s\n%!" line s
    ) prev_lines ;
  Printf.eprintf "%4d--%s (position: %d)\n%!" line curr_line pos;
  List.iter (fun ( line, s ) ->
      Printf.eprintf "%4d  %s\n%!" line s
    ) next_lines ;
  ()


let () =
  let s = {|123
ab
cd
ef
gh
ij
kl
mn
op
qr
st
uv
|} in
  let len = String.length s in
  for i = 0 to len - 1 do
    Printf.eprintf "OCCURRENCE AT %d/%d\n%!" i len;
    test s i
  done
*)

  let pos occ = occ
  let text ~db = db.db_text
end
