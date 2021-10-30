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

module EzSearch = struct

  open EzFile.OP

  let version = 1

  module TYPES = struct

    type file = {
      file_name : string ;
      file_entry : string ;
      file_pos : int ;
      file_length : int ;
    }

    type db = {
      db_text : string ;
      db_index : file array ;
    }

    type occurrence = {
      occ_file : file ;
      occ_pos : int ;
    }

    type context = {
      prev_lines : ( int * string ) list ;
      curr_line : string ;
      curr_pos : int ;
      next_lines : ( int * string ) list ;
    }

  end

  open TYPES

  let time msg f x =
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let t1 = Unix.gettimeofday () in
    Printf.eprintf "%s lasted: %.2f s\n%!" msg ( t1 -. t0 ) ;
    y


  let output_index ~db_dir ( index : file array ) =
    let index_file = db_dir // "sources.index" in
    let oc = open_out_bin index_file in
    output_value oc version ;
    output_value oc index ;
    close_out oc

  let input_index ~db_dir =
    let index_file = db_dir // "sources.index" in
    let ic = open_in_bin index_file in
    let v = input_value ic in
    if v <> version then assert false;
    let ( index : file array ) = input_value ic in
    close_in ic ;
    index


  let index_directory ~db_dir dir =

    let plain_file = db_dir // "sources.text" in
    let plain_oc = open_out_bin plain_file in
    let index = ref [] in

    Unix.chdir dir;
    let files = Sys.readdir "." in
    let pos = ref 0 in
    Array.iter (fun file_entry ->

        let index_file path =
          match EzFile.read_file ( file_entry // path ) with
          | exception exn ->
              Printf.eprintf "Warning with file %S >> %S:\n%!" file_entry path;
              Printf.eprintf "  Exception %s\n%!" (Printexc.to_string exn)
          | s ->
              let file_length = String.length s in
              Printf.fprintf plain_oc "%s\000\000\000\000" s ;
              index := {
                file_name = path ;
                file_entry ;
                file_pos = !pos ;
                file_length ;
              } :: !index;
              pos := !pos + file_length + 4;
        in

        EzFile.make_select EzFile.iter_dir ~deep:true
          ~f:(fun path ->
              let basename = Filename.basename path in
              let _, ext = EzString.rcut_at basename '.' in
              match ext with
              | "ml" | "mll" | "mly" | "mli" ->
                  index_file path
              | _ -> ()
            ) file_entry
      ) files;
    close_out plain_oc ;
    let db_index = Array.of_list !index in
    EzArray.rev db_index;
    output_index ~db_dir db_index

  let load_db ~db_dir =
    let db_index = input_index ~db_dir in
    let source_file = db_dir // "sources.text" in
    let st = Unix.lstat source_file in
    let size = st.Unix.st_size in
    let s = Bytes.create size in
    let ic = open_in_bin source_file in
    really_input ic s 0 size ;
    close_in ic;
    let db_text = Bytes.unsafe_to_string s in
    { db_index ; db_text }

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

  let search ~db ~f ?pos ?last regexp =
    let pos = match pos with
      | Some pos -> pos
      | None ->
          match last with
          | None -> 0
          | Some occ ->
              occ.occ_file.file_pos + occ.occ_pos + 1
    in
    let rec iter pos =
      match Re.Str.search_forward regexp db.db_text pos with
      | exception _ -> ()
      | pos ->
          let file = find_file db.db_index pos in
          let occ = {
            occ_pos = pos - file.file_pos ;
            occ_file = file ;
          } in
          if f occ then
            iter (pos+1)
    in
    iter pos

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
      ~occ_pos: ( pos + occ.occ_pos )

  let occurrence_context ~db ~line occ ~max =
    let s = db.db_text in
    let file = occ.occ_file in
    let file_pos = file.file_pos in
    let pos = file_pos + occ.occ_pos in
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

end
