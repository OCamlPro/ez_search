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

open EzFile.OP
open V1  (* from outside, should be: open Ez_search.V1 *)
open EzSearch.TYPES

let find_term ~db ~is_case_sensitive ~is_regexp
    ~lines ~maxn ~verbose term =

  let find =
    if true then
      let regexp = match is_regexp, is_case_sensitive with
        | true, true -> Str.regexp term
        | true, false -> Str.regexp_case_fold term
        | false, true -> Str.regexp_string term
        | false, false -> Str.regexp_string_case_fold term
      in
      fun ~pos ~len s ->
        Str.search_forward ~len regexp s pos
    else
      let regexp = match is_regexp, is_case_sensitive with
        | true, true -> ReStr.regexp term
        | true, false -> ReStr.regexp_case_fold term
        | false, true -> ReStr.regexp_string term
        | false, false -> ReStr.regexp_string_case_fold term
      in
      fun ~pos ~len s ->
        ReStr.search_forward ~len:(len-pos) regexp s pos
  in
  let ncores = Parmap.get_default_ncores () in
  if verbose then
    Printf.eprintf "Ncores: %d\n%!" ncores;
  let maxlen = EzSearch.length ~db in
  let seglen = maxlen / ncores + 1 in
  let sequence = Array.init ncores (fun n ->
      max ( n * seglen - 1000 ) 0
    ) in
  let print_occ pos =
    let occ = EzSearch.occurrence_file ~db pos in
    let file = occ.occ_file in

    Printf.printf "%s:%s\n%!" file.file_entry file.file_name ;
    let line = EzSearch.occurrence_line ~db occ in
    let c = EzSearch.occurrence_context ~db ~line occ ~max:lines in
    List.iter (fun ( line, s ) ->
        Printf.printf "%4d  %s\n%!" line s
      ) c.prev_lines ;
    Printf.printf "%4d--%s (position: %d)\n%!" line
      c.curr_line c.curr_pos;
    List.iter (fun ( line, s ) ->
        Printf.printf "%4d  %s\n%!" line s
      ) c.next_lines ;
  in
  let f () =
    let list =
      Parmap.parmap ~ncores
        (fun pos ->
           let n = ref 0 in
           let occs = ref [] in
           EzSearch.search ~db find ~pos ~len:(pos+seglen) ~f:(fun occ ->
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
    if verbose then
      Printf.eprintf "Found %d occurrences\n%!" !total;
    let n = ref 0 in
    List.iter (fun occ ->
        if !n < maxn then
          print_occ occ;
        incr n;

      ) !total_occs
  in
  if verbose then
    EzSearch.time "Search" f ()
  else f ()

let home_dir = match Sys.getenv "HOME" with
  | home_dir -> home_dir
  | exception _ -> "/root"

let db_dir_default = home_dir // ".opam" // "ocp-search"

let main () =

  let to_index = ref None in
  let db_dir = ref db_dir_default in
  let sources = ref true in
  let search = ref None in
  let is_regexp = ref false in
  let is_case_sensitive = ref true in
  let count_lines = ref false in
  let use_mapfile = ref true in
  let lines = ref 1 in
  let n = ref 10 in
  let content = ref None in
  let verbose = ref true in

  let arg_list = Arg.align  [

      "--index", Arg.String (fun dir -> to_index := Some dir),
      "DIR Index directory";

      "--count", Arg.Set count_lines,
      " Print number of lines in database";

      "-i", Arg.Clear is_case_sensitive,
      " Search in case insensitive way";

      "--string", Arg.String (fun term ->
          is_regexp := false ;
          search := Some term),
      "TERM Term to search";

      "--regexp", Arg.String (fun term ->
          is_regexp := true ;
          search := Some term),
      "TERM Term to search";

      "--no-mmap", Arg.Clear use_mapfile,
      " Do not map file in memory";

      "--lines", Arg.Int ( (:=) lines ),
      "NLINES Number of lines of context to print";

      "-n", Arg.Int ( (:=) n ),
      "NBR Maximal number of occurrences";

      "--file", Arg.String (fun s -> content := Some s),
      "ENTRY:FILENAME Dump content of filename";

      "-q", Arg.Clear verbose,
      " Do not display debug info";

      "--build", Arg.Clear sources,
      " Index/search build files"

    ]

  in
  let arg_usage = "ocp-search [ARGS]: index and search sources" in
  Arg.parse arg_list
    (fun arg ->
       Printf.eprintf "Error: unexpected argument %S\n%!" arg;
       Arg.usage arg_list arg_usage ;
       exit 2)
    arg_usage;

  let work_done = ref false in

  let db_dir = !db_dir in
  let use_mapfile = !use_mapfile in
  let pwd = Sys.getcwd () in
  let db_dir =
    if Filename.is_relative db_dir then
      pwd // db_dir
    else
      db_dir
  in

  let db_name = if !sources then "sources" else "build" in

  begin
    match !to_index with
    | None -> ()
    | Some dir ->
        work_done := true;
        EzFile.make_dir ~p:true db_dir;
        let select path =
          let basename = Filename.basename path in
          let basename, ext = EzString.rcut_at basename '.' in
          if !sources then
            match ext with
            | "ml" | "mll" | "mly" | "mli" -> true
            | _ -> false
          else
            match String.lowercase_ascii basename with
            | "dune"
            | "makefile" -> true
            | _ -> false
        in
        EzSearch.index_directory dir ~db_dir ~db_name ~select
  end;

  let db =
    let db = ref None in
    fun () ->
      match !db with
      | None ->
          work_done := true ;
          let x =
            let f () =
              EzSearch.load_db ~db_dir ~db_name ~use_mapfile ()
            in
            if !verbose then
              EzSearch.time "Load index" f ()
            else
              f ()
          in
          db := Some x;
          x
      | Some db -> db
  in

  if !count_lines then  begin
    let db = db() in
    Printf.eprintf "Length: %d chars\n" ( EzSearch.length ~db);
    let n = EzSearch.count_lines_total ~db in
    Printf.eprintf "Indexed: %d lines\n%!" n;
  end ;

  begin
    match !search with
    | None -> ()
    | Some term ->
        let is_regexp = !is_regexp in
        let is_case_sensitive = !is_case_sensitive in
        let db = db() in
        find_term ~db ~is_regexp ~is_case_sensitive
          ~lines:!lines ~verbose:!verbose
          ~maxn:!n term
  end;

  begin
    match !content with
    | None -> ()
    | Some content ->
        let db = db () in
        let files = EzSearch.files ~db in
        let re = Re.Glob.glob ~anchored:true ~pathname:false content in
        let re = Re.compile re in
        let results = ref [] in
        Array.iter (fun file ->
            let s = Printf.sprintf "%s:%s" file.file_entry file.file_name in
            if Re.execp re s then
              results := file :: !results
          ) files;
        let len = List.length !results in
        match !results with
          [ file ] ->
            let content = EzSearch.file_content ~db file in
            let basename = Filename.basename file.file_name in
            Printf.printf "%s:%s\n%!" file.file_entry file.file_name;
            EzFile.write_file basename content ;
            Printf.eprintf "Content saved to %S\n%!" basename
        | results ->
            Printf.eprintf "%d files found\n%!" len;
            List.iter (fun file ->
                Printf.printf "%s:%s\n%!" file.file_entry file.file_name
              ) ( List.rev results );
  end;

  if not !work_done then
    Arg.usage arg_list arg_usage;

  ()
