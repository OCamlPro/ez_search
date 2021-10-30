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

let find_term ~db ~is_case_sensitive ~is_regexp term =

  let regexp = match is_regexp, is_case_sensitive with
    | true, true -> Re.Str.regexp term
    | true, false -> Re.Str.regexp_case_fold term
    | false, true -> Re.Str.regexp_string term
    | false, false -> Re.Str.regexp_string_case_fold term
  in

  EzSearch.time "Search" (fun () ->
      EzSearch.search ~db regexp ~pos:0 ~f:(fun occ ->
          let file = occ.occ_file in
          Printf.eprintf "Occurrence:\n%!";
          Printf.eprintf "  Entry: %s\n%!" file.file_entry ;
          Printf.eprintf "  Filename: %s\n%!" file.file_name ;
          Printf.eprintf "  Position: %d\n%!" occ.occ_pos ;
          let line = EzSearch.occurrence_line ~db occ in
          Printf.eprintf "  Line: %d\n%!" line ;
          let c = EzSearch.occurrence_context ~db ~line occ ~max:3 in
          List.iter (fun ( line, s ) ->
              Printf.eprintf "%4d  %s\n%!" line s
            ) c.prev_lines ;
          Printf.eprintf "%4d--%s (position: %d)\n%!" line
            c.curr_line c.curr_pos;
          List.iter (fun ( line, s ) ->
              Printf.eprintf "%4d  %s\n%!" line s
            ) c.next_lines ;

          true
        )
    ) ()


let main () =

  let to_index = ref None in
  let db_dir = ref "." in
  let search = ref None in
  let is_regexp = ref false in
  let is_case_sensitive = ref true in
  let count_lines = ref false in
  let use_mapfile = ref true in

  Arg.parse [

    "--index", Arg.String (fun dir -> to_index := Some dir),
    "DIR Index directory";

    "--db_dir", Arg.String (fun dir -> db_dir := dir),
    "DIR Database directory";

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

  ]
    (fun _ -> assert false)
    "Index and Search" ;

  let db_dir = !db_dir in
  let use_mapfile = !use_mapfile in
  let pwd = Sys.getcwd () in
  let db_dir =
    if Filename.is_relative db_dir then
      pwd // db_dir
    else
      db_dir
  in

  begin
    match !to_index with
    | None -> ()
    | Some dir ->
        EzSearch.index_directory dir ~db_dir
  end;

  let db =
    let db = ref None in
    fun () ->
      match !db with
      | None ->
          let x =
            EzSearch.time "Load index" (fun () ->
                EzSearch.load_db ~db_dir ~use_mapfile ()) ()
          in
          db := Some x;
          x
      | Some db -> db
  in

  if !count_lines then  begin
    let db = db() in
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
        find_term ~db ~is_regexp ~is_case_sensitive term
  end;

  ()
