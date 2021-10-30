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

module EzSearch : sig

  (** This module implements full-text search with regexps in a set of
     files.  Two steps are required: in the first step, a database is
     generated from all the files; in the second step, searches are
     performed in the database.  *)

  module TYPES : sig

    type db

    type file = {
      file_name : string ;
      file_entry : string ; (* name of top-directory containing this file *)
      file_pos : int ;      (* internal position of file in database *)
      file_length : int ;
    }

    (* An occurrence of the searched regexp in the database *)
    type occurrence = {
      occ_file : file ;
      occ_pos : int ;   (* occurrence position in file *)
    }

    type context = {
      prev_lines : ( int * string ) list ;
      curr_line : string ;
      curr_pos : int ;     (* position of occurrence in line *)
      next_lines : ( int * string ) list ;
    }
  end

  open TYPES

  (** [index_directory ~db_dir ~select DIRECTORY] index all files in
     [DIRECTORY], and store the index in [db_dir]. Every top-directory
     in DIRECTORY is considered as a [file_entry] name, and
     [file_name] are relative paths within top-directories.  [select]
     takes a path in argument and returns [true] if the content of the
     path should be indexed.  *)
  val index_directory :
    db_dir:string -> select:( string -> bool ) -> string -> unit

  (** [load_db ~db_dir ?use_mapfile ()] loads the database in memory.
     [use_mapfile] controls whether to use a memory-mapped file or
     load it normally. Memory-mapped files are normally more
     efficient, but support may be more unstable. *)
  val load_db : db_dir:string -> ?use_mapfile:bool -> unit -> db

  (** [count_lines_total ~db] counts the number of '\n' in the
     database. *)
  val count_lines_total : db:db -> int

  (** [search ~db ~f ?pos ?last regexp] searches a [regexp] in the
     database, starting either from [pos], from after the last
     occurrence [last], or from the beginning. Calls [f] for every
     occurrence found. [f] returns a boolean, that should be [true] if
     the search should continue after, or [false] if the search should
     terminate immediately. *)
  val search :
    db:db -> f:(occurrence -> bool) ->
    ?pos:int -> ?last:occurrence ->
    Re.Str.regexp -> unit

  (** [occurrence_line ~db occ] returns the line number in the file. *)
  val occurrence_line : db:db -> occurrence -> int

  (** [occurrence_context ~db ~line occ ~max] returns the context of
     the occurrence of in the file. The [line] number of the
     occurrence, as provided by [occurrence_line] should be
     provided. The parameter [max] controls how many lines should be
     returned before and after the occurrence. *)
  val occurrence_context :
    db:db -> line:int -> occurrence -> max:int -> context

  (** [file_content ~db file] returns the content of the file, as
     retrieved from the database. *)
  val file_content : db:db -> file -> string

  (** [files ~db] returns all the files stored in the database. *)
  val files : db:db -> file array

  (** [time msg f x] prints the time spent executing [f x]. *)
  val time : string -> ('a -> 'b) -> 'a -> 'b
end
