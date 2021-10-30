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

  (** [index_directory ~db_dir DIRECTORY] index all files in
     [DIRECTORY], and store the index in [db_dir]. Every top-directory
     in DIRECTORY is considered as a [file_entry] name, and
     [file_name] are within top-directories.  *)
  val index_directory : db_dir:string -> string -> unit

  (** [load_db ~db_dir] loads the database in memory. Note: in the
     next version, we should aim at using mmap.  *)
  val load_db : db_dir:string -> db

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

  (** [time msg f x] prints the time spent executing [f x]. *)
  val time : string -> ('a -> 'b) -> 'a -> 'b
end
