(** Module that performs computations on file differences *)

(** [State] is a map of line numbers to [differences]. *)
module State : (Map.S with type key = int)

(** [Versions] is a map of version numbers to states.*)
module Versions : (Map.S with type key = int)

(** [NonExistentVersion] is the exception raised when a version is not found. *)
exception NonExistentVersion

(** [differences] is the variant of changes computed between files. *)
type differences = 
  | C of string
  | D

(** [state] is the state of a file, which maps a line number to a [differences]
    type *)
type state = differences State.t

(** [version] is the version of a file, which maps a verison number to a 
    [state].*)
type version = state Versions.t

(** [initial_version] is the version in which evaluation begins. 
    It should not have any allocated version numbers.*)
val initial_version : version

(** [initial_state] is the state in which evaluation begins.
    It should not have any allocated line numbers. *)
val initial_state : state

(** [get_initial_version] is the version with the initial version mapped from 0 
    to its state. *)
val get_initial_version : string -> version

(** [apply_changes v_num ver] is the accumulated state up to version number
    [v_num] of version [ver]
*)
val apply_changes : int -> version -> state

(** [string_of_version v_num ver] is the string of the version in [ver] 
    specified by [v_num].*)
val string_of_version : int -> version -> string

(** [create_new_version ver file] is the version with the updated [file]
    stored as a new version in [ver]. *)
val create_new_version : version -> string -> version

(** [convert_differences st o_diff_lst] is the state with all differences in
    [o_diff_lst] converted into a [state] *)
val convert_differences : state -> Odiff.diff list -> state

(** [create_file_from_num v_num ver f_name] adds a file [f_name] with 
    the string of version [v_num] in [ver]. *)
val create_file_from_num : int -> version -> string -> unit

(** [file_to_string file] is the string representation of a file. *)
val file_to_string : string -> string