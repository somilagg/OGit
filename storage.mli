(** Module that handles file IO, used for storage when system is turned on
    and off, causing the need for changes to be stored locally *)

(** [state_to_string state] returns a string version of the state [state] 
    passed in as input in an identifiable format that can be parsed at a later 
    time to rebuild the state. *)
val state_to_string : Diff.state -> string

(** [str_to_change str text] returns the parsed version of the input 
    string [str] and string [text] as a difference from [Diff.differences]. *)
val str_to_change : string -> string -> Diff.differences

(** [string_to_kv str] returns a tuple containing the line number as the first
    element and the difference associated with that line [str] as 
    the second element. *)
val string_to_kv : string -> int * Diff.differences 

(** [str_splitter str] splits string [str] into a string list based on the 
    position of the delimiter. *)
val str_splitter : string -> string list 

(** [parse_state_regex str] returns a state from a given string input [str]. 
    This string input is parsed for the keys and values using identifiers that were placed 
    in the string when converting the state to string.  *)
val parse_state_regex : string -> Diff.state

(** [read_file file] reads a specified file [file] and converts each line to 
    a version that is then made into the version data type and used. *)
val read_file : string -> Diff.version

(** [write_file ver name] writes the given version map [ver] to a file [name] 
    to be read in at a later time. *)
val write_file : Diff.state Diff.Versions.t -> string -> unit

(** [compare versions v1 v2] returns true if the two inputted versions 
    [v1] and [v2]are equal and false otherwise. *)
val compare_versions : Diff.state Diff.Versions.t -> Diff.state Diff.Versions.t
  -> bool