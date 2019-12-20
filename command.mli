(** [file_name] is the string name of a file. *)
type file_name = string 

(** [file_names] is an array of [file_name]. *)
type file_names = file_name list

(** [command] is the data type corresponding to the user input that is 
    interpreted and then executed upon. *)
type command = 
  | Push of file_names 
  | Pull of file_name
  | VersionHistory of file_name
  | Add of file_names
  | Remove of file_names
  | SwitchUser
  | Help
  | Quit
  | Files

exception Empty

exception Malformed

(** [str_eval str] compares string [str] to the empty string. *)
val str_eval : string -> bool

(** [str_splitter str] is the list of [str] split by the spaces with the 
    back-to-back spaces removed. *)
val str_splitter : string -> string list

(** [parse str] returns the corresponding command based on [str]. Raises
    Malformed if [str] isn't a valid command. Raises Empty if [str] is an 
    empty string *)
val parse : string -> command