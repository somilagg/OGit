type file_name = string
type file_names = file_name list

(** [command] represents a valid command that the user can use in the command
    line *)
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
let str_eval str = 
  str <> ""

(** [str_splitter str] is the list of [str] split by the spaces with the 
    back-to-back spaces removed. *)
let str_splitter str = 
  let new_str = String.split_on_char ' ' str in 
  List.filter str_eval new_str

(** [parse str] returns the corresponding command based on [str]. Raises
    Malformed if [str] isn't a valid command. Raises Empty if [str] is an 
    empty string *)
let parse str = 
  let string_list = str |> String.trim |> str_splitter in
  match string_list with
  | [] -> raise Empty
  | h :: t -> 
    begin
      if h = "push" then 
        if List.length t != 0 then Push t else raise Malformed
      else if h = "quit" then 
        if List.length t = 0 then Quit else raise Malformed
      else if h = "pull" then 
        if List.length t != 0 then Pull (List.hd t) else raise Malformed
      else if h = "version" then 
        match t with
        | "history" :: t' ->
          begin
            if List.length  t' != 0 then VersionHistory (List.hd t') 
            else raise Malformed
          end
        | _ -> raise Malformed
      else if h = "add" then
        if List.length t != 0 then Add t else raise Malformed
      else if h = "help" then
        if List.length t = 0 then Help else raise Malformed
      else if h = "remove" then 
        if List.length t != 0 then Remove t else raise Malformed 
      else if h = "files" then
        if List.length t = 0 then Files else raise Malformed
      else if h = "switch" then
        match t with 
        | "user" :: t' -> 
          begin 
            if List.length t' = 0 then SwitchUser 
            else raise Malformed 
          end
        | _ -> raise Malformed
      else raise Malformed
    end 
(* end *)