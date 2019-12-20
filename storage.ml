open Diff

let delimiter = "##"

(** [return_state tup] takes in the tuple [tup] of Versions and returns 
    the value. *)
let return_state tup = 
  match tup with 
  | (i, st) -> st

(** [return_state tup] takes in the tuple [tup] of Versions and returns 
    the key. *)
let return_line_num tup = 
  match tup with 
  | (i, st) -> i

(** [state_to_string state] returns the to_string conversion of [state].
    Each key and value are formatted together and then separated by an 
    indentifiable sequenced to be indentified later for parsing. *)
let state_to_string (state : state) = 
  let bnd = State.bindings state in 
  let rec helper b s = 
    match b with 
    | [] -> s 
    | h :: t -> 
      begin 
        let v = return_state h in 
        let k = return_line_num h in 
        match v with 
        | C str -> 
          let txt = (string_of_int k) ^ ": c of [" ^ str ^ "]" ^ delimiter 
          in helper t (txt ^ s)
        | D -> 
          let txt = (string_of_int k) ^ ": d" ^ delimiter in 
          helper t (txt ^ s)
      end 
  in helper bnd ""

(** [str_to_change str text] returns [text] in the form of a change based on 
    the value of [str]. *)
let str_to_change str text = 
  if str = "c" then C text 
  else D 

(** [string_to_kv str] returns a tuple of the line number and the change 
    corresponding to the information in the string [str] by parsing it for 
    known identifiers of where each piece of information for that specific key
    and value are. *)
let string_to_kv str = 
  let colon_index = String.index str ':' in 
  let line_num = int_of_string (String.sub str 0 colon_index) in 
  let ch_str = String.sub str (colon_index+2) 1 in 
  if ch_str = "d" then (line_num, D)
  else 
    let begin_index = String.index str '[' in 
    let end_index = String.index str ']' in 
    let text = String.sub str (begin_index+1) (end_index - begin_index - 1) in 
    let chng = str_to_change ch_str text in 
    (line_num, chng)

(** [str_splitter str] returns a string list by parsing [str] based on the 
    positions of the delimiter. *)
let str_splitter str  = 
  let e = Str.regexp delimiter in 
  Str.split e str

(** [parse_state_regex str] returns a State map with keys being the line 
    numbers and values being the change data types from string [str]. *)
let parse_state_regex str = 
  let raw_str_arr = str_splitter str in 
  let map = State.empty in 
  let rec parse_helper lst (m : 'a Diff.State.t) = 
    match lst with
    | [] -> m 
    | h :: t -> 
      begin
        let tup = string_to_kv (h ^ delimiter) in 
        match tup with 
        | (k, v) -> parse_helper t (State.add k v m)
      end 
  in 
  parse_helper raw_str_arr map

(** [read_file file] reads in a file corresponding to filename [file] and 
    returns a Versions map with keys corresponding to the version number
    and values being the states that correspond to that version number. *)
let read_file file = 
  let ver = Versions.empty in 
  let ch = open_in file in 
  let rec read_versions chnl v c = 
    try 
      let line = input_line chnl in 
      read_versions chnl (Versions.add c (parse_state_regex line) v) (c+1)
    with End_of_file -> v 
  in let ret_val = read_versions ch ver 0 in 
  let() = close_in ch in 
  ret_val

(** [write_file ver name] writes the Versions map [ver] to a file with name 
    [name] to be stored when the system is turned off and then read in at a 
    later time. *)
let write_file (ver : Diff.state Diff.Versions.t) name = 
  let ch = open_out name in 
  let bnd = Versions.bindings ver in 
  let rec helper b = 
    match b with 
    | [] -> ()
    | h :: t -> 
      begin 
        match h with 
        | (k, v) -> 
          let txt = (state_to_string v) in 
          Stdlib.output_string ch (txt ^ "\n");
          helper t
      end 
  in let ret_val = helper bnd in 
  let () = close_out ch in 
  ret_val

(** [compare_versions v1 v2] compares version maps [v1] and [v2]. Returns true
    if they are equivalent and false otehrwise. *)
let compare_versions v1 v2 = 
  if (Versions.cardinal v1 != Versions.cardinal v2) then false 
  else 
    let bnd1 = Versions.bindings v1 in 
    let bnd2 = Versions.bindings v2 in 
    let rec helper b1 b2 = 
      match b1 with 
      | [] -> List.length b2 = 0
      | h :: t -> 
        begin 
          match b2 with 
          | [] -> false
          | he :: ta ->
            begin 
              let st1 = return_state h in 
              let st2 = return_state he in
              let i1 = return_line_num h in 
              let i2 = return_line_num he in 
              if (i1 != i2) then false  
              else if State.equal (=) st1 st2 then helper t ta
              else false
            end 
        end 
    in helper bnd1 bnd2