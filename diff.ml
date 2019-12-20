open Odiff
open Str
open Unix

(** [State] tracks the changes between two versions and helps us perform 
    efficient computation to generate the versions of specific files. *)
module State = Map.Make(Int)

(** [Versions] tracks the [state] of differences for one file, and records every
    version that was pushed to this system. *)
module Versions = Map.Make(Int)

(** [NonExistentVersion] is the exception when a version number is not found
    in a [version] map *)
exception NonExistentVersion

(** [differences] is the variant that specifies lines that were changed, 
    specifically if the line was deleted or the string in the line was changed.
    AF: C of s represents a new change with the new string s.
    D represents a deleted line. 
    RI: C of s must not have any new lines stored as s.
*)
type differences =
  | C of string
  | D

(** AF: the map {n0: d0; n1: d1; ...; nm: dm} represents the file with 
    [differences] d1 to dm, the line numbers n1 to nm, and the length of the 
    file written as a string in d0. the empty map represents no file. 
    RI: 0 does not map to a string of a line, it only maps to the length of a 
    file in a string representation. *)
type state = differences State.t

(** AF: the map {n0: s0; n1: s1; ...; nm: sm} represents the file with [state]
    s0 to sm, the version numbers n0 to nm
    the empty map represents no version for a file. 
    RI:  the size of the map should not exceed the number of versions pushed *)
type version = state Versions.t

(** Initially, our state and version are set to be empty, as no information
    should be present and initialized when we start a program. *)
let initial_version = Versions.empty

let initial_state = State.empty

(* [regex] is the regex that delimits new lines. *)
let regex = regexp "[\n]"

(** [get_lines acc f in_chan] gets all the lines in a file using in channels. *)
let rec get_lines acc f in_chan =
  try match input_line in_chan with
    | s -> get_lines (s :: acc) f in_chan
  with
  | End_of_file -> close_in in_chan; acc

(** [i -- j] is the list of integers from [i] to [j], inclusive.
    Tail recursive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(** [get_initial_version current_st_file] is the initial version of 
    [current_st_file], which is version 0. Every line in [current_st_file] is 
    added as a [differences] type for the initial version. In the state stored
    in the initial version, 0 maps to the length of the file, and all the other
    numbers map the line number to the [differences].
*)
let rec get_initial_version current_st_file : version = 
  (* [parse_file acc file_name] is the list of lines in [file_name].*)
  let parse_file acc file_name = 
    if (stat (file_name)).st_kind = S_REG then 
      match get_lines [] file_name (file_name |> open_in) with 
      | exception Unix_error(_,_,_) -> acc 
      | line_list -> line_list 
    else acc 
  in
  (* [add_num n acc lst] is the tuple list of line numbers from [n] to the size 
     of [lst] and of the strings in each line. *)
  let rec add_num n acc lst = 
    match lst with 
    | [] -> acc 
    | h::t -> add_num (n+1) ((n,h)::acc) t in 
  let st' = parse_file [] current_st_file 
            |> List.rev 
            |> add_num 1 [] 
            |> lines_to_initial_state (initial_state) in 
  let len = List.length (State.bindings st') in 
  let sta = State.add 0 (C (string_of_int len)) st' in
  Versions.add 0 sta (initial_version)

(*[lines_to_initial_state st str] is the state where every line in str is added
   as a line number starting from 1 to the size of the file (in line numbers).*)
and lines_to_initial_state (st:state) = function 
  | [] -> st 
  | (n, s) :: t -> lines_to_initial_state (State.add n (C s) st) t 

(* [compare_states acc_st st2_bindings] is the state generated after adding 
   [st2_bindings] to [acc_st]. This applies the changes generated in 
   [st2_bindings].
*)
let rec compare_states acc_st st2_bindings = 
  match st2_bindings with 
  | [] -> acc_st 
  | (loc, s)::t -> compare_states (State.add loc s acc_st) t 

(* [apply_changes_aux acc_st ver nums] is the [state] with all [differences] 
   applied in version numbers [nums].  *)
let rec apply_changes_aux acc_st (ver:version) nums = 
  match nums with 
  | [] -> acc_st
  | h :: t -> 
    try apply_changes_aux 
          (compare_states acc_st ((Versions.find h ver)|> State.bindings))
          ver t
    with Not_found -> raise NonExistentVersion

(** [apply_changes v_num ver] is the state of all the [differences] applied
    from version 0 to version [v_num] in [ver] using an integer list and the 
    states stored in [ver]. *)
let rec apply_changes v_num (ver:version) = 
  apply_changes_aux (State.empty) ver (0 -- v_num)

(* [string_of_state state] is the string representation of a [state]. *)
let string_of_state (state : differences State.t)  =
  let i = State.cardinal state in 
  let rec helper (state) (i : int) = 
    if i = -1 then "" 
    else 
      let chng = State.find_opt i state in 
      match chng with 
      | Some x -> 
        begin 
          match x with 
          | C str -> 
            (string_of_int i) ^ ": c of [" ^ str ^ "]\n" ^ (helper state (i-1))
          | D -> (string_of_int i) ^ ": d\n" ^ (helper state (i-1)) 
        end 
      | None -> "" ^ (helper state (i-1))
  in 
  helper state i

(* [string_of_version_aux str st' nums] is the string of all lines in [st']. 
   [st'] is the super [state], or the [state] with all lines in the specified
   version number. *)
let rec string_of_version_aux str st' = function
  | [] -> str
  | i :: t -> 
    begin
      match State.find i st' with
      | C s -> string_of_version_aux (str ^ "\n" ^ s) st' t
      | D -> string_of_version_aux str st' t
      | exception Not_found -> failwith (string_of_int i) 
    end

(** [string_of_version v_num (ver:version)] is the string of a version of a file
     and is used to compute and return version histories for end users. [v_num]
     is the version number the user specified, and [ver] is the version map of 
     the file specified. *)
let string_of_version v_num (ver:version) =
  let st' = apply_changes v_num ver in 
  let nums = (1 -- (List.length (State.bindings st') - 1)) in
  string_of_version_aux "" st' nums

(* [readable_of_state st] is the tuple list representation of a state. *)
let readable_of_state (st: state) =
  let bin = List.length (State.bindings st) - 1 in
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper ((h, State.find h st)::acc) t in
  helper [] (0--bin) |> List.rev

(* [append_many acc n string_list] is the [state] of the numbers in [n]
    matched with the strings in [string_list]. This is used for additions and
    changes, not deletions. *)
let rec append_many st n string_list = 
  match n, string_list with
  | [] , [] -> st 
  | [] , l -> st 
  | l , [] -> st
  | i :: t1 , s :: t2 -> append_many (State.add i (C s) st) t1 t2

(* [print_str_list str] is a unit. It prints out strings in 
   a string list [str].*)
let rec print_str_list = function
  | [] -> ()
  | s::t -> print_string (s^";"); print_str_list t

(* [one_many s i] is the [state] of all lines in [s] with the
    line numbers of any line that was changed. This is used for additions 
    and changes, not deletions.*)
let one_many st s id = 
  match id with
  | One n -> State.add n (C s) st
  | Many (n1,n2) -> append_many st (n1--n2) (split regex s)
(* (append_many st (n1--n2) (split regex s)) *)

(* [append_many_del st n ] is the [state] of the numbers in [n]
    matched with D in [differences]. This is used for deletions only. *)
let rec append_many_del st n = 
  match n with
  | [] -> st 
  | i :: t -> append_many_del (State.add i D st) t

(* [one_many_del st] is the [state] of all lines in [st] with the
    line numbers of any line that was changed. This is used for additions 
    and changes, not deletions.*)
let one_many_del st = function
  | One n -> State.add n D st
  | Many (n1,n2) -> (append_many_del st (n1--n2))

(* [convert_aux st o_diff] is the [state] of the indexes in [o_diff] added to
   [st]. *)
let rec convert_aux (st:state) o_diff = 
  match o_diff with
  | Odiff.Add (_,i2,s)  -> 
    one_many st (s) i2
  | Odiff.Change (i1, _, i2, s) -> one_many st s i2
  | Odiff.Delete (_, i2, s) -> one_many_del st i2

(** [convert_differences st o_diff_lst] is the [state] of [differences]
    computed using the OCamlDiff module. We convert additions and changes to C
    and we convert deletions to D. [o_diff_lst] is the list of all OCamlDiff
    differences that need to be converted into [differences] in [st]. *)
let rec convert_differences (st:state) o_diff_lst = 
  match o_diff_lst with
  | [] -> st
  | h :: t -> convert_differences (convert_aux st h) t

(** [create_file_from_num v_num ver f_name] is a unit. It outputs the string of
    the [v_num] version of [ver] in a file with the name [f_name] using out
    channels. *)
let create_file_from_num v_num ver f_name =
  let str_num = String.trim (string_of_version v_num ver) in
  let out_chan = open_out f_name in
  output_string out_chan str_num;
  flush out_chan

(** [file_to_string file] is the string with all lines in [file] outputted
    with "\n" injected in between each line. *)
let file_to_string file = 
  let chan = open_in file in
  let rec help chan str =
    try
      let line = input_line chan in
      help chan (str ^ "\n" ^ line)
    with End_of_file -> str
  in 
  let ret_val = help chan "" in 
  let () = close_in chan in 
  ret_val

(* [remove_bindings st lines_inc] is the state with all lines from [lines_inc]
   to the max number of lines in [st] removed (i.e. with D as the value). 
   [lines_inc] needs to be the number of lines in the file to be added + 1 so 
   that the final line in the file doesn't get removed. *)
let remove_bindings st lines_inc = 
  let rec remove_bindings_aux st = function
    | [] -> st
    | i :: t -> remove_bindings_aux (State.add i D st) t in
  let max = List.length (State.bindings st) - 1 in 
  if lines_inc > max then st
  else remove_bindings_aux st (lines_inc -- max)

(* [create_new_version ver file] is the version with a new state added. [ver] is
   the current version. [file] is the name of the file that needs to be computed 
   for [differences], and subsequently added as a state to compute a updated 
   version.
*)
let rec create_new_version (ver: version) file =
  let len = List.length (Versions.bindings ver) in 
  if len <= 0 then failwith "Ver doesn't have enough versions" 
  else 
    let x = string_of_version (len-1) ver in
    let y =  file_to_string file in 
    let str_of_ver = String.sub x 1 (String.length x - 1) in
    let line_number = List.length (split regex y) in
    let str_of_file = String.sub y 1 (String.length y - 1) in 
    let diff_list = strings_diffs str_of_ver str_of_file in
    let sup_st = apply_changes (len-1) ver in

    let st' = convert_differences 
        (State.add 0 (C (string_of_int line_number)) sup_st) diff_list in
    let st'' = remove_bindings st' (line_number + 1) in
    (* print_string (file);
       print_newline();
       print_string (string_of_state st'');
       print_string (string_of_int line_number); *)
    Versions.add len st'' ver 

