(** [make_inital_user_file] generates a new version of [.users_file/.users.txt]. *)
let make_initial_user_file = 
  try
    Unix.mkdir ".users_file/" 493;
    let ch = open_out ".users_file/.users.txt"
    in close_out ch
  with Unix.Unix_error (_,_,_) -> ()

(** [add_user username password file] adds a user with username [username] and 
    hashed version of password [password] to the file [file].*)
let add_user username password file = 
  (* IMPLEMENT HASHING STUFF HERE *)
  let p_hash = password in 
  let ch = open_out_gen [Open_append; Open_creat] 0o666 file in 
  let () = output_string ch (username ^ "\n") in 
  let () = output_string ch (p_hash  ^ "\n") in 
  output_string ch "\n";
  flush ch

(** [get_password user file] returns the password associated with user
    with username [user] in file [file]. *)
let get_password user file = 
  let ch = open_in file in 
  let rec helper chnl = 
    try
      let u = input_line ch in 
      if u = user then 
        let p = input_line chnl in Some p
      else 
        let _ = input_line chnl in
        let _ = input_line chnl in  
        helper chnl
    with End_of_file -> None
  in helper ch 

(** [get_total_users file] gets the total number of users in file [file]. *)
let get_total_users file = 
  let ch = open_in file in 
  let counter = 0 in 
  let rec helper chnl c = 
    try 
      let _ = input_line chnl in 
      let _ = input_line chnl in 
      let _ = input_line chnl in 
      helper chnl (c+1)
    with End_of_file -> c
  in helper ch counter 

(** [check_repeated_username user file] checks to see if username [user] 
    already exists in file [file]. Returns false if there is a repeat. *)
let check_repeated_username user file = 
  let ch = open_in file in 
  let rec helper chnl = 
    try
      let u = input_line chnl in 
      if u = user then false 
      else 
        let _ = input_line chnl in 
        let _ = input_line chnl in 
        helper chnl
    with End_of_file -> true
  in helper ch 

(** [file_to_string_array file] returns a [string list] of the file with 
    filename [file]. *)
let rec file_to_string_array file = 
  let read_ch = open_in file in 
  let rec helper arr chnl = 
    try 
      let s = input_line chnl in 
      helper (s :: arr) chnl
    with End_of_file -> arr
  in helper [] read_ch

(** [add_file_to_user u f file] adds a file [f] to a username [u] for the user
    file association system in file with filename [file]. *)
let add_file_to_user u f file =
  let read_ch = open_in file in 
  let rec helper chnl c = 
    try
      let curr_user = input_line chnl in 
      let c = c + 1 in 
      if u = curr_user then 
        let _ = input_line chnl in 
        let c = c + 1 in 
        let curr_line = input_line chnl in 
        let c = c + 1 in 
        close_in chnl;
        let str_arr_file = List.rev (file_to_string_array file) in 
        let write_ch = open_out file in
        let rec write_to_file num arr = 
          if c = (num+1) then 
            begin 
              output_string write_ch (f ^" " ^ curr_line ^ "\n");
              match arr with 
              | [] -> close_out write_ch
              | h :: t -> write_to_file (num+1) t
            end 
          else 
            begin
              match arr with 
              | [] -> close_out write_ch
              | h :: t -> output_string write_ch (h^"\n");
                write_to_file (num+1) t
            end
        in write_to_file 0 str_arr_file;
      else 
        let _ = input_line chnl in 
        let c = c + 1 in 
        let _ = input_line chnl in 
        let c = c + 1 in 
        helper chnl c
    with End_of_file -> close_in chnl
  in helper read_ch 0;
  close_in read_ch

(** [check_valid_username u] checks username [u] to see if it is at least 5 
    characters long. *)
let check_valid_username u = 
  if String.length u < 5 then false else true

(** [check_valid_password p u] checks password [p] to see if it is at least 8
    characters long and is not the same as username [u]. *)
let check_valid_password p u = 
  if (String.length p < 8) || (p = u) then false else true

(* finds position in file based on username *)
(** [find_user_position user file] returns an int option either [Some] with the 
    position of username [user] in file [file] or [None] 
    if it does not exist. *)
let find_user_position user file = 
  let ch = open_in file in 
  let rec helper chnl =
    try 
      let u = input_line chnl in 
      if u = user then Some (pos_in chnl)
      else 
        let _ = input_line chnl in 
        let _ = input_line chnl in 
        helper chnl
    with End_of_file -> close_in chnl; None
  in helper ch

(* makes string list of file names associated with user *)
(** [create_file_list line] returns a string list of filenames from string 
    [line]. *)
let create_file_list line =
  let r = Str.regexp " " in 
  Str.split r line

(** [user_files username file] returns a list of the files associated with
    username [username] in the specified file with filename [file]. *)
let user_files username file = 
  let find_u = find_user_position username file in 
  let ch = open_in file in 
  match find_u with 
  | Some f_int ->
    let () = seek_in ch f_int in 
    let _ = input_line ch in 
    let h2 = input_line ch in 
    create_file_list h2
  | None -> []

(** [create_file_string arr s] returns a string concatenation of all the 
    elements in string array [arr]. *)
let rec create_file_string arr s = 
  match arr with 
  | [] -> s
  | h :: t -> create_file_string t (h ^ " " ^ s)

(** [remove_file_from_user u f file] removes the file [f] from username [u]
    in the user file association system. *)
let remove_file_from_user u f file = 
  let read_ch = open_in file in 
  let rec helper chnl c = 
    try
      let curr_user = input_line chnl in 
      let c = c + 1 in 
      if u = curr_user then 
        let _ = input_line chnl in 
        let c = c + 1 in 
        let curr_line = input_line chnl in 
        let c = c + 1 in 
        close_in chnl;
        let arr_rev = List.rev (file_to_string_array file) in 
        let write_ch = open_out file in 
        let str_arr = create_file_list curr_line in 
        let new_arr = List.filter (fun y -> f <> y) str_arr in 
        let new_line = create_file_string new_arr "" in 
        let rec write_to_file num arr = 
          if c = (num+1) then 
            begin 
              output_string write_ch (new_line ^ "\n");
              match arr with 
              | [] -> close_out write_ch
              | h :: t -> write_to_file (num+1) t
            end 
          else 
            begin
              match arr with 
              | [] -> close_out write_ch
              | h :: t -> output_string write_ch (h^"\n");
                write_to_file (num+1) t
            end
        in write_to_file 0 arr_rev;
        close_out write_ch
      else 
        let _ = input_line chnl in 
        let c = c + 1 in 
        let _ = input_line chnl in 
        let c = c + 1 in 
        helper chnl c
    with End_of_file -> close_in chnl;
  in helper read_ch 0;
  close_in read_ch