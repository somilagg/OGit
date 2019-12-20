open Command
open Users

module Files = Map.Make(String)

(** [files] is a map where the keys are the name of the file being stored and
    the values associated are the versions associated for each file. *)
type files = Diff.version Files.t

let empty_files =  Files.empty 

(** [print_string_array arr] prints the string array [arr]. *)
let print_string_array arr =
  let rec helper a = 
    match a with 
    | [] -> ()
    | h :: t -> 
      print_endline h;
      helper t
  in helper arr 

(** [valid_password_log_in username password] takes in the password and checks 
    if it matches password [password] and displays the files associated with 
    username [username] if it is valid. *)
let rec valid_password_log_in username password = 
  print_endline "Please type in your password.";
  print_string "> ";
  match password with 
  | Some pa -> 
    let p = read_line () in 
    if p = "quit" then 
      begin 
        print_string "Now exiting the system.\n";
        print_string "Thank you for using our service.\n";
        exit 0
      end  
    else if pa = p then 
      begin 
        print_endline "Correct login credentials!";
        print_endline "Getting your information now...";
        print_endline 
          "These are the files associated with your account:";
        let uf = user_files username ".users_file/.users.txt" in 
        print_string_array uf;
        username
      end 
    else 
      begin 
        print_endline "Wrong password.";
        print_endline "Please try again.";
        valid_password_log_in username password;
      end 
  | None -> ""

(** [valid_username_log_in ()] takes in the username in log in and checks if 
    it is valid. *)
let rec valid_username_log_in () = 
  print_endline "Please type in your username.";
  print_string "> ";
  let username = read_line() in 
  if username = "quit" then 
    begin 
      print_string "Now exiting the system.\n";
      print_string "Thank you for using our service.\n";
      exit 0
    end 
  else 
    let res = find_user_position username ".users_file/.users.txt" in 
    if res = None then
      begin 
        print_endline "That is not a registered username in this system.";
        print_endline "Please try again.";
        valid_username_log_in ();
      end
    else
      begin 
        let password = get_password username ".users_file/.users.txt" in 
        valid_password_log_in username password
      end

(** [valid_password_sign_up username] takes in the password and 
    displays the files associated with username [username] if it is valid 
    which would be none. *)
let rec valid_password_sign_up username = 
  print_endline "Please input the password you would like to use:";
  print_string "> ";
  let pswrd = read_line () in 
  if pswrd = "quit" then 
    begin 
      print_string "Now exiting the system.\n";
      print_string "Thank you for using our service.\n";
      exit 0
    end 
  else if (check_valid_password pswrd ".users_file/.users.txt") = false 
  then 
    begin 
      print_endline "That is either not a valid password.";
      print_endline "Your password must be:";
      print_endline "  (1) 8 characters or longer, and";
      print_endline "  (2) not the same as your username";
      print_endline "Please try again.";
      valid_password_sign_up username;
    end 
  else 
    begin 
      add_user username pswrd ".users_file/.users.txt";
      print_endline "We have successfully created your account!";
      print_endline "Now you can link files to your account."; 
      print_endline "You can keep track of them as you make changes!";
    end 

(** [valid_username_sign_up ()] takes in the username in sign up and checks if 
    it is valid. *)
let rec valid_username_sign_up () = 
  print_endline "Please enter the username you would like to use:";
  print_string "> ";
  let usr = read_line () in 
  if usr = "quit" then 
    begin 
      print_string "Now exiting the system.\n";
      print_string "Thank you for using our service.\n";
      exit 0
    end 
  else if (check_valid_username usr = false) then 
    begin 
      print_endline "Not a valid length for username.";
      print_endline "The username should be at least 5 characters long.";
      print_endline "Please try again";
      valid_username_sign_up ()
      (* [] *)
    end 
  else 
    begin 
      if (check_repeated_username usr ".users_file/.users.txt") = false 
      then
        begin 
          print_endline "This username has already been used.";
          print_endline "Please try again.";
          valid_username_sign_up ()
        end 
      else 
        begin 
          print_endline "That is a valid username!";
          valid_password_sign_up usr;
          usr
        end 
    end 

(**  [user_login ()] controls the users interaction with the log in or sign
     up system. *)
let rec user_login () = 
  print_endline "----------------";
  print_endline "Now starting the user login system.";
  let num = get_total_users ".users_file/.users.txt" in 
  print_endline ("We currently have " ^ (string_of_int num) ^ " users!");
  print_endline "Do you have an account or are you a new user?";
  print_endline "Please enter in lowercase.";
  print_endline "Please type \"log in\" if you have an account.";
  print_endline "Please type \"sign up\" if you are a new user.";
  print_endline "You can type quit at any point to exit.";
  print_string "> ";
  let inp = read_line () in 
  let v_files = if inp = "log in" then valid_username_log_in ()
    else if inp = "sign up" then valid_username_sign_up () 
    else if inp = "quit" then 
      begin 
        print_string "Now exiting the system.\n";
        print_string "Thank you for using our service.\n";
        exit 0
      end 
    else 
      let str = "Not a valid command. Please try again." in 
      print_endline str; 
      user_login()
  in v_files

(** [condense_file_list f_list] is a string with all the elements of [f_list] 
    with a comma seperating each element. (And also removes the trailing
    comma at the end. *)
let condense_file_list f_list =
  let rec help f_list =
    match f_list with
    | [] -> ""
    | h :: t -> h ^ ", " ^ help t in
  let trailing_comma = help f_list in
  let index = String.rindex trailing_comma ',' in 
  String.sub trailing_comma 0 index

(** [push fi f_list v_files] constructs a new version for each file in 
    [f_list] and replaces the old version with the new one in [fi], except
    if the file is not in [v_files]. *)
let rec push fi f_list v_files =
  match f_list with
  | [] -> fi
  | h :: t ->
    begin
      try
        if not (List.mem h v_files) then 
          let str = "You don't have access to file " ^ h ^ "\n" in
          print_string str;
          push fi t v_files
        else 
          let ver = Files.find h fi in
          let v_num = (Diff.Versions.cardinal ver) - 1 in
          let v_str = Diff.string_of_version v_num ver in
          let f_str = Diff.file_to_string h in
          let ver' = Diff.create_new_version ver h in
          let fi' = if v_str = f_str then fi else Files.add h ver' fi in
          push fi' t v_files
      with Not_found -> 
        print_string ("File " ^ h ^ " not found.\n");
        push fi t v_files
    end

(** [pull fi f v_num v_files] recreates the file [f] of version number [v_num] 
    using the version in [fi] only if file [f] is located in [v_files]. *)
let pull fi f v_num v_files =
  try
    if not (List.mem f v_files) then 
      print_string ("You don't have access to file " ^ f ^ "\n" )
    else 
      let ver = Files.find f fi in
      Diff.create_file_from_num v_num ver f
  with Not_found -> print_string ("File " ^ f ^ " not found")
     | Diff.NonExistentVersion ->
       print_endline "This version does not exist."

(** [version_history fi f v_num v_files] reconstructs the correct version 
    number [v_num] of file [f] and returns the string of that 
    using the version stored in [fi] if file [f] is located in [v_files]. *)
let version_history fi f v_num v_files =
  try 
    if not (List.mem f v_files) then 
      "You don't have access to file " ^ f ^ "\n"
    else
      let ver = Files.find f fi in
      let str = Diff.string_of_version v_num ver in
      str
  with Not_found -> "File " ^ f ^ " not found"
     | Diff.NonExistentVersion ->
       "This version does not exist."

(** [add fi f_list v_files username] constructs an initial version for the 
    files in [f_list] and adds the version to [fi]. If the file is already in 
    [fi] fi is unchanged. The file is also added to [v_files] and to user 
    [username] in the file that stores the user information. *)
let rec add fi f_list v_files username =
  match f_list with
  | [] -> (fi, v_files)
  | h :: t ->
    begin
      try 
        let init_ver = Diff.get_initial_version h in
        let fi' = if Files.mem h fi then fi else Files.add h init_ver fi in
        let v_files' = List.sort_uniq String.compare (h :: v_files) in
        add_file_to_user username h ".users_file/.users.txt";
        add fi' t v_files' username
      with Unix.Unix_error _ -> print_endline ("Can't find file " ^ h); 
        add fi t v_files username
    end 

(** [remove f_list v_files username] removes all the files in [f_list] 
    from [v_files] and the user [username] in the file that stores the user
    information. *)
let rec remove f_list v_files username =
  match f_list with
  | [] -> v_files
  | h :: t ->
    begin
      remove_file_from_user username h ".users_file/.users.txt";
      let v_files' = List.filter (fun x -> x <> h) v_files in 
      remove t v_files' username
    end 

(** [display_files v_files] is a string of all the elements in [v_files] 
    seperated by new line characters *)
let display_files v_files = 
  let rec help lst =
    match lst with
    | [] -> ""
    | h :: t -> h ^ "\n" ^ help t in
  let str = help v_files in
  if str = "" then "No Files Being Tracked\n" else str

(** [hidden_directories_and_file f_name] parses through the file path of 
    [f_name] creating directories if they are not create*)
let hidden_directories_and_file f_name = 
  let list = String.split_on_char '/' f_name in
  try 
    let f = List.hd (List.rev list) in
    let rec help lst fi =
      if List.length lst = 1 then fi ^ f 
      else match lst with
        | [] -> ""
        | h :: t ->
          let new_dir = fi ^ h   in 
          try 
            Unix.mkdir new_dir 493;
            help t (new_dir ^ "/")
          with Unix.Unix_error _ -> 
            help t (new_dir ^ "/") in 
    help list ""
  with Failure _ -> failwith ("No file in " ^ f_name)

(** [quit fi_bin] writes all the versions in [fi_bin] to hidden 
    storage directory, using the write_file method in Storage. *)
let rec quit fi_bin =
  match fi_bin with
  | [] -> ()
  | (k, v) :: t ->
    begin
      let dir = ".storage/" ^ k in 
      try 
        Storage.write_file v (hidden_directories_and_file dir);
        quit t
      with Sys_error _-> print_string "\nwrite file thwrowing code";
        quit t
    end

(** [start_system fi username v_files] takes in the users input and 
    then responds with the appropriate message based on the command the 
    user inputted, where [fi] is the map that stores the different file
     version, [username] is the name of the current user that is logged in,
     and [v_files] is a list of all the files that user [username] has 
     access to. *)
let rec start_system fi username v_files = 
  print_endline "----------------";
  print_string "\nWhat would you like to do?\n";
  print_string "> ";
  let str = read_line() in 
  try 
    let command = Command.parse str in 
    match command with 
    | Push f_list ->
      begin
        let f = condense_file_list f_list in
        print_string ("\nNow pushing files " ^ f ^ ".\n");
        print_string "A new version has now been generated.\n";
        print_string "Push complete.\n";
        let fi' = push fi f_list v_files in
        start_system fi' username v_files
      end
    | Pull f ->
      begin
        print_string ("Now pulling file" ^ f ^ ".\n");
        print_string "Our systems allows you to access all previous versions of this file.\n";
        print_string "Please enter the version number you would like to access:\n";
        print_string  "> ";
        let v_num = int_of_string (read_line()) in 
        pull fi f v_num v_files;
        start_system fi username v_files
      end
    | VersionHistory f ->
      begin 
        print_string "Now entering the version history viewring mode.\n";
        print_string ("In this mode, you will be able to see the previous" ^
                      "versions of your file.\n");
        print_string "Please enter the version number you would like to access:\n";
        print_string  "> ";
        let ver_num = int_of_string (read_line()) in 
        let str = version_history fi f ver_num v_files in
        print_string (str^"\n"); 
        start_system fi username v_files
      end
    | Add f_list ->
      begin
        let f = condense_file_list f_list in
        print_string ("Now adding file " ^ f ^ " to the tracking system.\n");
        print_string "You will now be able to push and pull versions as you make changes to this file.\n";
        let fi' = add fi f_list v_files username in
        match fi' with 
        | (x, y) -> 
          start_system x username y
      end
    | Remove f_list ->
      begin 
        let f = condense_file_list f_list in
        print_string ("Now removing file " ^ f ^ " from the tracking system.\n");
        print_string "You will no longer be able to push and pull versions of this file.\n";
        let v_files' = remove f_list v_files username in
        start_system fi username v_files'
      end
    | SwitchUser ->
      begin 
        print_endline "Now logging you out.";
        print_endline "Returning to the user log in system.";
        let user = user_login () in
        let v_files' = user_files user ".users_file/.users.txt" in
        start_system fi user v_files'
      end 
    | Help ->
      begin 
        print_string "The functions of our system are: \n";
        print_string "> push (filenames)\n";
        print_string "> pull (filename)\n";
        print_string "> version history (filename)\n";
        print_string "> add (filenames)\n";
        print_string "> remove (filenames)\n";
        print_string "> switch user\n";
        print_string "> files\n";
        print_string "> quit\n";
        start_system fi username v_files
      end
    | Quit ->
      begin
        print_string "Now exiting the system.\n";
        print_string "Thank you for using our service.\n";
        quit (Files.bindings fi);
        exit 0
      end 
    | Files ->
      begin
        print_string "Now printing all files being tracked.\n";
        let str = display_files v_files in
        print_string str;
        start_system fi username v_files
      end
  with Sys_error _ ->
    start_system fi username v_files 
     |
       Command.Malformed -> 
       print_string "Invalid command\n";
       start_system fi username v_files

(** [storage_str_list ()] is the list of all files within hidden 
    storage directory. Requires the .storage/ to already be created. *)
let storage_str_list () =
  let dir_handle = Unix.opendir ".storage/" in
  let rec help hdl dir acc =
    try
      let input = Unix.readdir hdl in
      if input = "." || input = ".." then help hdl dir acc else
        let str = dir ^ input in
        try
          let hdl' = Unix.opendir  str in
          help hdl' (str ^ "/") acc
        with Unix.Unix_error _ -> help hdl dir (str :: acc)
    with End_of_file -> Unix.closedir hdl; acc
  in
  help dir_handle ".storage/" []

(** [make_storage_dir ()] creates the hidden storage directory if it isn't
    already created and is the list of all files within the hidden storage
    directory. *)
let make_storage_dir () =
  try
    Unix.mkdir ".storage/" 493;
    storage_str_list ()
  with Unix.Unix_error _ -> 
    storage_str_list ()

(* [recreate_maps ()] reconstructs the Files map by parsing the files the 
   hidden storage directory. *)
let recreate_maps () =
  let lst = make_storage_dir () in 
  let rec help lst acc =
    match lst with 
    | [] -> acc
    | h :: t -> 
      begin
        let ver = Storage.read_file h in 
        let index = String.index h '/' + 1 in
        let new_len = String.length h - index in 
        let key = String.sub h index new_len in 
        let acc' = Files.add key ver acc in
        help t acc'
      end 
  in
  help lst Files.empty

(** [main ()] prompts for the system to begin operating, then begins execution
    of the users commands. *)
let main () =
  let _ = Sys.command "clear" in 
  ANSITerminal.(print_string [red]
                  "Welcome to OGit!\n");
  print_endline "by: Somil Aggarwal, Samik Shrotriya, and Neil Patel.\n";
  let fi = recreate_maps () in
  let username = make_initial_user_file; user_login () in
  let v_files = user_files username ".users_file/.users.txt" in
  start_system fi username v_files

(* Execute the git engine. *)
let () = main ()