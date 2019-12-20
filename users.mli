(** Module that handles the user and file association system. *)

(** [make_initial_user_file] makes the inital version of the users file. *)
val make_initial_user_file : unit

(** [add_user username password file] adds a username [username] and password 
    [password] to the specified file [file] with an empty associated file 
    list. *)
val add_user : string -> string -> string -> unit 

(** [get_password user file] gets the password from the given username [user] 
    in the specified file [file]. *)
val get_password : string -> string -> string option

(** [get_total users file] gets the total number of users in the specified 
    file [file]. *)
val get_total_users : string -> int

(** [check_repeated_username user file] checks to see if the given username 
    [user] already exists in the specified file [file]. *)
val check_repeated_username : string -> string -> bool

(** [add_file_to_user u f file] adds the filename [f] to the associated 
    file list of the user [u] in the specified file [f]. *)
val add_file_to_user : string -> string -> string -> unit

(** [check_valid_username u] checks to see if the given username [u] is of the 
    appropriate length. *)
val check_valid_username : string -> bool

(** [check_valid password p u] checks to see if the given password [p] is of 
    the appropriate length and is not the same as the associated 
    username [u]. *)
val check_valid_password : string -> string -> bool 

(** [find_user_position user file] finds the position of the username [user] 
    in the specified file [file] if it exists as an [int option] and returns 
    [None] if it does not exist. *)
val find_user_position : string -> string -> int option

(** [create_file_list line] breaks apart a string [line] of file names into a 
    string list with each element being the name of a file. *)
val create_file_list : string -> string list

(** [user_files username file] returns the string list of files associated 
    with the inputted username [username] in the given file [file] based on 
    the filename inputted. *)
val user_files : string -> string -> string list

(** [remove_file_from_user u f file] removes a file [f] from the user [u] in 
    the user association system file [file].*)
val remove_file_from_user : string -> string -> string -> unit