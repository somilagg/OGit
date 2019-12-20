(* DIFF TESTING *)
(* Diff is comprised of the following functions, and we discuss why
   each function is explicitly tested or not. We tested this module with 
   black box testing by looking at the specification of [string_of_version]
   and coming up with different cases to try to break the system. For example, 
   we tested small files moving to larger sizes and large files moving to 
   smaller sizes.
   :
   -apply_changes: This was tested via the [string_of_version] tests. 
   [apply_changes] is an integral part of [create_new_version] (applies changes
   to generate a new version), which we have
   used in this test suite, and [create_new_version] is tested with 
   [string_of_version].
   -string_of_version: This was extensively tested because we needed to find
   the correct string representation of a version.
   -create_new_version: This was not explicitly tested because we wanted to 
   compare the string of the versions rather than the actual versions. We tested
   this function extensively using [string_of_version] tests because we would
   compare the string representation of newly generated versions.
   -convert_differences: This was tested via the [string_of_version] tests. 
   [convert_differences] is an integral part of [create_new_version] 
   (converts OCamlDiff differences to [differences] in order to compute a new
   version), which we have used in this test suite, and [create_new_version] 
   is tested with [string_of_version]. 
   -create_file_from_num: This function is an integral part of our main.ml file
   because it's needed for our pull command. We tested this using play testing
   because we didn't want to generate many files and because it was simpler to
   find bugs while play testing. 
   -file_to_string: This function is an integral part of our main.ml file
   because it's needed for our push command. We tested this using play testing
   because we didn't want to generate many files and because it was simpler to
   find bugs while play testing. 
*)

(* STORAGE TESTING using black box. *)
(* Storage is comprised of the following functions, and each's purpose and 
   rationalle for testing/not testing in the test suite will be explained: 
   - state_to_string: This was tested with a variety of 
     different state maps to see whether it returned the correct parsed format
     of the string. 
   - string_to_change: This was tested to see all possible combinations of 
     commands. Because we control the input, we did not have to test for 
     unexpected inputs. 
   - string_to_kv: This was tested against possible combinations of 
     the way a state could be parsed. 
   - str_splitter: This was tested against the different possible string
     versions of state maps. 
   - parse_state_regex: This was tested by  manually creating correct 
     state maps for the string we inputted and testing those results. 
   - read_file: This was tested by manually creating the correct version maps 
     and then testing the equivlance of the read_file outputs on manually
     created files for correctness. 
   - write_file: This was tested by creating the correct version of the output 
     lines, and then comparing the string versions of the files 
     contents against the output of this method for correctness. 
   - return_state, return_line_num, and compare_versions were all helper 
     methods that were tested individually with utop testing and also 
     implicitely had to work if the other methods were completing properly. *)

(* COMMAND TESTING *)
(* Command only had one method that needed to get tested, which is parse. 
   Testing for parse was done with black box testing. There was two tests that 
   were created for parse. One to test for a valid command being created and one
   to test for the correction execption to be raised. These tests were created 
   to ensure that parse method in command was creating commands and raising 
   execptions when given a certain input. Methods str_eval and str_splitter were
   helper methods that parse used and were tested with utop.
*)

(* USERS TESTING using black box testing. *)
(* Users is comprised of the following functions, and each's purpose and 
   rationalle for testing/not testing in the test suite will be explained: 
   - add_user: This was tested by adding users to a file and comparing the 
     manually written correct versions of the files. 
   - get_password: This was tested by getting known passwords from a file
     and testing if they were correct. 
   - get_total_users: This was tested with files in which the number of users 
     were known. 
   - check_repeated_username: This was checked with usernames that we knew 
     existed and didn't exist in our manually created file. 
   - check_valid_username: This was tested against usernames we knew did and 
     didn't work.
   - check_valid_password: This was tested against passwords we knew did 
     and didn't work. 
   - create_file_list: This was tested by passing in a string against the string 
     array we knew we wanted outputted. 
   - make_initial_user_file: We knew that this had to work for the environment 
     to set up properly. 
   - file_to_string_array, find_user_position, user_files, and 
     create_file_string were all helper methods that were tested individually
     with utop testing and also implicitely had to work if the other methods
     were completing properly. 
   - add_file_to_user and remove_file_from_user wre both testing with utop 
     testing because to test them using the test suite would've meant creating
     two files for every single test each, which would have been very cluttered
     and hard to navigate. *)

(* MAIN TESTING *)
(* The main module initial had test cases for the various commands, such as 
   add, remove, push, etc. This was to ensure that the files map being used 
   to store versions associated with each file was getting updated correctly.
   But over time with the integration with users and other updates to main, 
   the methods updated files were to unpredictable to test using OUnit. 
   So we decided to switch to utop testing to test each command that main and
   other invalid inputs.*)

open OUnit2
open Diff 
open Command
open Storage 
open Users
open Odiff
open Stream

let first = "this is a test\nwe are testing test\nthe current state";;
let one_more_line = "I am not but I still like apples. \n Apples rock \n 
Banas are ok too.";;
let one_less_line = "I dont like apples.";;
let str1 = "I like to eat bananas. \n And apples \ntoo.";;
let str2 = "I like to eat bananas. \n\n And apples \ntoo.";;

let orig_diffs = Odiff.strings_diffs str1 str2


let make_convert_differences_test 
    (name : string)
    (st : state) 
    (o_diff_lst : Odiff.diff list) 
    (output : (state)) : test = 
  name >:: (fun _ -> 
      assert_equal output (convert_differences st o_diff_lst))

let make_string_of_version_test 
    (name : string)
    (v_num : int) 
    (ver : version) 
    (output : string) : test = 
  name >:: (fun _ -> 
      assert_equal output (string_of_version v_num ver) ~printer:(fun x -> x))

let make_get_initial_version_test
    (name : string)
    (current_st_file: string)
    (output : version) : test = 
  name >:: (fun _ -> 
      assert_equal output (get_initial_version current_st_file))

let make_apply_changes_test
    (name : string)
    (v_num : int)
    (ver : version)
    (output : state) : test = 
  name >:: (fun _ -> 
      assert_equal output (apply_changes v_num ver)) 

let make_create_new_version_test
    (name : string)
    (ver : version)
    (file : string)
    (output : version) : test =
  name >:: (fun _ -> 
      assert_equal output (create_new_version ver file))

let ver = get_initial_version "diff_test/file0.txt";;
let ver' = create_new_version ver "diff_test/file1.txt";;
let ver_2 = create_new_version ver' "diff_test/file2.txt";;
let ver_3 = create_new_version ver_2 "diff_test/file3.txt";;
let ver_4 = create_new_version ver_3 "diff_test/file4.txt";;
let ver_5 = create_new_version ver_4 "diff_test/file5.txt";;
let ver_6 = create_new_version ver_5 "diff_test/file6.txt" 

let ver_6' = get_initial_version "diff_test/file6.txt" 
let ver_0' = create_new_version ver_6' "diff_test/file0.txt"
let ver_1' = create_new_version ver_0' "diff_test/file1.txt"
let ver_2' = create_new_version ver_1' "diff_test/file2.txt"
let ver_3' = create_new_version ver_2' "diff_test/file3.txt"
let ver_4' = create_new_version ver_3' "diff_test/file4.txt"
let ver_5' = create_new_version ver_4' "diff_test/file5.txt"

let ver_5'' = get_initial_version "diff_test/file5.txt"
let ver_0'' = create_new_version ver_5'' "diff_test/file0.txt"
let ver_1'' = create_new_version ver_0'' "diff_test/file1.txt"
let ver_2'' = create_new_version ver_1'' "diff_test/file2.txt"
let ver_3'' = create_new_version ver_2'' "diff_test/file3.txt"
let ver_4'' = create_new_version ver_3'' "diff_test/file4.txt"
let ver_6'' = create_new_version ver_4'' "diff_test/file6.txt"


let ver_4''' = get_initial_version "diff_test/file4.txt"
let ver_6''' = create_new_version ver_4''' "diff_test/file6.txt"
let ver_1''' = create_new_version ver_6''' "diff_test/file1.txt"
let ver_3''' = create_new_version ver_1''' "diff_test/file3.txt"
let ver_5''' = create_new_version ver_3''' "diff_test/file5.txt"
let ver_2''' = create_new_version ver_5''' "diff_test/file2.txt"
let ver_0''' = create_new_version ver_2''' "diff_test/file0.txt"


let ver_2'''' = get_initial_version "diff_test/file2.txt"
let ver_5'''' = create_new_version ver_2'''' "diff_test/file5.txt"
let ver_1'''' = create_new_version ver_5'''' "diff_test/file1.txt"
let ver_3'''' = create_new_version ver_1'''' "diff_test/file3.txt"
let ver_6'''' = create_new_version ver_3'''' "diff_test/file6.txt"
let ver_4'''' = create_new_version ver_6'''' "diff_test/file4.txt"
let ver_0'''' = create_new_version ver_4'''' "diff_test/file0.txt"
let re_ver_2'''' = create_new_version ver_0'''' "diff_test/file2.txt" 


(*
  let doub_v2 = get_initial_version "file2.txt"
  let doub_v2' = create_new_version doub_v2 "file2.txt"
  let doub_v1 = create_new_version doub_v2' "file1.txt"
  let doub_v1' = create_new_version doub_v1 "file1.txt" *)



let diff_tests = [

  make_string_of_version_test "init" 0 (ver) 
    "\nthis is a test\nwe are testing test\nthe current state";

  make_string_of_version_test "file1 changing one line" 1 (ver')
    "\nthis is a test\nwe are testing\nthe current state";

  make_string_of_version_test "file2 adding empty line" 2 (ver_2)
    "\nthis is a test\nwe are testing\nNew State";

  make_string_of_version_test "file3 adding many empty lines" 3 (ver_3)
    "\nthis is a test\nwe are testing\nThird Version Testing\n\n\n\n\nf"; 

  (* Does not delete bottom lines properly. *)
  make_string_of_version_test "file4 delete all lines, add one space" 4 (ver_4)
    "\n ";

  (* Inserts only 9 lines instead of 10...*)
  make_string_of_version_test "file5 add many empty lines" 5 (ver_5)
    "\n \n \n \n \n \n \n \n \n ";  

  make_string_of_version_test "file6 add many lines and empty lines" 6 
    (ver_6) 
    "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n\nclean dishes\nln 6, col11\n\n\nutf-8\n\nmerlin\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nplain test\n\n\nlet x = 5\nfor i in range(5):\n  print(\"hi\")"; 
  (* "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n \nclean dishes\nln 6, col11\n \n \nutf-8\n \nmerlin\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \nplain test\n \n \nlet x = 5\nfor i in range(5):\n  print(\"hi\")";   *)
  make_string_of_version_test "init file6 big" 0 (ver_6') 
    "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n\nclean dishes\nln 6, col11\n\n\nutf-8\n\nmerlin\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nplain test\n\n\nlet x = 5\nfor i in range(5):\n  print(\"hi\")"; 

  (* Going from state with many lines to state with few lines is not 
     correct. *)
  make_string_of_version_test "file0 from big" 1 (ver_0')
    "\nthis is a test\nwe are testing test\nthe current state";

  make_string_of_version_test "file1 changing one line" 2 (ver_1')
    "\nthis is a test\nwe are testing\nthe current state";

  make_string_of_version_test "file2 adding empty line" 3 (ver_2')
    "\nthis is a test\nwe are testing\nNew State";

  make_string_of_version_test "file3 adding many empty lines" 4 (ver_3')
    "\nthis is a test\nwe are testing\nThird Version Testing\n\n\n\n\nf"; 

  make_string_of_version_test "file4 delete all lines, add one space" 5 
    (ver_4')
    "\n ";

  make_string_of_version_test "file5 add many empty lines" 6 (ver_5')
    "\n \n \n \n \n \n \n \n \n "; 

  make_string_of_version_test "init file5 empty" 0 (ver_5'')
    "\n \n \n \n \n \n \n \n \n \n ";

  make_string_of_version_test "file0 from big" 1 (ver_0'')
    "\nthis is a test\nwe are testing test\nthe current state";

  make_string_of_version_test "file1 changing one line" 2 (ver_1'')
    "\nthis is a test\nwe are testing\nthe current state";

  make_string_of_version_test "file2 adding empty line" 3 (ver_2'')
    "\nthis is a test\nwe are testing\nNew State";

  make_string_of_version_test "file3 adding many empty lines" 4 (ver_3'')
    "\nthis is a test\nwe are testing\nThird Version Testing\n\n\n\n\nf";

  make_string_of_version_test "file4 delete all lines, add one space" 5 
    (ver_4'')
    "\n ";

  make_string_of_version_test "file6 add many lines" 6 (ver_6'')
    "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n\nclean dishes\nln 6, col11\n\n\nutf-8\n\nmerlin\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nplain test\n\n\nlet x = 5\nfor i in range(5):\n  print(\"hi\")";

  make_string_of_version_test "file4 init" 0 (ver_4''')
    "\n ";

  make_string_of_version_test "file6 add many lines" 1 (ver_6''')
    "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n\nclean dishes\nln 6, col11\n\n\nutf-8\n\nmerlin\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nplain test\n\n\nlet x = 5\nfor i in range(5):\n  print(\"hi\")";

  make_string_of_version_test "file1 line deletion" 2 (ver_1''') 
    "\nthis is a test\nwe are testing\nthe current state";


  make_string_of_version_test "file3 line insertion plus spaces" 3 (ver_3''')
    "\nthis is a test\nwe are testing\nThird Version Testing\n\n\n\n\nf";


  make_string_of_version_test "file5 line deletion" 4 (ver_5''')
    "\n \n \n \n \n \n \n \n \n \n ";

  make_string_of_version_test "file2 line change" 5 (ver_2''')
    "\nthis is a test\nwe are testing\n\nNew State";

  make_string_of_version_test "file0 line deletions" 6 (ver_0''')
    "\nthis is a test\nwe are testing test\nthe current state";

  make_string_of_version_test "file2 init" 0 (ver_2'''') 
    "\nthis is a test\nwe are testing\n\nNew State";

  make_string_of_version_test "file5 empty lines" 1 (ver_5'''')
    "\n \n \n \n \n \n \n \n \n \n ";

  make_string_of_version_test "file1 adding lines" 2 (ver_1'''')
    "\nthis is a test\nwe are testing\nthe current state";

  make_string_of_version_test "file3 adding many lines" 3 (ver_3'''')
    "\nthis is a test\nwe are testing\nThird Version Testing\n\n\n\n\nf";

  (* make_string_of_version_test "file6 adding even more lines" 4 (ver_6'''')
     (* "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n\nclean dishes\nln 6, col11\n\n\nutf-8\n\nmerlin\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nplain test\n\n\nlet x = 5\nfor i in range(5):\n  print(\"hi\")"; *)
     "\nhi my name is\ndisk is almost full\nsave space by optimizing storage. \n \nclean dishes\nln 6, col11\n \n \nutf-8\n \nmerlin\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \nplain test\n \n \nlet x = 5\nfor i in range(5):\n  print(\"hi\")"; *)

  make_string_of_version_test "file4 line deletion" 5 (ver_4'''')
    "\n ";

  make_string_of_version_test "file0 line addition" 6 (ver_0'''')
    "\nthis is a test\nwe are testing test\nthe current state";

  make_string_of_version_test "file2 redo" 7 (re_ver_2'''')
    "\nthis is a test\nwe are testing\n\nNew State";   

]

(* COMMAND *)
(** [make_parse_tests name str cmnd] constructs an OUnit test named [name]
    that asserts the equality between [cmnd] and [Command.parse str]. *)
let make_parse_tests 
    (name : string)
    (str : string)
    (cmnd : Command.command) : test =
  name >:: (fun _ -> 
      assert_equal cmnd (Command.parse str))

(** [make_invalid_parse_tests name str expected_output] constructs an OUnit
    test named [name] that verifies that the expection [expected_output] is
     raised by [Command.parse str]. *)
let make_invalid_parse_tests
    (name : string)
    (str : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (fun () -> Command.parse str))

let command_tests = [
  make_parse_tests "push 1 file" "push sample.txt" (Push ("sample.txt" :: []));
  make_parse_tests "push 2 files" "push sample.txt sample2.txt" 
    (Push ("sample.txt" :: "sample2.txt" ::[]));
  make_parse_tests "pull 1 file" "pull sample.txt" (Pull "sample.txt");
  make_parse_tests "quit" "quit" (Quit);
  make_parse_tests "verion history 1" "version history 1" (VersionHistory "1");
  make_parse_tests "add 1 file" "add sample.txt" (Add ("sample.txt" :: []));
  make_parse_tests "add 2 files" "add sample.txt sample2.txt" 
    (Add ("sample.txt" :: "sample2.txt" :: []));
  make_parse_tests "help" "help" (Help);
  make_parse_tests "remove 1 file" "remove sample.txt" 
    (Remove ("sample.txt" :: [] ));
  make_parse_tests "remove 2 files" "remove sample.txt sample2.txt" 
    (Remove ("sample.txt" :: "sample2.txt" :: []));
  make_parse_tests "files command" "files" (Files);

  make_invalid_parse_tests "Empty command" "" Command.Empty;
  make_invalid_parse_tests "Invalid push command construction" 
    "push" Command.Malformed;
  make_invalid_parse_tests "Invalid pull command construction"
    "pull" Command.Malformed;
  make_invalid_parse_tests "Invalid quit command construction"
    "quit test" Command.Malformed;
  make_invalid_parse_tests "Invalid version history command construction"
    "version history" Command.Malformed;
  make_invalid_parse_tests "Invalid add command construction"
    "add" Command.Malformed;
  make_invalid_parse_tests "Invalid remove command construction"
    "remove" Command.Malformed;
  make_invalid_parse_tests "Invalid help command construction"
    "help test" Command.Malformed;
  make_invalid_parse_tests "Invalid files command construction"
    "files test" Command.Malformed;
]

(* STORAGE *)
(** [make_state_to_string_tests name state_map output_string] constructs an OUnit
    test named [name] that asserts the quality of [output_string]
    with [state_map]. *)
let make_state_to_string_tests
    (name : string)
    (state_map : differences State.t)
    (output_string : string) : test = 
  name >:: (fun _ -> 
      assert_equal output_string (state_to_string state_map))

(** [make_string_to_change_tests name str1 str2 chng] constructs an OUnit
    test named [name] that asserts the quality of [chng]
    with [str1 str2]. *)
let make_string_to_change_tests
    (name : string)
    (str1 : string)
    (str2 : string) 
    (chng : differences) : test = 
  name >:: (fun _ -> 
      assert_equal chng (str_to_change str1 str2))

(** [make_string_to_kv_tests name str output_tuple] constructs an OUnit
    test named [name] that asserts the quality of [output_tuple]
    with [str]. *)
let make_string_to_kv_tests
    (name : string)
    (str : string)
    (output_tuple : int * differences) : test =
  name >:: (fun _ ->
      assert_equal output_tuple (string_to_kv str))

(** [make_str_splitter_tests name str output_lst] constructs an OUnit
    test named [name] that asserts the quality of [output_lst]
    with [str]. *)
let make_str_splitter_tests
    (name : string)
    (str : string)
    (output_lst : string list) : test = 
  name >:: (fun _ ->
      assert_equal output_lst (str_splitter str))

(** [make_parse_state_regex_tests name str output_map] constructs an OUnit
    test named [name] that asserts the quality of [output_map]
    with [str]. *)
let make_parse_state_regex_tests
    (name : string)
    (str : string)
    (output_map : differences State.t) : test = 
  name >:: (fun _ -> 
      assert_bool "not equal" (State.equal (=) output_map (parse_state_regex str)))

(** [make_read_file_tests name filename ver_map] constructs an OUnit
    test named [name] that asserts the quality of [ver_map]
    with [filename]. *)
let make_read_file_tests
    (name : string)
    (filename : string)
    (ver_map : Diff.version) : test = 
  name >:: (fun _ ->
      assert_bool "not equal" (compare_versions ver_map (read_file filename)))

(** [make_write_file_tests name v n output_file] constructs an OUnit
    test named [name] that asserts the quality of [output_file]
    with [v n]. *)
let make_write_file_tests
    (name : string)
    (v : Diff.state Diff.Versions.t)
    (n : string)
    (output_file : string) : test = 
  name >:: (fun _ -> 
      write_file v n;
      assert_bool "not equal" ((file_to_string n) = 
                               (file_to_string output_file)))

let test_map =
  let s1 = State.add 1 (C "abc") State.empty in 
  let s2 = State.add 2 (D) s1 in 
  State.add 3 (C "d") s2

let map_text = "1: c of [abc]##2: d##3: c of [d]##"
let map_text_rev = "3: c of [d]##2: d##1: c of [abc]##"


let state1 = 
  State.add 1 (C "abc") State.empty

let state1_text = "1: c of [abc]##"

let state2 = 
  let v1 = State.add 1 (C "a") State.empty in 
  let v2 = State.add 2 (C "b") v1 in 
  let v3 = State.add 3 (C "c") v2 in 
  State.add 4 (C "d") v3

let state2_text = "4: c of [d]##3: c of [c]##2: c of [b]##1: c of [a]##"

let state3 = 
  State.add 2 (D) State.empty

let state3_text = "2: d##"

let ver_main = 
  let vm1 = Versions.add 0 state1 Versions.empty in 
  let vm2 = Versions.add 1 state2 vm1 in 
  Versions.add 2 state3 vm2

let state7 = 
  let d1 = State.add 1 (C "abc") State.empty in 
  State.add 2 (C "xyz") d1

let state7_text = "2: c of [xyz]##1: c of [abc]##"

let state8 = 
  let vv1 = State.add 1 (D) State.empty in 
  let vv2 = State.add 2 (D) vv1 in 
  State.add 3 (C "mno") vv2

let state8_text = "3: c of [mno]##2: d##1: d##"

let state9 = 
  let x1 = State.add 1 (D) State.empty in 
  let x2 = State.add 2 (C "l") x1 in 
  State.add 3 (D) x2

let state9_text = "3: d##2: c of [l]##1: d##"

let ver_main2 = 
  let vmm1 = Versions.add 0 state7 Versions.empty in 
  let vmm2 = Versions.add 1 state8 vmm1 in 
  Versions.add 2 state9 vmm2

let storage_tests = [
  (* state_to_string *)
  make_state_to_string_tests "state 1" state1 state1_text;
  make_state_to_string_tests "state 2" state2 state2_text;
  make_state_to_string_tests "state 3" state3 state3_text;
  make_state_to_string_tests "state 7" state7 state7_text;
  make_state_to_string_tests "state 8" state8 state8_text;
  make_state_to_string_tests "state 9" state9 state9_text;
  make_state_to_string_tests "empty state" State.empty "";
  make_state_to_string_tests "state string test 1" test_map map_text_rev;

  (* string_to_change *)
  make_string_to_change_tests "c valid" "c" "abc" (C "abc");
  make_string_to_change_tests "c empty" "c" "" (C "");
  make_string_to_change_tests "d valid" "d" "" (D);
  make_string_to_change_tests "d with text" "d" "abc" (D);

  (* string_to_kv *)
  make_string_to_kv_tests "c2 test" "1: c of [abc]##" (1, C "abc");
  make_string_to_kv_tests "c2 test2" "5: c of []##" (5, C "");
  make_string_to_kv_tests "d2 test" "10: d##" (10, D);
  make_string_to_kv_tests "d2 test" "2: d##" (2, D);

  (* str_splitter *)
  make_str_splitter_tests "split 1" "1: c of [a]##" ["1: c of [a]"];
  make_str_splitter_tests "split 2" "1: c of [a]##2: d##3: c of [abc]##" 
    ["1: c of [a]";"2: d";"3: c of [abc]"];
  make_str_splitter_tests "split 3" "1: c of [a]##2: d##" 
    ["1: c of [a]";"2: d"];
  make_str_splitter_tests "split empty" "" [];

  (* parse_state_regex *)
  make_parse_state_regex_tests "parse test 1" map_text test_map;
  make_parse_state_regex_tests "parse test 2" "" State.empty;
  make_parse_state_regex_tests "parse test 3" state1_text state1;
  make_parse_state_regex_tests "parse test 4" state2_text state2;
  make_parse_state_regex_tests "parse test 5" state3_text state3;
  make_parse_state_regex_tests "parse test 6" state7_text state7;
  make_parse_state_regex_tests "parse test 7" state8_text state8;
  make_parse_state_regex_tests "parse test 8" state9_text state9;

  (* read_file *)
  make_read_file_tests "read file 1" "resources/read_file_test.txt" ver_main;
  make_read_file_tests "read file 2" "resources/read_file_test2.txt" ver_main2;

  (* write_file *)
  make_write_file_tests "write file 1" ver_main "resources/write_file_test.txt" 
    "resources/write_real.txt";
  make_write_file_tests "write file 2" ver_main2 "resources/write_file_test2.txt"
    "resources/write_real2.txt"
]

(* USERS *)
(** [make_add_user_tests name u p f output_file] constructs an OUnit
    test named [name] that asserts the quality of [output_file]
    with [u p f]. *)
let make_add_user_tests
    (name : string)
    (u : string)
    (p : string)
    (f : string)
    (output_file : string) : test = 
  name >:: (fun _ ->
      add_user u p f;
      assert_bool "not equal" ((file_to_string f) = 
                               (file_to_string output_file)))

(** [make_get_password_tests name u f output_option] constructs an OUnit
    test named [name] that asserts the quality of [output_option]
    with [u p f]. *)
let make_get_password_tests
    (name : string)
    (u : string)
    (f : string)
    (output_option : string option) : test = 
  name >:: (fun _ ->
      assert_equal output_option (get_password u f))

(** [make_get_total_tests name f output_int] constructs an OUnit
    test named [name] that asserts the quality of [output_int]
    with [f]. *)
let make_get_total_users_tests
    (name : string)
    (f : string)
    (output_int : int) : test = 
  name >:: (fun _ ->
      assert_equal output_int (get_total_users f))

(** [make_check_repeated_username_tests name u f output_bool] constructs an OUnit
    test named [name] that asserts the quality of [output_bool]
    with [u f]. *)
let make_check_repeated_username_tests
    (name : string)
    (u : string)
    (f : string)
    (output_bool : bool) : test = 
  name >:: (fun _ ->
      assert_equal output_bool (check_repeated_username u f))

(** [make_check_valid_username_tests name u output_bool] constructs an OUnit
    test named [name] that asserts the quality of [output_bool]
    with [u]. *)
let make_check_valid_username_tests
    (name : string)
    (u : string)
    (output_bool : bool) : test = 
  name >:: (fun _ ->
      assert_equal output_bool (check_valid_username u))

(** [make_check_valid_password_tests name p u output_bool] constructs an OUnit
    test named [name] that asserts the quality of [output_bool]
    with [p u]. *)
let make_check_valid_password_tests
    (name : string)
    (p : string)
    (u : string)
    (output_bool : bool) : test = 
  name >:: (fun _ ->
      assert_equal output_bool (check_valid_password p u))

(** [make_create_file_list_tests name u output_list] constructs an OUnit
    test named [name] that asserts the quality of [output_list]
    with [u]. *)
let make_create_file_list_tests
    (name : string)
    (str : string)
    (output_list : string list) : test = 
  name >:: (fun _ ->
      assert_equal output_list (create_file_list str))

(* let () = make_initial_user_file
   let chnnl = open_out ".users_file/.users.txt"
   let () = close_out chnnl *)
let chnnl = open_out "resources/ut.txt"
let () = close_out chnnl 
let ut = "resources/ut.txt"
let users_tests = [
  (* add_user *)
  make_add_user_tests "add user test 1" "username1" "password1" 
    ut "resources/user_test1.txt"; 
  make_add_user_tests "add user test 2" "username2" "password2" 
    ut "resources/user_test2.txt";
  make_add_user_tests "add user test 3" "username3" "password3" 
    ut "resources/user_test3.txt";

  (* get_password *)
  make_get_password_tests "get password 1" "username1" ut
    (Some "password1");
  make_get_password_tests "get password 2" "username2" ut 
    (Some "password2");
  make_get_password_tests "get password 3" "username3" ut 
    (Some "password3");

  (* get_total_users *)
  make_get_total_users_tests "total 1" "resources/user_test1.txt" 1;
  make_get_total_users_tests "total 2" "resources/user_test2.txt" 2;
  make_get_total_users_tests "total 3" "resources/user_test3.txt" 3;

  (* check_repeated_username *)
  make_check_repeated_username_tests "repeat 1" "username1" 
    "resources/user_test1.txt" false;
  make_check_repeated_username_tests "nonrepeat 1" "username2" 
    "resources/user_test1.txt" true;
  make_check_repeated_username_tests "repeat 2" "username2" 
    "resources/user_test2.txt" false;
  make_check_repeated_username_tests "nonrepeat 2" "username3" 
    "resources/user_test2.txt" true;
  make_check_repeated_username_tests "repeat 3" "username3" 
    "resources/user_test3.txt" false;
  make_check_repeated_username_tests "nonrepeat 3" "username4" 
    "resources/user_test3.txt" true;

  (* COME UP WITH NEW CHECK AND IMPLEMENT - add_file_to_users *)

  (* check_valid_username *)
  make_check_valid_username_tests "invalid1" "a" false;
  make_check_valid_username_tests "valid1" "fullvalidname" true;
  make_check_valid_username_tests "invalid2" "no" false;
  make_check_valid_username_tests "valid2" "anotherfullname" true;

  (* check_valid_password *)
  make_check_valid_password_tests "same invalid1" "a" "a" false;
  make_check_valid_password_tests "same invalid1" "aaaaaaaaaa" "aaaaaaaaaa" 
    false;
  make_check_valid_password_tests "same invalid1" "ab" "ab" false;
  make_check_valid_password_tests "valid1" "username1" "password1" true;
  make_check_valid_password_tests "valid2" "username2" "password2" true;
  make_check_valid_password_tests "valid3" "username3" "password3" true;

  (* NOT SURE IF SHOULD TEST - find_user_position *)

  (* create_file_list *)
  make_create_file_list_tests "create file list test1" "a" ["a"];
  make_create_file_list_tests "create file list test2" "a b" ["a";"b"];
  make_create_file_list_tests "create file list test3" "c" ["c"];
  make_create_file_list_tests "create file list test4" "" [];
]
let suite = 
  "test suite for A2"  >::: List.flatten [
    diff_tests;
    command_tests;
    storage_tests;
    users_tests;
  ]


let _ = run_test_tt_main suite