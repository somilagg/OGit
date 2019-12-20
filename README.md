# OGit

*created by: Somil Aggarwal, Samik Shrotria, and Neil Patel*

OGit is our pseduo interpretation of the git system used widely
today. At the core of this project is our modified implementation of OCaml's
Odiff library, which allows us to percieve differences between files. OGit 
allows the user to use this library interactively, with added functionality.

---

OGit implements a user-file association system that determines whether a user
can access a certain file, and these file permissions can be changed via
the commands listed below. The user enters the system by other logging in, 
or signing up. Upon shutting off the system and returning to it, OGit will
remember previously created users and the files associated with their accounts.

The full capabilities (commands) of this system are listed as follows:

1. `Push`

When changes are made to a file, the user can push this file to and have the
pushed iteration of the file as the next version. 

2. `Pull`

When a user has access to a file, they are able to pull whichever version
they would like to and have access to an editable version of that file. 

3. `Version History`

The user is also able to see the contents of a specific version in the file's
version history using this command.

4. `Add`

To initiate the tracking of a certain file, the user must add the file to the
tracking system so that OGit allows the user to push, pull, etc. 

5. `Remove`

If the user wishes to stop tracking a certain file, they can remove it from 
the system. 

6. `Switch User`

If the user wishes to switch accounts, they have the ability to do so while
preserving file permissions. 