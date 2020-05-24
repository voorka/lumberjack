# Lumberjack
## A logical extension for quick note-taking.
<br/>
One of the main barriers to consistent note-taking is the time it takes to write a note.   
The text file based note-taking described below is quite fast.
Lumberjack is an extension to make it easier to interact with this file and aggregate the notes you want.

### Setup Instructions
1. Clone repository
2. Designate a .txt file you want to use to store the notes and put this is in place of <notes.txt>
3. ```eval `opam config env` ```
4. compile with ```make note```
5. Set up .bash_profile

### Quick Notes WorkFlow
1. Type ff into your terminal
2. Type the text of the note
3. <kbd>enter</kbd>
  
 ### In the .bash_profile
```
lumberjack () {
    ocamlrun ~/lumberjack/main.byte -i <notes.txt> "$1" "$2"
}

alias ff=lumberjack
```

### Flags
```
  -gd Prints note from date to stdout. Date must be of form MM/DD/YYYY
  -gds Prints note from date range to stdout. Date range must be of form MM/DD/YYYY-MM/DD/YYYY
  -ga Prints all notes
  -f Finds all notes containing keyword
  -m Prints character count metrics from past months
  -nc Prints the total number of notes
  -fc Prints the number of notes containing keyword
  -r Generate a random note template
  -n Write a new note
 ```


