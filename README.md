# Lumberjack
## A logical extension for quick note-taking.
<br/>
One of the main barriers to consistent note-taking is the time it takes to write a note.   
The text file based note-taking described below is quite fast.
Lumberjack is an extension to make it easier to interact with this file and aggregate the notes you want.

### Setup Instructions
1. Clone repository
2. Designate a .txt file you want to use to store the notes and put this is in place of <notes.txt>
3. Do 1 & 2 of Lumberjack Extension Workflow below
4. Set up .bash_profile to make it easier to use
5. Follow Quick Notes Workflow for day-to-day use

### Quick Notes WorkFlow
1. Type ff into your terminal
2. Type the text of the note
3. <kbd>ctrl</kbd>+<kbd>d</kbd>+<kbd>d</kbd> to end the note

### Lumberjack Extension Workflow 
  1. ```eval `opam config env` ```
  2. compile with ```make note```
  3. run commands `./main.byte -init ~/notes.txt <commands>`
  *  `--get-date 5/8 ` Prints all notes from this day. Date must be of form MM/DD/YYYY//HH:MM:SS
  * ```--get-dates 5/8-6/8``` Prints all notes from this range. ^^
  * ```--get-all``` Prints all notes
  * ```-find <string>``` Prints all notes that contain string
  * ```-metrics``` Prints number of characters for each month
  * ```--find-count <string>``` Prints number of occurences of string
  
 ### In the .bash_profile
  There is likely a better way but this is how I connected the terminal to lumberjack.
  
```alias ff='echo $'\n' >> ~/notes.txt;echo $'\n' >> ~/notes.txt; date "+%m/%d/%Y %H:%M:%S" >> ~/notes.txt; cat >> ~/notes.txt;'```

Add the functions you want from the commands above

```alias ff-metrics='ocamlrun ~/lumberjack/main.byte -init ~/notes.txt -metrics'```

```alias ff-find=find```

```alias ff-fc=find_count```
```
find () {
        ocamlrun ~/lumberjack/main.byte -init ~/notes.txt -find $1
}
```
```
find_count () {
        ocamlrun ~/lumberjack/main.byte -init ~/notes.txt --find-count $1
}
```
  
