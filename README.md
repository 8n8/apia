# APIA
## Description
APIA (A Prize is Assured) is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia. 

## Time format
The time is in days since 00:00 on 1st October 2016 in UTC, so there are no years, months, weeks, hours, minutes or seconds, just days.  This is very convenient for programming and reasoning with.

## Examples
Start work on a task with two tags, `work` and `code`:
```apia clockin work code```

Stop work:
```apia clockout```

Make a chart showing a breakdown of the work done today:
```apia today```

## Installation

You will need the Haskell Tool Stack installed.  I have only tested this installation in Arch Linux.  After cloning this repository, run `stack setup`, then `stack build` in it to compile.  You can then run the Apia commands from within the directory containing the code by preceding them with `stack exec`. The usage instructions can be viewed with `stack exec apia`.  The binary is buried quite deep inside a folder called .stack-work.  The exact path is shown after compiling with `stack build`.  I then made a link to it from one of the directories on my PATH with the command `ln -s /path/to/apia/binary /usr/bin`, so that I could use the `apia` command from any directory.


