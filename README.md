# APIA
## Description
APIA (A Prize is Assured) is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia. 

## Time format
The time is in days since 00:00 on 1st October 2016 in UTC, so there are no years, months, weeks, hours, minutes or seconds, just days.  This is very convenient for programming and reasoning with.

## Usage
Start work on a task with two tags, `work` and `code`:

```apia clockin work code```

Switch to a task with tags `job` and `new`:

```apia switch job new``` 

Stop work:

```apia clockout```

Find out if you are currently clocked in:

```apia clockedin```

Make a chart showing a breakdown of the work done today:

```apia today```

Make a summary of the work done between day 100 and day 120:

```apia summary 100 200```

Show the amount of work done each day on the tags `work` and `code` between day 100 and day 120:

```apia daily 100 120 work code```

Make a list of the current tags:

```apia taglist```

Find the average daily work on the tags `work` and `code` between days 101 and 102:

```apia dailymean 101 102 work code```

The order of the tags does not matter.  For the `daily` option and similar, supplying a tag excludes all the records that don't have it, so providing no tags includes everything.

The day range results include both end days.  If just one day's results is required then the same day number should be given for the start and end.

To get the day number for today:

```apia now```

## Installation

I have only tested this installation in Arch Linux, but I don't see why it shouldn't work anywhere.  You will need the Haskell Tool Stack installed.  After cloning this repository, run `stack setup`, then `stack build` in it to compile.  You can then run the Apia commands from within the directory containing the code by preceding them with `stack exec`.  The binary is buried quite deep inside a folder called .stack-work.  The exact path is shown after compiling with `stack build`.  It is convenient to make a soft link to it from one of the directories on your PATH with the command 

```ln -s /path/to/apia/binary /usr/bin```

so that you can use the `apia` command from any directory.
