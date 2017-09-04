# APIA
## Description
APIA (A Prize is Assured) is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia. 

## Installation

You should have git and wget installed.  Then run this command to download and run the installation script:

```sh -c "$(wget https://bitbucket.org/5-o/apia/raw/master/install.sh -O -)"```

This takes ages (~ 15 mins) on a clean Ubuntu install as it has to download and install all the Haskell stuff and compile everything.

To uninstall this program, delete the file ~/.local/bin/apia.  To uninstall all the Haskell stuff, delete the directory ~/.stack

## Clock file format

The data file format follows these rules:

+ each work session is on a separate line

+ each line contains a list of tags separated by spaces, followed by the start and end times

+ a tag can be any sequence of non-space characters, but must contain at least one character that is not 0-9 or . (full stop)

+ only the session on the last line in the file is allowed to not have an end time, if the session has not ended yet

## Usage examples

The time is in days since 00:00 on 1st October 2016 in UTC, so there are no years, months, weeks, hours, minutes or seconds, just days.

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

```apia summary 100 120```

Show the amount of work done each day on the tags `work` and `code` between day 100 and day 120:

```apia daily 100 120 work code```

Make a list of the current tags:

```apia taglist```

Find the average daily work on the tags `work` and `code` between days 101 and 102:

```apia dailymean 101 102 work code```

Get the current time:

```apia now```

The order of the tags does not matter.  For the `daily` option and similar, supplying a tag excludes all the records that don't have it, so providing no tags includes everything.

The day range results include both end days.  If just one day's results is required then the same day number should be given for the start and end.
