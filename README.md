# APIA
## Description
Apia is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia. 

## Installation

The installation has been tested in Ubuntu 16.04 and Fedora 26.

If you are used to using Git and the Haskell Tool Stack then you can just clone this repository and compile and install in the usual way.

If not, you should first make sure you have Git and Wget installed.  In Ubuntu they can be installed with the command:

```sudo apt install wget git```

and in Fedora with:
 
```sudo dnf install wget git```

Then run this command to download and run the installation script:

```sh -c "$(wget https://bitbucket.org/5-o/apia/raw/master/install.sh -O -)"```

This takes about 20 minutes on a clean Ubuntu install.

To uninstall this program, delete the file $HOME/.local/bin/apia.  To uninstall the Haskell Tool Stack, delete the directory $HOME/.stack

## Clock file format

The data file format follows these rules:

+ each work session is on a separate line

+ each line contains a list of tags separated by spaces, followed by the start and end times

+ a tag can be any sequence of non-space characters, but must contain at least one character that is not 0-9 or . (full stop)

+ only the session on the last line in the file is allowed to not have an end time, if the session has not ended yet

## Usage examples

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

The time is in days since 00:00 on 1st October 2016 in UTC, so there are no years, months, weeks, hours, minutes or seconds, just days.  The time outputs are in millidays (1 day / 1000).

The order of the tags does not matter.  For the `daily` option and similar, supplying a tag excludes all the records that don't have it, so providing no tags includes everything.

The day range results include both end days.  If just one day's results is required then the same day number should be given for the start and end.
