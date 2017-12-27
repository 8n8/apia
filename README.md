# Apia
## Description
Apia is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia.  It is useful for quickly logging what you are working on and summarising what you have done.

## Installation

You need to know how to use Git and the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).  Clone this repository, cd into it and run ```stack install```.

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
