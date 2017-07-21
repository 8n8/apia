# APIA
## Description
APIA (A Prize is Assured) is a personal work time logger written in Haskell.  It has a command-line interface.  The data is kept in a plain text file at ~/.apia. 

## Installation

I have only tested this installation in Arch Linux, but I don't see why it shouldn't work anywhere.  First [install the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).  Then clone this repository and run `stack setup` then `stack build` in it to compile.  

To make the `apia` command available in the terminal make sure there is a directory in the home folder called .local/bin.  Make it if it does not exist.  Check this directory is in the PATH variable with the command:
```env | grep "PATH"```
My output is:
```
MOZ_PLUGIN_PATH=/usr/lib/mozilla/plugins
PATH=/home/t/.local/bin:/home/t/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl
WINDOWPATH=1
```
You can see that there is a directory called .local/bin in my home directory in the list of directories in the PATH variable.  If it isn't there in yours add
```
export PATH=$HOME/.local/bin:$PATH
```
to the file .bashrc in the home directory.  Run `source ~/.bashrc` to reread the .bashrc file.   From within the `apia` directory run `stack install` which will put the binary in the ~/.local/bin directory and make the `apia` command available.

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

```apia summary 100 200```

Show the amount of work done each day on the tags `work` and `code` between day 100 and day 120:

```apia daily 100 120 work code```

Make a list of the current tags:

```apia taglist```

Find the average daily work on the tags `work` and `code` between days 101 and 102:

```apia dailymean 101 102 work code```

Get the day number for today:

```apia now```

The order of the tags does not matter.  For the `daily` option and similar, supplying a tag excludes all the records that don't have it, so providing no tags includes everything.

The day range results include both end days.  If just one day's results is required then the same day number should be given for the start and end.
