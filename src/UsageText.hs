module UsageText (text) where

text :: String
text =
    "Usage examples:\n\
    \--------------\n\
    \\n\
    \Start work on a task with two tags, `work` and `code`:\n\
    \\n\
    \$ apia clockin work code\n\
    \\n\
    \Switch to a task with tags `job` and `new`:\n\
    \\n\
    \$ apia switch job new\n\
    \\n\
    \Stop work:\n\
    \\n\
    \$ apia clockout\n\
    \\n\
    \Find out if you are currently clocked in:\n\
    \\n\
    \$ apia clockedin\n\
    \\n\
    \Make a chart showing a breakdown of the work done today:\n\
    \\n\
    \$ apia today\n\
    \\n\
    \Make a summary of the work done between day 100 and day 120:\n\
    \\n\
    \$ apia summary 100 120\n\
    \\n\
    \Show the amount of work done each day on the tags `work` and `code` between day 100 and day 120:\n\
    \\n\
    \$ apia daily 100 120 work code\n\
    \\n\
    \Make a list of the current tags:\n\
    \\n\
    \$ apia taglist\n\
    \\n\
    \Find the average daily work on the tags `work` and `code` between days 101 and 102:\n\
    \\n\
    \$ apia dailymean 101 102 work code\n\
    \\n\
    \Get the current time:\n\
    \\n\
    \$ apia now\n\
    \\n\
    \The time is in days since 00:00 on 1st October 2016 in UTC, so there are no years, months, weeks, hours, minutes or seconds, just days.  The time outputs are in millidays (1 day / 1000).\n\
    \\n\
    \The order of the tags does not matter.  For the `daily` option and similar, supplying a tag excludes all the records that don't have it, so providing no tags includes everything.\n\
    \\n\
    \The day range results include both end days.  If just one day's results is required then the same day number should be given for the start and end."
