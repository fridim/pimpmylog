pimpmylogs
==========

minimal UI for IRC logs

[Live demo](http://logs.onfi.re/racket/)

This is a simple UI for IRC log. I coded my own, because:

* I want a minimalist UI, simple and clean look
* I want it to be simple but functional
* I want it to be written in [Racket](http://racket-lang.org)

Usage :

    racket main.rkt -h
    main.rkt [ <option> ... ] <filename>
     where <option> is one of
    / -w, --weechat : input file is in weechat log format
    \ -r, --racket-org : input file is in format as files on http://racket-lang.org/irc-logs/racket/
      -n <cn>, --name <cn> : Name of the channel
      -p <p>, --port <p> : Listening port
      --help, -h : Show this help
      -- : Do not treat any remaining argument as a switch (at this level)
     /|\ Brackets indicate mutually exclusive options.
     Multiple single-letter switches can be combined after one `-'; for
      example: `-h-' is the same as `-h --'

Example :

    racket main.rkt -r -n '#racket' logs/racket-logs/racket.log

Features
--------

* handy navigation (day, week, month, ...)
* stupid search (a grep)
* a specific line in history can be shared (simply copy/paste the link to that line)
* works fine with proxy cache (tested with [varnish](https://www.varnish-cache.org/) default conf) as it sets Cache-Control HTTP header

Contrib?
--------

* TODO: add ajax fetch on scrolling (faster)
* TODO: log format definitions should be in a separate forlder
* TODO: add a checkbox to enable RAW format
* TODO: support mobile/pad
* TODO: add a calendar to go to a specific day/week/month
* TODO: highlight searched word(s) in search result
* IDEA: support multiple files ?
