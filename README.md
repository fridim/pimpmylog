pimpmylog
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
* stupid search (a grep -i)
* a specific line in history can be shared (simply copy/paste the link to that line)
* works fine with proxy cache (tested with [varnish](https://www.varnish-cache.org/) default conf) as it sets Cache-Control HTTP header
* easy interface to support other log formats. Supported log formats :
  * #racket@freenode [archives](http://racket-lang.org/irc-logs/racket/)
  * weechat
* HTML5

Install
-------
(Package coming soon)

1. install [Racket](http://racket-lang.org)
2. clone the repository
   <pre>git clone https://github.com/fridim/pimpmylog.git
cd pimpmylog</pre>
3. usage
   <pre>racket main.rkt -h</pre>

4. run the demo
   <pre>./tools/fetch_racket-lang.org.sh
racket main.rkt -r -n '#racket' logs/racket.log</pre>


Build
-----

Racket can pre-compile to bytecode :

    raco make main.rkt

or build a standalone binary :

    raco exe main.rkt -o pimpmylog

Tests
-----

    raco test .

See [Rackunit](http://docs.racket-lang.org/rackunit/).

Contrib?
--------

* TODO: create a package and use raco to install pimpmylog binary
* TODO: use lang rackjure
* TODO: add a checkbox to enable RAW format
* TODO: add irssi log format
* TODO: add a converter interface to translate from one format to another
* TODO: support mobile/pad
* TODO: add a calendar to go to a specific day/week/month
* TODO: add current topic next to title
* TODO: enable multi-word search
* TODO: speed: create a map date/fileposition and use file-position
* IDEA: support multiple files ?
* IDEA: add a timeline

### Done
* <del>make it a command line tool</del>
* <del>add structure for a msg</del>
* <del>a new log-format support should be as simple as a procedure string -> struct msg</del>
* <del>add structure for a msg</del>
* <del>find a way to put command-line in module main (in a clean way) AKA fix tests</del>
* <del>add color for nicknames</del>
* <del>add listening port as option</del>
* <del>log format definitions should be in a separate forlder</del>
* <del>highlight searched word(s) in search result</del>
* <del>make « 1 day » the homepage</del>
* <del>make the navigation bar sticky</del>
* <del>homepage: jump to bottom (lastlog)</del>
* <del>HTML5</del>
* <del>validate HTML and CSS</del>
* <del>make search case-insensitive</del>
* <del>fix color for date and nickname when row is highlighted</del>
* <del>add ajax fetch on scrolling (faster)</del>
