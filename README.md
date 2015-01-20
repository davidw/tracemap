TraceMap
========

TraceMap attempts to make a map of messages passed between processes.

Currently, it works, but could be expanded and improved and made more
solid.

Don't run it on a production system!

Usage
-----

In Erlang:

    tracemap:map(ranch).

    ... time passes, and a few http requests are made ...

    %% Make a graphviz file:
    tracemap:graphviz("trace.dot").
    %% Make a js-sequence-diagrams file
    tracemap:sequence_js("seq.txt").

    ... you're done tracing things ...

    tracemap:stop().

From the shell:

    neato trace.dot -Tpng > trace.png

    # Or whatever other image viewer you prefer:
    eog trace.png

To make the sequence diagram work, you'll need to include the output
of the file sequence_js creates in an HTML file. See the `gallery/`
directory for examples.

Gallery
-------

Example images: http://davidw.github.io/tracemap/

Ideas
-----

* Swimlane diagrams, to show, for instance, how a system comunicates.

* Parse OTP style messages to get more information about who is
  sending what where.

* Live display via the web/javascript.

* Make the code safe for a production system by limiting the amount of
  tracing done.

* Permit tracing of multiple applications.

* As an application adds children, start tracing them.
