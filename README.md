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

    tracemap:graphviz("trace.dot").

From the shell:

    neato trace.dot -Tpng > trace.png

    # Or whatever other image viewer you prefer
    eog trace.png
