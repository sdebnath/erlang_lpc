lpc
===

The code for LPC is derived from Erlang's OTP sources for RPC. It
has been pruned and modified to contain only the bare essentials
required for executing MFAs locally on the node. The end result is
a highly parallel local RPC-style pmap execution framework.

This is meant to be used within a OTP application with supervisors
looking after child processes.

Usage
-----

```erl-sh
bar_fun(X, F) -> F(X).
do_blocking_work() ->
    % Blocking call to lpc:pmap/3
    lpc:pmap({foo_module, bar_fun},
             [fun(X) -> timer:sleep(100), X end],
             lists:seq(1, 10000)),

do_async_work() ->
    % Non-blocking call to async/3 to start the parallel work
    Obj = lpc:async({foo_module, bar_fun},
                    [fun(X) -> timer:sleep(100), X end],
                    lists:seq(1, 10000)),

    % Go take care of other things while the heavy workload is
    % executed in parallel
    timer:sleep(60),

    % Intermediary task done, lets wait for the responses from
    % the parallel work.
    lpc:await(Obj, infinity),

```

Build
-----

    $ rebar3 compile

Copyright
---------

Copyright Ericsson AB 1996-2011.
Copyright (c) 2015, Shawn Debnath.
All rights reserved.

The contents of this file are subject to the Erlang Public License,
Version 1.1, (the "License"); you may not use this file except in
compliance with the License. You should have received a copy of the
Erlang Public License along with this software. If not, it can be
retrieved online at http://www.erlang.org/.

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.
