%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011.
%% Copyright (c) 2015, Shawn Debnath.
%% All rights reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% The code for lpc is derived from Erlang's OTP sources for rpc. It
%% has been pruned and modified to contain only the bare essentials
%% required for executing MFAs locally on the node. The end result is
%% a highly parallel local rpc-style pmap execution framework.
%%
%% This is meant to be used within a OTP application with supervisors
%% looking after child processes.
%%
%% Modified by: Shawn Debnath (sdebnath)
%%

-module(lpc).

%%% API Function Exports
-export([pmap/3,
         async/3,
         await/2]).

%%% -------------------------------------------------------
%%% API functions
%%% -------------------------------------------------------

%% pmap/3
%%
%% API for local parallel map execution. This is a blocking call
%% that will farm out the parallel work and wait for replies back
%% from each worker. More or less, a wrapper around async/await.
pmap({Mod, Fun}, Args, List)
  when is_atom(Mod), is_atom(Fun), is_list(Args), is_list(List) ->
    Keys = async({Mod, Fun}, Args, List),
    await(Keys, infinity).

%% async/3
%%
%% Fires off async work requests for every member of the list by
%% calling map_eval. Returns a list of keys to be used to wait
%% for and gather replies. The return value is opaque to the caller.
async({Mod, Fun}, Args, List)
  when is_atom(Mod), is_atom(Fun), is_list(Args), is_list(List) ->
    map_eval(Mod, Fun, Args, List);
async({Mod, Fun}, Args, Element) ->
    async({Mod, Fun}, Args, [Element]).

%% await/2
%% Waits for replies in the order of the members of the keys list. async/3
%% returns the list in reverse order, so processing it serially from the
%% begining here gets us the responses in the proder order (i.e., the original
%% list sent to async/3). Note, the value Keys is opaque to the caller.
await(Keys, Timeout)
  when is_list(Keys), is_integer(Timeout); Timeout == infinity ->
    [wait_on_reply(K, Timeout) || K <- Keys].

%%% -------------------------------------------------------
%%% Private functions
%%% -------------------------------------------------------

%% map_eval/4
%%
%% Recursively call async/3 to spawn off processes to execute
%% MFA for each list member.
map_eval(_M, _F, _A, []) ->
    [];
map_eval(M, F, A, [E | Tail]) ->
    [send_msg(M, F, [E | A]) | map_eval(M, F, A, Tail)].

%% async/3
%%
%% Spawn process for MFA which will execute and send a message
%% back with the response. Caller can use await/2 to retrieve it
send_msg(Mod, Fun, Args) ->
    ReplyTo = self(),
    spawn_link(
        fun() ->
            R =
                case catch apply(Mod, Fun, Args) of
                    {'EXIT',_} = V ->
                        {badlpc, V};
                    Other ->
                        Other
                end,
            ReplyTo ! {self(), {promise_reply, R}}
        end).

%% await/2
%%
%% Waits for a message with a specific key.
wait_on_reply(Key, Timeout) when is_pid(Key) ->
    receive
        {Key, {promise_reply, R}} ->
            R
        after Timeout ->
            timeout
    end.
