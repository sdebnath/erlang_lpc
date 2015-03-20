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
-export([pmap/3]).

%%% -------------------------------------------------------
%%% API functions
%%% -------------------------------------------------------

%% pmap/3
%%
%% Entry point for local parallel map execution.
pmap({Mod, Fun}, Args, List) ->
    parallel_eval(Mod, Fun, Args, List).

%%% -------------------------------------------------------
%%% Private functions
%%% -------------------------------------------------------

%% parallel_eval/4
%%
%% Fires off async work requests for every member of the list by
%% calling map_eval. Then awaits for their response in order of 
%% the reverse of the original list resulting in list with responses
%% in order of the original list.
parallel_eval(Mod, Fun, Args, List)
  when is_atom(Mod), is_atom(Fun), is_list(Args), is_list(List) ->
    Keys = map_eval(Mod, Fun, Args, List),
    [await(K, infinity) || K <- Keys].

%% map_eval/4
%%
%% Recursively call async/3 to spawn off processes to execute
%% MFA for each list member.
map_eval(_M, _F, _A, []) ->
    [];
map_eval(M, F, A, [E | Tail]) ->
    [async(M, F, [E | A]) | map_eval(M, F, A, Tail)].

%% async/3
%%
%% Spawn process for MFA which will execute and send a message
%% back with the response. Caller can use await/2 to retrieve it
async(Mod, Fun, Args) ->
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
await(Key, Timeout) when is_pid(Key) ->
    receive
        {Key, {promise_reply, R}} ->
            R
        after Timeout ->
            timeout
    end.
