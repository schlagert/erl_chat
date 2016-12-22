%%%=============================================================================
%%% Copyright 2016, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% Module implementing all user-centric functions. This include database
%%% management of user as well as the REST interface to manipulate users.
%%%
%%% A user does basically exist and can be created and retrieved.
%%% @end
%%%=============================================================================

-module(chat_user).

-behaviour(lbm_kv).

%% API
-export([init/0,
         get/1]).

%% lbm_kv callbacks
-export([handle_conflict/3]).

%% Cowboy callbacks
-export([init/3,
         allowed_methods/2,
         content_types_accepted/2]).

%% Cowboy handlers
-export([process_post/2]).

-record(user, {name :: binary()}). %% private

-type ref() :: {Id :: binary(), Data :: #user{}}.

-export_type([ref/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Initiates this modules database backend and returns the REST mappings.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> {ok, [tuple()]} | {error, term()}.
init() ->
    case lbm_kv:create(?MODULE) of
        ok    -> {ok, [{"/participant", ?MODULE, []}]};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the user associated with the given decoded JSON proplist. The
%% `<<"id">>' key is used to look up the user.
%% @end
%%------------------------------------------------------------------------------
-spec get(proplists:proplist()) -> {ok, ref()} | {error, term()}.
get(Proplist) ->
    Id = proplists:get_value(<<"id">>, Proplist),
    case lbm_kv:get(?MODULE, Id) of
        {ok, [User = {Id, _}]} -> {ok, User};
        {ok, []}               -> {error, not_found};
        Error                  -> Error
    end.

%%%=============================================================================
%%% lbm_kv Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% In case of conflict, we just keep the local user and discard the remote one,
%% who cares anyway...
%%------------------------------------------------------------------------------
handle_conflict(_Id, Local, _Remote) -> {value, Local}.

%%%=============================================================================
%%% Cowboy callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, process_post}], Req, State}.

%%%=============================================================================
%%% Cowboy handlers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_post(Req, State) ->
    {Body, Req2} = cowboy_req:body(Req),
    Id = chat:id(),
    {ok, {_, Data}} = decode(Body),
    {ok, []} = lbm_kv:put(?MODULE, Id, Data),
    error_logger:info_msg("Created user ~s with data ~128p", [Id, Data]),
    {encode(Id, Data), Req2, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode(Id, #user{name = Name}) -> jsx:encode([{id, Id}, {name, Name}]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
decode(JSON) when is_binary(JSON) ->
    case jsx:is_json(JSON) of
        true  ->
            Decoded = jsx:decode(JSON),
            Id = proplists:get_value(<<"id">>, Decoded),
            Name = proplists:get_value(<<"name">>, Decoded),
            {ok, {Id, #user{name = Name}}};
        false ->
            {error, {invalid_json, JSON}}
    end.
