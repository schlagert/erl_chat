%%%=============================================================================
%%%
%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @copyright (C) 2016, Lindenbaum GmbH
%%%
%%% @doc
%%% @end
%%%=============================================================================

-module(chat_participant).

-behaviour(lbm_kv).

%% API
-export([init/0]).

%% lbm_kv callbacks
-export([handle_conflict/3]).

%% Cowboy callbacks
-export([init/2,
         allowed_methods/2,
         content_types_accepted/2]).

%% Cowboy handlers
-export([process_post/2]).

-define(NAME, <<"Bort">>).

-include("chat.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> {ok, [tuple()]} | {error, term()}.
init() ->
    case lbm_kv:create(?MODULE) of
        ok    -> {ok, [{"/participant", ?MODULE, []}]};
        Error -> Error
    end.

%%%=============================================================================
%%% lbm_kv Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% In case of conflict, we just keep the local participant and discard the
%% remote one, who cares anyway...
%%------------------------------------------------------------------------------
handle_conflict(_Id, Local, _Remote) -> {value, Local}.

%%%=============================================================================
%%% Cowboy callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Req, Opts) -> {cowboy_rest, Req, Opts}.

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
    Participant = decode(Body),
    {ok, []} = lbm_kv:put(?MODULE, Id, Participant),
    {encode(Id, Participant), Req2, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Leaves out the private `rooms'.
%%------------------------------------------------------------------------------
encode(Id, #participant{name = Name}) ->
    jsx:encode([{id, Id}, {name, Name}]).

%%------------------------------------------------------------------------------
%% @private
%% Generates a participant with empty `rooms'.
%%------------------------------------------------------------------------------
decode(JSON) ->
    decode(jsx:is_json(JSON), JSON).
decode(false, JSON) ->
    #participant{name = ?NAME};
decode(true, JSON) ->
    #participant{name = get_value(<<"name">>, jsx:decode(JSON), ?NAME)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_value(Key, Value, Default) -> proplists:get_value(Key, Value, Default).