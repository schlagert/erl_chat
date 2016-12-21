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

-module(chat_room).

-behaviour(lbm_kv).

%% API
-export([init/0]).

%% lbm_kv callbacks
-export([handle_conflict/3]).

%% Cowboy callbacks
-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2]).

%% Cowboy handlers
-export([process_get/2,
         process_post/2]).

-define(TITLE, <<"Title">>).

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
        ok ->
            {ok, [
                  {"/chatroom", ?MODULE, []},
                  {"/chatroom/[:room_id]", ?MODULE, []}
                 ]};
        Error ->
            Error
    end.

%%%=============================================================================
%%% lbm_kv Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% In case of conflict, we just keep the local room and discard the
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
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, process_get}], Req, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, process_post}], Req, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
resource_exists(Req, State) ->
    {Value, Req2} = cowboy_req:binding(room_id, Req),
    {case Value of
         undefined ->
             true;
         RoomId ->
             case lbm_kv:get(?MODULE, RoomId) of
                 {ok, [{RoomId, _Room}]} -> true;
                 _                       -> false
             end
     end, Req2, State}.

%%%=============================================================================
%%% Cowboy handlers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_get(Req, State) ->
    {<<"/chatroom", _/binary>>, Req2} = cowboy_req:path(Req),
    {RoomId, Req3} = cowboy_req:binding(room_id, Req2),
    {case RoomId of
         undefined -> jsx:encode(chat_storage:rooms());
         RoomId    -> jsx:encode(chat_storage:room(RoomId))
     end, Req3, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_post(Req, State) -> {true, Req, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Leaves out the private `log'.
%%------------------------------------------------------------------------------
encode(Id, #room{title = Title}) ->
    jsx:encode([{id, Id}, {title, Title}]).

%%------------------------------------------------------------------------------
%% @private
%% Generates a room with empty `log'.
%%------------------------------------------------------------------------------
decode(JSON) ->
    decode(jsx:is_json(JSON), JSON).
decode(false, JSON) ->
    #room{title = ?TITLE};
decode(true, JSON) ->
    #room{title = get_value(<<"title">>, jsx:decode(JSON), ?TITLE)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_value(Key, Value, Default) -> proplists:get_value(Key, Value, Default).
