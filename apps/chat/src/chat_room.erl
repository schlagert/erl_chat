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
%%% Module implementing all room-centric functions. This include database
%%% management of rooms as well as the REST interface to manipulate rooms.
%%%
%%% Chat rooms have an id, an associated process group and contain the room's
%%% message log. Group membership is modeled by process group membership.
%%% @end
%%%=============================================================================

-module(chat_room).

-behaviour(lbm_kv).

%% API
-export([init/0,
         get/1]).

%% lbm_kv callbacks
-export([handle_conflict/3]).

%% Cowboy callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2]).

%% Cowboy handlers
-export([process_get/2,
         process_post/2]).

-record(chat_room, {title :: binary(), log = [] :: [binary()]}). %% opaque

-type ref() :: {Id :: binary(), Data :: #chat_room{}}.

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
        ok ->
            {ok, [
                  {"/chatroom", ?MODULE, []},
                  {"/chatroom/[:room_id]", ?MODULE, []}
                 ]};
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the chat associated with the given decoded JSON proplist. The
%% `<<"id">>' key is used to look up the room.
%% @end
%%------------------------------------------------------------------------------
-spec get(proplists:proplist()) -> {ok, ref()} | {error, term()}.
get(Proplist) ->
    Id = proplists:get_value(<<"id">>, Proplist),
    case lbm_kv:get(?MODULE, Id) of
        {ok, [Room = {Id, _}]} -> {ok, Room};
        {ok, []}               -> {error, not_found};
        Error                  -> Error
    end.

%%%=============================================================================
%%% lbm_kv Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% In case of conflict, we just keep the local room and discard the remote one,
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
rest_init(Req, _Opts) -> {ok, Req, undefined}.

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
resource_exists(Req, _State) ->
    {Value, Req2} = cowboy_req:binding(room_id, Req),
    case Value of
        undefined ->
            {true, Req2, undefined};
        RoomId ->
            case lbm_kv:get(?MODULE, RoomId) of
                {ok, [{RoomId, Room}]} -> {true, Req2, Room};
                _                      -> {false, Req2, undefined}
            end
    end.

%%%=============================================================================
%%% Cowboy handlers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_get(Req, State = undefined) ->
    {ok, Rooms} = lbm_kv:match_key(?MODULE, '_'),
    {jsx:encode(lists:map(fun to_proplist/1, Rooms)), Req, State};
process_get(Req, State = #chat_room{log = Log}) ->
    {jsx:encode([{messageLog, join(Log, $\n)}]), Req, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_post(Req, State) ->
    {Body, Req2} = cowboy_req:body(Req),
    Id = chat:id(),
    {ok, {_, Data}} = decode(Body),
    {ok, []} = lbm_kv:put(?MODULE, Id, Data),
    ok = pg2:create(Id),
    error_logger:info_msg("Created room ~s with data ~128p", [Id, Data]),
    {jsx:encode(to_proplist({Id, Data})), Req2, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Leaves out `log'.
%%------------------------------------------------------------------------------
to_proplist({Id, #chat_room{title = Title}}) -> [{id, Id}, {title, Title}].

%%------------------------------------------------------------------------------
%% @private
%% Will have an empty `log'.
%%------------------------------------------------------------------------------
decode(JSON) when is_binary(JSON) ->
    case jsx:is_json(JSON) of
        true  ->
            Decoded = jsx:decode(JSON),
            Id = proplists:get_value(<<"id">>, Decoded),
            Title = proplists:get_value(<<"title">>, Decoded),
            {ok, {Id, #chat_room{title = Title}}};
        false ->
            {error, {invalid_json, JSON}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
join(BinList, Sep)      -> join(BinList, Sep, <<>>).
join([], _Sep, Acc)     -> Acc;
join([H | T], Sep, Acc) -> join(T, Sep, <<Acc/binary, Sep, H/binary>>).
