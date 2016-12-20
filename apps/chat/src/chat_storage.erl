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

-module(chat_storage).

%% API
-export([init/0]).

%% lbm_kv callbacks
-export([handle_conflict/3]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init() -> ok | {error, term()}.
init() -> lbm_kv:create(?MODULE).

%%%=============================================================================
%%% lbm_kv Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Only list values can be merged. In this case both configuration entries will
%% be appended and uniquely sorted.
%%------------------------------------------------------------------------------
handle_conflict(_Key, Local, Remote) when is_list(Local), is_list(Remote) ->
    {value, lists:usort(lists:append(Local, Remote))}.
