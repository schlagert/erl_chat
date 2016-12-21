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
%%%=============================================================================

-ifndef(chat_hrl_).
-define(chat_hrl_, 1).

-record(room, {
          title    :: binary(),
          log = [] :: [binary()]}).

-record(participant, {
          name       :: binary(),
          rooms = [] :: [binary()]}).

-endif. %% chat_hrl_
