%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 8:58 PM
%%%-------------------------------------------------------------------
-module(event_db).
-author("alejandrorodriguez").

-include("../../include/events.hrl").

%% API
-export([create_event/1, get_event/1, get_all_events/0, update_event/1, delete_event/1]).

create_event(Event) ->
  mnesia:transaction(
    fun() ->
      mnesia:write(Event)
    end).

get_event(EventType) ->
  mnesia:transaction(
    fun() ->
      case mnesia:read({event, EventType}) of
        [Record] -> Record;
        [] -> not_found
      end
    end).

get_all_events() ->
  mnesia:transaction(
    fun() ->
      Events = mnesia:match_object(#event{_ = '_'}),
      Events
    end).

update_event(Event) ->
  mnesia:transaction(
    fun() ->
      mnesia:write(Event)
    end).

delete_event(EventType) ->
  case event_db:get_event(EventType) of
    {atomic, not_found} ->
      not_found;
    _ ->
      mnesia:transaction(
        fun() ->
          mnesia:delete({event, EventType}),
          ok
        end)
  end.