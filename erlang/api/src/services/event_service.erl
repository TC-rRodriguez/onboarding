%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 8:31 PM
%%%-------------------------------------------------------------------
-module(event_service).
-author("alejandrorodriguez").

%% API
-export([post_event/1, delete_event/1, get_event/1, put_event/1]).

post_event(Event) ->
  case event_db:create_event(Event) of
    {atomic, ok} ->
      ok;
    error ->
      error
  end.

delete_event(EventType) ->
  case EventType of
    undefined -> bad;
    _ ->
      case event_db:delete_event(EventType) of
        {atomic, ok} -> ok;
        not_found -> not_found;
        _ -> error
      end
  end.

get_event(EventType) ->
  case EventType of
    undefined ->
      {atomic, Events} = event_db:get_all_events(),
      Json = event_parser:events_to_json(Events);
    _ ->
      {atomic, Event} = event_db:get_event(EventType),
      Json = case Event of
               not_found -> "Required event not found";
               _ -> event_parser:event_to_json(Event)
             end
  end,
  Json.

put_event(Event) ->
  case event_db:update_event(Event) of
    {atomic, ok} ->
      ok;
    not_found ->
      not_found
  end.