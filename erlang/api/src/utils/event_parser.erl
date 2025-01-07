%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 8:53 PM
%%%-------------------------------------------------------------------
-module(event_parser).
-author("alejandrorodriguez").

-include("../../include/events.hrl").

%% API
-export([json_to_event/1, event_to_json/1, events_to_json/1]).

json_to_event(Data) when is_binary(Data) ->
  try
      jsx:decode(Data, [return_maps]) of
        Map ->
          EventType = maps:get(<<"event_type">>, Map),
          EventDataMap = maps:get(<<"data">>, Map),
          EventData = #event_data{
            first_name = maps:get(<<"first_name">>, EventDataMap),
            last_name = maps:get(<<"last_name">>, EventDataMap),
            gender = maps:get(<<"gender">>, EventDataMap),
            mrn = maps:get(<<"mrn">>, EventDataMap),
            organization = maps:get(<<"organization">>, EventDataMap)
          },
          #event{
            event_type = EventType,
            data = EventData
          }
  catch
    error:badarg -> error
  end.

record_to_map(#event{
  event_type = EventType,
  data = #event_data{
    first_name = FirstName,
    last_name = LastName,
    gender = Gender,
    mrn = MRN,
    organization = Organization}
}) ->
  EventDataMap = #{first_name => FirstName,
    last_name => LastName,
    gender => Gender,
    mrn => MRN,
    organization => Organization},
  #{event_type => EventType, data => EventDataMap};
record_to_map(Value) ->
  Value.

event_to_json(Event) ->
  EventMap = record_to_map(Event),
  jsx:encode(EventMap).

events_to_json(Events) ->
  EventMaps = [record_to_map(Event) || Event <- Events],
  jsx:encode(EventMaps).
