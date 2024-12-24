%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2024 9:11 PM
%%%-------------------------------------------------------------------
-module(event_parser_SUITE).
-author("alejandrorodriguez").

-include("../include/events.hrl").
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([json_to_event_test/1, event_to_json_test/1, events_to_json_test/1]).

%% Test cases
all() -> [json_to_event_test, event_to_json_test, events_to_json_test].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

%% Test json_to_event/1
json_to_event_test(_Config) ->
  Json = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  ExpectedEvent = #event{event_type = <<"type1">>, data = #event_data{first_name = <<"John">>, last_name = <<"Doe">>, gender = <<"male">>, mrn = <<"12345">>, organization = <<"Org1">>}},
  Event = event_parser:json_to_event(Json),
  Event =:= ExpectedEvent.

%% Test event_to_json/1
event_to_json_test(_Config) ->
  Event = #event{event_type = <<"type1">>, data = #event_data{first_name = <<"John">>, last_name = <<"Doe">>, gender = <<"male">>, mrn = <<"12345">>, organization = <<"Org1">>}},
  ExpectedJson = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  Json = event_parser:event_to_json(Event),
  Json =:= ExpectedJson.

%% Test events_to_json/1
events_to_json_test(_Config) ->
  Events = [
    #event{event_type = <<"type1">>, data = #event_data{first_name = <<"John">>, last_name = <<"Doe">>, gender = <<"male">>, mrn = <<"12345">>, organization = <<"Org1">>}},
    #event{event_type = <<"type2">>, data = #event_data{first_name = <<"Jane">>, last_name = <<"Doe">>, gender = <<"female">>, mrn = <<"67890">>, organization = <<"Org2">>}}
  ],
  ExpectedJson = <<"[{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}},{\"event_type\":\"type2\",\"data\":{\"first_name\":\"Jane\",\"last_name\":\"Doe\",\"gender\":\"female\",\"mrn\":\"67890\",\"organization\":\"Org2\"}}]">>,
  Json = event_parser:events_to_json(Events),
  Json =:= ExpectedJson.