%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2024 7:54 PM
%%%-------------------------------------------------------------------
-module(event_db_SUITE).
-author("alejandrorodriguez").

-include("../include/events.hrl").
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
  create_event_test/1,
  get_event_test/1,
  get_nonexistent_event_test/1,
  get_all_events_test/1,
  update_event_test/1,
  delete_event_test/1,
  delete_nonexistent_event_test/1]).

%% Test cases
all() -> [create_event_test,
  get_event_test,
  get_nonexistent_event_test,
  get_all_events_test,
  update_event_test,
  delete_event_test,
  delete_nonexistent_event_test].

%% Initialize and clean up
init_per_suite(Config) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(event, [{attributes, record_info(fields, event)}]),
  Config.

end_per_suite(_Config) ->
  mnesia:stop(),
  ok.

init_per_testcase(_TestCase, Config) ->
  mnesia:clear_table(event),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%% Test data
-define(TEST_EVENT, #event{event_type = <<"type1">>, data = <<"test_data">>}).

%% Test create_event/1
create_event_test(_Config) ->
  {atomic, ok} = event_db:create_event(?TEST_EVENT),
  {atomic, [#event{event_type = <<"type1">>, data = <<"test_data">>}]} = mnesia:transaction(fun() -> mnesia:read({event, <<"type1">>}) end).

%% Test get_event/1
get_event_test(_Config) ->
  %% Create a record first
  {atomic, ok} = event_db:create_event(?TEST_EVENT),
  {atomic, #event{event_type = <<"type1">>, data = <<"test_data">>}} = event_db:get_event(<<"type1">>).

%% Test get_event/1
get_nonexistent_event_test(_Config) ->
%% Create a record first
{atomic, not_found} = event_db:get_event(<<"non_existent_type">>).

%% Test get_all_events/0
get_all_events_test(_Config) ->
  %% Create a record first
  {atomic, ok} = event_db:create_event(?TEST_EVENT),
  {atomic, [#event{event_type = <<"type1">>, data = <<"test_data">>}]} = event_db:get_all_events().

%% Test update_event/1
update_event_test(_Config) ->
  UpdatedEvent = #event{event_type = <<"type1">>, data = <<"updated_data">>},
  %% Create a record first
  {atomic, ok} = event_db:create_event(?TEST_EVENT),
  {atomic, ok} = event_db:update_event(UpdatedEvent),
  {atomic, [UpdatedEvent]} = mnesia:transaction(fun() -> mnesia:read({event, <<"type1">>}) end).

%% Test delete_event/1
delete_event_test(_Config) ->
  %% Create a record first
  {atomic, ok} = event_db:create_event(?TEST_EVENT),
  {atomic, ok} = event_db:delete_event(<<"type1">>).

%% Test delete_event/1
delete_nonexistent_event_test(_Config) ->
  %% Create a record first
  {atomic, not_found} = event_db:get_event(<<"type1">>).