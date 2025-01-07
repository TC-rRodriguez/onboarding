%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2024 8:14 PM
%%%-------------------------------------------------------------------
-module(event_service_SUITE).
-author("alejandrorodriguez").

-include_lib("common_test/include/ct.hrl").
-include("../include/events.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  post_event_ok_test/1, delete_event_ok_test/1,
  delete_event_not_found_test/1, delete_event_error_test/1,
  get_event_all_test/1, get_event_one_test/1, get_event_error_test/1,
  put_event_ok_test/1
]).

%% Test cases
all() -> [
  post_event_ok_test, delete_event_ok_test,
  delete_event_not_found_test, delete_event_error_test,
  get_event_all_test, get_event_one_test, get_event_error_test,
  put_event_ok_test
].


init_per_suite(Config) ->
  {ok, App_Start_List} = start([api]),
  inets:start(),
  [{app_start_list, App_Start_List}|Config].

end_per_suite(Config) ->
  inets:stop(),
  stop(?config(app_start_list, Config)),
  Config.

start(Apps) ->
  {ok, do_start(_To_start = Apps, _Started = [])}.

do_start([], Started) ->
  Started;
do_start([App|Apps], Started) ->
  case application:start(App) of
    ok ->
      do_start(Apps, [App|Started]);
    {error, {not_started, Dep}} ->
      do_start([Dep|[App|Apps]], Started)
  end.

stop(Apps) ->
  _ = [ application:stop(App) || App <- Apps ],
  ok.

%% Test data
-define(TEST_EVENT, #event{event_type = <<"type1">>, data = #event_data{first_name = <<"John">>, last_name = <<"Doe">>, gender = <<"male">>, mrn = <<"12345">>, organization = <<"Org1">>}}).
-define(UPDATED_EVENT, #event{event_type = <<"type1">>, data = #event_data{first_name = <<"Jane">>, last_name = <<"Doe">>, gender = <<"female">>, mrn = <<"67890">>, organization = <<"Org2">>}}).

%% Test post_event/1
post_event_ok_test(_Config) ->
  ok = event_service:post_event(?TEST_EVENT).

%% Test delete_event/1
delete_event_ok_test(_Config) ->
  ok = event_service:post_event(?TEST_EVENT),
  ok = event_service:delete_event(<<"type1">>).

delete_event_not_found_test(_Config) ->
  not_found = event_service:delete_event(<<"non_existent_type">>).

delete_event_error_test(_Config) ->
  bad = event_service:delete_event(undefined).

%% Test get_event/1
get_event_all_test(_Config) ->
  ok = event_service:post_event(?TEST_EVENT),
  Events = event_service:get_event(undefined),
  %% Convert the JSON to a binary string
  JsonBinary = event_parser:event_to_json(?TEST_EVENT),
  PrefixedJsonBinary = <<"[" , JsonBinary / binary, "]">>,
  PrefixedJsonBinary = Events.

%% Test get_event/1
get_event_one_test(_Config) ->
  ok = event_service:post_event(?TEST_EVENT),
  Event = event_service:get_event(<<"type1">>),
  Json = event_parser:event_to_json(?TEST_EVENT),
  Json = Event.

get_event_error_test(_Config) ->
  "Required event not found" = event_service:get_event(<<"non_existent_type">>).

%% Test put_event/1
put_event_ok_test(_Config) ->
  ok = event_service:post_event(?TEST_EVENT),
  ok = event_service:put_event(?UPDATED_EVENT).