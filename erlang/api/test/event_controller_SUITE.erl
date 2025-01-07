%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2024 1:55 PM
%%%-------------------------------------------------------------------
-module(event_controller_SUITE).
-author("alejandrorodriguez").

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1, post_event_test/1,
  post_event_invalid_data_test/1,
  get_event_test/1,
  get_all_events_test/1,
  get_non_existent_event_test/1,
  put_event_test/1,
  put_event_invalid_data_test/1,
  delete_event_test/1,
  delete_non_existent_event_test/1,
  delete_event_without_eventtype_test/1]).

all() -> [
  post_event_test,
  post_event_invalid_data_test,
  get_event_test,
  get_all_events_test,
  get_non_existent_event_test,
  put_event_test,
  put_event_invalid_data_test,
  delete_event_test,
  delete_non_existent_event_test,
  delete_event_without_eventtype_test].

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

post_event_test(_Config) ->
  %% Simulate a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, Status, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body}, [], []),
  201 = Status.

post_event_invalid_data_test(_Config) ->
  %% Simulate a POST request with invalid data type
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  %% Included a single double quote to make the JSON invalid
  InvalidBody = <<"{\"event_type\":123\",\"data\":{\"first_name\":true,\"last_name\":false,\"gender\":null,\"mrn\":[],\"organization\":{}}}">>,
  {ok, {{_, Status, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", InvalidBody}, [], []),
  %% Expecting a 400 Bad Request status
  400 = Status.

get_event_test(_Config) ->
  %% First, create a record using a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, PostStatus, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body}, [], []),
  201 = PostStatus,

  %% Simulate a GET request
  URL_Get = "http://localhost:8080/events/type1",
  {ok, {{_, GetStatus, _}, _, ResponseBody}} = httpc:request(get, {URL_Get, []}, [], []),
  200 = GetStatus,

  %% Define the expected response
  ExpectedResponse = "{\"data\":{\"first_name\":\"John\",\"gender\":\"male\",\"last_name\":\"Doe\",\"mrn\":\"12345\",\"organization\":\"Org1\"},\"event_type\":\"type1\"}",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

get_all_events_test(_Config) ->
  %% First, create a record using a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body1 = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  Body2 = <<"{\"event_type\":\"type2\",\"data\":{\"first_name\":\"Jane\",\"last_name\":\"Doe\",\"gender\":\"female\",\"mrn\":\"67890\",\"organization\":\"Org2\"}}">>,
  {ok, {{_, PostStatus1, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body1}, [], []),
  201 = PostStatus1,
  {ok, {{_, PostStatus2, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body2}, [], []),
  201 = PostStatus2,

  %% Simulate a GET request without eventType to retrieve all events
  URL_Get_All = "http://localhost:8080/events",
  {ok, {{_, GetStatus, _}, _, ResponseBody}} = httpc:request(get, {URL_Get_All, []}, [], []),
  200 = GetStatus,

  %% Define the expected response
  ExpectedResponse = "[{\"data\":{\"first_name\":\"John\",\"gender\":\"male\",\"last_name\":\"Doe\",\"mrn\":\"12345\",\"organization\":\"Org1\"},\"event_type\":\"type1\"},{\"data\":{\"first_name\":\"Jane\",\"gender\":\"female\",\"last_name\":\"Doe\",\"mrn\":\"67890\",\"organization\":\"Org2\"},\"event_type\":\"type2\"}]",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

get_non_existent_event_test(_Config) ->
  %% Simulate a GET request for a non-existent event
  URL = "http://localhost:8080/events/non_existent_event",
  {ok, {{_, GetStatus, _}, _, ResponseBody}} = httpc:request(get, {URL, []}, [], []),
  200 = GetStatus,

  %% Define the expected response for a non-existent event
  ExpectedResponse = "Required event not found",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

put_event_test(_Config) ->
  %% First, create a record using a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, PostStatus, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body}, [], []),
  201 = PostStatus,

  %% Update the record using a PUT request
  UpdatedBody = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"Jane\",\"last_name\":\"Doe\",\"gender\":\"female\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, PutStatus, _}, _, _}} = httpc:request(put, {URL, Headers, "application/json", UpdatedBody}, [], []),
  200 = PutStatus,

  %% Simulate a GET request to verify the update
  URL_Get = "http://localhost:8080/events/type1",
  {ok, {{_, GetStatus, _}, _, ResponseBody}} = httpc:request(get, {URL_Get, []}, [], []),
  200 = GetStatus,

  %% Define the expected response
  ExpectedResponse = "{\"data\":{\"first_name\":\"Jane\",\"gender\":\"female\",\"last_name\":\"Doe\",\"mrn\":\"12345\",\"organization\":\"Org1\"},\"event_type\":\"type1\"}",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

put_event_invalid_data_test(_Config) ->
  %% First, create a record using a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, PostStatus, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body}, [], []),
  201 = PostStatus,

  %% Simulate a PUT request with invalid data type
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  %% Removed a single double quote to make the JSON invalid
  InvalidBody = <<"{\"event_type\":\"type1,\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, Status, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", InvalidBody}, [], []),
  %% Expecting a 400 Bad Request status
  400 = Status.

delete_event_test(_Config) ->
  %% First, create a record using a POST request
  URL = "http://localhost:8080/events",
  Headers = [{"Content-Type", "application/json"}],
  Body = <<"{\"event_type\":\"type1\",\"data\":{\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender\":\"male\",\"mrn\":\"12345\",\"organization\":\"Org1\"}}">>,
  {ok, {{_, PostStatus, _}, _, _}} = httpc:request(post, {URL, Headers, "application/json", Body}, [], []),
  201 = PostStatus,

  %% Delete the record using a DELETE request
  URL_Delete = "http://localhost:8080/events/type1",
  {ok, {{_, DeleteStatus, _}, _, _}} = httpc:request(delete, {URL_Delete, []}, [], []),
  204 = DeleteStatus,

  %% Simulate a GET request to verify the deletion
  {ok, {{_, GetStatus, _}, _, ResponseBody}} = httpc:request(get, {URL_Delete, []}, [], []),
  200 = GetStatus,

  %% Define the expected response for a deleted record
  ExpectedResponse = "Required event not found",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

delete_non_existent_event_test(_Config) ->
  %% Simulate a DELETE request for a non-existent event
  URL = "http://localhost:8080/events/non_existent_event",
  {ok, {{_, DeleteStatus, _}, _, ResponseBody}} = httpc:request(delete, {URL, []}, [], []),
  404 = DeleteStatus,

  %% Define the expected response for a non-existent event
  ExpectedResponse = "Invalid record",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.

delete_event_without_eventtype_test(_Config) ->
  %% Simulate a DELETE request without eventtype
  URL = "http://localhost:8080/events",
  {ok, {{_, DeleteStatus, _}, _, ResponseBody}} = httpc:request(delete, {URL, []}, [], []),
  400 = DeleteStatus,

  %% Define the expected response for a wrong request
  ExpectedResponse = "Wrong request",

  %% Validate the response
  ResponseBody =:= ExpectedResponse.