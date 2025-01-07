%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 8:23 PM
%%%-------------------------------------------------------------------
-module(event_controller).
-author("alejandrorodriguez").

-include("../../include/events.hrl").

%% API
-export([init/2]).

init(Req, State) ->
  Method = cowboy_req:method(Req),
  case Method of
    <<"POST">> -> post_event(Req, State);
    <<"GET">> -> get_event(Req, State);
    <<"PUT">> -> put_event(Req, State);
    <<"DELETE">> -> delete_event(Req, State);
    <<"OPTIONS">> -> options(Req, State);
    _ -> Req2 = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, <<"Method Not Allowed">>, Req),
      {ok, Req2, State}
  end.

options(Req, State) ->
  AllowedMethods = <<"OPTIONS, GET, POST, PUT, DELETE">>,
  Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>, <<"allow">> => AllowedMethods}, <<>>, Req),
  {ok, Req2, State}.

post_event(Req, State) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  case event_parser:json_to_event(Body) of
    error ->
      Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Bad Request">>, Req2),
      {ok, Req3, State};
    Event ->
      case event_service:post_event(Event) of
        ok ->
          Req3 = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, <<"Record created">>, Req2);
        error ->
          Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"An error ocurred while creating the record">>, Req2)
      end,
      {ok, Req3, State}
  end.

delete_event(Req, State) ->
  EventType = cowboy_req:binding(eventType, Req),

  case event_service:delete_event(EventType) of
    ok ->
      Req2 = cowboy_req:reply(204, #{<<"content-type">> => <<"application/json">>}, <<>>, Req);
    not_found ->
      Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Invalid record">>, Req);
    bad ->
      Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Wrong request">>, Req);
    error ->
      Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"An error ocurred while deleting the record">>, Req)
  end,
  {ok, Req2, State}.

get_event(Req, State) ->
  EventType = cowboy_req:binding(eventType, Req),
  Response = event_service:get_event(EventType),
  Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req),
  {ok, Req2, State}.

put_event(Req, State) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  case event_parser:json_to_event(Body) of
    error ->
      Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Bad Request">>, Req2),
      {ok, Req3, State};
    Event ->
      case event_service:post_event(Event) of
        ok ->
          Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"Record updated">>, Req2);
        error ->
          Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"An error ocurred while creating the record">>, Req2)
      end,
      {ok, Req3, State}
  end.