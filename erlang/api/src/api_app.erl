%%%-------------------------------------------------------------------
%% @doc api public API
%% @end
%%%-------------------------------------------------------------------

-module(api_app).
-behaviour(application).

-include("../include/events.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia:start(),
    mnesia:create_table(event, [
        {attributes, record_info(fields, event)},
        {ram_copies, [node()]}
    ]),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/events[/:eventType]", event_controller, []}
        ]}
    ]),

    ReturnValue = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    case ReturnValue of
        {ok, Pid} when is_pid(Pid) ->
            api_sup:start_link();
        {error, Reason} ->
            {stop, Reason};
        _ ->
            {stop, unexpected_response}
    end.

stop(_State) ->
    ok = cowboy:stop_listener(http_listener).

%% internal functions
