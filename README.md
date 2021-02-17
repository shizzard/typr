# typr

Typr is a set of shortcuts to help erlang programmers get their code typed and checked with dialyzer.

## Generic specs

`typr.erl` file contains a huge set of types to be used for your custom code specs. Most of the time you will use `typr:ok_return/0,1,2`, `typr:error_return/0,1` and `typr:generic_return/0,1,2,3`:

```erlang
-spec somefun() ->
    typr:ok_return().

somefun() ->
    ok.
```

```erlang
-spec somefun() ->
    typr:ok_return(OkRet :: value).

somefun() ->
    {ok, value}.
```

```erlang
-spec somefun() ->
    typr:ok_return(OkRet1 :: value1, OkRet2 :: value2).

somefun() ->
    {ok, value1, value2}.
```

```erlang
-spec somefun() ->
    typr:error_return().

somefun() ->
    error.
```

```erlang
-spec somefun() ->
    typr:error_return(ErrorRet :: no_reason).

somefun() ->
    {error, no_reason}.
```

```erlang
-spec somefun() ->
    typr:generic_return(ErrorRet :: no_reason).

somefun() ->
    case foo() of
        ok ->
            ok;
        error ->
            {error, no_reason}
    end.
```

```erlang
-spec somefun() ->
    typr:generic_return(OkRet :: value, ErrorRet :: no_reason).

somefun() ->
    case foo() of
        ok ->
            {ok, value};
        error ->
            {error, no_reason}
    end.
```

```erlang
-spec somefun() ->
    typr:generic_return(OkRet1 :: value1, OkRet2 :: value2, ErrorRet :: no_reason).

somefun() ->
    case foo() of
        ok ->
            {ok, value1, value2};
        error ->
            {error, no_reason}
    end.
```

You may also take a look at very generic `typr:proplist/2` type.

## OTP behaviors specs

A number of header files contains different predefined specs for OTP behaviors like `supervisor` or `gen_server`. You may just include it to your file and forgot about filling proper types for all of that `handle_` functions and more. You can find examples of `supervisor` module and `gen_server` module down below.

### supervisor

Typical supervisor with typespecs:

```erlang
-module(some_sup).
-behaviour(supervisor).
-include_lib("some_helper/include/some_specs_supervisor.hrl").

-export([start_link/0, init/1]).



%% Interface



-spec start_link() ->
    typr:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {some_child_sup, {
            some_child_sup, start_link, []
        }, permanent, 5000, supervisor, [some_child_sup]},
        {some_worker, {
            some_worker, start_link, []
        }, permanent, 5000, worker, [some_worker]}
    ]}}.

```

### gen_server

You need to specify `state()` type for `typr_specs_gen_server.hrl` to work. Here is a very simple `gen_server` module that is handling some non-negative value in its state:

```erlang
-module(some_server).
-behaviour(gen_server).

-include_lib("typr/include/typr_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([set_value/1, get_value/0]).



-define(set_value(Value), {set_value, Value}).
-define(get_value(), {get_value}).

-type value() :: non_neg_integer().
-export_type([value/0]).

-record(state, {
    value = 0 :: value()
}).
-type state() :: #state{}.



%% Interface



-spec set_value(Value :: value()) ->
    %% ok | {error, badarg}
    Reply :: typr:generic_return(ErrorRet :: badarg).

set_value(Value) ->
    gen_server:call(?MODULE, ?set_value(Value)).



-spec get_value() ->
    %% {ok, value()}
    Reply :: typr:ok_return(OkRet :: value()).

get_value(Value) ->
    gen_server:call(?MODULE, ?get_value(Value)).



-spec start_link() ->
    %% {ok, pid()}
    typr:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% gen_server interface



init([]) ->
    {ok, #state{}}.



handle_call(?set_value(Value), _GenReplyTo, S0) ->
    handle_call_set_value(Value, S0);

handle_call(?get_value(), _GenReplyTo, S0) ->
    handle_call_get_value(S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_call_set_value(Value :: value(), State :: state()) ->
    %% {reply, ok | {error, badarg}, state()}
    typr:gen_server_reply_simple(
        Reply :: typr:generic_return(ErrorRet :: badarg),
        State :: state()
    ).

handle_call_set_value(Value, S0) when Value < 0 ->
    {reply, badarg, S0};

handle_call_set_value(Value, S0) ->
    {reply, ok, S0#state{value = Value}}.



-spec handle_call_get_value(Value :: value(), State :: state()) ->
    %% {reply, {ok, value()}, state()}
    typr:gen_server_reply_simple(
        Reply :: typr:ok_return(OkRet :: value()),
        State :: state()
    ).

handle_call_get_value(S0) ->
    {reply, {ok, S0#state.value}, S0}.
```
