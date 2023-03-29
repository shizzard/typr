-module(typr).


%% Types


% application specs

-type application_start_type_normal() :: normal.
-type application_start_type_takeover() :: {takeover, Node :: node()}.
-type application_start_type_failover() :: {failover, Node :: node()}.
-type application_start_type() :: application_start_type_normal() | application_start_type_takeover() | application_start_type_failover().

-type application_start_return() :: ok_return(OkRet1 :: pid(), OkRet2 :: term()) | generic_return(OkRet :: pid(), ErrorRet :: term()).

-export_type([
    application_start_type_normal/0, application_start_type_takeover/0, application_start_type_failover/0, application_start_type/0,
    application_start_return/0
]).


% gen specs

-type gen_from() :: {Pid :: pid(), Tag :: term()}.

-export_type([gen_from/0]).

% gen_server specs

-type gen_server_generic_message() :: term().

-type gen_server_reply_simple(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT}.
-type gen_server_reply_timeout(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT, Timeout :: non_neg_integer() | infinity}.
-type gen_server_reply_hibernate(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT, hibernate}.
-type gen_server_reply_continue(ReplyT, StateT) :: {reply, Reply :: ReplyT, NewState :: StateT, Continue :: {continue, term()}}.
-type gen_server_reply(ReplyT, StateT) ::
    gen_server_reply_simple(ReplyT, StateT) |
    gen_server_reply_timeout(ReplyT, StateT) |
    gen_server_reply_hibernate(ReplyT, StateT) |
    gen_server_reply_continue(ReplyT, StateT).

-type gen_server_noreply_simple(StateT) :: {noreply, NewState :: StateT}.
-type gen_server_noreply_timeout(StateT) :: {noreply, NewState :: StateT, Timeout :: non_neg_integer() | infinity}.
-type gen_server_noreply_hibernate(StateT) :: {noreply, NewState :: StateT, hibernate}.
-type gen_server_noreply_continue(StateT) :: {noreply, NewState :: StateT, Continue :: {continue, term()}}.
-type gen_server_noreply(StateT) ::
    gen_server_noreply_simple(StateT) |
    gen_server_noreply_timeout(StateT) |
    gen_server_noreply_hibernate(StateT) |
    gen_server_noreply_continue(StateT).

-type gen_server_stop_simple(ReplyT, StateT) :: {stop, Reason :: term(), Reply :: ReplyT, NewState :: StateT}.
-type gen_server_stop_noreply(StateT) :: {stop, Reason :: term(), NewState :: StateT}.
-type gen_server_stop(ReplyT, StateT) :: gen_server_stop_simple(ReplyT, StateT) | gen_server_stop_noreply(StateT).

-type gen_server_return(ReplyT, StateT) :: gen_server_reply(ReplyT, StateT) | gen_server_noreply(StateT) | gen_server_stop(ReplyT, StateT).


-export_type([
    gen_server_generic_message/0,
    gen_server_reply_simple/2, gen_server_reply_timeout/2, gen_server_reply_hibernate/2, gen_server_reply_continue/2, gen_server_reply/2,
    gen_server_noreply_simple/1, gen_server_noreply_timeout/1, gen_server_noreply_hibernate/1, gen_server_noreply_continue/1, gen_server_noreply/1,
    gen_server_stop_simple/2, gen_server_stop_noreply/1, gen_server_stop/2,
    gen_server_return/2
]).


% gen_statem specs are defined in the gen_statem module itself
% see https://erlang.org/doc/man/gen_statem.html sect. "Data Types"

% supervisor specs

-type supervisor_start_link_return() :: generic_return(
    OkRet :: pid(),
    ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
) | ignore.
-type supervisor_child_id() :: term().
-type supervisor_mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type supervisor_restart() :: permanent | transient | temporary.
-type supervisor_shutdown() :: brutal_kill | non_neg_integer().
-type supervisor_child_type() :: worker | supervisor.
-type supervisor_modules() :: [module()] | dynamic.
-type supervisor_child_spec() ::
    #{id := supervisor_child_id(), start := supervisor_mfargs(), restart => supervisor_restart(), shutdown => supervisor_shutdown(), type => supervisor_child_type(), modules => supervisor_modules()} |
    {Id :: supervisor_child_id(), StartFunc :: supervisor_mfargs(), Restart :: supervisor_restart(), Shutdown :: supervisor_shutdown(), Type :: supervisor_child_type(), Modules :: supervisor_modules()}.
-type supervisor_strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.
-type supervisor_maxr() :: non_neg_integer().
-type supervisor_maxt() :: pos_integer().
-type supervisor_flags() ::
    #{strategy => supervisor_strategy(), intensity => supervisor_maxr(), period => supervisor_maxt()} |
    {RestartStrategy :: supervisor_strategy(), MaxR :: supervisor_maxr(), MaxT :: supervisor_maxt()}.
-type supervisor_init_return() ::
    {ok, {SupFlags :: supervisor_flags(), [ChildSpec :: supervisor_child_spec()]}} |
    ignore.

-export_type([
    supervisor_start_link_return/0,
    supervisor_strategy/0, supervisor_maxr/0, supervisor_maxt/0,
    supervisor_child_id/0, supervisor_mfargs/0, supervisor_restart/0,
    supervisor_shutdown/0, supervisor_child_type/0, supervisor_modules/0,
    supervisor_child_spec/0, supervisor_flags/0, supervisor_init_return/0
]).


% generic return values

-type ok_return() :: ok.
-type ok_return(RetT) :: {ok, Ret :: RetT}.
-type ok_return(RetT1, RetT2) :: {ok, Ret1 :: RetT1, Ret2 :: RetT2}.

-type error_return() :: error_return(term()).
-type error_return(ReasonT) :: {error, Reason :: ReasonT}.

-type generic_return() :: ok_return() | error_return().
-type generic_return(ErrorReasonT) :: ok_return() | error_return(ErrorReasonT).
-type generic_return(OkRetT, ErrorReasonT) :: ok_return(OkRetT) | error_return(ErrorReasonT).
-type generic_return(OkRet1, OkRet2, ErrorReasonT) :: ok_return(OkRet1, OkRet2) | error_return(ErrorReasonT).

-export_type([
    ok_return/0, ok_return/1, ok_return/2,
    error_return/0, error_return/1,
    generic_return/0, generic_return/1, generic_return/2, generic_return/3
]).
