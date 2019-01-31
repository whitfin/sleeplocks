%% @doc
%% BEAM friendly spinlocks for Elixir/Erlang.
%%
%% This module provides a very simple API for managing locks
%% inside a BEAM instance. It's modeled on spinlocks, but works
%% through message passing rather than loops. Locks can have
%% multiple slots to enable arbitrary numbers of associated
%% processes. The moment a slot is freed, the next awaiting
%% process acquires the lock.
%%
%% All of this is done in a simple Erlang process so there's
%% very little dependency, and management is extremely simple.
-module(sleeplocks).
-compile(inline).

%% Public API
-export([new/1, new/2, acquire/1, attempt/1, execute/2, release/1, start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Record definition for internal use.
-record(lock, {slots, current=#{}, waiting=queue:new()}).

%% Inlining for convenience functions.
-compile({inline, [new/1, start_link/1, start_link/2]}).

%% Name references available to call a lock.
-type name() ::
    atom() |
    {local, Name :: atom()} |
    {global, GlobalName :: any()} |
    {via, Module :: atom(), ViaName :: any()}.

%% Startup call return types to pass back through.
-type start_ret() :: {ok, pid()} | ignore | {error, term()}.

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc
%% Creates a new lock with a provided concurrency factor.
-spec new(Slots :: pos_integer()) -> start_ret().
new(Slots) ->
    new(Slots, []).

%% @doc
%% Creates a new lock with a provided concurrency factor.
-spec new(Slots :: pos_integer(), Args :: list()) -> start_ret().
new(Slots, Args) when
    is_number(Slots),
    is_list(Args)
->
    case proplists:get_value(name, Args) of
        undefined ->
            gen_server:start_link(?MODULE, Slots, []);
        Name when is_atom(Name) ->
            gen_server:start_link({local, Name}, ?MODULE, Slots, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Slots, [])
    end.

%% @doc
%% Acquires a lock for the current process.
%%
%% This will block until a lock can be acquired.
-spec acquire(Name :: name()) -> ok.
acquire(Ref) ->
    gen_server:call(Ref, acquire, infinity).

%% @doc
%% Attempts to acquire a lock for the current process.
%%
%% In the case there are no slots available, an error will be
%% returned immediately rather than waiting.
-spec attempt(Name :: name()) -> ok | {error, unavailable}.
attempt(Ref) ->
    gen_server:call(Ref, attempt).

%% @doc
%% Executes a function when a lock can be acquired.
%%
%% The lock is automatically released after the function has
%% completed execution; there's no need to manually release.
-spec execute(Name :: name(), Exec :: fun(() -> any())) -> ok.
execute(Ref, Fun) ->
    acquire(Ref),
    try Fun() of
        Res -> Res
    after
        release(Ref)
    end.

%% @doc
%% Releases a lock held by the current process.
-spec release(Name :: name()) -> ok.
release(Ref) ->
    gen_server:call(Ref, release).

%% @hidden
%% Aliasing for Elixir interoperability.
-spec start_link(Slots :: pos_integer()) -> start_ret().
start_link(Slots) ->
    new(Slots).

%% @hidden
%% Aliasing for Elixir interoperability.
-spec start_link(Slots :: pos_integer(), Args :: list()) -> start_ret().
start_link(Slots, Args) ->
    new(Slots, Args).

%%====================================================================
%% Callback functions
%%====================================================================

%% @hidden
%% Initialization phase.
init(Slots) ->
    {ok, #lock{slots = Slots}}.

%% @hidden
%% Handles a lock acquisition (blocks until one is available).
handle_call(acquire, Caller, #lock{waiting = Waiting} = Lock) ->
    case try_lock(Caller, Lock) of
        {ok, NewLock} ->
            {reply, ok, NewLock};
        {error, unavailable} ->
            {noreply, Lock#lock{waiting = queue:snoc(Waiting, Caller)}}
    end;

%% @hidden
%% Handles an attempt to acquire a lock.
handle_call(attempt, Caller, Lock) ->
    case try_lock(Caller, Lock) of
        {ok, NewLock} ->
            {reply, ok, NewLock};
        {error, unavailable} = E ->
            {reply, E, Lock}
    end;

%% @hidden
%% Handles the release of a previously acquired lock.
handle_call(release, {From, _Ref}, #lock{current = Current} = Lock) ->
    NewLock = case maps:is_key(From, Current) of
        false -> Lock;
        true  ->
            NewCurrent = maps:remove(From, Current),
            next_caller(Lock#lock{current = NewCurrent})
    end,
    {reply, ok, NewLock}.

%% @hidden
%% Empty shim to implement behaviour.
handle_cast(_Msg, Lock) ->
    {noreply, Lock}.

%% @hidden
%% Empty shim to implement behaviour.
handle_info(_Msg, Lock) ->
    {noreply, Lock}.

%% @hidden
%% Empty shim to implement behaviour.
terminate(_Reason, _Lock) ->
    ok.

%% @hidden
%% Empty shim to implement behaviour.
code_change(_Vsn, Lock, _Extra) ->
    {ok, Lock}.

%%====================================================================
%% Private functions
%%====================================================================

%% Locks a caller in the internal locks map.
lock_caller({From, _Ref}, #lock{current = Current} = Lock) ->
    Lock#lock{current = maps:put(From, ok, Current)}.

%% Attempts to pass a lock to a waiting caller.
next_caller(#lock{waiting = Waiting} = Lock) ->
    case queue:out(Waiting) of
        {empty, {[], []}} ->
            Lock;
        {{value, Next}, NewWaiting} ->
            gen_server:reply(Next, ok),
            NewLock = lock_caller(Next, Lock),
            NewLock#lock{waiting = NewWaiting}
    end.

%% Attempts to acquire a lock for a calling process
try_lock(Caller, #lock{slots = Slots, current = Current} = Lock) ->
    case maps:size(Current) of
        S when S == Slots ->
            {error, unavailable};
        _ ->
            {ok, lock_caller(Caller, Lock)}
    end.

%% ===================================================================
%% Private test cases
%% ===================================================================

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").
-endif.
