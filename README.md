# msglocks
[![Build Status](https://img.shields.io/travis/whitfin/msglocks.svg?label=unix)](https://travis-ci.org/whitfin/msglocks) [![Hex.pm Version](https://img.shields.io/hexpm/v/msglocks.svg)](https://hex.pm/packages/msglocks) [![Documentation](https://img.shields.io/badge/docs-latest-blue.svg)](https://hexdocs.pm/msglocks/)

This library is designed to provide simple locking mechanisms in Erlang/Elixir, similar to
how spinlocks work in other languages - except using messages to communicate locking.

This is useful for libraries which require lock synchronization, without having to roll your
own (however simple). Locks can be held by arbitrary numbers of process, making it possible
to implement various throttling mechanisms.

Best of all, this library is tiny! It builds upon basic OTP principles to implement lock
behaviour via simple processes and message passing.

## Installation

### Rebar

Follow the instructons found [here](https://hex.pm/docs/rebar3_usage) to configure your
Rebar setup to use Hex as a dependency source, then you can grab it directly:

```erlang
{deps,[
  % pulls the latest version
  msglocks,
  % to pull the latest version from github
  {msglocks, {git, "git://github.com/whitfin/msglocks.git"}}
]}.
```

### Mix

To install it for your project, you can pull it directly from Hex. Rather
than use the version shown below, you can use the latest version from
Hex (shown at the top of this README).

```elixir
def deps do
  [{:msglocks, "~> 0.1"}]
end
```

## Usage

### Erlang

```erlang
% create a new single lock (with a name)
1> msglocks:new(1, [{name, {local, my_lock}}]).
{ok,<0.179.0>}

% take ownership of the lock
2> msglocks:acquire(my_lock).
ok

% release the current hold on a lock
3> msglocks:release(my_lock).
ok

% attempt to acquire a lock (which will succeed)
4> msglocks:attempt(my_lock).
ok

% now that it's taken, other attempts will fail
5> msglocks:attempt(my_lock).
{error,unavailable}

% release the lock again
6> msglocks:release(my_lock).
ok

% handle acquisition and locking automatically
7> msglocks:execute(my_lock, fun() ->
7>   3
7> end).
3
```

### Elixir

```elixir
# create a new single lock (with a name)
iex(1)> :msglocks,new(1, [ name: :my_lock ])
{:ok, #PID<0.179.0>}

# take ownership of the lock
iex(2)> :msglocks.acquire(:my_lock)
:ok

# release the current hold on a lock
iex(3)> :msglocks.release(:my_lock)
:ok

# attempt to acquire a lock (which will succeed)
iex(4)> :msglocks.attempt(:my_lock)
:ok

# now that it's taken, other attempts will fail
iex(5)> :msglocks.attempt(:my_lock)
{:error, :unavailable}

# release the lock again
iex(6)> :msglocks.release(:my_lock)
:ok

% handle acquisition and locking automatically
iex(7)> :msglocks.execute(:my_lock, fn ->
iex(7)>   3
iex(7)> end)
3
```

## Examples

This example is in Elixir, but it should be fairly understandable for those coming from
both languages. It contains a counter that's global to the entire runtime, which creates
itself on the first call. This is useful because all applications using this counter would
increment atomically, even if they're not explicitly linked directly.

```elixir
# First create a new lock, with 2 slots only
{:ok, ref} = :msglocks.new(2)

# Then spawn 6 tasks, which each just sleep for 10 seconds
# after acquiring the lock. This means that 2 processes will
# acquire a lock and then release after 10 seconds. This
# will repeat 3 times (6 / 2) until 30 seconds are up.
for idx <- 1..6 do
  Task.start(fn ->
    :msglocks.execute(ref, fn ->
      IO.puts("Locked #{idx}")
      Process.sleep(10_000)
      IO.puts("Releasing #{idx}")
    end)
  end)
end
```
