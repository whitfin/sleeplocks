# sleeplocks
[![Build Status](https://img.shields.io/github/workflow/status/whitfin/sleeplocks/CI)](https://github.com/whitfin/sleeplocks/actions) [![Hex.pm Version](https://img.shields.io/hexpm/v/sleeplocks.svg)](https://hex.pm/packages/sleeplocks) [![Documentation](https://img.shields.io/badge/docs-latest-blue.svg)](https://hexdocs.pm/sleeplocks/)

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
  sleeplocks,
  % to pull the latest version from github
  {sleeplocks, {git, "git://github.com/whitfin/sleeplocks.git"}}
]}.
```

### Mix

To install it for your project, you can pull it directly from Hex. Rather
than use the version shown below, you can use the latest version from
Hex (shown at the top of this README).

```elixir
def deps do
  [{:sleeplocks, "~> 1.0"}]
end
```

## Usage

Snippets below contain sample usage in both Erlang and Elixir, and cover most of the small
API space offered by `sleeplocks`. For a more complete example, scroll down!

### Erlang

```erlang
% create a new single lock (with a name)
1> sleeplocks:new(1, [{name, {local, my_lock}}]).
{ok,<0.179.0>}

% take ownership of the lock
2> sleeplocks:acquire(my_lock).
ok

% release the current hold on a lock
3> sleeplocks:release(my_lock).
ok

% attempt to acquire a lock (which will succeed)
4> sleeplocks:attempt(my_lock).
ok

% now that it's taken, other attempts will fail
5> sleeplocks:attempt(my_lock).
{error,unavailable}

% release the lock again
6> sleeplocks:release(my_lock).
ok

% handle acquisition and locking automatically
7> sleeplocks:execute(my_lock, fun() ->
7>   3
7> end).
3
```

### Elixir

```elixir
# create a new single lock (with a name)
iex(1)> :sleeplocks.new(1, [ name: :my_lock ])
{:ok, #PID<0.179.0>}

# take ownership of the lock
iex(2)> :sleeplocks.acquire(:my_lock)
:ok

# release the current hold on a lock
iex(3)> :sleeplocks.release(:my_lock)
:ok

# attempt to acquire a lock (which will succeed)
iex(4)> :sleeplocks.attempt(:my_lock)
:ok

# now that it's taken, other attempts will fail
iex(5)> :sleeplocks.attempt(:my_lock)
{:error, :unavailable}

# release the lock again
iex(6)> :sleeplocks.release(:my_lock)
:ok

# handle acquisition and locking automatically
iex(7)> :sleeplocks.execute(:my_lock, fn ->
iex(7)>   3
iex(7)> end)
3
```

## Examples

This example is in Elixir, but it should be fairly understandable for those coming from
both languages. It simply spawns 6 processes which each attempt to hold a lock for 10
seconds. As the lock is created with only 2 slots, this runs for 30 seconds and 2 of our
spawned tasks can hold the lock at any given time.

```elixir
# First create a new lock, with 2 slots only
{:ok, ref} = :sleeplocks.new(2)

# Then spawn 6 tasks, which each just sleep for 10 seconds
# after acquiring the lock. This means that 2 processes will
# acquire a lock and then release after 10 seconds. This
# will repeat 3 times (6 / 2) until 30 seconds are up.
for idx <- 1..6 do
  Task.start(fn ->
    :sleeplocks.execute(ref, fn ->
      IO.puts("Locked #{idx}")
      Process.sleep(10_000)
      IO.puts("Releasing #{idx}")
    end)
  end)
end
```

