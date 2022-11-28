#!/usr/bin/env luax

--[[@@@

The Lua implementation of Fizzbuzz is based on a functional style,
using function compositions.

It computes the `"fizz"` and `"buzz"` parts and return them
if at least one of them is not `nil`{.lua}.
Otherwise it returns its argument unchanged.

```{ .dot render="{{dot}}" width=100% }
digraph {

n [label="n" shape=oval]

compute_fizz [label="\"fizz\"\nif n = 0 (mod 3)" shape=box]
compute_buzz [label="\"buzz\"\nif n = 0 (mod 5)" shape=box]
concat [label="concat" shape=box]
select [label="Choose\n\"fizz\", \"buzz\", \"fizzbuzz\"\nor n" shape=box]

fizz [label="\"fizz\" or nil" shape=oval]
buzz [label="\"buzz\" or nil" shape=oval]
fizzbuzz [label="\"fizz\", \"buzz\", \"fizzbuzz\"\nor nil" shape=oval]
fizzbuzz_n [label="\"fizz\", \"buzz\", \"fizzbuzz\"\nor n" shape=oval]

n -> compute_fizz -> fizz -> concat
n -> compute_buzz -> buzz -> concat
n -> select
concat -> fizzbuzz -> select
select -> fizzbuzz_n

{ rank=same; concat, fizzbuzz, select }

}
```

```{.lua include="fizzbuzz.lua" pattern="%-%-%s*fizzbuzz%s*{%s*(.-)%s*%-%-%s*}" format="%1"}
```
@@@]]

local F = require "fun"

-- fizzbuzz {

local function div(d, s)
    return function(n)
        return n % d == 0 and s or nil
    end
end

local fizz = div(3, "fizz")
local buzz = div(5, "buzz")

local function concat(a, b)
    return a and (a..(b or "")) or b
end

local function fizzbuzz(n)
    return concat(fizz(n), buzz(n)) or n
end

-- }

local n = tonumber(arg[1])
assert(n, tostring(arg[1])..": not a number")

F.range(n)
    : map(fizzbuzz)
    : mapi(print)
