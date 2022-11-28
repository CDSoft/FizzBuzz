#!/usr/bin/env luax

local fs = require "fs"
local F = require "fun"

local result_file = arg[1]
local N = tonumber(arg[2])

assert(result_file and N, "Wrong arguments")

local indices, fizzbuzzes = fs.read(result_file)
    : lines()
    : map(string.words)
    : unzip()
indices = indices:map(tonumber)

local tests = F{}

-- fizzbuzz list used to render the test results
tests.fizzbuzz = fizzbuzzes

---------------------------------------------------------------------------
-- The number of line shall be N
---------------------------------------------------------------------------

tests.valid_number_of_lines =
    #indices == N and #fizzbuzzes == N
    and F.op.ueq(indices, F.range(N))

---------------------------------------------------------------------------
-- Multiples of 3 but not 5 are "fizz"
---------------------------------------------------------------------------

tests.valid_fizz =
    fizzbuzzes
        : filteri(function(i, _) return i%3 == 0 and i%5 ~= 0 end)
        : all(F.curry(F.op.eq) "fizz")

---------------------------------------------------------------------------
-- Multiples of 5 but not 3 are "buzz"
---------------------------------------------------------------------------

tests.valid_buzz =
    fizzbuzzes
        : filteri(function(i, _) return i%3 ~= 0 and i%5 == 0 end)
        : all(F.curry(F.op.eq) "buzz")

---------------------------------------------------------------------------
-- Multiples of 3 and 5 are "fizzbuzz"
---------------------------------------------------------------------------

tests.valid_fizzbuzz =
    fizzbuzzes
        : filteri(function(i, _) return i%3 == 0 and i%5 == 0 end)
        : all(F.curry(F.op.eq) "fizzbuzz")

---------------------------------------------------------------------------
-- Non multiples of 3 and 5 are themselves
---------------------------------------------------------------------------

tests.valid_numbers =
    fizzbuzzes
        : mapi(function(i, s)
            return i%3 == 0 or i%5 == 0 or F.read(s) == i
        end)
        : land()

---------------------------------------------------------------------------
-- Statistics
---------------------------------------------------------------------------

local results = tests
    : filtert(function(res) return type(res) == "boolean" end)
    : values()

tests.nb = #results
tests.nb_pass = #results:filter(F.curry(F.op.eq)(true))
tests.nb_fail = #results:filter(F.curry(F.op.eq)(false))

---------------------------------------------------------------------------
-- Format test results
---------------------------------------------------------------------------

print("--[[ Fizzbuzz output")
print("indices", F.show(indices))
print("fizzbuzzes", F.show(fizzbuzzes))
print("]]")

print("return", F.show(tests))
