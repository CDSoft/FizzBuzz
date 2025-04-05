---
title: Fizz buzz - LuaX demo
date: @DATE
author: @AUTHOR
keywords:
    - Lua
    - Script
    - Documentation
    - Tests
    - Build system
fontsize: 10pt
---

```meta
logo = os.getenv "LOGO"
```

# Disclaimer

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=50%}

This document is not about Fizzbuzz.

This document is a suggestion to simplify the build process of software
projects.

Fizzbuzz is just an application example.

::::::::::

:::::::::: {.column width=40%}

![]({{logo}}){width=100%}

::::::::::

:::::::::::::::::::::::::::::

# Introduction

Lots of software projects involve various tools, free as well as commercial.

## Lots of different data formats and scripting languages

- projects are less scalable and harder to maintain
- sharing data is painful and counter productive

e.g.: Bash, Python, Javascript, plain text, YAML, JSON, XML, CSV, INI, TOML, ...

## Suggested solution

- a **common, simple and powerful data format**
- and a **reduced set of highly configurable tools**.

# Lua - General programming language

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Lua is a

- powerful,
- efficient,
- lightweight,
- embeddable scripting language.

## It supports

- procedural programming,
- object-oriented programming,
- functional programming,
- data-driven programming,
- and data description.

::::::::::

:::::::::: {.column width=48%}

## Why choose Lua

- proven, robust language
- fast
- portable
- powerful but simple
- small
- free

## Get Lua

<https://www.lua.org>

::::::::::

:::::::::::::::::::::::::::::

# LuaX - Extended Lua interpreter / compiler

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Lua eXtended

- Lua interpreter and REPL
- based on Lua 5.4
- more built-in packages
- multiplatform Lua *compiler*
- zero dependency

## Improved Lua prompt

- history
- human-readable tables

## General modules

- `F`: functional programming
- `fs`: file system
- `sh`: shell commands

::::::::::

:::::::::: {.column width=48%}

## Math modules

- `mathx`: `math` extension
- `imath`: arbitrary precision
- `qmath`: rational numbers
- `complex`: complex numbers

## And more...

- `crypt`: cryptography
- `lz4`: compression
- `lpeg`: parsers
- `luasocket`: network
- ...

## Get LuaX

<https://codeberg.org/cdsoft/luax>

::::::::::

:::::::::::::::::::::::::::::

# Scripting with LuaX

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Fully compatible with Lua

- general programming language
- good documentation

## Lua tables

- common data format
- human-readable and structured

## project/software configuration

- Lua tables
- project configuration
- software configuration
- readable by any Lua interpreter

::::::::::

:::::::::: {.column width=48%}

## Lua table usages

- documentation generation
- code generation
- test results
- test reports
- requirement coverage

## Get LuaX

<https://codeberg.org/cdsoft/luax>

::::::::::

:::::::::::::::::::::::::::::

# bang - Ninja file generator scriptable in LuaX

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## bang

- Ninja file generator scriptable in LuaX
- Lua/LuaX macros

## LuaX

- compiled with LuaX
- all LuaX modules available in bang build scripts

## How does bang work?

- bang takes a build description (a LuaX script)
- and generates a Ninja file

::::::::::

:::::::::: {.column width=48%}

## Features

- ninja primitives (variables, rules, build statements, ...)
- rule/build statement pairs described in a single function call
- file listing and filenames list management using LuaX modules
- functional programming (LuaX `F` module)
- pipe simulation using rule composition
- "clean", "install" and "help" targets

## Get bang

<https://codeberg.org/cdsoft/bang>

::::::::::

:::::::::::::::::::::::::::::

# ypp - text preprocessing

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## ypp

- minimalist and generic text preprocessor
- Lua/LuaX macros

## LuaX

- compiled with LuaX
- all LuaX modules available in ypp macros

## How does ypp work?

- ypp searches for Lua expressions
- and replaces their sources by their results

::::::::::

:::::::::: {.column width=48%}

## Features

- Lua expression evaluation
- file inclusion
- conditional text
- functional programming (LuaX `F` module)
- file management (LuaX `fs` module)
- requirement management (still experimental)

## Get ypp

<https://codeberg.org/cdsoft/ypp>

::::::::::

:::::::::::::::::::::::::::::

# ypp examples

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

@q[=====[

## File inclusion

```{.lua}
@include "file.md"
```

## Lua definitions

``````{.lua}
@@[[
    local foo = 42
    N = foo * 23 + 34
    local function sq(x)
        return x*x
    end
    function sumsq(n)
        return F.range(N)
            : map(sq)
            : sum()
    end
]]
``````

]=====]

@@[[
    local foo = 42
    N = foo * 23 + 34
    local function sq(x)
        return x*x
    end
    function sumsq(n)
        return F.range(N)
            : map(sq)
            : sum()
    end
]]

::::::::::

:::::::::: {.column width=48%}

## Lua macros

@q[=====[

``````{.markdown}
$$
\sum_{i=1}^{@N} i^2 =
    @sumsq(N)
$$
``````

]=====]

## Example

$$
\sum_{i=1}^{@N} i^2 =
    @sumsq(N)
$$

::::::::::

:::::::::::::::::::::::::::::

# Pandoc - Documentation converter / generation

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## What is Pandoc?

Pandoc is a swiss-army knife to convert from and to a bunch of document formats.

## Why Pandoc?

Pandoc uses Lua scripts:

- custom readers
- custom writers
- Lua filters on Pandoc AST

::::::::::

:::::::::: {.column width=48%}

## Excellent documentation

**Manual:**

<https://pandoc.org/MANUAL.html>

**Lua filters:**

<https://pandoc.org/lua-filters.html>


## Get Pandoc

<https://pandoc.org>

::::::::::

:::::::::::::::::::::::::::::

# Panda - Pandoc addons

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Panda

- Pandoc Lua filters
- variable expansion
- conditional blocks
- file inclusion
- script execution
- diagrams

::::::::::

:::::::::: {.column width=48%}

## Based on Pandoc Lua

- based on the Pandoc Lua interpreter
- LuaX specific functions are available

## Get Panda

<https://codeberg.org/cdsoft/panda>

::::::::::

:::::::::::::::::::::::::::::

# Panda examples

## Documentation extraction from source code

``````{.c include=deep_thought.c}

``````

# Panda examples

## Documentation extraction from source code

The Panda `doc` macro extract documentation (in Markdown) from an external file:

`````` markdown
:::{doc=deep_thought.c}
:::
``````

## This is rendered as

:::{doc=deep_thought.c}
:::

# Panda examples

``` meta
_dot = "{{dot}}"
_gnuplot = "{{gnuplot}}"
```

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Embedded diagrams

- code blocks
- replaced by an image by panda.

::::::::::

:::::::::: {.column width=48%}

```{.dot render="{{dot}}" name=slideshow-example-graphviz}
digraph {
    rankdir=LR;
    input -> pandoc -> output
    pandoc -> panda -> {pandoc, diagrams}
    { rank=same; pandoc, panda }
    { rank=same; diagrams, output }
}
```

::::::::::

:::::::::::::::::::::::::::::

## Example

`````` dot
    ```{.dot render="{{_dot}}"}
    digraph {
        rankdir=LR;
        input -> pandoc -> output
        pandoc -> panda -> {pandoc, diagrams}
        { rank=same; pandoc, panda }
        { rank=same; diagrams, output }
    }
    ```
``````

# Panda examples

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Embedded diagrams

- code blocks
- replaced by an image by panda.

::::::::::

:::::::::: {.column width=48%}

```{render="{{gnuplot}}" name=example-slideshow-gnuplot}
set xrange [-pi:pi]
set yrange [-1.5:1.5]
plot sin(x) lw 4, cos(x) lw 4
```

::::::::::

:::::::::::::::::::::::::::::

## Example

``````
    ```{render="{{_gnuplot}}"}
    set xrange [-pi:pi]
    set yrange [-1.5:1.5]
    plot sin(x) lw 4, cos(x) lw 4
    ```
``````

# Fizzbuzz

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Concrete example of using

- LuaX
- YPP
- Pandoc
- Panda

## using Lua tables everywhere

- requirement database (still experimental)
- tests results
- test report

::::::::::

:::::::::: {.column width=48%}

## to specify

- examples using ypp and Panda macros
- requirement management

## and test

- test execution
- test report

## Complete Fizzbuzz example

- [codeberg.org/cdsoft/fizzbuzz](https://codeberg.org/cdsoft/fizzbuzz)
- [fizzbuzz.pdf](https://codeberg.org/cdsoft/FizzBuzz/releases)
- [fizzbuzz.html](https://codeberg.org/cdsoft/FizzBuzz/releasesl)

::::::::::

:::::::::::::::::::::::::::::

# References

@@[[
    link = F.curry(function(name, url)
        return F.I{name=name, url=url}"[**$(name)**]($(url)): <$(url)>\n"
    end)
]]

- @link "Fizzbuzz repository" "https://codeberg.org/cdsoft/fizzbuzz"
- @link "Lua" "https://www.lua.org"
    - @link "Lua documentation" "https://www.lua.org/manual/5.4/"
- @link "LuaX" "https://codeberg.org/cdsoft/luax"
- @link "bang" "https://codeberg.org/cdsoft/bang"
- @link "ypp" "https://codeberg.org/cdsoft/ypp"
- @link "Pandoc" "https://pandoc.org"
    - @link "Pandoc manual" "https://pandoc.org/MANUAL.html"
    - @link "Pandoc Lua filters" "https://pandoc.org/lua-filters.html"
- @link "Panda" "https://codeberg.org/cdsoft/panda"
- @link "LuaX binaries" "https://codeberg.org/cdsoft/pub"

# Questions

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Web / email

[cdsoft.codeberg.page](https://cdsoft.codeberg.page)

## Github

[codeberg.org/cdsoft](https://codeberg.org/cdsoft)

::::::::::

:::::::::: {.column width=48%}

## Fizzbuzz

[codeberg.org/cdsoft/fizzbuzz](https://codeberg.org/cdsoft/fizzbuzz)

[fizzbuzz.pdf](https://codeberg.org/cdsoft/fizzbuzz/releases)

## LinkdIn

[linkedin.com/in/cdelord/](https://www.linkedin.com/in/cdelord/)

::::::::::

:::::::::::::::::::::::::::::
