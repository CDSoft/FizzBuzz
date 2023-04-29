---
title: Fizz buzz - LuaX demo
date: @(DATE)
author: @(AUTHOR)
keywords:
    - Lua
    - Script
    - Documentation
    - Tests
    - Build system
---

```meta
logo = os.getenv "LOGO"
background = os.getenv "BACKGROUND"
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

# LuaX - Extended Lua interpretor / compiler

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Lua eXtended

- Lua interpretor and REPL
- based on Lua 5.4.4
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

<https://github.com/CDSoft/luax>

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
- readable by any Lua interpretor

::::::::::

:::::::::: {.column width=48%}

## Lua table usages

- documentation generation
- code generation
- test results
- test reports
- requirement coverage

## Get LuaX

<https://github.com/CDSoft/luax>

::::::::::

:::::::::::::::::::::::::::::

# UPP - text preprocessing

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## UPP

- minimalist and generic text preprocessor
- Lua/LuaX macros

## LuaX

- compiled with LuaX
- all LuaX modules available in UPP macros

## How does UPP work?

- UPP searches for Lua expressions
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

## Get UPP

<https://github.com/CDSoft/upp>

::::::::::

:::::::::::::::::::::::::::::

# UPP examples

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

?(false)

## File inclusion

```{.lua}
@(include "file.md")
```

## Lua definitions

``````{.lua}
@@( local foo = 42
    N = foo * 23 + 34
    local function sq(x)
        return x*x
    end
    function sumsq(n)
        return F.range(N)
            : map(sq)
            : sum()
    end
)
``````

?(true)

@@( local foo = 42
    N = foo * 23 + 34
    local function sq(x)
        return x*x
    end
    function sumsq(n)
        return F.range(N)
            : map(sq)
            : sum()
    end
)

::::::::::

:::::::::: {.column width=48%}

## Lua macros

?(false)

``````{.markdown}
$$
\sum_{i=1}^{@(N)} i^2 =
    @(sumsq(N))
$$
``````

?(true)

## Example

$$
\sum_{i=1}^{@(N)} i^2 =
    @(sumsq(N))
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

- based on the Pandoc Lua interpretor
- LuaX specific functions are available

## Get Panda

<https://github.com/CDSoft/panda>

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

```{.dot render="{{dot}}"}
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

```{render="{{gnuplot}}"}
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

# MakeX

## `makex.mk`

- single Makefile
- install LuaX, upp, pandoc, panda, ...
- meant to be included in a Makefile

## Simple installation

``` sh
wget http://cdelord.fr/makex/makex.mk
```

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Simple usage

``` makefile
include makex.mk
```

::::::::::

:::::::::: {.column width=48%}

## Get makex

<https://github.com/CDSoft/makex>

::::::::::

:::::::::::::::::::::::::::::

# Fizzbuzz

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Concrete example of using

- LuaX
- UPP
- Pandoc
- Panda
- with a complete Makefile example using `makex.mk`

## using Lua tables everywhere

- requirement database (still experimental)
- tests results
- test report

::::::::::

:::::::::: {.column width=48%}

## to specify

- examples using UPP and Panda macros
- requirement management

## and test

- test execution
- test report

## Complete Fizzbuzz example

- [github.com/CDSoft/fizzbuzz](https://github.com/CDSoft/fizzbuzz)
- [cdelord.fr/fizzbuzz](http://cdelord.fr/fizzbuzz):
    - [fizzbuzz.pdf](http://cdelord.fr/fizzbuzz/fizzbuzz.pdf)
    - [fizzbuzz.html](http://cdelord.fr/fizzbuzz/fizzbuzz.html)

::::::::::

:::::::::::::::::::::::::::::

# References

@@( link = F.curry(function(name, url)
        return F.I{name=name, url=url}"[**$(name)**]($(url)): <$(url)>\n"
    end)
)

- @(link "Fizzbuzz repository" "https://github.com/CDSoft/fizzbuzz")
- @(link "Lua" "https://www.lua.org")
    - @(link "Lua documentation" "https://www.lua.org/manual/5.4/")
- @(link "LuaX" "https://github.com/CDSoft/luax")
- @(link "UPP" "https://github.com/CDSoft/upp")
- @(link "Pandoc" "https://pandoc.org")
    - @(link "Pandoc manual" "https://pandoc.org/MANUAL.html")
    - @(link "Pandoc Lua filters" "https://pandoc.org/lua-filters.html")
- @(link "Panda" "https://github.com/CDSoft/panda")
- @(link "makex" "https://github.com/CDSoft/makex")

# Questions

::::::::::::::::::::::::::::: {.columns}

:::::::::: {.column width=48%}

## Web / email

[cdelord.fr](http://cdelord.fr)

## Github

[github.com/CDSoft](https://github.com/CDSoft)

::::::::::

:::::::::: {.column width=48%}

## Fizzbuzz

[github.com/CDSoft/fizzbuzz](https://github.com/CDSoft/fizzbuzz)

[cdelord.fr/fizzbuzz](http://cdelord.fr/fizzbuzz)
[cdelord.fr/fizzbuzz/fizzbuzz.pdf](http://cdelord.fr/fizzbuzz/fizzbuzz.pdf)

## LinkdIn

[linkedin.com/in/cdelord/](https://www.linkedin.com/in/cdelord/)

::::::::::

:::::::::::::::::::::::::::::
