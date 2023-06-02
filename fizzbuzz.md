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

titlepage: true

caption-justification: raggedright

toc-own-page: true

listings-disable-line-numbers: false
listings-no-page-break: true
disable-header-and-footer: false

footnotes-pretty: true
footnotes-disable-backlinks: true

book: true
classoption: oneside

titlepage-logo: "{{logo}}"
logo-width: 60mm

table-use-row-colors: true

code-block-font-size: "\\small"
---

```meta
logo = os.getenv "LOGO"
background = os.getenv "BACKGROUND"
```

# Disclaimer

This document is not about Fizzbuzz. This document is a suggestion to simplify
the build process of software projects. Fizzbuzz is just an application
example.

![]({{logo}}){width=50%}

# Introduction

Lots of software projects involve various tools, free as well as commercial, to
build the software, run the tests, produce the documentation, ... These tools
use different data formats and scripting languages, which makes the projects
less scalable and harder to maintain.

Sharing data between configuration files, documentations, tests results can
then be painful and counter productive (the necessary glue is often more
complex than the tools themselves).

Usually people script their build systems and processes with languages like
Bash, Python, Javascript and make them communicate with plain text, YAML, JSON,
XML, CSV, INI, TOML. Every script shall rely on specific (existing or not)
libraries to read and write these data formats.

This document presents a commun and powerful data format and some tools to
script the build process of a project and generate documentation.

To sum up the suggested solution is:

- a **single data format**
- and a **reduced set of highly configurable tools**.

# Lua[^lua]

[Lua](https://www.lua.org) is the perfect candidate for both a
common data format and a script language.

## What is Lua?

Lua is a powerful, efficient, lightweight, embeddable scripting language. It
supports procedural programming, object-oriented programming, functional
programming, data-driven programming, and data description.

Lua combines simple procedural syntax with powerful data description constructs
based on associative arrays and extensible semantics. Lua is dynamically typed,
runs by interpreting bytecode with a register-based virtual machine, and has
automatic memory management with incremental garbage collection, making it
ideal for configuration, scripting, and rapid prototyping.

[^lua]: from <https://www.lua.org/about.html>

## Why choose Lua?

**Lua is a proven, robust language**

Lua has been used in many industrial applications (e.g., Adobe's Photoshop
Lightroom), with an emphasis on embedded systems (e.g., the Ginga middleware
for digital TV in Brazil) and games (e.g., World of Warcraft and Angry Birds).
Lua is currently the leading scripting language in games. Lua has a solid
reference manual and there are several books about it. Several versions of Lua
have been released and used in real applications since its creation in 1993.
Lua featured in HOPL III, the Third ACM SIGPLAN History of Programming
Languages Conference, in 2007. Lua won the Front Line Award 2011 from the Game
Developers Magazine.

**Lua is fast**

Lua has a deserved reputation for performance. To claim to be "as fast as Lua"
is an aspiration of other scripting languages. Several benchmarks show Lua as
the fastest language in the realm of interpreted scripting languages. Lua is
fast not only in fine-tuned benchmark programs, but in real life too.
Substantial fractions of large applications have been written in Lua.

**Lua is portable**

Lua is distributed in a small package and builds out-of-the-box in all
platforms that have a standard C compiler. Lua runs on all flavors of Unix and
Windows, on mobile devices (running Android, iOS, BREW, Symbian, Windows
Phone), on embedded microprocessors (such as ARM and Rabbit, for applications
like Lego MindStorms), on IBM mainframes, etc.

**Lua is powerful (but simple)**

A fundamental concept in the design of Lua is to provide meta-mechanisms for
implementing features, instead of providing a host of features directly in the
language. For example, although Lua is not a pure object-oriented language, it
does provide meta-mechanisms for implementing classes and inheritance. Lua's
meta-mechanisms bring an economy of concepts and keep the language small, while
allowing the semantics to be extended in unconventional ways.

**Lua is small**

Adding Lua to an application does not bloat it. The tarball for Lua 5.4.4,
which contains source code and documentation, takes 353K compressed and 1.3M
uncompressed. The source contains around 30000 lines of C. Under 64-bit Linux,
the Lua interpreter built with all standard Lua libraries takes 281K and the
Lua library takes 468K.

**Lua is free**

Lua is free open-source software, distributed under a very liberal license (the
well-known MIT license). It may be used for any purpose, including commercial
purposes, at absolutely no cost. Just download it and use it.

# LuaX

[LuaX](https://github.com/CDSoft/luax) is a Lua interpretor and REPL based on
Lua 5.4.4, augmented with some useful packages. LuaX can also produce
standalone executables from Lua scripts.

LuaX runs on several platforms with no dependency:

- Linux (x86_64, i386, aarch64)
- MacOS (x86_64, aarch64)
- Windows (x86_64, i386)

LuaX can cross-compile scripts from and to any of these platforms.

LuaX comes with a standard Lua interpretor and provides some libraries (embedded
in a single executable, no external dependency required):

- [LuaX interactive usage](https://github.com/CDSoft/luax/blob/master/doc/repl.md): improved Lua REPL
- [F](https://github.com/CDSoft/luax/blob/master/doc/F.md): functional programming inspired functions
- [fs](https://github.com/CDSoft/luax/blob/master/doc/fs.md): file system management
- [sh](https://github.com/CDSoft/luax/blob/master/doc/sh.md): shell command execution
- [mathx](https://github.com/CDSoft/luax/blob/master/doc/mathx.md): complete math library for Lua
- [imath](https://github.com/CDSoft/luax/blob/master/doc/imath.md): arbitrary precision integer and rational arithmetic library
- [qmath](https://github.com/CDSoft/luax/blob/master/doc/qmath.md): rational number library
- [complex](https://github.com/CDSoft/luax/blob/master/doc/complex.md): math library for complex numbers based on C99
- [ps](https://github.com/CDSoft/luax/blob/master/doc/ps.md): Process management module
- [sys](https://github.com/CDSoft/luax/blob/master/doc/sys.md): System module
- [crypt](https://github.com/CDSoft/luax/blob/master/doc/crypt.md): cryptography module
- [lz4](https://github.com/CDSoft/luax/blob/master/doc/lz4.md): Extremely Fast Compression algorithm
- [lpeg](https://github.com/CDSoft/luax/blob/master/doc/lpeg.md): Parsing Expression Grammars For Lua
- [linenoise](https://github.com/CDSoft/luax/blob/master/doc/linenoise.md): light readline alternative
- [luasocket](https://github.com/CDSoft/luax/blob/master/doc/luasocket.md): Network support for the Lua language
- [inspect](https://github.com/CDSoft/luax/blob/master/doc/inspect.md): Human-readable representation of Lua tables

More information here: <http://cdelord.fr/luax>

# Scripting with LuaX

LuaX can be used as a general programming language.
There are plenty of [good documentations for Lua](https://www.lua.org/docs.html)
and [LuaX](http://cdelord.fr/luax).

A big advantage of Lua is the usage of Lua tables as a common data format usable by various tools.
It is Human-readable and structured. It can be generated by Lua scripts but also by any software producing text files.

Typical usages are:

- project/software configuration
    - a Lua table can be used to describe a project or a software configuration
        - read by an embedded Lua interpretor
        - used to generate documentation or source code
- tests results
    - a test suite can generate test results as a Lua table
    - tests results can be used to render documentation (tests reports) and compute a test coverage

The next chapters present some tools written in Lua/LuaX or using Lua as a scripting engine.

# Ypp

Ypp is a minimalist and generic text preprocessor using Lua macros.

Ypp is compiled by LuaX, i.e. Lua and LuaX functions and modules are available
in macros.

More information here: <http://cdelord.fr/ypp>

Ypp is pretty simple. It searches for Lua expressions and replaces macros with their results.

?(false)

Macro       Result
----------- -------------------------------------------------------------------------
`@(...)`    Evaluates the Lua expression `...` and replaces the macro by its result
`@@(...)`   Executes the Lua chunk `...` and replaces the macro by its result (if not `nil`{.lua})

Some expression do not require parentheses (function calls).

?(true)

## Example

?(false)

``````{.markdown}
$$
\sum_{i=1}^{100} i^2 = @F.range(100):map(function(x) return x*x end):sum()
$$
``````

?(true)

is rendered as

> $$
> \sum_{i=1}^{100} i^2 = @F.range(100):map(function(x) return x*x end):sum()
> $$

Macros can also define variables reusable later by other macros.

?(false)
``````{.markdown}
@@[[
    local foo = 42
    N = foo * 23 + 34
    local function sq(x) return x*x end
    function sumsq(n) return F.range(N):map(sq):sum() end
]]
``````
?(true)

@@[[
    local foo = 42
    N = foo * 23 + 34
    local function sq(x) return x*x end
    function sumsq(n) return F.range(N):map(sq):sum() end
]]

defines `N` ($N = @N$) which can be read in a Lua expression or with ?(false)`@N`?(true)
and `sumsq` which computes the sum of squares.

Then

?(false)

``````{.markdown}
$$
\sum_{i=1}^{@N} i^2 = @sumsq(N)
$$
``````

?(true)

becomes

> $$
> \sum_{i=1}^{@N} i^2 = @sumsq(N)
> $$

# Pandoc

[Pandoc](https://pandoc.org/) is a swiss-army knife to convert from and to a
bunch of document formats.

A big advantage of Pandoc is the ability to use Lua scripts to define custom
readers and writers for unsupported formats and also Lua filters to manipulate
the pandoc abstract syntax tree (AST). This is the main pandoc feature
exercised in this document.

Pandoc has an excellent documentation:

- main pandoc documentation: <https://pandoc.org/MANUAL.html>
- Lua filter documentation: <https://pandoc.org/lua-filters.html>

Fizzbuzz uses pandoc Lua filters with Panda (see next chapter) which bundles
some useful filters in a single script.

# Panda

Panda is a [Pandoc Lua filter](https://pandoc.org/lua-filters.html) that works
on internal Pandoc’s AST.

It provides several interesting features:

- variable expansion (minimalistic templating)
- conditional blocks
- file inclusion (e.g. for source code examples)
- script execution (e.g. to include the result of a command)
- diagrams (Graphviz, PlantUML, ditaa, Asymptote, blockdiag, mermaid…)

The documentation of Panda is here: <http://cdelord.fr/panda>

## Examples

There are lots of examples in the documentation of panda. We will see here two of them.

**Documentation extraction from source code**

The source code can be documented by adding special marks in comments. The documentation shall be written in Markdown. The default mark is `@@@` and can be customized.

For instance, the following C source contains documentation that can be extracted and included to a Pandoc document.

``````{.c include=deep_thought.c}

``````

To extract the documentation, panda provides a macro to replace a `div` element by the documentation chunks from a file. E.g.:

`````` markdown
:::{doc=deep_thought.c}
:::
``````

will be replaced by:

> :::{doc=deep_thought.c}
> :::

**Diagrams**

Diagrams can be embedded in Pandoc documents. Diagrams are specified as code
blocks and are replaced by an image by panda.

``` meta
_dot = "{{dot}}"
_gnuplot = "{{gnuplot}}"
```

    ```{.dot render="{{_dot}}" width=67%}
    digraph {
        rankdir=LR;
        input -> pandoc -> output
        pandoc -> panda -> {pandoc, diagrams}
        { rank=same; pandoc, panda }
        { rank=same; diagrams, output }
    }
    ```

```{.dot render="{{dot}}" width=67%}
digraph {
    rankdir=LR;
    input -> pandoc -> output
    pandoc -> panda -> {pandoc, diagrams}
    { rank=same; pandoc, panda }
    { rank=same; diagrams, output }
}
```

    ```{render="{{_gnuplot}}" width=67%}
    set xrange [-pi:pi]
    set yrange [-1.5:1.5]
    plot sin(x) lw 4, cos(x) lw 4
    ```

```{render="{{gnuplot}}" width=67%}
set xrange [-pi:pi]
set yrange [-1.5:1.5]
plot sin(x) lw 4, cos(x) lw 4
```

# MakeX

`makex.mk` is a Makefile. It is intended to be included in any Makefile to
easily install some tools based on LuaX and Pandoc to pre-process files and
generate documents, using Lua as a common, simple and powerful scripting
language.

## Example

Fizzbuzz itself is an example of makex usage.

Easy installation, only `makex.mk` is needed:

``` sh
wget http://cdelord.fr/makex/makex.md
```

And easy usage with other Makefiles:

``` makefile
include makex.mk
```

# Fizzbuzz

Fizzbuzz is a concrete example of the usage of LuaX/ypp/pandoc/panda to specify
and test a software.

## Specification

From [Wikipedia](https://en.wikipedia.org/wiki/Fizz_buzz):

> Fizz buzz is a group word game for children to teach them about division.
> Players take turns to count incrementally, replacing any number divisible by
> three with the word "fizz", and any number divisible by five with the word
> "buzz".

`fizzbuzz` is a function that returns `"fizz"`, `"buzz"`, `"fizzbuzz"` or `n` for any positive integer `n`.

$$
    fizzbuzz : \mathbb{N}^+ \to \{fizz, buzz, fizzbuzz\} \cup \mathbb{N}^+
$$
$$
    fizzbuzz(n) =
        \begin{cases}
            \text{"fizzbuzz" }  & \text{if } (3|n) \land (5|n) \\
            \text{"fizz" }      & \text{if } (3|n) \land \lnot (5|n) \\
            \text{"buzz" }      & \text{if } (5|n) \land \lnot (3|n) \\
            n                   & \text{if } \lnot (3|n) \land \lnot (5|n) \\
        \end{cases}
$$

@@[[
    function fizzbuzz(n)
        if n % 15 == 0 then return "fizzbuzz" end
        if n % 3 == 0 then return "fizz" end
        if n % 5 == 0 then return "buzz" end
        return n
    end
]]

### Requirements

@req "SPEC_API: fizzbuzz command line argument"

The fizzbuzz program takes one argument that specify the number for fizzbuzz
values to generate.

@req "SPEC_OUT: fizzbuzz output on stdout"

The fizzbuzz program emits fizzbuzz values on the standard output.
Each line contains `n` and `fizzbuzz(n)`.

e.g.:

```
$ fizzbuzz 6
@F.range(6):map(function(n) return F{n, fizzbuzz(n)}:str "\t" end)
```

@req "SPEC_FIZZ: fizz when n is a multiple of 3 but not 5"

If `n` is a multiple of 3 but not 5, then `fizzbuzz(n)` is `"fizz"`.

@req "SPEC_BUZZ: buzz when n is a multiple of 5 but not 3"

If `n` is a multiple of 5 but not 3, then `fizzbuzz(n)` is `"buzz"`.

@req "SPEC_FIZZBUZZ: fizzbuzz n is a when multiple of 3 and 5"

If `n` is a multiple of 3 and 5, then `fizzbuzz(n)` is `"fizzbuzz"`.

@req "SPEC_NUM: n when n is a not a multiple of 3 and 5"

If `n` is a multiple of 3 and 5, then `fizzbuzz(n)` is `"fizzbuzz"`.

### Examples

@[[
    {
        "n  | fizzbuzz(n) | n | fizzbuzz(n) | n | fizzbuzz(n) | n | fizzbuzz(n) ",
        "---|-------------|---|-------------|---|-------------|---|-------------",
    }
    ..
    F.range(5):map(function(n)
        return F{
            n,      fizzbuzz(n),
            n+5,    fizzbuzz(n+5),
            n+10,   fizzbuzz(n+10),
            n+15,   fizzbuzz(n+15),
        }:str "|"
    end)
]]

## Implementation

### Lua implementation

:::{doc=fizzbuzz.lua shift=3}
:::

### C implementation

:::{doc=fizzbuzz.c shift=3}
:::

### Haskell implementation

:::{doc=fizzbuzz.hs shift=3}
:::

## Tests

The results of the Fizzbuzz executables are checked by the test script `fizzbuzz_test.lua`.
This script check the fizzbuzz results and produces a Lua table with the test results.
This script will later be used to build the test reports.

### Test plan

@@( test_cfg = require "test_config" )

Each fizzbuzz implementation is executed (with @test_cfg.N values). The results are
checked by `fizzbuzz_test.lua` and stored in a Lua table.

The fizzbuzz values are recorded in the `fizzbuzz` field of the test result table.

@req "TEST_API: number of fizzbuzz values" {
    refs = "SPEC_API",
}

The fizzbuzz list contains @test_cfg.N values.

The result of this test is recorded in the `valid_number_of_lines` field of the test result table.

@req "TEST_OUT: output on stdout" {
    refs = "SPEC_OUT",
}

The fizzbuzz list is emitted on stdout.

@req "TEST_FIZZ: \"fizz\" values" {
    refs = "SPEC_FIZZ",
}

All multiples of 3 but not 5 are `"fizz"`.

The result of this test is recorded in the `valid_fizz` field of the test result table.

@req "TEST_BUZZ: \"buzz\" values" {
    refs = "SPEC_BUZZ",
}

All multiples of 5 but not 3 are `"buzz"`.

The result of this test is recorded in the `valid_buzz` field of the test result table.

@req "TEST_FIZZBUZZ: \"fizzbuzz\" values" {
    refs = "SPEC_FIZZBUZZ",
}

All multiples of 3 and 5 are `"fizzbuzz"`.

The result of this test is recorded in the `valid_fizzbuzz` field of the test result table.

@req "TEST_NUM: integral values" {
    refs = "SPEC_NUM",
}

All non multiples of 3 and 5 are themselves.

The result of this test is recorded in the `valid_numbers` field of the test result table.

## Test reports

### Lua implementation

@@(lua_tests = require "result_lua")

The Lua fizzbuzz function returns:

@F.str(lua_tests.fizzbuzz, ", ")

@req.test "RES_LUA_API: number of fizzbuzz values" {
    refs = "TEST_API",
    status = lua_tests.valid_number_of_lines,
}

@req.test "RES_LUA_OUT: output on stdout" {
    refs = "TEST_OUT",
    status = lua_tests.valid_number_of_lines,
}

@req.test "RES_LUA_FIZZ: \"fizz\" values" {
    refs = "TEST_FIZZ",
    status = lua_tests.valid_fizz,
}

@req.test "RES_LUA_BUZZ: \"buzz\" values" {
    refs = "TEST_BUZZ",
    status = lua_tests.valid_buzz,
}

@req.test "RES_LUA_FIZZBUZZ: \"fizzbuzz\" values" {
    refs = "TEST_FIZZBUZZ",
    status = lua_tests.valid_fizzbuzz,
}

@req.test "RES_LUA_NUM: integral values" {
    refs = "TEST_NUM",
    status = lua_tests.valid_numbers,
}

**Summary**: @lua_tests.nb_pass / @lua_tests.nb tests passed

### C implementation

@@(c_tests = require "result_c")

The C fizzbuzz function returns:

@F.str(c_tests.fizzbuzz, ", ")

@req.test "RES_C_API: number of fizzbuzz values" {
    refs = "TEST_API",
    status = c_tests.valid_number_of_lines,
}

@req.test "RES_C_OUT: output on stdout" {
    refs = "TEST_OUT",
    status = c_tests.valid_number_of_lines,
}

@req.test "RES_C_FIZZ: \"fizz\" values" {
    refs = "TEST_FIZZ",
    status = c_tests.valid_fizz,
}

@req.test "RES_C_BUZZ: \"buzz\" values" {
    refs = "TEST_BUZZ",
    status = c_tests.valid_buzz,
}

@req.test "RES_C_FIZZBUZZ: \"fizzbuzz\" values" {
    refs = "TEST_FIZZBUZZ",
    status = c_tests.valid_fizzbuzz,
}

@req.test "RES_C_NUM: integral values" {
    refs = "TEST_NUM",
    status = c_tests.valid_numbers,
}

**Summary**: @c_tests.nb_pass / @c_tests.nb tests passed

### Haskell implementation

@@(hs_tests = require "result_hs")

The Haskell fizzbuzz function returns:

@F.str(hs_tests.fizzbuzz, ", ")

@req.test "RES_HS_API: number of fizzbuzz values" {
    refs = "TEST_API",
    status = hs_tests.valid_number_of_lines,
}

@req.test "RES_HS_OUT: output on stdout" {
    refs = "TEST_OUT",
    status = hs_tests.valid_number_of_lines,
}

@req.test "RES_HS_FIZZ: \"fizz\" values" {
    refs = "TEST_FIZZ",
    status = hs_tests.valid_fizz,
}

@req.test "RES_HS_BUZZ: \"buzz\" values" {
    refs = "TEST_BUZZ",
    status = hs_tests.valid_buzz,
}

@req.test "RES_HS_FIZZBUZZ: \"fizzbuzz\" values" {
    refs = "TEST_FIZZBUZZ",
    status = hs_tests.valid_fizzbuzz,
}

@req.test "RES_HS_NUM: integral values" {
    refs = "TEST_NUM",
    status = hs_tests.valid_numbers,
}

**Summary**: @hs_tests.nb_pass / @hs_tests.nb tests passed

### Lua / C / Haskell comparison

@[[
    {
        "n  | Lua | C | Haskell | Comparison",
        "---|-----|---|---------|-----------",
    } .. F.zip {
        lua_tests.fizzbuzz,
        c_tests.fizzbuzz,
        hs_tests.fizzbuzz,
    }:mapi(function (i, res)
        local expected = tostring(fizzbuzz(i))
        local ok = res:all(F.partial(F.op.eq, expected))
        return ({i}..res..{ok and "*OK*" or "**FAIL**"}):str "|"
    end)
]]

## Coverage matrix

@req.matrix "g"

```{.dot render="{{dot}}"}
@req.dot()
```

# References

@@[[
    link = F.curry(function(name, url)
        return F.I{name=name, url=url}"[**$(name)**]($(url)): <$(url)>\n"
    end)
]]

@link "Fizzbuzz repository" "https://github.com/CDSoft/fizzbuzz"
> This document is not about Fizzbuzz. This document is a suggestion to
> simplify the build process of software projects. Fizzbuzz is just an
> application example.

@link "Lua" "https://www.lua.org"
> Lua is a powerful, efficient, lightweight, embeddable scripting language. It
> supports procedural programming, object-oriented programming, functional
> programming, data-driven programming, and data description.

@link "Lua documentation" "https://www.lua.org/manual/5.4/"
> The reference manual is the official definition of the Lua language.

@link "LuaX" "https://github.com/CDSoft/luax"
> LuaX is a Lua interpretor and REPL based on Lua 5.4.4, augmented with some
> useful packages. LuaX can also produce standalone executables from Lua
> scripts.

@link "ypp" "https://github.com/CDSoft/ypp"
> Ypp is a minimalist and generic text preprocessor using Lua macros.

@link "Pandoc" "https://pandoc.org"
> Pandoc is a universal document converter. If you need to convert files from
> one markup format into another, pandoc is your swiss-army knife.

@link "Pandoc manual" "https://pandoc.org/MANUAL.html"
> Pandoc User’s Guide

@link "Pandoc's Markdown" "https://pandoc.org/MANUAL.html#pandocs-markdown"
> Pandoc understands an extended and slightly revised version of John Gruber’s
> Markdown syntax. This document explains the syntax, noting differences from
> original Markdown.

@link "Pandoc Lua filters" "https://pandoc.org/lua-filters.html"
> Pandoc has long supported filters, which allow the pandoc abstract syntax
> tree (AST) to be manipulated between the parsing and the writing phase.
> Traditional pandoc filters accept a JSON representation of the pandoc AST and
> produce an altered JSON representation of the AST. They may be written in any
> programming language, and invoked from pandoc using the `--filter` option.
>
> Although traditional filters are very flexible, they have a couple of
> disadvantages. First, there is some overhead in writing JSON to stdout and
> reading it from stdin (twice, once on each side of the filter). Second,
> whether a filter will work will depend on details of the user’s environment.
> A filter may require an interpreter for a certain programming language to be
> available, as well as a library for manipulating the pandoc AST in JSON form.
> One cannot simply provide a filter that can be used by anyone who has a
> certain version of the pandoc executable.
>
> Starting with version 2.0, pandoc makes it possible to write filters in Lua
> without any external dependencies at all. A Lua interpreter (version 5.3) and
> a Lua library for creating pandoc filters is built into the pandoc
> executable. Pandoc data types are marshaled to Lua directly, avoiding the
> overhead of writing JSON to stdout and reading it from stdin.

@link "Panda" "https://github.com/CDSoft/panda"
> Panda is a Pandoc Lua filter that works on internal Pandoc's AST.

# Appendices

This chapter contains the sources of this document.

## README.md

```{.markdown include=README.md}
```

## LICENSE

```{.markdown include=LICENSE}
```

## fizzbuzz.md

```{.markdown include=fizzbuzz.md}
```

## project_data.lua

```{.lua include=project_data.lua}
```

## fizzbuzz.lua

```{.lua include=fizzbuzz.lua}
```

## fizzbuzz.c

```{.c include=fizzbuzz.c}
```

## fizzbuzz.hs

```{.hs include=fizzbuzz.hs}
```

## test_config.lua

```{.lua include=test_config.lua}
```

## fizzbuzz_test.lua

```{.lua include=fizzbuzz_test.lua}
```

## Makefile

```{.makefile include=Makefile}
```

## makex.mk

```{.makefile include=makex.mk}
```
