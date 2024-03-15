local F = require "F"

---------------------------------------------------------------------------
section "Project directories"
---------------------------------------------------------------------------

var "builddir" ".build"
var "img" "img"

clean "$builddir"

local all = {}
require "atexit"(function()
    phony "all" (all)
    default "all"
end)

---------------------------------------------------------------------------
section "Help"
---------------------------------------------------------------------------

help.description "Fizzbuzz build system"
help "all" "compile, test and document FizzBuzz"

---------------------------------------------------------------------------
section "Tests"
---------------------------------------------------------------------------

local test_config = require "test_config"

rule "run_test" {
    command = { "$in", test_config.N, "> $out" }
}

section "Lua test"

acc(all) {
    build "$builddir/tests/fizzbuzz_lua" { "fizzbuzz.lua",
        command = "luax -q -o $out $in",
    },
    build "$builddir/tests/fizzbuzz_lua.txt" {
        "run_test", "$builddir/tests/fizzbuzz_lua",
    },
}

section "C test"

acc(all) {
    build "$builddir/tests/fizzbuzz_c" { "fizzbuzz.c",
        command = "gcc $in -o $out",
    },
    build "$builddir/tests/fizzbuzz_c.txt" {
        "run_test", "$builddir/tests/fizzbuzz_c",
    },
}

section "Haskell test"

acc(all) {
    build "$builddir/tests/fizzbuzz_hs" { "fizzbuzz.hs",
        command = "ghc -outputdir ${out}_tmp $in -o $out",
    },
    build "$builddir/tests/fizzbuzz_hs.txt" {
        "run_test", "$builddir/tests/fizzbuzz_hs",
    },
}

section "Test results"

rule "check" {
    command = { "luax", "$in", test_config.N, "> $out" }
}

acc(all) {
    F"lua c hs":words():map(function(lang)
        return build("$builddir/tests/result_"..lang..".lua") { "check",
            "fizzbuzz_test.lua",
            "$builddir/tests/fizzbuzz_"..lang..".txt",
        }
    end)
}

---------------------------------------------------------------------------
section "Documentation"
---------------------------------------------------------------------------

local env = {
    'export LUA_PATH="$builddir/tests/?.lua;./?.lua";',
    'export REQDB="$builddir/reqdb.lua";',
    'export REQTARGET="fizzbuzz.pdf";',
}

local ypp_flags = {
    "-p .",
    "-l project_data",
    "-l req",
}

rule "ypp" {
    command = { env, "ypp", ypp_flags, "--MD --MF $depfile", "$in -o $out" },
    depfile = "$builddir/dependencies/$out.d",
}

local pandoc_flags = {
    "--table-of-content",
    --"--fail-if-warnings",
}

local html_flags = {
    pandoc_flags,
    "--to html5",
    "--css", "$$PANDOC_USER_DATA_DIRECTORY/panam.css",
    "--embed-resources --standalone",
    "--mathml",
}

rule "panda_html" {
    command = {
        env,
        "export PANDA_TARGET=$out;",
        "export PANDA_DEP_FILE=$depfile;",
        "export LOGO=$logo_html;",
        "export PANDOC_USER_DATA_DIRECTORY=`pandoc -v | awk -F': *' '$$1==\"User data directory\" {print $$2}'`;",
        "panda", html_flags, "$in -o $out",
    },
    depfile = "$builddir/dependencies/$out.d",
    implicit_in = {
        "$logo_html",
    },
}

local pdf_flags = {
    pandoc_flags,
    "--number-sections",
    "--highlight-style tango",
    "--top-level-division=chapter",
}

rule "panda_pdf" {
    command = {
        env,
        "export PANDA_TARGET=$out;",
        "export PANDA_DEP_FILE=$depfile;",
        "export LOGO=$logo_pdf;",
        "panda", pdf_flags, "$in -o $out",
    },
    depfile = "$builddir/dependencies/$out.d",
    implicit_in = {
        "$logo_pdf",
    },
}

local markdown_flags = {
    pandoc_flags,
    "--to gfm",
    "--number-sections",
    "--highlight-style tango",
    "--top-level-division=chapter",
}

rule "panda_gfm" {
    command = {
        env,
        "export PANDA_TARGET=$out;",
        "export PANDA_DEP_FILE=$depfile;",
        "export LOGO=$logo_html;",
        "panda", markdown_flags, "$in -o $out",
    },
    depfile = "$builddir/dependencies/$out.d",
    implicit_in = {
        "$logo_html",
    },
}

local beamer_flags = {
    "--to beamer",
    "-V theme:Madrid",
    "-V colortheme:default",
}

rule "panda_beamer" {
    command = {
        env,
        "export PANDA_TARGET=$out;",
        "export PANDA_DEP_FILE=$depfile;",
        "export LOGO=$logo_pdf;",
        "panda", beamer_flags, "$in -o $out",
    },
    depfile = "$builddir/dependencies/$out.d",
    implicit_in = {
        "$logo_pdf",
    },
}

var "logo_pdf" "$builddir/logo.pdf"
var "logo_html" "$img/logo.svg"

rule "lsvg" {
    command = "lsvg $in -o $out --MF $depfile",
    depfile = "$builddir/$out.d",
}

acc(all) {
    build "$logo_pdf"  { "lsvg", "logo.lua" },
    build "$logo_html" { "lsvg", "logo.lua" },
}

local fizzbuzz_md = build "$builddir/fizzbuzz.md" { "ypp", "fizzbuzz.md",
    implicit_in = {
        "$builddir/tests/result_lua.lua",
        "$builddir/tests/result_c.lua",
        "$builddir/tests/result_hs.lua",
    },
}
acc(all) {
    build "$builddir/fizzbuzz.html" { "panda_html", fizzbuzz_md },
    build "$builddir/fizzbuzz.pdf"  { "panda_pdf",  fizzbuzz_md },
    build "README.md"               { "panda_gfm",  fizzbuzz_md },
}

acc(all) {
    build("$builddir/fizzbuzz_slideshow.pdf") { "panda_beamer",
        build "$builddir/fizzbuzz_slideshow.md" { "ypp", "fizzbuzz_slideshow.md" }
    }
}
