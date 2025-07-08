AUTHOR = "Christophe Delord - <https://codeberg.org/cdsoft/fizzbuzz>"
DATE = os.date("%a %b %e, %Y", sh "git log -1 --format=%ct")

yreq.tags {
    "spec",
    "testplan",
    "test",
}

yreq.refs {
    "tests",
    "runs",
}
