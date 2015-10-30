Definition Pulling Tool Set
===========================

This tool set is used to retrieve definition from wiktionary, which is
suited for use in Anki. But since the ouput file is `csv`, you can use
it for other purpose, too.

### Prerequisite

##### Must:

wget, make, a bash compatible shell, perl (with module uri installed)

##### Optional:

A haskell compiler, ghc perferrable. If you want to recompile.

### Basic Usage:

1.  Write the vocabularies one line per word in text files which must be
    ended with `.sc` extension. You want to use multiple files thus only
    changed ones will be rebuilt.

2.  Run `automake.sh` in the same directory of the source files to
    generate Makefile

3.  In the same directory run `make`. When you do that `get.sh` will be
    run for every source file modified since last build.

### Details:

-   `ParserQ.hs`

    Used to encode word and its definition into a single `csv` record.

-   `ParserD.hs`

    Used to edit and view `csv` file. You don't need to use it.

-   `ParseWiki.hs`

    Used to parse the html retrieved from wiktionary and extract the
    information you need. For example, when you are learning German, you
    obviously don't want to see definition for other languages. Modify
    it to suit you needs.

-   `replace.hs`

    Used to parse the words you wrote, which allows you to use escape
    sequence to write non-English character. For example, you can type
    German umlaut like `\u \a \o`, which would be replaced with `ü ä ö`.
    Modify it to suit you needs.

### Tips

-   You can put compiled `ParserQ`, `ParseWiki` and `replace` any where
    you like, but you must modify `get.sh` so it can find them.

-   You can modify or supply environment variable to `automake.sh` to
    change the output file name.

### Troubleshooting

(1) "Field max limit exceeded" or alike problem when importing
    generated files.

Too bad. You gotta find and truncate the field which is too long
manually. For example, the definition for `Haus` on wiktionary is just
too long. I can't make Anki import it anyhow.

### Contact

Send an email to <hughwung@gmail.com> if you have any questions.
