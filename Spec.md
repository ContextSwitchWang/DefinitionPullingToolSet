#Definition Pulling Tool Set


This tool set is used to retrieve definition from wiktionary, which is suited for use in Anki.

###Prerequisite

#####      Must:
        wget, make, a bash compatible shell, perl (with module uri installed)

#####      Optional:
        A haskell compiler, ghc perferrable.

###Basic Usage:

* Write the vocabularies one line per word in text files  which must be ended with `.sc` extension. 
You want to use multiple files thus only changed ones will be rebuilt.

* Run `automake.sh` in the same directory to generate Makefile

* In the same directory run `make`. When you do that `get.sh` will be run for every source file modified since last build.

###Details:

* `ParserQ.hs`

    Used to encode word and its definition into a single `csv` record.

* `ParserD.hs`

    Used to edit and view `csv` file. You don't need to use it.

* `ParseWiki.hs` 

    Used to parse the html retrieved from wiktionary and extract the information you need.
    For example, when you are learning German, you obviously don't want to see definition for other languages.
    Modify it to suit you needs.

* `replace.hs`   

    Used to parse the words you wrote, which allows you to use escape sequence to write non-English character.
    For example, you can type German umlaut like `\u \a \o`, which would be replaced with `ü ä ö`. 
    Modify it to suit you needs.
