pandoc=pandoc
ghc=ghc

.PHONY: all clean

all: README.md README.html ParserD  ParserQ	ParseWiki replace

README.md: Spec.md
	${pandoc} -f markdown README.md -o README.md

README.html: Spec.md
	${pandoc} -t slidy -s README.md -o README.html 


ParserD:   ParserD.hs
	${ghc} ParserD.hs
ParserQ:   ParserQ.hs
	${ghc} ParserQ.hs
ParseWiki: ParseWiki.hs
	${ghc} ParseWiki.hs
replace:   replace.hs  
	${ghc} replace.hs

clean:
	rm -f *.0 *.hi
