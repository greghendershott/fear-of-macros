scrbl := index.rkt

html := /home/greg/src/blog/src/static/fear-of-macros

all: html

clean:
	rm -rf $(html)

html: html-single html-multi
	racket add-to-head.rkt

html-single: $(scrbl)
	raco scribble \
		--html \
		--dest $(html) \
		--dest-name all.html \
		++style gh.css \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(scrbl)

## TO-DO: Fix so scribble builds directly to $(html) not to
## fear-of-macros/subdir.
html-multi: $(scrbl)
	raco scribble \
		--htmls \
		--dest-name $(html) \
		++style gh.css \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(scrbl)
	cp fear-of-macros/* $(html)/
