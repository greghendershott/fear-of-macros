SCRBL=index.rkt

all: publish

clean:
	rm -rf html

html: html-single html-multi
	racket add-to-head.rkt

html-single: $(SCRBL)
	raco scribble \
		--html \
		--dest html \
		--dest-name all.html \
		++style gh.css \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(SCRBL)

html-multi: $(SCRBL)
	raco scribble \
		--htmls \
		--dest-name html \
		++style gh.css \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(SCRBL)

pages:
	@(git branch -v | grep -q gh-pages || (echo local gh-pages branch missing; false))
	@echo
	@git branch -av | grep gh-pages
	@echo
	@(echo 'Is the branch up to date? Press enter to continue.'; read dummy)
	git clone -b gh-pages . pages

publish: html pages
	rm -rf pages/*
	cp -r html/. pages/.
	(cd pages; git add -A)
	-(cd pages; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	(cd pages; git push origin gh-pages)
	rm -rf pages
	git push origin gh-pages
