scrbl := index.rkt

www := fear-of-macros

.PHONY: all clean html html-single html-multi

all: html

clean:
	rm -rf $(www)

html: html-single html-multi
	racket add-to-head.rkt

html-single: $(scrbl)
	raco scribble \
		--html \
		--dest $(www) \
		--dest-name all.html \
		++style gh.css \
		++main-xref-in \
		--redirect-main https://docs.racket-lang.org/ \
		\
		$(scrbl)

html-multi: $(scrbl)
	raco scribble \
		--htmls \
		--dest-name $(www) \
		++style gh.css \
		++main-xref-in \
		--redirect-main https://docs.racket-lang.org/ \
		\
		$(scrbl)

######################################################################
# S3 bucket deploy

aws  := aws --profile greg
dest := s3://www.greghendershott.com/fear-of-macros/

.PHONY: deploy

deploy:
	$(aws) s3 sync --no-follow-symlinks $(www) $(dest)
