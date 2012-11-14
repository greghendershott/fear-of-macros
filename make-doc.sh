scribble --htmls ++style gh.css ++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/" index.rkt
racket add-to-head.rkt
cp index/* ./
