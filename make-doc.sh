# Multiple HTML files
# Note: I can't seem to make Scribble generate these in the current
# directory, so, generate to an `index' subdir then `cp' them up.
scribble --htmls ++style gh.css ++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/" index.rkt
cp index/* ./
rm -rf index/

# All in one HTML file
scribble --html ++style gh.css ++xref-in setup/xref load-collections-xref --redirect-main "http://docs.racket-lang.org/" --dest-name all.html index.rkt

# Update <head> of HTML files
racket add-to-head.rkt
