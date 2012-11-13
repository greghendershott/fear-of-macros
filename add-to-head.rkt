#lang racket

;; Realm of kludge:
;;
;; AFIK no way via Scribble to put something into the <head> section.
;;
;; This reads "main.html", injects some stuff immediately before the
;; </head> closing tag, writes to "index.html".

(define web-font
  "<link href='http://fonts.googleapis.com/css?family=Fenix' rel='stylesheet' type='text/css'>")

(define ga-code
#<<EOF
<script type="text/javascript">
var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-29709446-1']);
_gaq.push(['_setDomainName', 'greghendershott.com']);
_gaq.push(['_trackPageview']);
(function() {
var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();
</script>
EOF
)

(define (meta k v)
  (format "<meta name=\"~a\" content=\"~a\">" k v))

(define metas
  (string-append
   (meta "keywords" "Racket,macros,Scheme")
   (meta "description" "Practical Racket macros")
   (meta "author" "Greg Hendershott")
   (meta "charset" "utf-8")))
  
(define </head> "</head>")

(define all (string-append metas web-font ga-code </head>))
(define subst (regexp-replace* "\n" all "")) ;minify

(define (do-file path)
  (define old (file->string path))
  (define new (regexp-replace </head> old subst))
  (with-output-to-file path
    (lambda () (display new))
    #:mode 'text
    #:exists 'replace))

(for ([path (find-files (lambda (path)
                          (regexp-match? #rx"\\.html" path))
                        (build-path 'same "index"))])
  (do-file path))
