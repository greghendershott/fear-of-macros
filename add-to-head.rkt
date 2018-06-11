#lang racket

;; Realm of kludge:
;;
;; AFIK no way via Scribble to put something into the <head> section.
;;
;; This reads all HTML files and injects some stuff immediately before the
;; </head> closing tag.

(define (meta k v)
  (format "<meta name=\"~a\" content=\"~a\">" k v))

(define metas
  (string-append
   (meta "keywords" "Racket, macros, Scheme")
   (meta "description" "Practical Racket macros")
   (meta "author" "Greg Hendershott")
   (meta "charset" "utf-8")))

(define </head> "</head>")

(define all (string-append metas </head>))
(define subst (regexp-replace* "\n" all "")) ;minify

(define (do-file path)
  (define old (file->string path))
  (define new (regexp-replace </head> old subst))
  (with-output-to-file path
    (lambda () (display new))
    #:mode 'text
    #:exists 'replace))

(require racket/runtime-path)
(define-runtime-path here ".")
(for ([path (find-files (lambda (path)
                          (regexp-match? #rx"\\.html" path))
                        here)])
  (do-file path))
