#lang racket

;; Realm of kludge:
;;
;; AFIK no way via Scribble to put something into the <head> section.
;;
;; This takes "main.html", adds some GA code, and outputs "index.html".

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
</head>
EOF
)

(define all (string-append web-font ga-code))
(define subst (regexp-replace* "\n" all "")) ;minify

(define old (file->string "main.html"))
(define new (regexp-replace "</head>" old subst))
(with-output-to-file (build-path 'same "index.html")
  (lambda () (display new))
  #:mode 'text
  #:exists 'replace)
