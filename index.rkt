#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          scribble/racket
          racket/date
          (for-syntax racket/base)
          (for-label racket)
          (for-label racket/stxparam)
          (for-label syntax/parse)
          (for-label racket/splicing)
          (for-label racket/syntax))
@(define evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f])
     (make-evaluator 'racket)))

@(define typed/evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f])
     (make-evaluator 'typed/racket)))

@(define-syntax-rule (i body ...)
   (interaction #:eval evaluator body ...))

@(define (current-year)
   (number->string (date-year (current-date))))

@title[#:version ""]{Fear of Macros}
@author[@hyperlink["http://www.greghendershott.com"
                   "Greg Hendershott"]]
@image["fear-of-macros.jpg"]
@para{A practical guide to @hyperlink["https://www.racket-lang.org"]{Racket} macros.}
@para[@smaller{Copyright (c) 2012-@current-year[] by Greg Hendershott. All rights reserved.}]
@para[@smaller["Last updated "
               (parameterize ([date-display-format 'iso-8601])
                 (date->string (current-date) #t))]]
@para{Feedback and corrections are @hyperlink["https://github.com/greghendershott/fear-of-macros/issues" "welcome here"].}

@table-of-contents{}

@; ----------------------------------------------------------------------------

@section{Preface}

I learned @hyperlink["https://www.racket-lang.org"]{Racket} after 25
years of mostly using C and C++.

Some psychic whiplash resulted.

"All the parentheses" was actually not a big deal. Instead, the first
mind warp was functional programming. Before long I wrapped my brain
around it, and went on to become comfortable and effective with many
other aspects and features of Racket.

But two final frontiers remained: Macros and continuations.

I found that simple macros were easy and understandable, plus there
were many good tutorials available. But the moment I stepped past
routine pattern-matching, I kind of fell off a cliff into a
terminology soup. I marinaded myself in material, hoping it would
eventually sink in after enough re-readings. I even found myself using
trial and error, rather than having a clear mental model what was
going on. Gah.

I'm starting to write this at the point where the shapes are slowly
emerging from the fog.

@margin-note{If you have any corrections, criticisms, complaints, or whatever,
@hyperlink["https://github.com/greghendershott/fear-of-macros/issues" "please
let me know"].}

My primary motive is selfish. Explaining something forces me to learn
it more thoroughly. Plus if I write something with mistakes, other
people will be eager to point them out and correct me. Is that a
social-engineering variation of meta-programming? Next question,
please. :)

Finally I do hope it may help other people who have a similar
background and/or learning style as me.

I want to show how Racket macro features have evolved as solutions to
problems or annoyances. I learn more quickly and deeply when I
discover the answer to a question I already have, or find the solution
to a problem whose pain I already feel. Therefore I'll give you the
questions and problems first, so that you can better appreciate and
understand the answers and solutions.

@; ----------------------------------------------------------------------------

@section{Our plan of attack}

The macro system you will mostly want to use for production-quality
macros is called @racket[syntax-parse]. And don't worry, we'll get to
that soon.

But if we start there, you're likely to feel overwhelmed by concepts
and terminology, and get very confused. I did.

1. Instead let's start with the basics: A syntax object and a function
to change it---a "transformer". We'll work at that level for awhile to
get comfortable and to de-mythologize this whole macro business.

2. Soon we'll realize that pattern-matching would make life
easier. We'll learn about @racket[syntax-case] and its shorthand
cousin, @racket[define-syntax-rule]. We'll discover we can get
confused if we want to munge pattern variables before sticking them
back in the template, and learn how to do that.

3. At this point we'll be able to write many useful macros. But, what
if we want to write the ever-popular anaphoric if, with a "magic
variable"?  It turns out we've been protected from making certain kind
of mistakes. When we want to do this kind of thing on purpose, we use
a syntax parameter. [There are other, older ways to do this. We won't
look at them. We also won't spend a lot of time
advocating "hygiene"---we'll just stipulate that it's good.]

4. Finally, we'll realize that our macros could be smarter when
they're used in error. Normal Racket functions optionally can have
contracts and types. These catch usage mistakes and provide clear,
useful error messages. It would be great if there were something
similar for macro. There is. One of the more-recent Racket macro
enhancements is @racket[syntax-parse].


@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section{Transform!}

@verbatim[#:indent 2]{
YOU ARE INSIDE A ROOM.
THERE ARE KEYS ON THE GROUND.
THERE IS A SHINY BRASS LAMP NEARBY.

IF YOU GO THE WRONG WAY, YOU WILL BECOME
HOPELESSLY LOST AND CONFUSED.

> pick up the keys

YOU HAVE A SYNTAX TRANSFORMER
}


@subsection{What is a syntax transformer?}

A syntax transformer is not one of the トランスフォーマ
@hyperlink["http://en.wikipedia.org/wiki/Transformers" "transformers"].

Instead, it is simply a function. The function takes syntax and
returns syntax. It transforms syntax.

Here's a transformer function that ignores its input syntax, and
always outputs syntax for a string literal:

@margin-note{These examples assume @litchar{#lang racket}. If you want
to try them using @litchar{#lang racket/base}, you'll need to
@litchar{(require (for-syntax racket/base))}.}

@(let-syntax([syntax (make-element-id-transformer
                      (lambda (stx)
                        #'@racket[syntax]))]) ;print as syntax not #'
@i[
(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))
]
)

Using it:

@i[
(foo)
]

When we use @racket[define-syntax], we're making a transformer
@italic{binding}. This tells the Racket compiler, "Whenever you
encounter a chunk of syntax starting with @racket[foo], please give it
to my transformer function, and replace it with the syntax I give back
to you." So Racket will give anything that looks like @racket[(foo
...)] to our function, and we can return new syntax to use
instead. Much like a search-and-replace.

Maybe you know that the usual way to define a function in Racket:

@racketblock[(define (f x) ...)]

is shorthand for:

@racketblock[(define f (lambda (x) ...))]

That shorthand lets you avoid typing @racket[lambda] and some parentheses.

Well there is a similar shorthand for @racket[define-syntax]:

@(let-syntax([syntax (make-element-id-transformer
                      (lambda (stx)
                        #'@racket[syntax]))]) ;print as syntax not #'
@i[
(define-syntax (also-foo stx)
  (syntax "I am also foo"))
(also-foo)
]
)

What we want to remember is that this is simply shorthand. We are
still defining a transformer function, which takes syntax and returns
syntax. Everything we do with macros, will be built on top of this
basic idea. It's not magic.

Speaking of shorthand, there is also a shorthand for @racket[syntax],
which is @tt{#'}:

@margin-note{@tt{#'} is short for  @racket[syntax] much like
@tt{'} is short for @racket[quote].}

@i[
(define-syntax (quoted-foo stx)
  #'"I am also foo, using #' instead of syntax")
(quoted-foo)
]

We'll use the @tt{#'} shorthand from now on.

Of course, we can emit syntax that is more interesting than a
string literal. How about returning @racket[(displayln "hi")]?

@i[
(define-syntax (say-hi stx)
  #'(displayln "hi"))
(say-hi)
]

When Racket expands our program, it sees the occurrence of
@racket[(say-hi)], and sees it has a transformer function for that. It
calls our function with the old syntax, and we return the new syntax,
which is used to evaluate and run our program.

@; ----------------------------------------------------------------------------

@subsection{What's the input?}

Our examples so far have ignored the input syntax and output some
fixed syntax. But typically we will want to transform the input syntax
into something else.

Let's start by looking closely at what the input actually @italic{is}:

@i[
(define-syntax (show-me stx)
  (print stx)
  #'(void))
(show-me '(+ 1 2))
]

The @racket[(print stx)] shows what our transformer is given: a syntax
object.

A syntax object consists of several things. The first part is the
S-expression representing the code, such as @racket['(+ 1 2)].

Racket syntax is also decorated with some interesting information such
as the source file, line number, and column. Finally, it has
information about lexical scoping (which you don't need to worry about
now, but will turn out to be important later.)

There are a variety of functions available to access a syntax object.
Let's define a piece of syntax:

@i[
(define stx #'(if x (list "true") #f))
stx
]

Now let's use functions that access the syntax object. The source
information functions are:

@margin-note{@racket[(syntax-source stx)] is returning @racket['eval],
only because of how I'm generating this documentation, using an
evaluator to run code snippets in Scribble. Normally this would be
something like "my-file.rkt".}

@i[
(syntax-source stx)
(syntax-line stx)
(syntax-column stx)
]

More interesting is the syntax "stuff" itself. @racket[syntax->datum]
converts it completely into an S-expression:

@i[
(syntax->datum stx)
]

Whereas @racket[syntax-e] only goes "one level down". It may return a
list that has syntax objects:

@i[
(syntax-e stx)
]

Each of those syntax objects could be converted by @racket[syntax-e],
and so on recursively---which is what @racket[syntax->datum] does.

In most cases, @racket[syntax->list] gives the same result as
@racket[syntax-e]:

@i[
(syntax->list stx)
]

(When would @racket[syntax-e] and @racket[syntax->list] differ? Let's
not get side-tracked now.)

When we want to transform syntax, we'll generally take the pieces we
were given, maybe rearrange their order, perhaps change some of the
pieces, and often introduce brand-new pieces.


@; ----------------------------------------------------------------------------

@subsection{Actually transforming the input}

Let's write a transformer function that reverses the syntax it was
given:

@margin-note{The @racket[values] at the end of the example allows the
result to evaluate nicely.  Try
@racket[(reverse-me "backwards" "am" "i")] to see why it's handy.}
@i[
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
(reverse-me "backwards" "am" "i" values)
]

Understand Yoda, can we. Great, but how does this work?

First we take the input syntax, and give it to
@racket[syntax->datum]. This converts the syntax into a plain old
list:

@i[
(syntax->datum #'(reverse-me "backwards" "am" "i" values))
]

Using @racket[cdr] slices off the first item of the list,
@racket[reverse-me], leaving the remainder:
@racket[("backwards" "am" "i" values)]. Passing that to
@racket[reverse] changes it to @racket[(values "i" "am" "backwards")]:

@i[
(reverse (cdr '(reverse-me "backwards" "am" "i" values)))
]

Finally we use @racket[datum->syntax] to convert this back to
@racket[syntax]:

@i[
(datum->syntax #f '(values "i" "am" "backwards"))
]

That's what our transformer function gives back to the Racket
compiler, and @italic{that} syntax is evaluated:

@i[
(values "i" "am" "backwards")
]

@margin-note{The first argument of @racket[datum->syntax] contains the lexical
context information that we want to associate with the @racket[syntax]
outputted by the transformer. If the first argument is set to @racket[#f] then
no lexical context will be associated.}

@; ----------------------------------------------------------------------------

@subsection{Compile time vs. run time}

@codeblock0{
(define-syntax (foo stx)
  (make-pipe) ;Ce n'est pas le temps d'exécution
  #'(void))
}

Normal Racket code runs at ... run time. Duh.

@margin-note{Instead of "compile time vs. run time", you may hear it
described as "syntax phase vs. runtime phase". Same difference.}

But a syntax transformer is called by Racket as part of the process of
parsing, expanding, and compiling our program. In other words, our
syntax transformer function is evaluated at compile time.

This aspect of macros lets you do things that simply aren't possible
in normal code. One of the classic examples is something like the
Racket form, @racket[if]:

@racket[(if <condition> <true-expression> <false-expression>)]

If we implemented @racket[if] as a function, all of the arguments
would be evaluated before being provided to the function.

@i[
(define (our-if condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))
(our-if #t
        "true"
        "false")
]

That seems to work. However, how about this:

@i[
(define (display-and-return x)
  (displayln x)
  x)
(our-if #t
        (display-and-return "true")
        (display-and-return "false"))
]

@margin-note{One answer is that functional programming is good, and
side-effects are bad. But avoiding side-effects isn't always
practical.}

Oops. Because the expressions have a side-effect, it's obvious that
they are both evaluated. And that could be a problem---what if the
side-effect includes deleting a file on disk?  You wouldn't want
@racket[(if user-wants-file-deleted? (delete-file) (void))] to delete
a file even when @racket[user-wants-file-deleted?] is @racket[#f].

So this simply can't work as a plain function. However a syntax
transformer can rearrange the syntax -- rewrite the code -- at compile
time.  The pieces of syntax are moved around, but they aren't actually
evaluated until run time.

Here is one way to do this:

@i[
(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))
(our-if-v2 #t
           (display-and-return "true")
           (display-and-return "false"))
(our-if-v2 #f
           (display-and-return "true")
           (display-and-return "false"))
]

That gave the right answer. But how?  Let's pull out the transformer
function itself, and see what it did. We start with an example of some
input syntax:

@i[
(define stx (syntax (our-if-v2 #t "true" "false")))
(displayln stx)
]

1. We take the original syntax, and use @racket[syntax->list] to
change it into a @racket[list] of syntax objects:

@i[
(define xs (syntax->list stx))
(displayln xs)
]

2. To change this into a Racket @racket[cond] form, we need to take
the three interesting pieces---the condition, true-expression, and
false-expression---from the list using @racket[cadr], @racket[caddr],
and @racket[cadddr] and arrange them into a @racket[cond] form:

@racketblock[
`(cond [,(cadr xs) ,(caddr xs)]
       [else ,(cadddr xs)])
]

3. Finally, we change that into @racket[syntax] using
@racket[datum->syntax]:

@i[
(datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                          [else ,(cadddr xs)]))
]

So that works, but using @racket[cadddr] etc. to destructure a list is
painful and error-prone. Maybe you know Racket's @racket[match]?
Using that would let us do pattern-matching.

@margin-note{Notice that we don't care about the first item in the
syntax list. We didn't take @racket[(car xs)] in our-if-v2, and we
didn't use @racket[name] when we used pattern-matching. In general, a
syntax transformer won't care about that, because it is the name of
the transformer binding. In other words, a macro usually doesn't care
about its own name.}

Instead of:

@i[
(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))
]

We can write:

@i[
(define-syntax (our-if-using-match stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))]

Great. Now let's try using it:

@i[
(our-if-using-match #t "true" "false")
]

Oops. It's complaining that @racket[match] isn't defined.

Our transformer function is working at compile time, not run time. And
at compile time, only @racket[racket/base] is required for you
automatically---not the full @racket[racket].

Anything beyond @racket[racket/base], we have to require
ourselves---and require it for compile time using the
@racket[for-syntax] form of @racket[require].

In this case, instead of using plain @racket[(require racket/match)],
we want @racket[(require (for-syntax racket/match))]---the
@racket[for-syntax] part meaning, "for compile time".

So let's try that:

@i[
(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))
(our-if-using-match-v2 #t "true" "false")
]

Joy.

@; ----------------------------------------------------------------------------

@subsection{@racket[begin-for-syntax]}

We used @racket[for-syntax] to @racket[require] the
@racket[racket/match] module because we needed to use @racket[match]
at compile time.

What if we wanted to define our own helper function to be used by a
macro?  One way to do that is put it in another module, and
@racket[require] it using @racket[for-syntax], just like we did with
the @racket[racket/match] module.

If instead we want to put the helper in the same module, we can't
simply @racket[define] it and use it---the definition would exist at
run time, but we need it at compile time. The answer is to put the
definition of the helper function(s) inside @racket[begin-for-syntax]:

@racketblock[
(begin-for-syntax
 (define (my-helper-function ....)
   ....))
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)
]

In the simple case, we can also use @racket[define-for-syntax], which
composes @racket[begin-for-syntax] and @racket[define]:

@racketblock[
(define-for-syntax (my-helper-function ....)
  ....)
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)
]

To review:

@itemize[

@item{Syntax transformers work at compile time, not run time. The good
news is this means we can do things like rearrange the pieces of
syntax without evaluating them. We can implement forms like
@racket[if] that simply couldn't work properly as run time functions.}

@item{More good news is that there isn't some special, weird language
for writing syntax transformers. We can write these transformer
functions using the Racket language we already know and love.}

@item{The semi-bad news is that the familiarity can make it easy to forget
that we're not working at run time. Sometimes that's important to
remember.

  @itemize[

  @item{For example only @racket[racket/base] is required for us
automatically. If we need other modules, we have to require them, and
do so @italic{for compile time} using @racket[for-syntax].}

  @item{Similarly, if we want to define helper functions in the same
file/module as the macros that use them, we need to wrap the
definitions inside a @racket[begin-for-syntax] form. Doing so makes
them available at compile time.}

  ]
}
]

@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section[#:tag "pattern-matching"]{Pattern matching: syntax-case and syntax-rules}

Most useful syntax transformers work by taking some input syntax, and
rearranging the pieces into something else.  As we saw, this is
possible but tedious using list accessors such as
@racket[cadddr]. It's more convenient and less error-prone to use
@racket[match] to do pattern-matching.

@margin-note{Historically, @racket[syntax-case] and
@racket[syntax-rules] pattern matching came first. @racket[match] was
added to Racket later.}

It turns out that pattern-matching was one of the first improvements
to be added to the Racket macro system. It's called
@racket[syntax-case], and has a shorthand for simple situations called
@racket[define-syntax-rule].

Recall our previous example:

@racketblock[
(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))
]

Here's what it looks like using @racket[syntax-case]:

@i[
(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))
(our-if-using-syntax-case #t "true" "false")
]

Pretty similar, huh? The pattern matching part looks almost exactly
the same. The way we specify the new syntax is simpler. We don't need
to do quasi-quoting and unquoting. We don't need to use
@racket[datum->syntax]. Instead, we supply a "template", which uses
variables from the pattern.

There is a shorthand for simple pattern-matching cases, which expands
into @racket[syntax-case]. It's called @racket[define-syntax-rule]:

@i[
(define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))
(our-if-using-syntax-rule #t "true" "false")
]

Here's the thing about @racket[define-syntax-rule]. Because it's so
simple, @racket[define-syntax-rule] is often the first thing people are
taught about macros. But it's almost deceptively simple. It looks so
much like defining a normal run time function---yet it's not. It's
working at compile time, not run time. Worse, the moment you want to
do more than @racket[define-syntax-rule] can handle, you can fall off
a cliff into what feels like complicated and confusing
territory. Hopefully, because we started with a basic syntax
transformer, and worked up from that, we won't have that problem. We
can appreciate @racket[define-syntax-rule] as a convenient shorthand,
but not be scared of, or confused about, that for which it's
shorthand.

Most of the materials I found for learning macros, including the
Racket @italic{Guide}, do a very good job explaining
@hyperlink["http://docs.racket-lang.org/guide/pattern-macros.html" "how
patterns and templates work"]. So I won't regurgitate that here.

Sometimes, we need to go a step beyond the pattern and template. Let's
look at some examples, how we can get confused, and how to get it
working.

@; ----------------------------------------------------------------------------

@subsection{Pattern variable vs. template---fight!}

Let's say we want to define a function with a hyphenated name, a-b,
but we supply the a and b parts separately. The Racket @racket[struct]
macro does something like this: @racket[(struct foo (field1 field2))]
automatically defines a number of functions whose names are variations
on the name @racket[foo]---such as @racket[foo-field1],
@racket[foo-field2], @racket[foo?], and so on.

So let's pretend we're doing something like that. We want to transform
the syntax @racket[(hyphen-define a b (args) body)] to the syntax
@racket[(define (a-b args) body)].

A wrong first attempt is:

@i[
(define-syntax (hyphen-define/wrong1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" a b))])
       #'(define (name args ...)
           body0 body ...))]))
]

Huh. We have no idea what this error message means. Well, let's try to
work it out.  The "template" the error message refers to is the
@racket[#'(define (name args ...)  body0 body ...)] portion. The
@racket[let] isn't part of that template. It sounds like we can't use
@racket[a] (or @racket[b]) in the @racket[let] part.

In fact, @racket[syntax-case] can have as many templates as you
want. The obvious, required template is the final expression supplying
the output syntax. But you can use @racket[syntax] (a.k.a. @tt{#'}) on a
pattern variable. This makes another template, albeit a small, "fun
size" template. Let's try that:

@i[
(define-syntax (hyphen-define/wrong1.1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...)
           body0 body ...))]))
]

No more error---good! Let's try to use it:

@i[
(hyphen-define/wrong1.1 foo bar () #t)
(foo-bar)
]

Apparently our macro is defining a function with some name other than
@racket[foo-bar]. Huh.

This is where the Macro Stepper in DrRacket is
invaluable. @margin-note{Even if you prefer mostly to use Emacs, this
is a situation where it's definitely worth temporarily using DrRacket
for its Macro Stepper.}

@image[#:scale 0.5 "macro-stepper.png"]

The Macro Stepper says that the use of our macro:

@racketblock[
(hyphen-define/wrong1.1 foo bar () #t)
]

expanded to:

@racketblock[
(define (name) #t)
]

Well that explains it. Instead, we wanted to expand to:

@racketblock[
(define (foo-bar) #t)
]

Our template is using the symbol @racket[name] but we wanted its
value, such as @racket[foo-bar] in this use of our macro.

Is there anything we already know that behaves like this---where using
a variable in the template yields its value? Yes: Pattern
variables. Our pattern doesn't include @racket[name] because we don't
expect it in the original syntax---indeed the whole point of this
macro is to create it. So @racket[name] can't be in the main
pattern. Fine---let's make an @italic{additional} pattern. We can do
that using an additional, nested @racket[syntax-case]:

@i[
(define-syntax (hyphen-define/wrong1.2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a" #'a #'b)))
                  ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
]

Looks weird? Let's take a deep breath. Normally our transformer
function is given syntax by Racket, and we pass that syntax to
@racket[syntax-case].  But we can also create some syntax of our own,
on the fly, and pass @italic{that} to @racket[syntax-case]. That's all
we're doing here. The whole @racket[(datum->syntax ...)] expression is
syntax that we're creating on the fly. We can give that to
@racket[syntax-case], and match it using a pattern variable named
@racket[name]. Voila, we have a new pattern variable. We can use it in
a template, and its value will go in the template.

We might have one more---just one, I promise!---small problem left.
Let's try to use our new version:

@i[
(hyphen-define/wrong1.2 foo bar () #t)
(foo-bar)
]

Hmm. @racket[foo-bar] is @italic{still} not defined. Back to the Macro
Stepper. It says now we're expanding to:

@racketblock[(define (|#<syntax:11:24foo>-#<syntax:11:28 bar>|) #t)]

Oh right: @racket[#'a] and @racket[#'b] are syntax objects. Therefore

@racketblock[(string->symbol (format "~a-~a" #'a #'b))]

is the printed form of both syntax objects, joined by a hyphen:

@racketblock[|#<syntax:11:24foo>-#<syntax:11:28 bar>|]

Instead we want the datum in the syntax objects, such as the symbols
@racket[foo] and @racket[bar]. Which we get using
@racket[syntax->datum]:

@i[
(define-syntax (hyphen-define/ok1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
                  ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
(hyphen-define/ok1 foo bar () #t)
(foo-bar)
]

And now it works!

Next, some shortcuts.

@subsubsection{@racket[with-syntax]}

Instead of an additional, nested @racket[syntax-case], we could use
@racket[with-syntax]@margin-note*{Another name for
@racket[with-syntax] could be, "with new pattern variable".}. This
rearranges the @racket[syntax-case] to look more like a @racket[let]
statement---first the name, then the value. Also it's more convenient
if we need to define more than one pattern variable.

@i[
(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a
                                        (string->symbol (format "~a-~a"
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/ok2 foo bar () #t)
(foo-bar)
]

Again, @racket[with-syntax] is simply @racket[syntax-case] rearranged:

@racketblock[
(syntax-case #,(italic "<syntax>") () [#,(bold "<pattern>") <body>])
(with-syntax ([#,(bold "<pattern>") #,(italic "<syntax>")]) <body>)
]

Whether you use an additional @racket[syntax-case] or use
@racket[with-syntax], either way you are simply defining additional
pattern variables. Don't let the terminology and structure make it
seem mysterious.

@subsubsection{@racket[with-syntax*]}

We know that @racket[let] doesn't let us use a binding in a subsequent
one:

@i[
(let ([a 0]
      [b a])
  b)
]

Instead we can nest @racket[let]s:

@i[
(let ([a 0])
  (let ([b a])
    b))
]

Or use a shorthand for nesting, @racket[let*]:

@i[
(let* ([a 0]
       [b a])
  b)
]

Similarly, instead of writing nested @racket[with-syntax]s, we can use
@racket[with-syntax*]:

@i[
(require (for-syntax racket/syntax))
(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
      (with-syntax* ([b #'a]
                     [c #'b])
        #'c)]))
]

One gotcha is that @racket[with-syntax*] isn't provided by
@racket[racket/base]. We must @racket[(require (for-syntax
racket/syntax))]. Otherwise we may get a rather bewildering error
message:

@italic{@tt{...: ellipses not allowed as an expression in: ...}}.


@subsubsection{@racket[format-id]}

There is a utility function in @racket[racket/syntax] called
@racket[format-id] that lets us format identifier names more
succinctly than what we did above:

@i[
(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/ok3 bar baz () #t)
(bar-baz)
]

Using @racket[format-id] is convenient as it handles the tedium of
converting from syntax to symbol datum to string ... and all the way
back.

The first argument of @racket[format-id], @racket[lctx], is the
lexical context of the identifier that will be created. You almost
never want to supply @racket[stx] --- the overall chunk of syntax that
the macro transforms. Instead you want to supply some more-specific
bit of syntax, such as an identifier that the user has provided to the
macro. In this example, we're using @racket[#'a]. The resulting
identifier will have the same scope as that which the user provided.
This is more likely to behave as the user expects, especially when our
macro is composed with other macros.

@subsubsection{Another example}

Finally, here's a variation that accepts an arbitrary number of name
parts to be joined with hyphens:

@i[
(require (for-syntax racket/string racket/syntax))
(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(names ...))])
       (with-syntax ([name (datum->syntax (car name-stxs)
                                          (string->symbol
                                           (string-join (for/list ([name-stx name-stxs])
                                                          (symbol->string
                                                           (syntax-e name-stx)))
                                                        "-")))])
         #`(define (name args ...)
             body0 body ...)))]))
(hyphen-define* (foo bar baz) (v) (* 2 v))
(foo-bar-baz 50)
]

Just as when we used @racket[format-id], when using
@racket[datum->syntax] we're being careful with the first,
@racket[lctx] argument. We want the identifier we create to use the
lexical context of an identifier provided to the macro by the user. In
this case, the user's identifiers are in the @racket[(names ...)]
template variable. We change this from one @racket[syntax] into a
@racket[list] of @racket[syntax]es. The first element we use for the
lexical context. Then of course we'll use all the elements to form the
hyphenated identifier.

To review:

@itemize[

  @item{You can't use a pattern variable outside of a template. But
you can use @racket[syntax] or @tt{#'} on a pattern variable to make
an ad hoc, "fun size" template.}

  @item{If you want to munge pattern variables for use in the
template, @racket[with-syntax] is your friend, because it lets you
create new pattern variables.}

  @item{Usually you'll need to use @racket[syntax->datum] to get the
interesting value inside.}

  @item{@racket[format-id] is convenient for formatting identifier
names.}

]

@; ----------------------------------------------------------------------------

@subsection{Making our own @racket[struct]}

Let's apply what we just learned to a more-realistic example. We'll
pretend that Racket doesn't already have a @racket[struct]
capability. Fortunately, we can write a macro to provide our own
system for defining and using structures.  To keep things simple, our
structure will be immutable (read-only) and it won't support
inheritance.

Given a structure declaration like:

@racketblock[
(our-struct name (field1 field2 ...))
]

We need to define some procedures:

@itemize[

@item{A constructor procedure whose name is the struct name. We'll
represent structures as a @racket[vector]. The structure name will be
element zero. The fields will be elements one onward.}

@item{A predicate, whose name is the struct name with @tt{?}
appended.}

@item{For each field, an accessor procedure to get its value. These
will be named struct-field (the name of the struct, a hyphen, and the
field name).}

]


@#reader scribble/comment-reader
(i
(require (for-syntax racket/syntax))
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ;; Define a constructor.
           (define (id fields ...)
             (apply vector (cons (quote id) (list fields ...))))
           ;; Define a predicate.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ;; Define an accessor for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

;; Test it out
(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail?
           (lambda () (foo-a "furble")))

;; The tests passed.
;; Next, what if someone tries to declare:
(our-struct "blah" ("blah" "blah"))
)

The error message is not very helpful. It's coming from
@racket[format-id], which is a private implementation detail of our macro.

You may know that a @racket[syntax-case] clause can take an
optional "guard" or "fender" expression.  Instead of

@racketblock[
[pattern template]
]

It can be:

@racketblock[
[pattern guard template]
]

Let's add a guard expression to our clause:

@#reader scribble/comment-reader
(i
(require (for-syntax racket/syntax))
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     ;; Guard or "fender" expression:
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ;; Define a constructor.
           (define (id fields ...)
             (apply vector (cons (quote id) (list fields ...))))
           ;; Define a predicate.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ;; Define an accessor for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

;; Now the same misuse gives a better error message:
(our-struct "blah" ("blah" "blah"))
)

Later, we'll see how @racket[syntax-parse] makes it even easier to
check usage and provide helpful messages about mistakes.


@subsection[#:tag "hash.refs"]{Using dot notation for nested hash lookups}

The previous two examples used a macro to define functions whose names
were made by joining identifiers provided to the macro. This example
does the opposite: The identifier given to the macro is split into
pieces.

If you write programs for web services you deal with JSON, which is
represented in Racket by a @racket[jsexpr?]. JSON often has
dictionaries that contain other dictionaries. In a @racket[jsexpr?]
these are represented by nested @racket[hasheq] tables:

@#reader scribble/comment-reader
(i
; Nested `hasheq's typical of a jsexpr:
(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))
)

In JavaScript you can use dot notation:

@codeblock{
foo = js.a.b.c;
}

In Racket it's not so convenient:

@racketblock[(hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)]

We can write a helper function to make this a bit cleaner:

@#reader scribble/comment-reader
(i
;; This helper function:
(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
      ([k (in-list ks)])
      (hash-ref h k))))

;; Lets us say:
(hash-refs js '(a b c))
)

That's better. Can we go even further and use a dot notation somewhat
like JavaScript?

@#reader scribble/comment-reader
(i
;; This macro:
(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ;; If the optional `default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))
;; Gives us "sugar" to say this:
(hash.refs js.a.b.c)
;; Try finding a key that doesn't exist:
(hash.refs js.blah)
;; Try finding a key that doesn't exist, specifying the default:
(hash.refs js.blah 'did-not-exist)
)

It works!

We've started to appreciate that our macros should give helpful
messages when used in error. Let's try to do that here.

@#reader scribble/comment-reader
(i
(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ;; Check for no args at all
    [(_)
     (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain)]
    ;; If the optional `default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (unless (identifier? #'chain)
       (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain))
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       ;; Check that we have at least hash.key
       (unless (and (>= (length ids) 2)
                    (not (eq? (syntax-e (cadr ids)) '||)))
         (raise-syntax-error #f "Expected hash.key" stx #'chain))
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))

;; See if we catch each of the misuses
(hash.refs)
(hash.refs 0)
(hash.refs js)
(hash.refs js.)
)

Not too bad. Of course, the version with error-checking is quite a bit
longer. Error-checking code generally tends to obscure the logic, and
does here. Fortunately we'll soon see how @racket[syntax-parse] can
help mitigate that, in much the same way as contracts in normal
Racket or types in Typed Racket.

Maybe we're not convinced that writing @racket[(hash.refs js.a.b.c)]
is really clearer than @racket[(hash-refs js '(a b c))]. Maybe we
won't actually use this approach. But the Racket macro system makes it
a possible choice.

@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section{Syntax parameters}

"Anaphoric if" or "aif" is a popular macro example. Instead of writing:

@racketblock[
(let ([tmp (big-long-calculation)])
  (if tmp
      (foo tmp)
      #f))
]

You could write:

@racketblock[
(aif (big-long-calculation)
     (foo it)
     #f)
]

In other words, when the condition is true, an @racket[it] identifier
is automatically created and set to the value of the condition. This
should be easy:


@i[
(define-syntax-rule (aif condition true-expr false-expr)
  (let ([it condition])
    (if it
        true-expr
        false-expr)))
(aif #t (displayln it) (void))
]

Wait, what? @racket[it] is undefined?

It turns out that all along we have been protected from making a
certain kind of mistake in our macros. The mistake is if our new
syntax introduces a variable that accidentally conflicts with one in
the code surrounding our macro.

The Racket @italic{Reference} section,
@hyperlink["http://docs.racket-lang.org/reference/syntax-model.html#(part._transformer-model)" "Transformer
Bindings"], has a good explanation and example. Basically, syntax
has "marks" to preserve lexical scope. This makes your macro behave
like a normal function, for lexical scoping.

If a normal function defines a variable named @racket[x], it won't
conflict with a variable named @racket[x] in an outer scope:

@i[
(let ([x "outer"])
  (let ([x "inner"])
    (printf "The inner `x' is ~s\n" x))
  (printf "The outer `x' is ~s\n" x))
]

When our macros also respect lexical scoping, it's easier to write
reliable macros that behave predictably.

So that's wonderful default behavior. But sometimes we want to
introduce a magic variable on purpose---such as @racket[it] for
@racket[aif].

There's a bad way to do this and a good way.

The bad way is to use @racket[datum->syntax], which is tricky to use correctly. @margin-note*{See @hyperlink["http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf" "Keeping it Clean with Syntax Parameters (PDF)"].}

The good way is with a syntax parameter, using
@racket[define-syntax-parameter] and
@racket[syntax-parameterize]. You're probably familiar with regular
parameters in Racket:

@i[
(define current-foo (make-parameter "some default value"))
(current-foo)
(parameterize ([current-foo "I have a new value, for now"])
  (current-foo))
(current-foo)
]

That's a normal parameter. The syntax variation works similarly. The
idea is that we'll define @racket[it] to mean an error by
default. Only inside of our @racket[aif] will it have a meaningful
value:

@i[
(require racket/stxparam)
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))
(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          true-expr)
        false-expr)))
(aif 10 (displayln it) (void))
(aif #f (displayln it) (void))
]

Inside the @racket[syntax-parameterize], @racket[it] acts as an alias
for @racket[tmp]. The alias behavior is created by
@racket[make-rename-transformer].

If we try to use @racket[it] outside of an @racket[aif] form, and
@racket[it] isn't otherwise defined, we get an error like we want:

@i[
(displayln it)
]

But we can still define @racket[it] as a normal variable in local
definition contexts like:

@i[
(let ([it 10])
  it)
]

or:

@i[
(define (foo)
  (define it 10)
  it)
(foo)
]


For a deeper look, see @hyperlink["http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf" "Keeping it Clean with Syntax Parameters"].

@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section{What's the point of @racket[splicing-let]?}

I stared at @racket[racket/splicing] for the longest time. What does
it do? Why would I use it? Why is it in the Macros section of the
reference?

Step one, @elem[#:style "strike"]{cut a hole in the box}
de-mythologize it. For example, using @racket[splicing-let] like this:

@#reader scribble/comment-reader
(i
(require racket/splicing)
(splicing-let ([x 0])
  (define (get-x)
    x))
;; get-x is visible out here:
(get-x)
;; but x is not:
x
)

is equivalent to:

@#reader scribble/comment-reader
(i
(define get-y
  (let ([y 0])
    (lambda ()
      y)))
;; get-y is visible out here:
(get-y)
;; but y is not:
y
)

This is the classic Lisp/Scheme/Racket idiom sometimes called "let
over lambda". @margin-note*{A
@hyperlink["http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg03277.html" "koan"]
about closures and objects.} A closure hides @racket[y], which can
only be accessed via @racket[get-y].

So why would we care about the splicing forms? They can be more
concise, especially when there are multiple body forms:

@i[
(require racket/splicing)
(splicing-let ([x 0])
  (define (inc)
    (set! x (+ x 1)))
  (define (dec)
    (set! x (- x 1)))
  (define (get)
    x))
]

The splicing variation is more convenient than the usual way:

@#reader scribble/comment-reader
(i
(define-values (inc dec get)
  (let ([x 0])
    (values (lambda ()  ;inc
              (set! x (+ 1 x)))
            (lambda ()  ;dec
              (set! x (- 1 x)))
            (lambda ()  ;get
              x))))
)

When there are many body forms---and we're generating them in a
macro---the splicing variations can be much easier.

@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section{Robust macros: syntax-parse}

Functions can be used in error. So can macros.

@subsection{Error-handling strategies for functions}

With plain old functions, we have several choices how to handle
misuse.

1. Don't check at all.

@#reader scribble/comment-reader
(i
(define (misuse s)
  (string-append s " snazzy suffix"))
;; User of the function:
(misuse 0)
;; I guess I goofed, but -- what is this "string-append" of which you
;; speak??
)

The problem is that the resulting error message will be confusing. Our
user thinks they're calling @racket[misuse], but is getting an error
message from @racket[string-append].  In this simple example they
could probably guess what's happening, but in most cases they won't.

2. Write some error handling code.

@#reader scribble/comment-reader
(i
(define (misuse s)
  (unless (string? s)
    (error 'misuse "expected a string, but got ~a" s))
  (string-append s " snazzy suffix"))
;; User of the function:
(misuse 0)
;; I goofed, and understand why! It's a shame the writer of the
;; function had to work so hard to tell me.
)

Unfortunately the error code tends to overwhelm and/or obscure our
function definition. Also, the error message is good but not
great. Improving it would require even more error code.

3. Use a contract.

@#reader scribble/comment-reader
(i
(define/contract (misuse s)
  (string? . -> . string?)
  (string-append s " snazzy suffix"))
;; User of the function:
(misuse 0)
;; I goofed, and understand why! I'm happier, and I hear the writer of
;; the function is happier, too.
)

This is the best of both worlds.

The contract is a simple and concise. Even better, it's
declarative. We say what we want to happen, not how.

On the other hand the user of our function gets a very detailed error
message. Plus, the message is in a standard, familiar format.

4. Use Typed Racket.

@codeblock{#lang typed/racket}
@interaction[#:eval typed/evaluator
(: misuse (String -> String))
(define (misuse s)
  (string-append s " snazzy suffix"))
(misuse 0)
]

Even better, Typed Racket can catch usage mistakes up-front at compile
time.

@subsection{Error-handling strategies for macros}

For macros, we have similar choices.

1. Ignore the possibility of misuse. This choice is even worse for
macros. The default error messages are even less likely to make sense,
much less help our user know what to do.

2. Write error-handling code. We saw how much this complicated our
macros in our example of @secref["hash.refs"]. And while we're still
learning how to write macros, we especially don't want more cognitive
load and obfuscation.

3. Use @racket[syntax-parse]. For macros, this is the equivalent of
using contracts or types for functions. We can declare that input
pattern elements must be certain kinds of things, such as an
identifier. Instead of "types", the kinds are referred to as "syntax
classes". There are predefined syntax classes, plus we can define our
own.

@subsection{Using @racket[syntax-parse]}

November 1, 2012: So here's the deal. After writing everything up to
this point, I sat down to re-read the documentation for
@racket[syntax-parse]. It was...very understandable. I didn't feel
confused.

Why? The documentation has a nice
@hyperlink["http://docs.racket-lang.org/syntax/stxparse-intro.html"]{Introduction}
with many simple examples, followed by an
@hyperlink["http://docs.racket-lang.org/syntax/stxparse-examples.html"]{Examples}
section illustrating many real-world scenarios.

@italic{Update:} Furthermore, Ben Greenman has created a package whose
docs provide an excellent set of even more
@hyperlink["http://docs.racket-lang.org/syntax-parse-example/index.html"]{Syntax
Parse Examples}.

Furthermore, everything I'd learned up to this point prepared me to
appreciate what @racket[syntax-parse] does, and why. The details of
how to use it seem pretty straightforward, so far.

This might well be a temporary state of me "not knowing what I don't
know". As I dig in and use it more, maybe I'll discover something
confusing or tricky. If/when I do, I'll come back here and update
this.

But for now I'll focus on improving the previous parts.

@; ----------------------------------------------------------------------------
@; ----------------------------------------------------------------------------

@section{References and Acknowledgments}

Eli Barzilay's blog post,
@hyperlink["http://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html" "Writing
‘syntax-case’ Macros"], helped me understand many key details and
concepts, and inspired me to use a "bottom-up" approach.

Eli wrote another blog post,
@hyperlink["http://blog.racket-lang.org/2008/02/dirty-looking-hygiene.html" "Dirty
Looking Hygiene"], which explains @racket[syntax-parameterize]. I
relied heavily on that, mostly just updating it since his post was
written before PLT Scheme was renamed to Racket.

Matthew Flatt's
@hyperlink["http://www.cs.utah.edu/plt/publications/macromod.pdf" "Composable
and Compilable Macros: You Want it When? (PDF)"] explains how Racket
handles compile time vs. run time.

@hyperlink["http://www.scheme.com/tspl4/syntax.html#./syntax:h0" "Chapter
8"] of @italic{The Scheme Programming Language} by Kent Dybvig
explains @racket[syntax-rules] and @racket[syntax-case].

@hyperlink["http://www.ccs.neu.edu/racket/pubs/icfp10-cf.pdf" "Fortifying
Macros (PDF)"] is the paper by Ryan Culpepper and Matthias Felleisen
introducing @racket[syntax-parse].

Shriram Krishnamurthi looked at a very early draft and encouraged me
to keep going. Sam Tobin-Hochstadt and Robby Findler also encouraged
me. Matthew Flatt showed me how to make a Scribble
@racket[interaction] print @racket[syntax] as @racket["syntax"] rather
than as @racket["#'"]. Jay McCarthy helped me catch some mistakes and
confusions. Jon Rafkind provided suggestions. Kieron Hardy reported a
font issue and some typos.

Finally, I noticed something strange. After writing much of this, when
I returned to some parts of the Racket documentation, I noticed it had
improved since I last read it. Of course, it was the same; I'd
changed. It's interesting how much of what we already know is
projected between the lines. My point is, the Racket documentation is
very good. The @italic{Guide} provides helpful examples and
tutorials. The @italic{Reference} is very clear and precise.
