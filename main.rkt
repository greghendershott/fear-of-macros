#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          racket/date
          (for-label racket)
          (for-label racket/stxparam)
          (for-label syntax/parse))
@(define evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket)))

@(define-syntax-rule (i body ...)
   (interaction #:eval evaluator body ...))

@image["fear-of-macros.jpg"]
@title[#:version ""]{Fear of Macros}
@author{Greg Hendershott}
@smaller{Copyright (c) 2012 by Greg Hendershott. All rights reserved.}
@para[@smaller["Last updated "
               (parameterize ([date-display-format 'iso-8601])
                 (date->string (current-date) #t))]]
@table-of-contents{}


@; ----------------------------------------------------------------------------

@section{Introduction}

I learned Racket after 25 years of doing C/C++ imperative programming.

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

My primary motive is selfish. Explaining something forces me to learn
it more thoroughly. Plus I expect that if I write something with
mistakes, other people will be eager to point them out and correct
me. Is that a social-engineering variation of meta-programming? Next
question, please. :)

Finally I do hope it may help other people who have a similar
background and/or learning style as me.

I want to show how Racket macro features have evolved as solutions to
problems or annoyances. I learn more quickly and deeply when I
discover the answer to a question I already have, or find the solution
to a problem whose pain I already feel. Therefore I'll give you the
questions and problems first, so that you can better appreciate and
understand the answers and solutions.

@; ----------------------------------------------------------------------------

@section{The plan of attack}

The macro system you will mostly want to use for production-quality
macros is called @racket[syntax-parse]. And don't worry, we'll get to
that soon.

But if we start there, you're likely to feel overwhelmed by concepts
and terminology, and get very confused. I did.

1. Instead let's start with the basics: A syntax object and a function
to change it (a "transformer"). We'll work at that level for awhile to
get comfortable and to de-mythologize this whole macro business.

2. Next, we'll realize that some pattern-matching would make life
easier. We'll learn about @racket[syntax-case], and its shorthand
cousin, @racket[define-syntax-rule]. We'll discover we can get
confused if we want to munge pattern variables before sticking them
back in the template, and learn how to do that.

3. At this point we'll be able to write many useful macros. But, what
if we want to write the ever-popular anaphoric if, with a "magic
variable"?  It turns out we've been protected from making certain kind
of mistakes. When we want to do this kind of thing on purpose, we use
a @racket[syntax parameter]. [There are other, older ways to do
this. We won't look at them. We also won't spend a lot of time talking
about "hygiene".]

4. Finally, we'll realize that our macros could be smarter when
they're used in error. Normal Racket functions can optionally have
contracts and types. These can catch mistakes and provide clear,
useful error messages. It would be great if there were something
similar for macros, and there is. One of the more-recent Racket macro
enhancements is @racket[syntax-parse].


@; ----------------------------------------------------------------------------

@section{Transformers}

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

Instead, it is quite simple. It is a function. The function takes
syntax and returns syntax. It transforms syntax.

Here's a transformer function that ignores its input syntax, and
always outputs syntax for a string literal:

@i[
(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))
(foo)
]

When we use @racket[define-syntax], we're making a transformer
@italic{binding}. This tells the Racket compiler, "Whenever you
encounter a chunk of syntax starting with @racket[foo], please give it
to my transformer function, and replace it with the syntax I give back
to you." So Racket will give anything that looks like @racket[(foo
...)] to our function, and we can change it. Much like a
search-and-replace.

Maybe you know that the usual way to define a function in Racket:

@racket[(define (f x) ...)]

is shorthand for:

@racket[(define f (lambda (x) ...))]

That shorthand lets you avoid typing @racket[lambda] and some parentheses.

Well there is a similar shorthand for @racket[define-syntax]:

@i[
(define-syntax (also-foo stx)
  (syntax "I am also foo"))
(also-foo)
]

What we want to remember is that this is simply shorthand. We are
still defining a transformer function, which takes syntax and returns
syntax. Everything we do with macros, will be built on top of this
basic idea. It's not magic.

Speaking of shorthand, there is also a shorthand for @racket[syntax],
which is @tt{#'}:

@i[
(define-syntax (quoted-foo stx)
  #'"I am also foo, using #' instead of syntax")
(quoted-foo)
]

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


@subsection{What is the input?}

Our examples so far have been ignoring the input syntax, and
outputting a fixed syntax. Usually, we want to transform the input to
something else.

But let's start by looking at what the input @italic{is}:

@i[
(define-syntax (show-me stx)
  (print stx)
  #'(void))
(show-me '(i am a list))
]

The @racket[(print stx)] shows what our transformer is given: a syntax
object.

A syntax object consists of several things. The first part is the
s-expression representing the code, such as @racket['(i am a
list)]. Racket (and Scheme and Lisp) expressions are s-expressions---
code and data have the same structure, and this makes it vastly easier
to rewrite syntax, i.e. write macros.

Racket syntax is also decorated with some interesting information such
as the source file, line number, and column. Finally, it has
information about lexical scoping (which you don't need to worry about
now, but will turn out to be important later.)

There are a variety of functions available to access a syntax object:

@i[
(define stx #'(if x (list "true") #f))
(syntax->datum stx)
(syntax-e stx)
(syntax->list stx)
(syntax-source stx)
(syntax-line stx)
(syntax-column stx)
]

When we want to transform syntax, we'll generally take the pieces we
were given, maybe rearrange their order, perhaps change some of the
pieces, and often introduce brand-new pieces.

@subsection{Actually transforming the input}

Let's write a transformer function that reverses the syntax it was
given:

@i[
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
(reverse-me "backwards" "am" "i" values)
]

What's going on here?  First we take the input syntax, and give it to
@racket[syntax->datum]. This converts the syntax into a plain old list:

@i[
(syntax->datum #'(reverse-me "backwards" "am" "i" values))
]

Using @racket[cdr] slices off the first item of the list,
@racket[reverse-me], leaving the remainder:
@racket[("backwards" "am" "i" values)]. Passing that to
@racket[reverse] changes it to @racket[(values "i" "am" "backwards")]:

@i[
(reverse (cdr '("backwards" "am" "i" values)))
]

Finally we use @racket[syntax->datum] to convert this back to
@racket[syntax]:

@i[
(datum->syntax #f '(values "i" "am" "backwards"))
]

That's what our transformer function gives back to the Racket
compiler, and @italic{that} syntax is evaluated:

@i[
(values "i" "am" "backwards")
]


@subsection{Compile time vs. run time}

Normal Racket code runs at ... run time. Duh.

@margin-note{Instead of "compile time vs. run time", you may hear it
described as "syntax phase vs. runtime phase". Same difference.}

But a syntax transformer is run by the Racket compiler, as part of the
process of parsing, expanding and understanding your code. In other
words, your syntax transformer function is evaluated at compile time.

This aspect of macros lets you do things that simply aren't possible
in normal code. One of the classic examples, is something like the
Racket @racket[if] form:

@racket[(if <condition> <true-expression> <false-expression>)]

If @racket[if] were implemented as a function, all of the arguments
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

1. We take the original syntax, and use @racket[syntax->datum] to
change it into a plain Racket @racket[list]:

@i[
(define xs (syntax->datum stx))
(displayln xs)
]

2. To change this into a Racket @racket[cond] form, we need to take
the three interesting pieces---the condition, true-expression, and
false-expression---from the list using @racket[cadr], @racket[caddr],
and @racket[cadddr] and arrange them into a @racket[cond] form:

@codeblock{
`(cond [,(cadr xs) ,(caddr xs)]
       [else ,(cadddr xs)])
}

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
                               [else ,false-expr]))]))
(our-if-using-match #t "true" "false")
]

But wait, we can't. It's complaining that @racket[match] isn't
defined. We haven't required the @racket[racket/match] module?

It turns out we haven't. Remember, this transformer function is
working at compile time, not run time. And at compile time, only
@racket[racket/base] is required for you automatically. If we want
something like @racket[racket/match], we have to require it
ourselves---and require it @italic{for compile time}. Instead of using
plain @racket[(require racket/match)], the way to say this is to use
@racket[(require (for-syntax racket/match))]---the @racket[for-syntax]
part meaning, "for compile time".

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

To review:

Syntax transformers work at compile time, not run time. The good
news is this means we can do things like delay evaluation, and
implement forms like @racket[if] which simply couldn't work properly
as run time functions.

Some other good news is that there isn't some special, weird language
for writing syntax transformers. We can write these transformer
functions using familiar Racket code. The semi-bad news is that the
familiarity can make it easy to forget that we're not working at run
time. Sometimes that's important to remember. For example only
@racket[racket/base] is required for us automatically. If we need other
modules, we have to require them, and we have to require them
@italic{for compile time} using @racket[(require (for-syntax))].

@; ----------------------------------------------------------------------------

@section{Pattern matching: syntax-case and syntax-rules}

Most useful syntax transformers work by taking some input syntax, and
rearranging the pieces into something else.  As we saw, this is
possible but tedious using list accessors such as @racket[cadddr]. It's
more convenient and less error-prone to use pattern-matching.

@margin-note{Historically, @racket[syntax-case] and
@racket[syntax-parse] pattern matching came first. @racket[match] was
added to Racket later.}

It turns out that pattern-matching was one of the first improvements
to be added to the Racket macro system. It's called
@racket[syntax-case], and has a shorthand for simple situations called
@racket[define-syntax-rule].

Recall our previous example:

@codeblock{
(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))
}

Here's what it looks like using @racket[syntax-case]:

@i[
(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))
(our-if-using-syntax-case #t "true" "false")
]

Pretty similar, huh?  The pattern part looks almost exactly the
same. The "template" part---where we specify the new syntax---is
simpler. We don't need to do quasi-quoting and unquoting. We don't need
to use @racket[datum->syntax]. We simply supply a template, which uses
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

@subsection{Patterns and templates}

Most of the materials I found for learning macros, including the
Racket Guide, do a very good job explaining how the patterns work. I'm
not going to regurgitate that here.

Instead, let's look at some ways we're likely to get tripped up.

@subsubsection{"A pattern variable can't be used outside of a template"}

Let's say we want to define a function with a hyphenated name, a-b,
but we supply the a and b parts separately. The Racket @racket[struct]
form does something like this---if we define a @racket[struct] named
@racket[foo], it defines a number of functions whose names are
variations on the name @racket[foo], such as @racket[foo-field1],
@racket[foo-field2], @racket[foo?], and so on.

So let's pretend we're doing something like that.  We want to
transform the syntax @racket[(hyphen-define a b (args) body)] to
the syntax @racket[(define (a-b args) body)].

A wrong first attempt is:

@i[
(define-syntax (hyphen-define/wrong1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" a b))])
       #'(define (name args ...)
           body0 body ...))]))
]

Huh. We have no idea what this error message means. Well, let's see.
The "template" is the @racket[#'(define (name args ...)  body0 body
...)] portion.  The @racket[let] isn't part of that template. It
sounds like we can't use @racket[a] (or @racket[b]) in the
@racket[let] part.

It turns out that @racket[syntax-case] can have multiple
templates. The final expression is the obvious template, used to
create the output syntax. But you can use @racket[syntax] a.k.a. #' on
a pattern variable, to make a template. Let's try that:

@i[
(define-syntax (hyphen-define/wrong1.1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/wrong1.1 foo bar () #t)
(foo-bar)
]

Our macro definition didn't give an error, but when we tried to use
it, it didn't work. It seems that foo-bar didn't get defined.

This is where the Macro Stepper in DrRacket is invaluable. Even if you
prefer to work in Emacs (like I do), in a situation like this it's
worth firing up DrRacket temporarily to use the Macro Stepper.

It shows us:

@codeblock{
(module anonymous-module racket
  (#%module-begin
   (define-syntax (hyphen-define/wrong1.1 stx)
     (syntax-case stx ()
       [(_ a b (args ...) body0 body ...)
        (let ([name (string->symbol (format "~a-~a" #'a #'b))])
          #'(define (name args ...) body0 body ...))]))
   (define (name) #t)))
}

It shows that we're expanding to @racket[(define (name) #t)], but we
wanted to expand to @racket[(define (foo-bar) #t)].

So the problem is we're getting @racket[name] when we wanted its
value, @racket[foo-bar].

The thing to reach for here is @racket[with-syntax]. This will say
that @racket[name] is in effect another pattern variable, the value of
which we want to use in our main output template.

@i[
(define-syntax (hyphen-define/wrong1.3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax stx
                                        (string->symbol (format "~a-~a"
                                                                #'a
                                                                #'b)))])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/wrong1.3 foo bar () #t)
(foo-bar)
]

Hmm. @racket[foo-bar] still not defined. Back to the Macro Stepper. It says we're expanding to:

@racket[(define (|#<syntax:11:24foo>-#<syntax:11:28 bar>|) #t)].

Ah, that's right. @racket[#'a] and @racket[#'b] are syntax
objects. @racket[format] is printing a representation of them as syntax
objects. What we want is the datum inside the syntax object, the
symbols @racket[foo] and @racket[bar]. So we should use
@racket[syntax->datum] on them:

@i[
(define-syntax (hyphen-define/ok1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax stx
                                        (string->symbol (format "~a-~a" 
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/ok1 foo bar () #t)
(foo-bar)
]

And now it works!

By the way, there is a utility function in @racket[racket/syntax]
called @racket[format-id] that lets us format identifier names more
succinctly. We remember to use @racket[for-syntax] with
@racket[require], since we need it at compile time:

@i[
(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id stx "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/ok2 bar baz () #t)
(bar-baz)
]

Using @racket[format-id] is convenient as it handles the tedium of
converting from syntax to datum and back again.

Recap: If you want to munge pattern variables for use in the template,
@racket[with-syntax] is your friend. Just remember you have to use
@racket[syntax] or @tt{#'} on the pattern variables to turn them into
fun size templates, and often also use @racket[syntax->datum] to get
the interesting value inside. Finally, @racket[format-id] is
convenient for formatting identifier names.

@; ----------------------------------------------------------------------------

@section{Syntax parameters}

"Anaphoric if" or "aif" is a popular macro example. Instead of writing:

@codeblock{
(let ([tmp (big-long-calculation)])
  (if tmp
      (foo tmp)
      #f))
}

It would be great to write:

@codeblock{
(aif (big-long-calculation)
     (foo it)
     #f)
}

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

Wait, what---@racket[it] is undefined?

It turns out that all along we have been protected from making a
certain kind of mistake in our macros. The mistake is to introduce a
variable that accidentally conflicts with one in the code that is
using our macro.

The Racket Reference
@hyperlink["http://docs.racket-lang.org/reference/syntax-model.html#(part._transformer-model)" "Section
1.2.3.5 Transformer Bindings."] has a good explanation of this, and an
example. (You can stop when you reach the part about set!
transformers.) Basically, the input syntax has "marks" to preserve
lexical scope. This makes your macro behave like a normal function. If
a normal function defines a variable named @racket[x], it won't
conflict with a variable named @racket[x] in an outer scope.

This makes it easy to write reliable macros that behave
predictably. Unfortunately, once in awhile, we want to introduce a
magic variable like @racket[it] for @racket[aif] on purpose.

The way to do this is with @racket[define-syntax-parameter] and
@racket[syntax-parameterize]. You're probably familiar with regular
parameters in Racket.

@i[
(define current-foo (make-parameter "some default value"))
(current-foo)
(parameterize ([current-foo "I have a new value, for now"])
  (current-foo))
(current-foo)
]

@margin-note{Historically, there are other ways to do this. If you
know them, you will want to unlearn them. But if you're the target
audience I'm writing for, you don't know them yet. You can skip
learning them now. (Someday if you want to understand someone else's
older macros, you can learn about them then.)}

The syntax variation of them works similarly. The idea is, we'll
define @racket[it] to mean an error by default. Only inside of our
@racket[aif] will it have a meaningful value:

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

If we try to use @racket[it] outside of an @racket[aif] form, and
@racket[it] isn't otherwise defined, we get an error like we want:

@i[
(displayln it)
]

But we can still define @racket[it] as a normal variable:

@i[
(define it 10)
it
]


@; ----------------------------------------------------------------------------

@section{Robust macros: syntax-parse}

TO-DO.
TO-DO.
TO-DO.

@; ----------------------------------------------------------------------------

@section{Other questions}

Hopefully I will answer these in the course of the other sections. But
just in case:

@subsection{What's the point of @racket[with-syntax]?}

Done.

@subsection{What's the point of @racket[begin-for-syntax]?}

TO-DO.

@subsection{What's the point of @racket[racket/splicing]?}

TO-DO.

@; ----------------------------------------------------------------------------

@section{References/Acknowledgments}

Eli Barzliay wrote a blog post,
 @hyperlink["http://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html" "Writing
‘syntax-case’ Macros"], which explains many key details. However it's written
especially for people already familiar with "un-hygienic" "defmacro"
style macros. If you're not familiar with those, it may seem slightly
weird to the extent it's trying to convince you to change an opinion
you don't have. Even so, many key details are presented in Eli's typically
concise, clear fashion.

Eli Barzilay wrote another blog post,
@hyperlink["http://blog.racket-lang.org/2008/02/dirty-looking-hygiene.html" "Dirty
Looking Hygiene"], which explains syntax-parameterize. I relied
heavily on that, mostly just updating it since his post was written
before PLT Scheme was renamed to Racket.

@; ----------------------------------------------------------------------------

@section{Epilogue}

@centered{
"Before I had studied Chan (Zen) for thirty years, I saw mountains as
mountains, and rivers as rivers. When I arrived at a more intimate
knowledge, I came to the point where I saw that mountains are not
mountains, and rivers are not rivers. But now that I have got its very
substance I am at rest. For it's just that I see mountains once again
as mountains, and rivers once again as rivers"

@smaller{--Buddhist saying originally formulated by Qingyuan Weixin,
later translated by D.T. Suzuki in his @italic{Essays in Zen
Buddhism}.}
}


Translated into Racket:

@codeblock{
(dynamic-wind (lambda ()
                (and (eq? 'mountains 'mountains)
                     (eq? 'rivers 'rivers)))
              (lambda ()
                (not (and (eq? 'mountains 'mountains)
                          (eq? 'rivers 'rivers))))
              (lambda ()
                (and (eq? 'mountains 'mountains)
                     (eq? 'rivers 'rivers))))
}

