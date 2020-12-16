#lang scribble/manual
@title[#:tag "syntax-implicits"]{Syntax Implicits}
@author+email["William Hatch" "william@hatch.uno"]

@(require
  (for-label
   "../main.rkt"
   racket/base
   racket/stxparam
   racket/splicing
   syntax/parse
   )
  (for-syntax
   syntax/parse
   ))

@defmodule[syntax-implicits]

TODO - give examples from Rash describing original motivation and more realistic use, explain that this package is me finally extracting that to a re-usable library.


@section{Guide}

TL;DR: syntax implicits are a generalization of syntax parameters.


Syntax implicits are a way to hygienically have implicit bindings that are configurable for different scopes.
In other words, some macro forms may have optional arguments with an implicit default, where that default can be configured.
The configuration is done with @racket[with-syntax-implicits] (or @racket[splicing-with-syntax-implicits]).
The @racket[with-syntax-implicits] form is different from but can be compared to @racket[syntax-parameterize] and @racket[let-syntax].


To motivate syntax parameters, let's walk through a contrived example.
This @tt{hello} macro desugars to an expression that prints hello to some planet.
If no planet is given it uses a default.


@racketblock[
             (define-syntax (hello stx)
               (syntax-parse stx
                 [(hello)
                  (code:comment "In this case we want to use a default")
                  #'(hello 'mercury)]
                 [(hello planet) #'(printf "hello ~a\n" planet)]))
]

What if we want to make that planet configurable, so we can write something like this:
@racketblock[
             (with-default-planet 'mars
               (code:comment "print “hello mars”")
               (hello)
               (code:comment "print “hello jupiter”")
               (hello 'jupiter)
               )
             (code:comment "print “hello mercury”, the overall default")
             (hello)
             (with-default-planet 'roshar
               (code:comment "print “hello roshar”")
               (hello))
             ]

We have some options to accomplish this.

First, we could reach for syntax parameters.
@racketblock[
             (define-syntax-parameter planet (syntax-parser [(_) #''mars]))
             (define-syntax (hello stx)
               (syntax-parse stx
                 [(hello)
                  #'(hello (planet))]
                 [(hello planet)
                  #'(eprintf "hello ~a\n" planet)]))
             (define-syntax (with-default-planet stx)
               (syntax-parse stx
                 [(_ a-planet body ...+)
                  #'(syntax-parameterize ([planet (syntax-parser [(_) #'a-planet])])
                      body ...)]))
             ]

This lets us configure the default planet!
But there's a catch once we consider using the @tt{hello} macro in a macro template.

What do we want to happen when we write:
@racketblock[
             (with-default-planet 'jupiter
               (define-syntax hello-jupiter
                 (syntax-parser [(_) #'(hello)])))
]

The name indicates that we want @tt{hello-jupiter} uses to say “hello jupiter”, and the @tt{hello} macro is indeed defined in a @tt{with-default-planet} that says jupiter.
Now let's use it:
@racketblock[
             (with-default-planet 'jupiter
               (define-syntax hello-jupiter
                 (syntax-parser [(_) #'(hello)]))
               (code:comment "This seems to work...")
               (hello-jupiter)
               (with-default-planet 'neptune
                 (code:comment "This will print “hello neptune”!")
                 (hello-jupiter))
               )
]

Now, if we want the “configuration” to always be tied to the innermost configuration at the @emph{use site of the hello-jupiter macro}, then syntax parameters are great.
But that's clearly not the intended thing here, where we want to be able to choose a default at the @emph{hello-jupiter definition site} that carries through to the use site.

For our second option, we could decide we don't care about hygiene and use a @racket[let-syntax] and manual hygiene-bending.
@RACKETBLOCK[
             (code:comment "Define the overall default")
             (define-syntax (planet stx)
               (syntax-parse stx
                 [(_) #''mars]))
             (define-syntax (hello stx)
               (syntax-parse stx
                 [(hello)
                  (code:comment "Here we bend hygiene so we refer to the binding of")
                  (code:comment "the symbol `planet` at the `hello` use site, which is")
                  (code:comment "the definition site of a macro that uses `hello`")
                  (code:comment "in its template.")
                  #`(hello #,(datum->syntax stx (list 'planet)))]
                 [(hello planet) #'(printf "hello ~a\n" planet)]))
             (define-syntax (with-default-planet stx)
               (syntax-parse stx
                 [(_ a-planet body ...+)
                  (code:comment "Here we bend syntax so we are introducing a binding with")
                  (code:comment "the scope of the `with-default-planet` use site.")
                  #`(let-syntax ([#,(datum->syntax stx 'planet)
                                  (syntax-parser [(_) #'a-planet])])
                      body ...)]))

             (with-default-planet 'jupiter
               (define-syntax hello-jupiter
                 (syntax-parser [(_) #'(hello)]))
               (hello))
             ]

This version gives us the effect of binding the configuration at the @tt{hello} use site.
However, now it's not hygienic.
If the user of @tt{hello} and @tt{with-default-planet} happens to have some other definition of the symbol @tt{planet}, it will be captured by @tt{with-default-planet}.

@racketblock[
             (let ([planet (λ () 'uranus)])
               (with-default-planet 'jupiter
                 (define-syntax hello-jupiter
                   (syntax-parser [(_) #'(hello)]))
                 (with-default-planet 'saturn
                   (code:comment "This prints “hello jupiter” like we wanted!")
                   (hello-jupiter)
                   (code:comment "But...")
                   (code:comment "This will print “hello saturn”, despite the apparent")
                   (code:comment "lexical binding of `planet` being `'uranus`!")
                   (hello (planet)))))
]

Also, if we for some reason wanted to hide the value of `planet` so it couldn't be used (eg. a private opaque flag of some sort), we couldn't help but leak it because we have to provide the overall default `planet` binding in the first place, and we can't even change its name.
As you probably suspected, hygiene bending is not good for building abstractions!


The solution is syntax implicits.
Syntax implicits provide a hygienic way to have configurable syntax-parameter-like identifiers that get their value anchored at different lexical sites.

@RACKETBLOCK[
             (define-syntax-implicit planet (syntax-parser [(_) #''mercury]))
             (define-syntax (hello stx)
               (syntax-parse stx
                 (code:comment "We put the context of `stx` on the parentheses,")
                 (code:comment "anchoring the use of `planet` to the use site")
                 (code:comment "of the `hello` macro.")
                 (code:comment "If we didn't change the context of the parentheses")
                 (code:comment "we would always be getting the value of `planet`")
                 (code:comment "from this macro template's context.")
                 (code:comment "But note that we are using the identifier `planet`")
                 (code:comment "with the context of this template, so it's always")
                 (code:comment "referring to the syntax-implicit defined above,")
                 (code:comment "not capturing some local binding of `planet`.")
                 [(_) #`(hello #,(datum->syntax stx (list #'planet)))]
                 [(_ planet) #'(printf "hello ~a\n" planet)]))
             (define-syntax (with-default-planet stx)
               (syntax-parse stx
                 [(_ a-planet body ...+)
                  #'(with-syntax-implicits ([planet (syntax-parser [(_) #'a-planet])])
                      body ...)]))

             (let ([planet (λ () 'uranus)])
               (with-default-planet 'jupiter
                 (define-syntax hello-jupiter
                   (syntax-parser [(_) #'(hello)]))
                 (with-default-planet 'middle-earth
                   (code:comment "Prints “hello middle-earth”.")
                   (hello)
                   (code:comment "Correctly prints “hello jupiter”.")
                   (hello-jupiter)
                   (code:comment "No capture of local variables like `planet`.")
                   (hello (planet)))))
             ]

This version works as expected -- @tt{hello-jupiter} prints “hello jupiter” whether it's in a @tt{with-default-planet} or not, users can use the name @tt{planet} without it being captured, and in fact the binding of @tt{planet} doesn't even need to be provided to users, so it can safely hold eg. a private flag value.


The @tt{hello} example is dumb and contrived, but highlights a real problem for macros that want configurable defaults.
However, it's not the end of the story.
What about macros that define macros that want the configuration captured at the use site (IE the definition site of the inner macro)?
You could imagine a chain of macro definitions where each macro defines a new macro up to arbitrary depth N, where you may want the configuration to be based on the definition site of any macro from the first to the Nth (or the use site, which corresponds to definition sites 2 to N+1).
By using @racket[syntax-implicit-value] in the appropriate template, you can choose which definition/use site the macro chain uses to get the binding.


So we can compare normal identifier binding with @racket[define-syntax] or @racket[let-syntax], “dynamic” or “floating” binding with syntax parameters, and anchored binding with syntax-implicits.

@itemlist[
@item{Normal identifiers:  Always get their value from the @emph{definition site}.}
@item{Syntax implicits:  Get their value from a @emph{macro definition or use site chosen by the macro author}.}
@item{Syntax parameters:  Get their value “dynamically” from the @emph{closest parameterization to the final use site}.}
]

If you imagine synatx implicits allowing you to choose site 1 to N+1, syntax parameters are like syntax implicits that are automatically anchored at site N+1.
However, if site N+1 is what you always want, syntax implicits are more unwieldy than syntax parameters because you have to take more effort to explicitly specify that.

Another way to look at syntax implicits is that they are like the implicit identifiers in Racket such as @racket[#%app], @racket[#%datum], and so on which are automatically inserted by the macro expander.
But syntax implicits are hygienic, unlike those identifiers.
When discussing Racket, people often say “there are no special names, not even lambda”.
But there @emph{are} special names -- @racket[#%app] and friends!
If they were implemented by inserting syntax implicits instead of inserting identifiers there could @emph{really} be no special names.

@subsection{Rash}

More realistically, but with less detail, I'll explain the original motivation for syntax implicits.
In the Rash shell language there are several macros with optional arguments where I wanted users to be able to configure the value.
For example, the default pipeline starter, the default line macro, etc.
I wanted configurability for most default arguments, so that different users (and the same user in different contexts) could choose which is the best default, or make shorthand macros with appropriate defaults that still allows overriding.
I originally started using syntax parameters, until I realized that they had the wrong semantics.
I made a one-off version of syntax implicits for one implicit thing, then another, then decided I should make a re-usable library.
Well, finally, I'm making this library to that end.


@section{Reference}

@defform[(define-syntax-implicit name default-value)]{
Defines a syntax implicit.
The @racket[default-value] is used whenever @racket[name] is used outside of any @racket[with-syntax-implicits].
The value of a syntax implicit is often a syntax transformer (eg. via @racket[syntax-parser]), but could be something else.
When a syntax implicit is used as a macro, the context for which value to use is retrieved from the parentheses of the macro call.
}

@defform[(with-syntax-implicits ([implicit value] ...) body ...)]{
Configures syntax implicits to have the given values in the body forms.

Note that the fields of this macro are at different phases, similar to @racket[let-syntax] or @racket[syntax-parameterize].
The name of each implicit and the body are both at the same phase as the @racket[with-syntax-implicits] macro use, while the value for each implicit is at phase +1 relative to the macro use phase.
}

@defform[(splicing-with-syntax-implicits ([implicit value] ...) body ...)]{
Like @racket[with-syntax-implicits], but splices into definition forms in the manner of @racket[splicing-let-syntax] or @racket[splicing-syntax-parameterize].
}

@defproc[(syntax-implicit-value [id identifier?] [#:context context syntax? id]) any/c]{
Provided for-syntax.

The @racket[id] must be bound to a syntax implicit.
Gives the value of the implicit for @racket[context].
This is particularly useful for situations where you want the syntax implicit to be some value other than a macro transformer, or for situations where you can't add the appropriate context to a parenthesis (eg. an identifier macro).
}

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-syntax-implicits"]{on github}.

This library is distributed under the MIT license and the Apache version 2.0 license, at your option.
(IE same license as Racket.)
