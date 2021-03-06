# Syntax Implicits

See the [documentation here](https://docs.racket-lang.org/syntax-implicits@syntax-implicits/index.html).

Syntax-implicits are something that is needed for certain kinds of macros in Racket.

They have an interface similar to syntax-parameters.  They have a lexically scoped, hygienic name.  They have a "parameterize"-like form for setting its value.  But whereas syntax-parameters get their meaning from the dynamic environment during expansion, syntax-implicits get their meaning from an anchor position, more like normal identifiers.  But unlike normal identifiers or syntax parameters, they can (hygienically, in a manner safe for composition) get their meaning from static positions other than their definition site.  For example, a macro can use a syntax-implicit in its body and choose where it will take the value from:  the macro definition site (like a normal identifier), the macro use site (similar to a macro that takes an identifier as an argument, except the argument is implicit), a site twice removed (maybe the first macro defines macro2, then the value is taken at macro2's use site).  Notably getting the value from a macro use site is different from syntax-parameters in the case where the macro use site is in the body of a macro definition (say macro-iu).  A syntax-parameter would get its value based on the parameterization at macro-iu's use site, whereas a syntax-implicit could get its value based on macro-iu's definition site.

The main use of syntax-implicits is for some optional configuration that should be implicit -- the original use is Rash's line macros and implicit pipeline start operator.  When you define a macro that expands to some rash code that leaves the line macro and/or pipeline starter implicit, it should get the one from the macro definition site, not the macro use site.

Something similar would be having a macro introduce a non-hygienic identifier -- say my-id, using the hygiene context of the macro use site.  But then that identifier has to be bound, and to the right thing, at the macro use site.  This is problematic.  With a lexically scoped, hygienic name, the implicit doesn't need to be visible in the lexical environment of the macro use site.  It may be there but renamed as a different name, it may not be available but be "parameterized" by the #%module-begin macro, or it could just be unavailable and therefore take its default value when used.  Also the parameterization form can verify that the user is setting it to the right type of thing.  Another way to view it is a more hygienic way to have something that behaves like a #%app.  The #%app-like thing can have any name at its definition site and any name (or be unavailable) at its use site, so there is no specially reserved #%name, and it also gives control over whether the #%app-like thing can be let-syntax-ed over, because instead of let-syntax-ing a bare id, you syntax-implicit-parameterize this lexical object.

I have written one-off implementations of the behavior of syntax implicits for those two things, but there are actually many things in Rash that should be implemented as syntax implicits.  Potentially many other libraries or libraries-to-be should use them.  But before really releasing and depending on syntax implicits I want to nail down a good API that I'm happy with.

# TODO

* I would like a way to use a syntax-implicit in syntax-parameter fashion -- IE get the dynamically deepest parameterization rather than an anchored parameterization.  Probably I should make some `(syntax-implicit-as-syntax-parameter my-implicit)` form.
* Currently this implementation uses gensyms, but that has issues with reproducible builds.  I should try non-gensymed uninterned symbols and see if they work and fix the issue.

