#lang scribble/manual
@require[@for-label[racket/base
                    (except-in xsmith module)
                    xsmith/canned-components]
         racket/runtime-path]

@title{XDsmith}
@author[@author+email["Sorawee Porncharoenwase" "soraweep@amazon.com"]]

@(define (bug-reported s)
  @link[(string-append "https://github.com/dafny-lang/dafny/issues/" s)]{This issue is reported as a bug.})

@section{Introduction}

Dafny is a programming language with built-in specification constructs,
and Dafny verifier can check that Dafny programs satisfy their specifications statically.

Since Dafny is a verifier, it cannot run programs by themselves.
Dafny programs must first be compiled to other languages, such as C# and Java, which can then be run.
However, Dafny compilers are not verified.
A mistake in a verifier could make the compiled program output,
even though the source Dafny program is logically correct and verified.

The goal of this project is to uncover bugs in Dafny compilers.

@section{Installation}

@subsection{Docker}

We recommend that you use Docker to run XDsmith.
Therefore, you will need to install @link["https://www.docker.com/"]{Docker},
either via the installer from the website or from your OS's software manager.
After that, run @exec{docker build -t test .} from the the project directory.
Then, you should be able to run @exec{docker run --entrypoint bash -it test} to start the XDsmith environment.

@subsection{Manual installation}

You can read @filepath{Dockerfile} for how to install necessary softwares. Roughly, the following programs are required:

@itemlist[
  @item{Racket}
  @item{Xsmith package for Racket}
  @item{Rosette package for Racket (for fuzzing the verifier)}
  @item{C# and .NET}
  @item{Dafny}
  @item{Java (for Java backend)}
  @item{node and npm (for JS backend and for fuzzing the verifier)}
  @item{Go (for Go backend)}
]

@section{Running XDsmith}

@subsection{Overview}

To run XDsmith in the compiler fuzzing mode, execute @exec{racket differ.rkt}, provided that the Dafny executable is at
@filepath{/workspace/dafny/Binaries/Dafny} (which is the case in the Docker environment).
If you install XDsmith manually, you can set the environment variable @envvar{DAFNYPATH} to the path of the Dafny executable.

Similarly, to run XDsmith in the verifier fuzzing mode, execute @exec{racket differ-verify.rkt}.

@subsection{fuzzer.rkt}

The file @filepath{fuzzer.rkt} is the program generator. You can invoke @exec{racket fuzzer.rkt} to generate an AST in the S-exp form.

Useful command-line options include:

@itemlist[
  @item{@DFlag{dafny-syntax true}: instead of generating an AST, generate a valid Dafny code via pretty printing.}
  @item{@DFlag{print-debug true}: print debugging information.}
  @item{@DFlag{seed <seed>}: using a particular seed for program generation. This is useful for reproducing an existing bug.}
  @item{@DFlag{timeout <timeout>}: set a timeout for program generation. This is useful for debugging the program generator when it loops infinitely.}
  @item{@DFlag{s-exp-on-error true}: print a partial AST when an error occurs during program generation.}
  @item{@DFlag{with-print-constrained true}: constrain printing for the verification fuzzing mode.}
  @item{@DFlag{reduction-script runner-for-reducer} and @DFlag{rediction-directory reduction-dir}: attempt to minimize program size when it encounters an error. Must be specified with @DFlag{dafny-syntax true}. Also see @secref{Overview} for the requirement about Dafny path. E.g., invoke it with @exec{DAFNYPATH=~/projects/dafny/Scripts/dafny racket fuzzer.rkt --dafny-syntax true --reduction-script runner-for-reducer --reduction-directory reduction-dir}. It is currently very slow and not practical to use in the real environment.}
  @item{@DFlag{num <num>}: continuously generate <num> programs. This parameter is weird in a sense that it must be specified as the last flag. It is only useful for debugging to make sure that the program generator really works (and not happens to be successful due to a lucky random seed).}
]

Other command-line options can be seen by invoking @exec{racket fuzzer.rkt --help}.

@subsection{differ.rkt}

The file @filepath{differ.rkt} invokes the program generator and the oracle to determine if there is a mismatch in backends. See @secref{Overview} for the requirement about Dafny path. Running it without any argument will generate random programs until a mismatch is encountered. Running it with an argument, which must be a path to a Dafny program, will determine a mismatch on that particular Dafny file.


@section{Adding a new construct}

There are three places that you need to modify to add a new construct.

@subsection{Adding a new construct to Xsmith}

First is the file @filepath{fuzzer.rkt}.

For example, to add the construct @tt{|<s>|} which computes the length of a sequence @tt{<s>}, we would add:

@racketblock[
 [SeqLength
  Expression ([e : Expression])
  #:prop type-info
  [int-type
   (λ (n t)
     (hash 'e (immutable (array-type (fresh-type-variable)))))]
  #:prop render-node-info
  (λ (n) `(SeqLength ,($xsmith_render-node (ast-child 'e n))))]
]

@racket[SeqLength] is the name of the construct.
It is an @racket[Expression], meaning it can appear in any expression position.
The node consists of @tt{<s>}, indicated by @racket[e], which is an @racket[Expression].
Next, we need to specify the typing constraints.
The resulting type of @tt{|<s>|} is @racket[int-type] (an integer),
and @racket[e] is of type @racket[(immutable (array-type (fresh-type-variable)))]
(an immutable array of an arbitrary type).
Lastly, we specify how to print the node out.
The Xsmith framework expects this to produce the final program string right away.
However, since we will perform tree surgery afterwards,
it is better to produce a tree structure, which is exactly what we are doing here.

@subsection{Pretty printing the new construct}

Second is the file @filepath{pretty.rkt}, which converts the tree structure you produce earlier into a program string.

For example, for @racket[SeqLength], we would add:

@racketblock[
    [`(SeqLength ,s) (h-append (text "|") (pretty s) (text "|"))]
]

This uses the @racketmodname[pprint] module for pretty printing.

@subsection{Threading tree surgery for the new construct}

Third is the file @filepath{differ-verify.rkt}, which performs a tree surgery to insert assertions.
You should not need to do anything special with the newly added constructs, besides making recursive calls so that
subnodes are handled.

For example, for @racket[SeqLength], we would add:

@racketblock[
  [`(SeqLength ,s) `(SeqLength ,(loop s))]
]

which reconstructs itself, with the inner node potentially modified.

@subsection{Adjusting the oracle for the new construct}

Lastly, if the newly added construct can introduce a known different output in different backends,
you need to @defterm{normalize} the output.

As an example, consider adding the set datatype.
Because elements in a set has no ordering, when a set is printed, elements could be in any order.
We need to find a canonical order and reorder elements accordingly.

The mechanism that we are using right now is based on @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{readtable},
though a more advanced parser could also be used.

@racketblock[
(define readtable/base
  (make-readtable
   #f
   #\,
   'terminating-macro
   (λ (ch port src line col pos) ", ")

   #\{
   'terminating-macro
   (λ (ch port src line col pos)
     (define xs (map ~a (read/recursive port ch #f)))
     (define-values (commas elems) (partition (λ (s) (equal? s ", ")) xs))
     (define all-elems (append (make-list (- (add1 (length commas)) (length elems)) "") elems))
     (~a "{" (string-join (sort all-elems string<?) ", ") "}"))))
]

In the above code, we construct a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{readtable} that

@itemlist[
  @item{When encountering @litchar{,}, will produce @racket[", "].}
  @item{When encounting @litchar["{"], will find the matching @litchar["}"] and read everything inside recursively.
        Once we have the result, we count the number of @racket[", "] to see how many elements there are.
        Next, we pad the existing elements with empty string elements (this is meant to deal with an output like @litchar["{, 1}"] which
        should normalizes to @litchar["{1, }"]) and sort all elements alphabetically.}
]

@section{Report}

@subsection{Fuzzing compilers}

To test the compilers, we need a source of ground truth. We leverage multiple compilers as the ground truth: when running compiled programs, their outputs should agree.

To do this, we use the @racketmodname[xsmith] framework,
which generates @emph{interesting} Dafny programs in a sense that it will always be a verified program.
This is done by specifying (typing) constraints to xsmith.

@subsection{Fuzzing the verifier}

As an extension of our work, we fuzz the verifier. This is done by leveraging concrete execution of a compiled program as the ground truth. In particular, at first, a program with no assertion is submitted. Of course, this program should be verified, since there's no assertion whatsoever. Next, we compile the program and run it. We then correlate the output from the program execution with program source location. This allows us to assert that in the location, a value must match the output.

In general, a program may contain method calls. This is problematic for our approach since methods are separately verified, so it is not true that if a method @racket[m] is called with an argument @racket[1], then the argument will actually always be @racket[1]. The fact that a method could be called several times also defeats our approach.

One way to think about this issue is to view each argument in a method as having a symbolic value. Performing an assertion that a symbolic value must match a concrete value obviously would not work in general.
However, this suggests a solution: we can concretize the arguments with the actual arguments that the method accepts by specifying the precondition. Since a method could be called several times,
we duplicate the method so that the duplicated methods can be separately concretized.

Lastly, there is no need to always perform a strict assertion. We use Rosette to synthesize (via angelic execution) a proposition to be asserted. For example, if @racket[x] is 1, we could assert @code{x < 10}.

@subsection{Results}

@subsubsection{Printing issues}

There are various issues with printing in Dafny.

First, in the compile and run mode of Java, an extra newline is produced when the target is Java.
This is caused from:

@verbatim{
      while (!proc.StandardOutput.EndOfStream) {
        outputWriter.WriteLine(proc.StandardOutput.ReadLine());
      }
}

which incorrectly assumes that readline + writeline will pump content accurately.
The problem occurs when the last line ends with EOF without a newline, where readline will read until EOF,
and writeline will write the line back with an extra newline.

Second, there's an issue with the @tt{print} statement.
This is technically not a bug since the specification of Dafny indicates that @tt{print} is platform dependent.
However, it would nice if they produce the same output regardless.

In the Java target, @tt{print}ing a string in a sequence will print it as a sequence of characters,
while this is not the case for C#, Go, and JS.

In the Go and JS target, printing an uninitialized string in a sequence will similarly print it as a (empty) sequence of characters,
while this is not the case for C#.

@bug-reported{1291}

@subsubsection{System crash from too deep AST}

When the program AST is too deep, Dafny crashes. For example:

@verbatim|{
method Main() {
  var _ := [[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]];
}
}|

results in:

@verbatim|{
Process terminated. Assumption failed.
   at Microsoft.Dafny.Resolver.Reaches(Type t, TypeProxy proxy, Int32 direction, HashSet`1 visited) in /Users/sorawee/projects/dafny/Source/Dafny/Resolver.cs:line 6198
   at Microsoft.Dafny.Resolver.<>c__DisplayClass112_0.<Reaches_aux>b__0(Type su) in /Users/sorawee/projects/dafny/Source/Dafny/Resolver.cs:line 6227
   at System.Linq.Enumerable.Any[TSource](IEnumerable`1 source, Func`2 predicate)
}|

and a system crash dialog:

@(define-runtime-path system-crash.png "./system-crash.png")

@image[system-crash.png #:scale 0.5]

@link["https://github.com/dafny-lang/dafny/blob/master/Source/Dafny/Resolver.cs#L6197"]{While the error is intentional}, the error message is subpar, and it should not result in a system crash.

@bug-reported{1331}

@subsubsection{C# compilation error from complex sequence}

I have not investigated the root cause of this bug, but the following program results in a compilation error for C#.

@verbatim|{
method Main () {
  var x1 := 1;
  var x2 := 1;
  var x3 := 1;
  var x4 := 1;
  var x5 := 1;
  var x6 := 1;
  var x7 := [1];
  var x8 := [1];
  var x9 := [1];
  var x10 := [1];
  var x11 := [1];
  var x12 := [[1]];
  var x13 := [[1]];
  var x14 := [[1]];
  var x15 := [[1]];
  var x16 := [[1]];
  var x17 := [[[1]]];
  var _ := () => [[[[x1, x2, x3], []], x12, x13, x14], [[x7, x8, x9], [[x4, x5, x6], x10, x11], x15, x16], x17];
}
}|

Compilation to other languages such as Java works fine.

@bug-reported{1326}

@subsubsection{Underspecified types}

This issue is weird because it is not a bug in a compiler. Instead, it is a bug in the verifier. The following program:

@verbatim|{
method Main () {
  var arr := [[1]];
  var x := |((arr + arr) + (arr + arr))|;
}
}|

results in:

@verbatim|{
Error: the type of this expression is underspecified
}|

Subtle changes in the the program interestingly makes it compile fine.

@verbatim|{
method Main () {
  var arr := [[1]];
  var x := ((arr + arr) + (arr + arr));
  var y := |x|;
}
}|

@bug-reported{1325}

@subsubsection{Java compilation error from non-final reference from lambda}

@verbatim{
method Main ()
{
  var x := 0;
  var z := (
    var y := 0;
    x
  );
}
}

results in:

@verbatim{
  public static void Main()
  {
    java.math.BigInteger _13_x = java.math.BigInteger.ZERO;
    _13_x = java.math.BigInteger.ZERO;
    java.math.BigInteger _14_z = java.math.BigInteger.ZERO;
    _14_z = dafny.Helpers.<java.math.BigInteger, java.math.BigInteger>Let(java.math.BigInteger.ZERO, _pat_let0_0 -> dafny.Helpers.<java.math.BigInteger, java.math.BigInteger>Let(_pat_let0_0, _15_y -> _13_x));
  }
}

which errors with:

@verbatim{
_System/__default.java:15: error: local variables referenced from a lambda expression must be final or effectively final
}

@bug-reported{1297}

@section{Compiling documentations}

Provided that you have Racket installed, you can install the project by using @exec{raco pkg install --name xdsmith} on the directory.
Then, you can run @exec{raco docs xdsmith}.

If you need to update the @exec{docs} directory, run @exec{make docs}.

If you need to update the README file, run @exec{racket generate-readme.rkt}.
You can also provide an option like @exec{racket generate-readme.rkt --relative 'mainline/..'} so that it generates a correct URL
for some code browser.
