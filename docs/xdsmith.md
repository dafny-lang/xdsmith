# XDsmith

## Introduction 

Dafny is a programming language with built-in specification constructs.
The Dafny verifier can check that Dafny programs satisfy their specifications statically,
and the Dafny compilers can compile a Dafny program into a target (mainstream) language.

This tool, XDsmith, aims to uncover bugs in the Dafny verifier and the Dafny compilers
by random testing.

## Installation

### Docker

We recommend that you use Docker to run XDsmith. Therefore, you will need to install Docker, either via the installer from the website or from your OS’s software manager. After that, run `docker build -t test .` from the the project directory. Then, you should be able to run `docker run --entrypoint bash -it test` to start the XDsmith environment.

### Manual Installtion

Alternatively, you can install XDsmith manually. Following are dependencies:

- Racket
- Java 8 
- Go 
- JavaScript (`node` and `npm`)
- Dafny
  - If you wish to _patch_ Dafny locally to suppress an error, you should build Dafny from source. Instruction can be found at https://github.com/dafny-lang/dafny/wiki/INSTALL
- C# (which should already be installed as a part of Dafny)

Following dependencies are additionally required:

- Rosette: run `raco pkg install -D --auto rosette` after Racket is installed.
- Xsmith: run
  ```
  git clone https://gitlab.flux.utah.edu/xsmith/xsmith.git
  cd xsmith/xsmith
  raco pkg install -D --auto
  ```
- In the `src` directory, run `npm install bignumber.js` for JavaScript library deps.

## Running XDsmith 

### Overview 

To run XDsmith in the compiler testing mode, execute `racket differ.rkt`, provided that the Dafny executable is at
`/workspace/dafny/Binaries/Dafny` (which is the case in the Docker environment).
If you install XDsmith manually, you can set the environment variable `DAFNYPATH` to the path of the Dafny executable.

Similarly, to run XDsmith in the verifier testing mode, execute `racket differ-verify.rkt`.

### `fuzzer.rkt`


The file `fuzzer.rkt` is the program generator. You can invoke `racket fuzzer.rkt` to generate an AST in the S-exp form.

Useful command-line options include:

- `--dafny-syntax true`: instead of generating an AST, generate a valid Dafny code via pretty printing.
- `--print-debug true`: print debugging information.
- `--seed <seed>`: using a particular seed for program generation. This is useful for reproducing an existing bug.
- `--timeout <timeout>`: set a timeout for program generation. This is useful for debugging the program generator when it loops infinitely.
- `--s-exp-on-error true`: print a partial AST when an error occurs during program generation.
- `with-print-constrained true`: constrain printing for the verification fuzzing mode.
- (experimental) `--reduction-script runner-for-reducer` and `--rediction-directory reduction-dir`: attempt to minimize program size when it encounters an error. Must be specified with `--dafny-syntax true`. Also see the [overview section](#overview) for the requirement about Dafny path. E.g., invoke it with `DAFNYPATH=~/projects/dafny/Scripts/dafny racket fuzzer.rkt --dafny-syntax true --reduction-script runner-for-reducer --reduction-directory reduction-dir`. It is currently very slow and not practical to use in the real environment.
- `--num <num>`: continuously generate <num> programs. This parameter is weird in a sense that it must be specified as the last flag. It is only useful for debugging to make sure that the program generator really works (and not happens to be successful due to a lucky random seed).

Other command-line options can be seen by invoking `racket fuzzer.rkt --help`.

### `differ.rkt`

The file `differ.rkt` invokes the program generator and the oracle to determine if there is a mismatch in backends. See [the overview section](#overview) for the requirement about Dafny path. Running it without any argument will generate random programs until a mismatch is encountered. Running it with an argument, which must be a path to a Dafny program, will determine a mismatch on that particular Dafny file.

### Adding a new construct

There are three places that you need to modify to add a new construct.

#### Adding a new construct to Xsmith

First is the file `fuzzer.rkt`.

For example, to add the construct `|<s>|` which computes the length of a sequence `<s>`, we would add:

```
 [SeqLength
  Expression ([e : Expression])
  #:prop type-info
  [int-type
   (λ (n t)
     (hash 'e (immutable (array-type (fresh-type-variable)))))]
  #:prop render-node-info
  (λ (n) `(SeqLength ,($xsmith_render-node (ast-child 'e n))))]
```

`SeqLength` is the name of the construct.
It is an `Expression`, meaning it can appear in any expression position.
The node consists of `<s>`, indicated by `e`, which is an `Expression`.
Next, we need to specify the typing constraints.
The resulting type of `|<s>|` is `int-type` (an integer),
and `e` is of type `(immutable (array-type (fresh-type-variable)))`
(an immutable array of an arbitrary type).
Lastly, we specify how to print the node out.
The Xsmith framework conventionally expects this step to produce the final program string right away.
However, since we will perform a tree surgery afterwards,
it is better to produce a tree structure, which is exactly what we are doing here.

#### Pretty printing the new construct

Second is the file `pretty.rkt`, which converts the tree structure you produce earlier into a program string.

For example, for `SeqLength`, we would add:

```
    [`(SeqLength ,s) (h-append (text "|") (pretty s) (text "|"))]
```

This uses the `pprint` module in Racket for pretty printing.

#### Threading tree surgery for the new construct

Third is the file `differ-verify.rkt`, which performs a tree surgery to insert assertions.
You should not need to do anything special with the newly added constructs, besides making recursive calls so that
subnodes are handled.

For example, for `SeqLength`, we would add:

```
  [`(SeqLength ,s) `(SeqLength ,(loop s))]
```

which reconstructs itself, with the inner node potentially modified.

#### Adjusting the oracle for the new construct

Lastly, if the newly added construct can introduce a known different output in different backends,
you need to _normalize_ the output.

As an example, consider adding the set datatype.
Because elements in a set has no ordering, when a set is printed, elements could be in any order.
We need to find a canonical order and reorder elements accordingly.

The mechanism that we are using right now is based on Racket's [readtable](https://docs.racket-lang.org/reference/readtables.html),
though a more advanced parser could also be used.

```
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
```

In the above code, we construct a readtable that

- When encountering `,`, will produce `", "`.
- When encounting `"{"`, will find the matching `"}"` and read everything inside recursively.
  Once we have the result, we count the number of `", "` to see how many elements there are.
  Next, we pad the existing elements with empty string elements (this is meant to deal with an output like `"{, 1}"` which
  should normalizes to `"{1, }"`) and sort all elements alphabetically.
