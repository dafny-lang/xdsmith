# XDsmith

## Introduction 

XDsmith is a tool aiming to uncover bugs in the Dafny verifier and the Dafny compilers by random testing.

## Installation

### Docker

We recommend that you use Docker to run XDsmith. If you choose to do this, you will need to install Docker, either via the installer from the website or from your OS’s software manager. After that, run `docker build -t test .` from the the project directory. Then, you should be able to run `docker run --entrypoint bash -it test` to start the XDsmith environment.

### Manual Installtion

Alternatively, you can install XDsmith manually. Following are dependencies:

- Racket
- Java 8 
- Go 
- JavaScript (`node` and `npm`)
- Dafny
  - If you wish to patch Dafny locally to suppress known errors, you should build Dafny from source. Instructions for Dafny installation can be found at https://github.com/dafny-lang/dafny/wiki/INSTALL. Patching instructions can be found at [the patching section](#patching)
- C# (which should already be installed as a part of Dafny)

In the `xdsmith` subdirectory (which has a file `info.rkt`), run `raco pkg install -D --auto`. This will install other dependencies from the Racket ecosystem.

Lastly, in the `work-dir` directory, run `npm install bignumber.js` to install the JavaScript library dependency.

#### Patching

This can be done by running `racket xdsmith/apply-patch.rkt <path-to-tmp-dir> <path-to-dafny-dir>`. Note that `<path-to-dafny-dir>` should be the top-level of the Dafny project (which contains subdirectories like `Binaries`, `Source`, etc.). Patches can be adjusted by modifying `apply-patch.rkt`.

## Running XDsmith 

### Overview 

Running should be done from the directory `work-dir` (which has `bignumber.js` installed).
To run XDsmith in the compiler testing mode, execute `racket ../xdsmith/differ.rkt`,
provided that the Dafny executable is at `/workspace/dafny/Binaries/Dafny` (which is the case in the Docker environment).
If you install XDsmith manually, you can set the environment variable `DAFNYPATH` to the path of the Dafny executable.

To run XDsmith in the verifier testing mode, execute either `racket ../xdsmith/differ-verify.rkt` or `racket ../xdsmith/differ-verify.rkt --negative`.

### `xdsmith/fuzzer.rkt`

The file `fuzzer.rkt` is the program generator. You can invoke `racket fuzzer.rkt` to generate an AST in the S-exp form.

Useful command-line options include:

- `--dafny-syntax true`: instead of generating an AST, generate a valid Dafny code via pretty printing.
- `--print-debug true`: print debugging information.
- `--seed <seed>`: using a particular seed for program generation. This is useful for reproducing an existing bug.
- `--timeout <timeout>`: set a timeout for program generation. This is useful for debugging the program generator when it loops infinitely.
- `--s-exp-on-error true`: print a partial AST when an error occurs during program generation.
- `with-print-constrained true`: constrain printing for the verification fuzzing mode.
- `--num <num>`: continuously generate <num> programs. This parameter is weird in a sense that it must be specified as the last flag. It is only useful for debugging to make sure that the program generator really works (and not happens to be successful due to a lucky random seed).

Other command-line options can be seen by invoking `racket fuzzer.rkt --help`.

### `xdsmith/differ.rkt`

The file `differ.rkt` invokes the program generator and the oracle to determine if there is a mismatch in backends. See [the overview section](#overview) for the requirement about Dafny path. Running it without any argument will generate random programs until a mismatch is encountered. Running it with an argument, which must be a path to a Dafny program, will determine a mismatch on that particular Dafny file.

### `xdsmith/differ-verify.rkt`

The file `differ-verify.rkt` performs verification testing. See [the overview section](#overview) for the requirement about Dafny path. Running it without any argument will generate random programs until a mismatch is encountered. Running it with an argument, which must be a seed number, will determine a mismatch on that particular seed number.

In the regular mode, verification testing attempts to generate "good" specifications. Therefore, it expects that the verification will hold. A verification failure would indicate a precision issue.

An optional flag `--negative` can be used to switch to the negative mode, where verification testing attempts to generate "bad" specifications. Therefore, it expects that the verification will not hold. A verification success would indicate a soundness issue.

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

## Testing

To run tests, execute `raco test .` in the `xdsmith-test` directory.
Unit tests are in the subdirectory `unit`, and integration tests are in the subdirectory `integration`.
