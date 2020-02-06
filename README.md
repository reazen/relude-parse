# relude-parse

[![GitHub CI](https://img.shields.io/github/workflow/status/reazen/relude-parse/CI/master)](https://github.com/reazen/relude-parse/actions)
[![npm](https://img.shields.io/npm/v/relude-parse.svg)](https://www.npmjs.com/package/relude-parse)

# Overview

`ReludeParse` is a pure-functional string parsing library for
ReasonML/Bucklescript, built on top of the functional programming types and
type classes provided by [Relude](https://github.com/reazen/relude).

`ReludeParse` was influenced by [purescript-string-parsers](https://github.com/purescript-contrib/purescript-string-parsers), [parsihax](https://github.com/deathbeam/parsihax), and [atto](https://github.com/tpolecat/atto), and several of the parsers in the Haskell ecosystem.

# Getting started

## Install the library

```bash
// Install the library from npm
npm install --save-dev relude relude-parse
// add relude and relude-parse to your bsconfig.json
```

## Import the types, functions, operators, etc.

`ReludeParse` is the root namespace module.

In ReasonML/Bucklescript, you can get access to a module's functions in a variety of ways, here
are a few examples:

```reason
// Method 1: Global open (not recommended)
open ReludeParse.Parser;
let _ = anyDigit <* eof |> runParser("1");

// Method 2: Local open (good but more verbose)
let _ = ReludeParse.Parser.(anyDigit <* eof |> runParser("1"));

// Method 3: Module alias with small global open (tradeoff between 1 and 2 above)
module P = ReludeParse.Parser;
open P.Infix; // Get all the infix operators in scope
let _ = P.anyDigit <* P.eof |> P.runParser("1");
```

I recommend using method 3 for most cases, but if you are running into conflicts, use method 2.
If you just want to live your life, use method 1.

For examples below, I'm going to assume method 3.

As a side note, the relude ecosystem prefers the `|>` pipe operator over `->`, so most
functions are "significant data last" style, and the "significant data" is typically
a value of type `Parser.t('a)`.  This allows you to do things like:

```reason
P.anyAlpha |> P.sepBy(str(","))
```

which is the same as below:

```reason
P.sepBy(str(","), P.anyAlpha)
```

Luckily, either way is perfectly legit, so use whichever style you prefer.

# Parsers

`ReludeParse.Parser.t('a)` is a data type which encapsulates the ability to parse a string
into a value of type `'a`, or fail with a fixed `ParseError.t` type, which provides an error message and parse position.

`ReludeParse` provides a wide variety of low-level, medium-level, and
high-level parsers which can be composed together to parse just about
anything.  Note that there are a few functions (like `many`) that are not stack safe,
so beware when parsing very large strings of repeating characters.  Someday these functions
maybe become stack safe.

The parse errors are currently pretty minimal only giving an error message and parse position.
Someday this may improve.

# Run a parser

A parser will attempt to consume input from a string until it can successfully finish and produce
a value or fail.  The parser will only consume enough of the string to satisfy
its own parsing needs (whatever it needs to produce the final value), and will leave the rest of the string for the next parser (if any).

To run a parser, simply pipe (or pass) it into the `P.runParser` function, along with the input string:

Success example:

```reason
// anyDigit will attempt to consume a single character, and succeeds if that character
// is a digit 0-9

// Pipe operator |>

P.anyDigit |> P.runParser("1") // Belt.Result.Ok("1")

// Or normal function application

P.runParser("1", P.anyDigit)
```

Failure example:

```reason
P.anyDigit |> P.runParser("!") // Belt.Result.Error(ParseError("Expected a digit"))
```

In the event of a failed parse, parsers will (in most cases) back-track so that an alternative
parser (if supplied) can pick-up where the previous failed.

There is also a `P.unParser` function, which gives you access to more of the internals
in the event of success or failure.  It's called `unParser` because it unwraps or "lowers" a
value of type `Parser.t('a)` to the raw parsing function contained within.

# Mapping a function over a parser

A `RelueParse.Parser` is a `functor`, so we can map a pure function over the parser's value.

```reason
// Warning: int_of_string is unsafe (can throw) - this is just an example
P.anyDigit |> P.map(int_of_string) |> P.runParser("1") // Belt.Result.Ok(1)

// <$> map operator version.  <$> is traditionally "function on the left"
int_of_string <$> P.anyDigit |> P.runParser("1");

// <#> flipped map operator version - <#> is function on the right hand side,
// which might be more readable for some.
// When you see `<#>` just think `.map(...)` from JavaScript
P.anyDigit <#> int_of_string |> P.runParser("1");
```

Note: see the monad section for an information on how to handle or produce errors while parsing.

`Parser.t('a)` comes with all the bells and whistles that are granted to all
`functors` in relude. This includes:

* the `map` function
* the `<$>` (`map`) operator
* `flipMap`
* the `<#>` (`flipMap`) operator
* the `<$` operator
* the `$>` operator

These things are all provided "for free" via the [Relude Functor Extensions](https://github.com/reazen/relude/blob/master/src/extensions/Relude_Extensions_Functor.re)

Note: `Parser.t('a)` is not a bi-functor - the error type is fixed to
a known data type for simplicity.

# Combining parsers (via Applicative)

A `ReludeParse.Parser.t('a)` is an `applicative functor`, so we can combine multiple parsers 
together using a variety of techniques.

```reason
// Combine two parsers into a tuple of the results (assuming all succeed)
P.tuple2(P.anyDigit, P.anyDigit) |> P.runParser("12") // Belt.Result.Ok(("1", "2"))

// <^> operator (operator version of tuple2)
P.anyDigit <^> P.anyDigit |> P.runParser("12") // Belt.Result.Ok(("1", "2"))

// Combine more parsers using tuple3 up to tuple5
P.tuple3(P.anyDigit, P.anyDigit, P.anyDigit)
|> P.runParser("123") // Belt.Result.Ok(("1", "2", "3"))

// Combine parse results using a function via map2 through map5
P.map2((a, b) => a + b, P.anyDigitAsInt, P.anyDigitAsInt)
|> P.runParser("12") // Belt.Result.Ok(3)

// Combine results from a tuple of parsers using mapTuple2 through mapTuple5
(P.anyDigitAsInt, P.anyDigitAsInt)
|> P.mapTuple2((a, b) => a + b)
|> P.runParser("12"); // Belt.Result.Ok(3)

// Use the *> operator to run two parsers, and only keep the result from the right side
// This is useful if you don't care what the left side parser produces (e.g. whitespace)
// but you need to consume that input.
// `ws` consumes all the whitespace it encounters and throws it away
P.ws
*> P.anyDigit
|> P.runParser("   3") // Belt.Result.Ok("3")

// Use the <* operator to run two parsers, and only keep the result from the left side
// This is useful if you don't care what the right side parser produces (e.g. whitespace)
// but you want to consume that input.
// It's common to use this operator with `eof` to make sure you've hit the end of the input
// (but you don't care about the value produced by `eof`).
// E.g. use both *> and <* to trim whitespace surrounding a value
P.ws
*> P.anyDigit
<* P.ws
<* P.eof
|> P.runParser("   3  ") // Belt.Result.Ok("3")

// ADVANCED: Incrementally collect parse results using a function and chained <$> map and <*> apply
// operators.
let add3 = (a, b, c) => a + b + c;
add3
<$> P.anyDigitAsInt
<*> P.anyDigitAsInt
<*> P.anyDigitAsInt
|> P.runParser("123"); // Belt.Result.Ok(6)
```

Many of these functions and operators come for free for any Applicative via [Relude Apply Extensions](https://github.com/reazen/relude/blob/master/src/extensions/Relude_Extensions_Apply.re)

# Sequencing parsers (via Monads)

A `Parser.t('a)` is also a `monad`, so you can put pure values directly into a parser using `pure`,
and more importantly, you can sequence parsers using `flatMap`, `bind`, or the `>>=` operator.
`bind`, `flatMap` and `>>=` all basically do the same thing - the take a `Parser.t('a)`, a function from `'a => Parser.t('b)` and give you a `Parser.t('b)`.  What this basically means is that you can "run" a parser to
produce a value (note: I don't mean return a value, but produce a value inside your monadic flow), then use that value to create a new parser with which to continue processing.  Note that you can't do that with the functors and applicative-based parsing, because `map`/`apply`/`<*>` and friends don't give you the opportunity to produce a new parser based on the value - you can only apply functions inside the context of a parser.

For intuition, `flatMap` is an apt name for this function because if you have a `myToOfA: t('a)`, a function `aToTOfB: 'a => t('b)`, and you do `myTOfA |> map(aToTOfB)` you'll get a `t(t('b))`.  A monad has the ability to "flatten" itself when in a nested structure like this, i.e. `flatten: t(t('b)) => t('b)`.  In some languages `flatten` is often named `join` - like you are flattening or joining a nested structure into a single structure.  `flatMap` can be implemented in terms of `map` and `flatten`, and flatten can be implmeented in terms of `flatMap`.

So `flatMap` is mapping a monadic function `'a => t('b)` over a `t('a)`, and then flattening the resulting `t(t('b))` to just `t('b)`.

The true power of monads is the ability to produce a new monadic value (i.e. a new parser) mid-flow, which can be used for producing and handling errors, or forking the parse flow to do something different, based on what you've previously parsed.

Note that monads have "fail-fast" semantics, because if a parser fails to produce a value, it's
not possible for the next parser to accept a value (because there is none).  In other words,
if a parser fails at some point in a chain, the rest of the parsers will not run.

```reason
// Lift a pure value into a parser.
// As you can see the parser just produces the given value regardless of the string.
P.pure(3)
|> runParser("abcdef") // Belt.Result.Ok(3)

// Sequence parse operations using flatMap.
// In this example we read a single digit as an int, then use that value
// to read a series of letters, and expect to consume the whole input.
// This is sequencing because we use the result of one parser to determine
// the next parser to run.
P.anyDigitAsInt
|> P.flatMap(count => P.anyAlpha |> P.times(count) <* P.eof)
|> P.map(chars => Relude.List.String.join(chars))
|> P.runParser("3abc"); // Belt.Result.Ok("abc")

// Sequence using >>= (flatMap/bind) and <#> (map) operators.
// If you are coming from JS -
// Don't be afraid of the operators - when you see >>= read ".flatMap(...)"
// and when you see "<#>" read ".map(...)".  Eventually these will become
// second nature.
P.anyDigitAsInt
>>= (count => P.times(count, P.anyAlpha) <* P.eof)
<#> Relude.List.String.join
|> P.runParser("3abc"); // Belt.Result.Ok("abc")
```

Many of these functions come for free for any Monad via [Relude Monad Extensions](https://github.com/reazen/relude/blob/master/src/extensions/Relude_Extensions_Monad.re)

# Add validation and error handling in a parse chain

You can also use the monadic behavior to optionally fail a parse inside a
`bind`/`flatMap`/`>>=` function.  Note that you can't fail a parse inside a `map`
because map uses a pure function from `'a => 'b`, so there's no way to indicate failure
of the parse - you are only allowed to produce a new value `'b` *inside* the context of an existing
parser.

```reason
P.anyDigitAsInt
>>= (
  count =>
    if (count >= 5) {
      // P.fail is a parser that always fails with the given message
      // just like P.pure always succeeds with the given value.
      // Using >>= and fail is a common way to inject validations and raise errors.
      P.fail("The count cannot be >= 5");
    } else {
      // Now that we have a valid count, carry on
      P.times(count, P.anyAlpha) <* eof;
    }
)
<#> Relude.List.String.join
|> runParser("9abc") // Belt.Result.Error(ParseError("The count cannot be >= 5"))
```

The `filter` function in `ReludeParse` is basically for this purpose.  Filter produces its
own generic error message if the predicate fails, but you can customize it like below:

```reason
P.anyDigitAsInt
|> P.filter(a => a > 5)
<?> "Expected an int greater than 5";
```

# Trying multiple parsers (via Alt)

`ReludeParse.Parser.t('a)` is also an `alt` functor, which means you can try one parser, and
if it fails, try another, as many times as you want.

The `<|>` operator is used for this - think of the `<|>` operator as an `orElse` function.

```reason
P.anyDigit <|> P.anyAlpha |> P.runParser("9") // Belt.Result.Ok("9")
P.anyDigit <|> P.anyAlpha |> P.runParser("a") // Belt.Result.Ok("a")
P.anyDigit <|> P.anyAlpha |> P.runParser("!") // Belt.Result.Error(...)
```

`<|>` can be chained as many times as you want - it attempts each parser left-to-right.

```reason
P.str("a") <|> P.str("b") <|> P.str("c") <|> P.str("d") ...and so on
```

If none of the parsers succeed, it will return the error of the last parser, so a common
technique is to use `<?>` to add a custom error message at the end

```reason
P.str("a") <|> P.str("b") <|> P.str("c") <?> "Expected a, b, or c"
```

Sometimes when using `<|>` with more complex parsers, the first parser might consume some input
before failing, which might mess up the next parser in the `<|>` chain.  Use the `tries`
function to force a parser to back-track all the way to it's original position if it fails.

```reason
// Without tries, this fails, because the first parser consumes the 9, then fails,
// but the next parser wants to consume a digit then a letter.  Using tries makes the
// parser fully back-track on failure if it had consumed any input.
P.tries(P.anyDigit *> P.anyDigit) // parse a digit, throw it away, then parse another digit
<|> (P.anyDigit *> P.anyAlpha) // parse a digit,throw it away, then parse a letter
|> P.runParser("9a") // Belt.Result.Ok("a")
```

# Customizing the error message

Use the `<?>` operator to put a custom error message on a parser.  This is useful
if you are composing a more complex parser from smaller parsers, and want a more meaningful error
message if the parser fails.

```reason
P.many1(P.anyDigit)
<?> "Expected one or more digits"
|> P.runParser("abc") // Belt.Result.Error(ParseError("Expected one or more digits"))
```

# Checking that all input is consumed

To make sure that all the input in the string has been consumed, use the `eof` (end-of-file) parser.
It's common to use `<* eof` to parse the end of input, because `<*` will just keep what's on
the left side of `eof`.

```reason
P.anyDigit <* P.eof // Succeeds for "3" but fails for "3 "
```

# Debugging

Use `tap` to inspect the result of a successful parse, and the parse position.

Use the `tapLog` function to inject some basic logging anywhere in a parser composition.

```reason
anyDigit *> anyDigit *> anyAlpha |> tapLog // etc.
```

# Examples

## IPv4

E.g. `127.0.0.1`

There are many different ways to compose a parser to parse values like this.  Below are just
some examples to show different techniques.

```reason
type t = | IPv4(int, int, int, int);
let make = (a, b, c, d) => IPv4(a, b, c, d);

// Using a tuple and mapTuple4
// parse a short (up to 255) and the dot separators
(
  anyPositiveShort <* str("."),
  anyPositiveShort <* str("."),
  anyPositiveShort <* str("."),
  anyPositiveShort
)
|> mapTuple4(make)
|> runParser("127.0.0.1");

// Using nested flatMaps and a final map at the end.
// These are nested because we have to collect each value as we go, and it has to
// be in scope at the end when we want to construct our final value.
// Note: language support for sequences of monadic binds (e.g. do notation or the
// upcoming let+/let* bindings in OCaml, this becomes a beautiful, flat expression,
// almost like imperative code, but with pure FP data structures and functions!
anyPositiveShort
>>= (
  a =>
    str(".")
    >>= (
      _ =>
        anyPositiveShort
        >>= (
          b =>
            str(".")
            >>= (
              _ =>
                anyPositiveShort
                >>= (
                  c =>
                    str(".")
                    >>= (
                      _ =>
                        anyPositiveShort
                        <#> (
                          d =>
                            make(a, b, c, d)
                        )
                    )
                )
            )
        )
    )
)
|> runParser("127.0.0.1");

// With a monadic flow, another technique is to map each successive result into
// an ever-expanding tuple, and pass the values along that way. This allows you
// to have a more flat structure, at the
// expense of wrapping and unwrapping tuples at each step.

// Using <$> and <*> (with <* and *> helpers)
// Our make function is (int, int, int, int) => IPv4
// The first map <$> creates a `Parser.t((int, int, int) => IPv4)`
// and each successive <*> fills another slot in our function,
// until we finally collect the 4 args.
// The `<* str(".")` reads a ".", but throw it away.
make 
<$> anyPositiveShort // collect a positive short
<* str(".")          // read and ignore .
<*> anyPositiveShort // collect a positive short
<* str(".")          // read and ignore .
<*> anyPositiveShort // collect a positive short
<* str(".")          // read and ignore .
<*> anyPositiveShort // collect a positive short
|> runParser("127.0.0.1")

// Using sepBy
// Note that sepBy produces a list of the values produced by the value parser,
// so we have to manually validate that we got the correct number in our list.
// This is done using `>>=`, so we can fail the parse with the `fail` function,
// which produces a failing parser.  If we get the 4 values we need, we use pure
// to create a parser that produces our desired IPv4 value.
anyPositiveShort
|> sepBy(str("."))
>>= (
  // sepBy gives us a list, so we have to pick the parts out
  shorts =>
    switch (shorts) {
    | [a, b, c, d] =>
      // Use pure here because we need to wrap the result in a parser to satisfy
      // the type signature of the >>= function
      pure(make(a, b, c, d))
    | _ => fail("Expected exactly 4 shorts separated by .")
           // fail produces a `Parser.t(_)` that will always fail
    }
)
|> runParser("127.0.0.1")
```

See the code and tests for more examples.

# API Documentation

For more details, examples, tests, etc., please refer to the code.  Below is a possibly incomplete
list of parser functions that come with `ReludeParse`.

## Basic operations

|Function|Description|Example|
|--------|-----------|-------|
|`runParser`|runs a parser with an input string to produce a `Belt.Result` with either the value or a `ParseError.t`
|`unParser`|runs a parser with an input string to produce a `Belt.Result` with either the value or a `ParseError.t` (with some additional metadata compared to `runParser`)

## Core FP functions and operators

|Function|Description|Example|
|--------|-----------|-------|
|`map`/`<$>`/`<#>`/`<$`/`$>`|functor functions for mapping pure functions over a parser
|`apply`/`<*>`/`<*`/`*>`|applicative functions for combining parsers
|`<^>`|combine two parsers to produce a tuple of results
|`tuple2`-`5`|combine parsers to produce tuples of results
|`map2`-`5`|combine parsers using a function to combine the results
|`mapTuple2`-`5`|combine a tuple of parsers using a function to combine the results
|`pure`|lift a pure value into a parser that always succeeds with the value
|`unit`|lift a pure `()` value into a parser that always succeeds with `()`
|`flatMap`/`bind`/`>>=`|map a function over a parser that can produce a new parser - used for sequencing parsers

**Parsers also get all of the additional utilities provided by all the `Relude` typeclass extensions.**

## Logging/side effects

|Function|Description|Example|
|--------|-----------|-------|
|`tap`|runs a side effect function in a parse change, and forwards the given result along
|`tapLog`|logs some information about the current parse position

## Error customization/handling

|Function|Description|Example|
|--------|-----------|-------|
|`withError`/`flipWithError`/`<?>`|provide a custom error message
|`throwError`|Create a parser that fails with the given `ParseError.t`
|`fail`|Create a parser that fails with the given `string` message
|`catchError`|Handle a failed parse by converting a `ParseError.t` into a new parser

## Repeated values

|Function|Description|Example|
|--------|-----------|-------|
|`many`|Run any parser 0 or more times to produce a `[]` of values (like `*` in regex)
|`many1`|Run any parser 1 or more times to produce a `Relude.NonEmptyList` (aka `Nel`) of values (like `+` in regex) - the result is a `Nel` because we are guaranteed to find at least one value, otherwise the parser will fail
|`times`|run a parser `count` times and produce a `[]` of results
|`times2`-`5`|run a parser exactly twice (up to 5) to produce a tuple of results
|`timesMin`|run a parser at least n times to produce a list of results
|`timesMax`|run a parser at most n times to produce a list of results
|`timesMinMax`|run a parser at least n times and at most m times to produce a list of results
|`manyUntilWithEnd`|parse 0 or more values until a terminator is reached, producing the results and the consumed terminator
|`many1UntilWithEnd`|parse 1 or more values until a terminator is reached, producing the results and the consumed terminator
|`manyUntil`|parse 0 or more values until a terminator is reached, producing the results and discarding the consumed terminator
|`many1Until`|parse 1 or more values until a terminator is reached, producing the results and discarding the consumed terminator
|`manyUntilPeekWithEnd`|parse 0 or more values until a terminator is reached, producing the results and the terminator, without consuming the terminator
|`many1UntilPeekWithEnd`|parse 1 or more values until a terminator is reached, producing the results and the terminator, without consuming the terminator
|`manyUntilPeek`|parse 0 or more values until a terminator is reached, producing the results and discarding the terminator, without consuming the terminator
|`many1UntilPeek`|parse 1 or more values until a terminator is reached, producing the results and discarding the terminator, without consuming the terminator

## Optional/default values

|Function|Description|Example|
|--------|-----------|-------|
|`opt`|attempt a parser, and wrap a success in `Some` and convert a failure to `None`
|`orDefault`|attempt a parser, and if it fails, produce a default value
|`orUnit`|attempt a parser, and if it fails, produce unit

## Delimited values

|Function|Description|Example|
|--------|-----------|-------|
|`between`|parse a value inside opening and closing delimiters|`(abc)`
|`sepBy`|parse zero or more values separated by a delimiter|`a,b,c,d`
|`sepBy1`|parse one or more values separated by a delimiter|`a,b,c,d`
|`sepByOptEnd`|parse zero or more values separated by a delimiter, optionally ending with the delimiter|`a,b,c,d` or `a,b,c,d,`
|`sepByOptEnd1`|parse one or more values separated by a delimiter, optionally ending with the delimiter|`a,b,c,d` or `a,b,c,d,`
|`sepByWithEnd`|parse zero or more delimited values ending with the delimiter|`a,b,c,`
|`sepByWithEnd1`|parse one or more delimited values ending with the delimiter|`a,b,c,`

## Associative operators

|Function|Description|Example|
|--------|-----------|-------|
|`chainr1`|parse values separated by a right-associative operator (useful for parsing math expressions)
|`chainl1`|parse values separated by a left-associative operator (useful for parsing math expressions)

## Trying different parsers

|Function|Description|Example|
|--------|-----------|-------|
|`tries`|Tries a parser, and backtracks all the way to the original start position for the parser on failure.  This is useful if you are using a more complex parser that might consume some input successfully before it fails.  The default behavior is to only backtrack to the start of the failure, whereas this function forces the parse to backtrack to the beginning of the complex parser's input.
|`alt`/`altLazy`/`orElse`/`orElseLazy`/`<pipe>`|try a parser, and if it fails, try the other||
|`anyOf`|Attempts to parse a value using a list of potential parsers, tried from left to right||

## Validation/extraction

|Function|Description|Example|
|--------|-----------|-------|
|`lookAhead`/`peek`|Run a parser to produce a value, without consuming any input
|`lookAheadNot`/`peekNot`|Run a parser which fails if it produces a value, without consuming any input
|`filter`|apply a predicate to the result of a parser to either continue or fail the parse
|`getSome`|converts a parser of `option('a)` into a parser of `'a`, failing if the value is `None`
|`getNonEmptyStr`|converts a parser of `string` into a parser of a non-empty `string`, failing if the value is `""`
|`getFst`|converts a parser of `('a, 'b)` into a parser of `'a`
|`getSnd`|converts a parser of `('a, 'b)` into a parser of `'b`

## Text parsers

|Function|Description|Example|
|--------|-----------|-------|
|`eof`|verify that the end of the input has been reached
|`orEOF`|attempts a parser, and throws away the result, or if it fails, attempts the `eof` parser
|`anyChar`|parses any single character
|`notChar`|parses any single character except the given
|`anyStr`|parses any string (WARNING: this will likely consume all remaining input)
|`anyNonEmptyStr`|parses any non-empty (`""`) string
|`str`|parses the given string
|`strIgnoreCase`|parses the given string case-insensitively
|`anyCharBy`|parses a character, and checks it with a predicate function
|`anyOfStr`|parses any of the given strings
|`anyOfStrIgnoreCase`|parses any of the given strings case insensitively
|`wsList`|parses any amount of whitespace characters and returns them in a list of single chars
|`wsString`|parses any amount of whitespace characters and returns them in a string
|`ws`|parses any amount of whitespace characters throws them away (produces `()`)
|`anyCharNotIn`|parses any single char not in the given list
|`anyCharNotInIgnoreCase`|parses any single char not in the given list, case-insensitive
|`anyCharInRange`|parses any character in the ASCII code range
|`anyNonDigit`|parses any non-digit character
|`anyLowerCaseChar`|parses a single lowercase letter char
|`anyUpperCaseChar`|parses a single uppercase letter char
|`anyAlpha`|parses any upper or lowercase letter char
|`anyAlphaOrDigit`|parses any upper or lowercase letter or digit char
|`regex`|parses a string matching the given regex
|`regexStr`|parses a string matching the given regex string
|`leftParen`|parses a `(`
|`rightParen`|parses a `)`
|`betweenParens`|parses a value inside `(` and `)`, consuming extra whitespace padding
|`leftCurly`|parses a `{`
|`rightCurly`|parses a `}`
|`betweenCurlies`|parses a value inside `{` and `}`, consuming extra whitespace padding
|`leftSquare`|parses a `[`
|`rightSquare`|parses a `]`
|`betweenSquares`|parses a value inside `[` and `]`, consuming extra whitespace padding
|`leftAngle`|parses a `<`
|`rightAngle`|parses a `>`
|`betweenAngles`|parses a value inside `<` and `>`, consuming extra whitespace padding
|`singleQuote`|parses a `'`
|`betweenSingleQuotes`|parses a value inside `'` and `'`
|`doubleQuote`|parses a `'`
|`betweenDoubleQuotes`|parses a value inside `"` and `"`
|`backTick`|parses a backtick
|`betweenBackTicks`|parses a value inside backticks
|`cr`|parses a `\r`
|`lf`|parses a `\n`
|`crlf`|parses a `\r\n`
|`eol`|parses a `\r\n`, `\r`, or a `\n`
|`orEOL`|runs a parser and discards the result, or if it fails, try the `eol` parser

## Numeric parsers

|Function|Description|Example|
|--------|-----------|-------|
|`anyDigit`|parses any single character, makes sure it's a digit 0-9 and produces it as a single character string
|`anyDigitAsInt`|parses any single character, makes sure it's a digit 0-9 and converts it to an int
|`anyNonEmptyDigits`|parses 1 or more consecutive digits as a string
|`anyNonZeroDigit`|parses any non-zero digit character
|`anyNonZeroDigitAsInt`|parses any non-zero digit character as an int
|`anyUnsignedInt`|parses an integer with no `+` or `-` prefix
|`anyPositiveInt`|parses a positive integer (optional + prefix)
|`anyNegativeInt`|parses a negative integer (- prefix)
|`anyInt`|parses any positive or negative int
|`anyUnsignedShort`|parses a short int
|`anyDecimal`|parses a decimal value with optional exponential notation
|`anyHexDigit`|parses any hex digit 0-9 or a-f or A-F
|`anyNonZeroHexDigit`|parses any hex digit 1-9 or a-f or A-F
|`anyBool`|parses a bool true or false value

See the code and tests for a complete list of functions and examples.

# Extra Utilities

`ReludeParse` comes with a few higher-level parsers for convenience and for educational
purposes.

* `ReludeParse.IPv4` - IPv4 addresses
* `ReludeParse.IPv6` - IPv6 addresses
* `ReludeParse.UUID` - 8-4-4-4-12 UUIDs
* `ReludeParse.NanpPhone` - North American Numbering Plan (NANP) phone numbers

Other parsers and utilities for things like `URL`s and `DateTime`s can be found in other libraries like
[relude-url](https://github.com/reazen/relude-url) and [relude-eon](https://github.com/reazen/relude-eon).

