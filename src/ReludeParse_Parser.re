open Relude.Globals;

/**
 * Represents the parse position (i.e. the current index in the string)
 */
module Pos = {
  type t = int;
};

/**
 * Tracks the current parse position in a specific string
 */
module PosString = {
  type t = {
    pos: Pos.t,
    str: string,
  };

  let make = (pos, str) => {pos, str};
};

/**
 * Represents parse errors
 */
module ParseError = {
  type t =
    | ParseError(string);

  let make: string => t = str => ParseError(str);

  let show: t => string = (ParseError(message)) => message;
};

/**
 * The successful result of a parser - a value of some type, and the remaining part of the string to parse
 */
type success('a) = {
  result: 'a,
  suffix: PosString.t,
};

/**
 * The failed result of a parser - the position of the parse failure, and an error
 */
type error = {
  pos: Pos.t,
  error: ParseError.t,
};

/**
 * Locks in the error type of the Result for convenience
 */
module ResultE =
  Result.WithError({
    type t = error;
  });

/**
 * A parser is a function from a position and string, which products either a successful parse
 * with some value and the rest of the string, or a parse error and position.
 */
type t('a) =
  | Parser(PosString.t => Belt.Result.t(success('a), error));

/**
Unwraps a Parser into the raw parse function
*/
let unParser: 'a. (PosString.t, t('a)) => Belt.Result.t(success('a), error) =
  (posString, Parser(p)) => p(posString);

/**
Runs a parser to produce either a value or error.
*/
let runParser: 'a. (string, t('a)) => Belt.Result.t('a, ParseError.t) =
  (input, Parser(p)) => {
    let result = p({str: input, pos: 0});
    switch (result) {
    | Ok({result}) => Ok(result)
    | Error({error}) => Error(error)
    };
  };

/**
Map a pure function over a parser.
*/
let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
  (f, Parser(pa)) =>
    Parser(
      posString =>
        pa(posString)
        |> Result.map(({result, suffix}) => {result: f(result), suffix}),
    );

/**
 * FUNCTOR instance for Parser
 */
module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude.Extensions.Functor.FunctorExtensions(Functor);

/**
Inspects a successful parse result using a callback with the value and the suffix
*/
let tap: 'a. (('a, PosString.t, PosString.t) => unit, t('a)) => t('a) =
  (f, Parser(pa)) =>
    Parser(
      posString =>
        pa(posString)
        |> Result.tap(({result, suffix}) => f(result, posString, suffix)),
    );

/**
Logs a successful parse result and suffix for debugging purposes.

This function can be chained onto any parser (even inside a more complicated applicative
or monadic composition of parsers) to inject some unobtrusive logging for that parser.

E.g. `many1(anyDigit) |> tapLog`
 */
let tapLog: t('a) => t('a) =
  pa =>
    pa
    |> tap((result, posStringBefore, posStringAfter) =>
         Js.log3(
           "ReludeParse log: input \""
           ++ posStringBefore.str
           ++ "\" at pos "
           ++ string_of_int(posStringBefore.pos)
           ++ " had result: ",
           result,
           " with resulting pos: " ++ string_of_int(posStringAfter.pos),
         )
       );

/**
Apply a wrapped function to a parser.
*/
let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
  (Parser(pf), Parser(pa)) => {
    Parser(
      posString =>
        pf(posString)
        |> Result.flatMap(({result: f, suffix: s1}) =>
             pa(s1)
             |> Result.flatMap(({result: a, suffix: s2}) =>
                  Ok({result: f(a), suffix: s2})
                )
           ),
    );
  };

/**
 * APPLY instance for Parser
 */
module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude.Extensions.Apply.ApplyExtensions(Apply);

/**
Lift a pure value into a parser.
*/
let pure: 'a. 'a => t('a) =
  a => Parser(posString => Belt.Result.Ok({result: a, suffix: posString}));

/**
 * A parser that produces a pure unit () value regardless of the input
 */
let unit: t(unit) = pure();

/**
 * APPLICATIVE instance for Parser
 */
module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude.Extensions.Applicative.ApplicativeExtensions(Applicative);

/**
Attempts to run a parser on the left, and if it fails, attempts the other parser on the right.
*/
let alt: 'a. (t('a), t('a)) => t('a) =
  (Parser(p1), Parser(p2)) => {
    Parser(
      posString =>
        switch (p1(posString)) {
        | Ok(_) as ok => ok
        | Error({pos}) as e =>
          if (posString.pos == pos) {
            p2(posString);
          } else {
            e;
          }
        },
    );
  };

/**
 * Attempts to run a parser on the left, and if it fails, attempts the other lazily-constructed parser on the right.
 */
let altLazy: 'a. (t('a), unit => t('a)) => t('a) =
  (Parser(p1), makeP2) => {
    Parser(
      posString =>
        switch (p1(posString)) {
        | Ok(_) as ok => ok
        | Error({pos}) as e =>
          if (posString.pos == pos) {
            let Parser(p2) = makeP2();
            p2(posString);
          } else {
            e;
          }
        },
    );
  };

/**
 * Handles an error by using a new parser
 *
 * Similar to `alt` or `<|>` with the arguments reversed (and the fallback arg named)
 */
let orElse = (~fallback: t('a), pa: t('a)): t('a) => alt(pa, fallback);

/**
 * Handles an error by using a new lazily-constructed parser
 *
 * Similar to `alt` or `<|>` with the arguments reversed (and the fallback arg named and lazy)
 */
let orElseLazy = (~fallback: unit => t('a), pa: t('a)): t('a) =>
  altLazy(pa, fallback);

/**
 * ALT instance for Parser
 */
module Alt: BsAbstract.Interface.ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};
include Relude.Extensions.Alt.AltExtensions(Alt);

/**
Monadic bind for sequencing parsers
*/
let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
  (Parser(pa), aToPB) =>
    Parser(
      posString =>
        pa(posString)
        |> Result.flatMap(({result: a, suffix: s1}) => {
             let Parser(pb) = aToPB(a);
             pb(s1);
           }),
    );

/**
 * MONAD instance for Parser
 */
module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude.Extensions.Monad.MonadExtensions(Monad);

/*
 module Semigroup: BsAbstract.Interface.SEMIGROUP_ANY with type t('a) = t('a) = {
   type nonrec t('a) = t('a);
   let append = (p1, p2) => map2((r1, r2) => switch((r1, r2)) {
     | (Ok({result: r1, suffix: s1}), Ok(result: r2, suffix: s2})) => Ok({result: r1
   }, p1, p2);
 };
 */

/**
Lifts an error message into a parser that will fail with the given message.
*/
let fail: 'a. string => t('a) =
  message => Parser(({pos}) => Error({pos, error: ParseError(message)}));

/**
 * Lifts a ParseError.t value into a parser that will fail with the given error value.
 */
let throwError: ParseError.t => t('a) =
  (ParseError(message)) => fail(message);

/**
 * MONAD_THROW instance for Parser
 */
module MonadThrow:
  Relude.Interface.MONAD_THROW with
    type t('a) = t('a) and type e = ParseError.t = {
  include Monad;
  type e = ParseError.t;
  let throwError = throwError;
};
include Relude.Extensions.MonadThrow.MonadThrowExtensions(MonadThrow);

/**
 * Handles an error by creating a new parser from the error
 */
let catchError: 'a. (ParseError.t => t('a), t('a)) => t('a) =
  (errorToPA, Parser(pa)) =>
    Parser(
      posString =>
        pa(posString)
        |> Relude.Result.catchError(({error}) => {
             let Parser(pa2) = errorToPA(error);
             pa2(posString);
           }),
    );

/**
 * MONAD_ERROR instance for Parser
 */
module MonadError:
  Relude.Interface.MONAD_ERROR with
    type t('a) = t('a) and type e = ParseError.t = {
  include MonadThrow;
  let catchError = catchError;
};
include Relude.Extensions.MonadError.MonadErrorExtensions(MonadError);

/**
In case of error, the default behavior is to backtrack if no input was consumed.

`tries` backtracks even if input was consumed
*/
let tries: 'a. t('a) => t('a) =
  (Parser(pa)) =>
    Parser(
      ({pos} as posString) =>
        pa(posString) |> Result.mapError(({error}) => {pos, error}),
    );

/**
Attempts a parser, and if it fails, provide a custom error message
*/
let withError: 'a. (string, t('a)) => t('a) =
  (message, p) => alt(p, fail(message));

/**
Flipped version of withError accepting the message as the second argument
 */
let flipWithError: 'a. (t('a), string) => t('a) =
  (pa, message) => withError(message, pa);

/**
Infix operator for flipWithError (e.g. many(anyChar) <?> "Expected many chars")
*/
module Infix = {
  include Relude.Extensions.Functor.FunctorInfix(Functor);
  include Relude.Extensions.Apply.ApplyInfix(Apply);
  let (<^>) = tuple2;
  include Relude.Extensions.Monad.MonadInfix(Monad);
  include Relude.Extensions.Alt.AltInfix(Alt);
  let (<?>) = flipWithError;
};

// Bring all the useful operators into scope for use below and in global/local opens of ReludeParse.Parser
include Infix;

////////////////////////////////////////////////////////////////////////////////
// Combinators
////////////////////////////////////////////////////////////////////////////////

//let lazy_: 'a. (unit => t('a)) => t('a) = getParser => getParser();

/**
Runs a parser to look ahead at a value, but keeps the parse position in its original location.
*/
let lookAhead: 'a. t('a) => t('a) =
  (Parser(p)) => {
    Parser(
      posString =>
        switch (p(posString)) {
        | Ok({result}) => Ok({result, suffix: posString})
        | Error(_) as error => error
        },
    );
  };

/**
Runs a parser to look ahead at a value, but keeps the parse position in its original location.

Alias for `lookAhead`
*/
let peek: 'a. t('a) => t('a) = lookAhead;

/**
 * Runs a parser to see if it succeeds, and if it does, fail the parse.  If the parser fails,
 * unit is returned, and the position is left unchanged.
 */
let lookAheadNot: 'a. t('a) => t(unit) =
  (Parser(p)) => {
    Parser(
      posString =>
        switch (p(posString)) {
        | Ok(_) =>
          Belt.Result.Error({
            pos: posString.pos,
            error: ParseError.make("Expected look ahead to fail"),
          })
        | Error(_) => Belt.Result.Ok({suffix: posString, result: ()})
        },
    );
  };

/**
 * Runs a parser to see if it succeeds, and if it does, fail the parse.  If the parser fails,
 * unit is returned, and the position is left unchanged.
 *
 * Alias for lookAheadNot
 */
let peekNot: 'a. t('a) => t(unit) = lookAheadNot;

/**
Attempts to run a parser 0 or more times to produce a list of results.

TODO: not stack safe - purescript-string-parsers uses manyRec the MonadRec version
*/
let rec many: 'a. t('a) => t(list('a)) =
  pa => {
    pa >>= (a => List.cons(a) <$> many(pa)) <|> pure([]);
  };

/**
Attempts to run a parser 1 or more times to produce a non-empty-list of results.
*/
let many1: 'a. t('a) => t(Nel.t('a)) = pa => Nel.make <$> pa <*> many(pa);

/**
Attempts to run a parser the given number of times

TODO: not stack safe
*/
let rec times: 'a. (int, t('a)) => t(list('a)) =
  (count, pa) =>
    if (count <= 0) {
      pure([]);
    } else {
      List.cons <$> pa <*> times(count - 1, pa);
    };

/**
Attempts to run the parser 2 times
 */
let times2: 'a. t('a) => t(('a, 'a)) = pa => tuple2(pa, pa);

/**
Attempts to run the parser 3 times
 */
let times3: 'a. t('a) => t(('a, 'a, 'a)) = pa => tuple3(pa, pa, pa);

/**
Attempts to run the parser 4 times
 */
let times4: 'a. t('a) => t(('a, 'a, 'a, 'a)) =
  pa => tuple4(pa, pa, pa, pa);

/**
Attempts to run the parser 5 times
 */
let times5: 'a. t('a) => t(('a, 'a, 'a, 'a, 'a)) =
  pa => tuple5(pa, pa, pa, pa, pa);

/**
Attempts to run a parser at least `min` times (inclusive) and as many times as possible after that.
*/
let timesMin: 'a. (int, t('a)) => t(list('a)) =
  (min, pa) => List.concat <$> times(min, pa) <*> many(pa);

/**
Attempts to run a parser as many times as possible up to `max` times (inclusive).

TODO: not stack safe
*/
let rec timesMax: 'a. (int, t('a)) => t(list('a)) =
  (max, pa) =>
    if (max == 0) {
      pure([]);
    } else {
      List.concat
      <$> (pa <#> (a => [a]) <|> pure([]))
      <*> timesMax(max - 1, pa);
    };

/**
Attempts to run a parser at least min (inclusive) and up to max times (inclusive) and returns the results in a list.

E.g. if you want to parse 2 to 5 digits, use: `timesMinMax(2, 5, anyDigit)`
*/
let timesMinMax: 'a. (int, int, t('a)) => t(list('a)) =
  (min, max, pa) => {
    List.concat <$> times(min, pa) <*> timesMax(max - min, pa);
  };

/**
Attempts to parse an opening delimiter, a value, and a closing delimiter, producing only the value.
*/
let between:
  'a 'opening 'closing.
  (t('opening), t('closing), t('a)) => t('a)
 =
  (po, pc, pa) => po *> pa <* pc;

/**
Attempts a parser, and if it fails, return the given default value (in a parser)
*/
let orDefault: 'a. ('a, t('a)) => t('a) =
  (default, p) => p <|> pure(default);

/**
Attempts a parser to consume some input and ignores failures.
*/
let orUnit: 'a. t('a) => t(unit) = p => p >>= (_ => pure()) <|> pure();

/**
Attempts a parser, and converts any errors into None, and wraps successful values in Some.
*/
let opt: 'a. t('a) => t(option('a)) =
  p => orDefault(None, Option.some <$> p);

/**
Parses 0 or more separated values.
*/
let rec sepBy: 'a 'sep. (t('sep), t('a)) => t(list('a)) =
  (ps, pa) => {
    Nel.toList <$> sepBy1(ps, pa) <|> pure([]);
  }

/**
Parses 1 or more separated values.
*/
and sepBy1: 'a 'sep. (t('sep), t('a)) => t(Nel.t('a)) =
  (ps, pa) => {
    pa
    >>= (
      h => {
        many(ps *> pa) <#> (t => Nel.make(h, t));
      }
    );
  };

/**
Parses 0 or more separated values, optionally ending with a separator
*/
let rec sepByOptEnd: 'a 'sep. (t('sep), t('a)) => t(list('a)) =
  (ps, pa) => Nel.toList <$> sepByOptEnd1(ps, pa) <|> pure([])

/**
Parses 1 or more separated values, optionally ending with a separator
*/
and sepByOptEnd1: 'a 'sep. (t('sep), t('a)) => t(Nel.t('a)) =
  (ps, pa) => {
    pa
    >>= (
      h => {
        ps
        >>= (
          _ => {
            sepByOptEnd(ps, pa) <#> (t => Nel.make(h, t));
          }
        )
        <|> pure(Nel.pure(h));
      }
    );
  };

/**
Parses 0 or more separated values, ending with a separator
 */
let sepByWithEnd: 'a 'sep. (t('sep), t('a)) => t(list('a)) =
  (ps, pa) => many(pa <* ps);

/**
Parses 1 or more separated values, ending with a separator
 */
let sepByWithEnd1: 'a 'sep. (t('sep), t('a)) => t(Nel.t('a)) =
  (ps, pa) => many1(pa <* ps);

/**
Parses 0 or more values separated by a right-associative operator.
 */
let rec chainr: 'a. (t(('a, 'a) => 'a), 'a, t('a)) => t('a) =
  (pf, a, pa) => chainr1(pf, pa) <|> pure(a)

/**
Parses 1 or more values separated by a right-associative operator.
 */
and chainr1: 'a. (t(('a, 'a) => 'a), t('a)) => t('a) =
  (pf, pa) => pa >>= (a => chainr1'(pf, a, pa))

/**
Parses 1 or more values separated by a right-associative operator.
 */
and chainr1': 'a. (t(('a, 'a) => 'a), 'a, t('a)) => t('a) =
  (pf, a, pa) =>
    pf >>= (f => chainr1(pf, pa) <#> (a2 => f(a, a2))) <|> pure(a);

/**
Parses 0 or more values separated by a left-associative operator.
 */
let rec chainl: 'a. (t(('a, 'a) => 'a), 'a, t('a)) => t('a) =
  (pf, a, pa) => chainl1(pf, pa) <|> pure(a)

/**
Parses 1 or more values separated by a left-associative operator.
 */
and chainl1: 'a. (t(('a, 'a) => 'a), t('a)) => t('a) =
  (pf, pa) => pa >>= (a => chainl1'(pf, a, pa))

/**
Parses 1 or more values separated by a left-associative operator.
 */
and chainl1': 'a. (t(('a, 'a) => 'a), 'a, t('a)) => t('a) =
  (pf, a, pa) =>
    pf >>= (f => pa >>= (a2 => chainl1'(pf, f(a, a2), pa))) <|> pure(a);

/**
Parses a value using any of the given parsers (first successful wins from left-to-right)
*/
let anyOf: 'a. list(t('a)) => t('a) =
  ps => List.foldLeft((<|>), fail("Nothing to parse"), ps);

/**
 * Converts a parser of an `option('a)` into a parser of `'a`, failing if the value is `None`
 */
let getSome: 'a. t(option('a)) => t('a) =
  popt =>
    popt
    >>= (
      fun
      | Some(a) => pure(a)
      | None => fail("Expected a non-empty option value")
    );

/**
 * Converts a parser of a string into a parser of a non-empty string, failing if the value is ""
 */
let getNonEmptyStr: t(string) => t(string) =
  pstr =>
    pstr
    >>= (
      str =>
        if (str |> Relude.String.isEmpty) {
          fail("Expected a non-empty string");
        } else {
          pure(str);
        }
    );

/**
 * Converts a parser of a tuple2 into a parser of the first value.
 */
let getFst: 'a 'b. t(('a, 'b)) => t('a) = pab => pab <#> (((a, _)) => a);

/**
 * Converts a parser of a tuple2 into a parser of the second value.
 */
let getSnd: 'a 'b. t(('a, 'b)) => t('b) = pab => pab <#> (((_, b)) => b);

/**
Parses 0 or more values up until an end value, producing a list of values and the end value
 */
let rec manyUntilWithEnd:
  'a 'terminator.
  (t('terminator), t('a)) => t((list('a), 'terminator))
 =
  (pt, pa) =>
    pt
    >>= (term => pure(([], term)))
    <|> (
      many1UntilWithEnd(pt, pa)
      <#> (((nel, term)) => (Nel.toList(nel), term))
    )

/**
Parses 1 or more values up until an end value, producing the Nel of values and the end value

TODO: not stack safe
 */
and many1UntilWithEnd:
  'a 'terminator.
  (t('terminator), t('a)) => t((Nel.t('a), 'terminator))
 =
  (pt, pa) =>
    pa
    >>= (
      a => {
        pt
        <#> (term => (Nel.pure(a), term))
        <|> (
          many1UntilWithEnd(pt, pa)
          <#> (((nel, term)) => (Nel.cons(a, nel), term))
        );
      }
    );

/**
Parses 0 or more values up until an end value, producing a list of values and consuming and throwing away the end value
 */
let manyUntil = (pt, pa) => manyUntilWithEnd(pt, pa) |> getFst;

/**
Parses 1 or more values up until an end value, producing a list of values and consuming and throwing away the end value
 */
let many1Until = (pt, pa) => many1UntilWithEnd(pt, pa) |> getFst;

/**
 * Parses 0 or more values up until an end value, and produces the values and end value, without consuming the end value.
 */
let rec manyUntilPeekWithEnd:
  'a 'terminator.
  (t('terminator), t('a)) => t((list('a), 'terminator))
 =
  (pt, pa) =>
    lookAhead(pt)
    >>= (term => pure(([], term)))
    <|> (
      many1UntilPeekWithEnd(pt, pa)
      <#> (
        ((nel, term)) => {
          (Nel.toList(nel), term);
        }
      )
    )

/**
 * Parses 1 or more values up until an end value, and produces the values and end value, without consuming the end value.
 */
and many1UntilPeekWithEnd:
  'a 'terminator.
  (t('terminator), t('a)) => t((Nel.t('a), 'terminator))
 =
  (pt, pa) =>
    pa
    >>= (
      a => {
        lookAhead(pt)
        <#> (term => (Nel.pure(a), term))
        <|> (
          many1UntilPeekWithEnd(pt, pa)
          <#> (((nel, term)) => (Nel.cons(a, nel), term))
        );
      }
    );

/**
 * Parses 0 or more values up until an end value, and produces the values, without consuming the end value.
 */
let manyUntilPeek = (pt, pa) => manyUntilPeekWithEnd(pt, pa) |> getFst;

/**
 * Parses 1 or more values up until an end value, and produces the values, without consuming the end value.
 */
let many1UntilPeek = (pt, pa) => many1UntilPeekWithEnd(pt, pa) |> getFst;

/**
Checks if the given parse result passes a predicate
 */
let filter: 'a. ('a => bool, t('a)) => t('a) =
  (pred, pa) => {
    tries(
      pa
      |> flatMap(a =>
           if (pred(a)) {
             pure(a);
           } else {
             fail("Result did not pass filter predicate");
           }
         ),
    );
  };

////////////////////////////////////////////////////////////////////////////////
// Text parsers
////////////////////////////////////////////////////////////////////////////////

/**
Matches the end of the input, or fails if there is text left to parse.
*/
let eof: t(unit) =
  Parser(
    ({pos, str} as posString) =>
      if (pos < String.length(str)) {
        Error({pos, error: ParseError("Expected EOF")});
      } else {
        Ok({result: (), suffix: posString});
      },
  );

/**
 * Runs a parser and if it succeeds, throw the value away (produce unit), and if it fails,
 * attempt to parse an eof.
 *
 * This might be useful for parsing delimited values, which can optionally end with eof.
 */
let orEOF: 'a. t('a) => t(unit) = pa => pa |> void <|> eof;

/**
Matches any character
*/
let anyChar: t(string) =
  Parser(
    ({pos, str}) =>
      switch (String.charAt(pos, str)) {
      | Some(c) => Ok({
                     result: c,
                     suffix: {
                       str,
                       pos: pos + 1,
                     },
                   })
      | None =>
        Error({
          pos,
          error: ParseError("Expected a character, but found EOF"),
        })
      },
  );

/**
 * Matches any char except the given char
 */
let notChar: string => t(string) =
  input =>
    Parser(
      ({pos, str}) =>
        switch (String.charAt(pos, str)) {
        | Some(c) =>
          if (c == input) {
            Error({
              pos,
              error: ParseError("Expected a char other than " ++ input),
            });
          } else {
            Ok({
              result: c,
              suffix: {
                str,
                pos: pos + 1,
              },
            });
          }
        | None =>
          Error({
            pos,
            error:
              ParseError(
                "Expected a character other than "
                ++ input
                ++ ", but found EOF",
              ),
          })
        },
    );

/**
Matches any digit 0-9 and converts it to an int
*/
let anyDigitAsInt: t(int) =
  tries(
    anyChar
    >>= (
      c =>
        c
        |> String.toInt
        |> Option.foldLazy(
             _ => fail("Expected a digit, but found character '" ++ c ++ "'"),
             pure,
           )
    ),
  );

/**
Matches any digit 0-9 as a string
*/
let anyDigit: t(string) = string_of_int <$> anyDigitAsInt;

/**
Matches any string (warning: this will likely consume as much input as possible)
 */
let anyStr: t(string) = many(anyChar) <#> List.String.join;

/**
Matches any non-empty string
 */
let anyNonEmptyStr: t(string) =
  many1(anyChar) <#> (Nel.toList >> List.String.join);

/**
Matches any non-empty string of digits
 */
let anyNonEmptyDigits: t(string) =
  many1(anyDigit) <#> (Nel.toList >> List.String.join);

/**
Matches the given string (case-sensitive)
*/
let str: string => t(string) =
  toMatch =>
    Parser(
      ({pos, str}) => {
        let matchLength = String.length(toMatch);
        let slice = String.slice(pos, pos + matchLength, str);
        if (slice == toMatch) {
          Ok({
            result: slice,
            suffix: {
              pos: pos + matchLength,
              str,
            },
          });
        } else {
          Error({pos, error: ParseError("Expected string " ++ toMatch)});
        };
      },
    );

/**
Matches the given string (case-insensitive)
*/
let strIgnoreCase: string => t(string) =
  toMatch =>
    Parser(
      ({pos, str}) => {
        let matchLength = String.length(toMatch);
        let slice = String.slice(pos, pos + matchLength, str);
        if (String.toLowerCase(slice) == String.toLowerCase(toMatch)) {
          Ok({
            result: slice,
            suffix: {
              pos: pos + matchLength,
              str,
            },
          });
        } else {
          Error({pos, error: ParseError("Expected string " ++ toMatch)});
        };
      },
    );

/**
Matches a single char that passes the given predicate
*/
let anyCharBy: (string => bool) => t(string) =
  pred => {
    tries(
      anyChar
      >>= (
        c =>
          if (pred(c)) {
            pure(c);
          } else {
            fail("Expected char to pass a predicate");
          }
      ),
    );
  };

/**
Matches any of the given strings (case-sensitive)
 */
let anyOfStr: list(string) => t(string) =
  whitelist => anyOf(List.map(str, whitelist));

/**
Matches any of the given strings (case-insensitive)
 */
let anyOfStrIgnoreCase: list(string) => t(string) =
  whitelist => anyOf(List.map(strIgnoreCase, whitelist));

/**
Matches any amoutn of whitespace and returns each ws char in a list
*/
let wsList: t(list(string)) = many(anyOfStr([" ", "\t", "\r", "\n"]));

/**
Matches any amount of whitespace, and returns it as a single string
 */
let wsStr: t(string) = List.String.join <$> wsList;

/**
Matches any amount of whitespace, and ignores it (returns unit)
*/
let ws: t(unit) = void(wsList);

/**
Matches any char except for any of the given chars (case-sensitive)
 */
let anyCharNotIn: list(string) => t(string) =
  blacklist => anyCharBy(c => !(blacklist |> List.String.contains(c)));

/**
Matches any char except for any of the given chars (case-insensitive)
 */
let anyCharNotInIgnoreCase: list(string) => t(string) =
  blacklist => {
    let blacklistLower = blacklist |> List.map(String.toLowerCase);
    anyCharBy(c =>
      !(blacklistLower |> List.String.contains(String.toLowerCase(c)))
    );
  };

/**
Parses any character in the range of ASCII codes min (inclusive) to max (inclusive)
*/
let anyCharInRange: (int, int) => t(string) =
  (min, max) =>
    tries(
      anyChar
      >>= (
        c => {
          let intValue = Js.Math.floor(Js.String.charCodeAt(0, c));
          if (min <= intValue && intValue <= max) {
            pure(c);
          } else {
            fail(
              "Expected character in ASCII range "
              ++ string_of_int(min)
              ++ " (inclusive) and "
              ++ string_of_int(max)
              ++ " (inclusive)",
            );
          };
        }
      ),
    );

/**
Matches any character that is not a digit
 */
let anyNonDigit: t(string) =
  anyCharNotIn(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]);

/**
Matches any non-zero digit character
 */
let anyNonZeroDigit: t(string) = anyCharInRange(49, 57);

/**
Matches any non-zero digit character as an int
 */
let anyNonZeroDigitAsInt: t(int) = anyNonZeroDigit <#> int_of_string;

/**
Matches a string of digits starting with a 0 or a digit 1-9 followed by any other digits
 */
let anyUnsignedInt: t(int) =
  str("0")
  <#> int_of_string
  <|> (
    anyNonZeroDigit
    >>= (
      nonZeroDigit =>
        many(anyDigit)
        <#> List.String.join
        <#> (otherDigits => int_of_string(nonZeroDigit ++ otherDigits)) // int_of_string is unsafe, but we are relatively sure we have a valid int string here, so we'll allow it
    )
  );

/**
Matches an unsigned int, or an int starting with a + sign
 */
let anyPositiveInt: t(int) = anyUnsignedInt <|> str("+") *> anyUnsignedInt;

/**
Matches a "-" negative sign followed by a digit 1-9, followed by any other digits
 */
let anyNegativeInt: t(int) =
  str("-")
  *> anyUnsignedInt
  <#> (i => i * (-1))
  <?> "Expected any negative int";

/**
Matches any int (negative or positive)
*/
let anyInt =
  anyNegativeInt
  <|> anyPositiveInt
  <?> "Expected any int (negative or positive)";

/**
Matches a positive short (0-255)
 */
let anyUnsignedShort: t(int) =
  tries(
    anyPositiveInt
    |> filter(i => i <= 255)
    <?> "Expected a positive short (0 - 255)",
  );

/**
Matches a short with an optional leading + sign
*/
let anyPositiveShort: t(int) = opt(str("+")) *> anyUnsignedShort;

/**
Matches any lower-case char (ASCII code 97-122)
*/
let anyLowerCaseChar: t(string) =
  anyCharInRange(97, 122) <?> "Expected lower-case character";

/**
Matches any lower-case char (ASCII code 65-90)
*/
let anyUpperCaseChar: t(string) =
  anyCharInRange(65, 90) <?> "Expected upper-case character";

/**
Matches any alpha character (a-z and A-Z)
*/
let anyAlpha: t(string) =
  anyLowerCaseChar
  <|> anyUpperCaseChar
  <?> "Expected any alphabet letter (upper or lowercase)";

/**
Matches any alpha or digit character
 */
let anyAlphaOrDigit: t(string) =
  anyAlpha <|> anyDigit <?> "Expected any alpha character or any digit";

/**
Matches any hex digit 0-9, a-f, or A-F */
let anyHexDigit: t(string) =
  anyDigit
  <|> anyOfStrIgnoreCase(["a", "b", "c", "d", "e", "f"])
  <?> "Expected any hex digit";

/**
Matches any hex digit except "0"
 */
let anyNonZeroHexDigit: t(string) =
  anyHexDigit |> filter(c => c != "0") <?> "Expected any non-zero hex digit";

/**
Matches the given regular expression.

Note: the regex will be prefixed with a ^ if one is not present to ensure a match at the current parse position,
and not later in the input.
*/
let regex: Js.Re.t => t(string) =
  regex =>
    Parser(
      ({pos, str}) => {
        // Get the string value of the regex, and make sure it starts with ^ so we only match the next parse position, and not later in the input
        let flags = Js.Re.flags(regex);
        let source = Js.Re.source(regex);
        let caretSource =
          if (String.startsWith(~search="^", source)) {
            source;
          } else {
            "^" ++ source;
          };
        let regexFinal = Js.Re.fromStringWithFlags(caretSource, ~flags);
        let input = String.sliceToEnd(pos, str);
        let resultOpt = Js.Re.exec_(regexFinal, input);
        let parseError = () =>
          ParseError.ParseError(
            "Expected match for regex "
            ++ caretSource
            ++ " with flags "
            ++ flags,
          );
        switch (resultOpt) {
        | None => Error({error: parseError(), pos})
        | Some(result) =>
          let captures: array(Js.nullable(string)) = Js.Re.captures(result);
          Array.head(captures)
          |> Option.flatMap(Js.Nullable.toOption)
          |> Option.foldLazy(
               () => Belt.Result.Error({error: parseError(), pos}),
               match =>
                 Belt.Result.Ok({
                   result: match,
                   suffix: {
                     str,
                     pos: pos + String.length(match),
                   },
                 }),
             );
        };
      },
    );

/**
Matches a string which matches the given regular expression

Note: the regex will be prefixed with a ^ if one is not present to ensure a match at the current parse position,
and not later in the input.
*/
let regexStr = (~flags: string="", regexString: string): t(string) =>
  regex(Js.Re.fromStringWithFlags(regexString, ~flags));

/**
Matches a decimal value like 123 or 123.456 or 1.23e-3, returned as a string

TODO: this doesn't handle commas correctly with groups of 3
*/
let anyUnsignedDecimalWithLeadingDigits: t(string) =
  regex([%re "/\\d[\\d,]*(?:\\.\\d+)?(?:e-?\\d+)?/i"]);

/**
Matches a decimal value like .456 or .23e-3, returned as a string
*/
let anyUnsignedDecimalWithoutLeadingDigits: t(string) =
  regex([%re "/\\.\\d+(?:e-?\\d+)?/i"]);

/**
Matches a decimal value like 123.456 or 1.23e-3 or .456 or .23e-3, returned as a string
*/
let anyUnsignedDecimal: t(string) =
  anyUnsignedDecimalWithLeadingDigits
  <|> anyUnsignedDecimalWithoutLeadingDigits
  <?> "Expected an unsigned decimal";

/**
Matches a decimal that starts with an optional + sign
 */
let anyPositiveDecimal: t(string) =
  opt(str("+")) *> anyUnsignedDecimal <?> "Expected a positive decimal";

/**
Matches a decimal that starts with an - sign
 */
let anyNegativeDecimal: t(string) =
  (str("-"), anyUnsignedDecimal)
  |> mapTuple2(String.concat)
  <?> "Expected a negative decimal";

/**
Matches a negative or positive decimal
 */
let anyDecimal: t(string) =
  anyNegativeDecimal <|> anyPositiveDecimal <?> "Expected any decimal";

/**
Matches a string "true" (case-insensitive)
 */
let boolTrue: t(bool) = strIgnoreCase("true") <#> (_ => true);

/**
Matches a string "false" (case-insensitive)
 */
let boolFalse: t(bool) = strIgnoreCase("false") <#> (_ => false);

/**
Matches true or false
 */
let anyBool: t(bool) = boolTrue <|> boolFalse <?> "Expected a bool";

/**
Matches a (
*/
let leftParen: t(string) = str("(");

/**
Matches a )
*/
let rightParen: t(string) = str(")");

/**
Parses a value from between ( and ), stripping out extra whitespace inside the ()'s
*/
let betweenParens: 'a. t('a) => t('a) =
  pa => between(leftParen, rightParen, ws *> pa <* ws);

/**
Matches a {
*/
let leftCurly: t(string) = str("{");

/**
Matches a }
*/
let rightCurly: t(string) = str("}");

/**
Parses a value from between { and }, stripping out extra whitespace inside the {}'s
*/
let betweenCurlies: 'a. t('a) => t('a) =
  pa => between(leftCurly, rightCurly, ws *> pa <* ws);

/**
Matches a [
*/
let leftSquare: t(string) = str("[");

/**
Matches a ]
*/
let rightSquare: t(string) = str("]");

/**
Parses a value from between [ and ], stripping out extra whitespace inside the []'s
*/
let betweenSquares: 'a. t('a) => t('a) =
  pa => between(leftSquare, rightSquare, ws *> pa <* ws);

/**
Matches a <
*/
let leftAngle: t(string) = str("<");

/**
Matches a >
*/
let rightAngle: t(string) = str(">");

/**
Parses a value from between < and >, stripping out extra whitespace inside the <>'s
*/
let betweenAngles: 'a. t('a) => t('a) =
  pa => between(leftAngle, rightAngle, ws *> pa <* ws);

/**
Matches a double quote character
*/
let doubleQuote: t(string) = str("\"");

/**
Matches a value enclosed in double quotes
 */
let betweenDoubleQuotes: t('a) => t('a) =
  pa => between(doubleQuote, doubleQuote, pa);

/**
Matches a ' character
*/
let singleQuote: t(string) = str("'");

/**
Matches a value enclosed in single quotes
 */
let betweenSingleQuotes: t('a) => t('a) =
  pa => between(singleQuote, singleQuote, pa);

/**
Matches a ` character
*/
let backTick: t(string) = str("`");

/**
Matches a value enclosed in backticks (`)
 */
let betweenBackTicks: t('a) => t('a) =
  pa => between(backTick, backTick, pa);

/**
 * Matches a \r carriage return line ending
 */
let cr: t(string) = str("\r");

/**
 * Matches a \n line feed line ending
 */
let lf: t(string) = str("\n");

/**
 * Matches a \r\n carriage return + line feed line ending
 */
let crlf: t(string) = cr <^> lf <#> (((a, b)) => a ++ b);

/**
 * Matches any of the common line endings `\r\n`, `\n` or `\r`
 */
let eol: t(string) = tries(crlf) <|> lf <|> cr;

/**
 * Matches the given parser, or EOL
 */
let orEOL: 'a. t('a) => t(unit) = pa => pa |> void <|> (eol |> void);