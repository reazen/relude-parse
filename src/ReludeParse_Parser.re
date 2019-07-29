let ((<<), (>>)) = Relude.Function.Infix.((<<), (>>));

module Pos = {
  type t = int;
};

module PosString = {
  type t = {
    pos: Pos.t,
    str: string,
  };

  let make = (pos, str) => {pos, str};
};

module ParseError = {
  type t =
    | ParseError(string);
};

type success('a) = {
  result: 'a,
  suffix: PosString.t,
};

type error = {
  pos: Pos.t,
  error: ParseError.t,
};

module ResultE =
  Relude.Result.WithError({
    type t = error;
  });

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
        |> Relude.Result.map(({result, suffix}) =>
             {result: f(result), suffix}
           ),
    );

module Functor: BsAbstract.Interface.FUNCTOR with type t('a) = t('a) = {
  type nonrec t('a) = t('a);
  let map = map;
};
include Relude_Extensions_Functor.FunctorExtensions(Functor);

/**
Inspects a successful parse result using a callback with the value and the suffix
*/
let tap: 'a. (('a, PosString.t) => unit, t('a)) => t('a) =
  (f, Parser(pa)) =>
    Parser(
      posString =>
        pa(posString)
        |> Relude.Result.tap(({result, suffix}) => f(result, suffix)),
    );

/**
Logs a successful parse result and suffix for debuggin purposes.
 */
let tapLog: t('a) => t('a) =
  pa =>
    tap(
      (result, suffix) => {
        Js.log("Result:");
        Js.log(result);
        Js.log("Suffix:");
        Js.log(suffix);
      },
      pa,
    );

/**
Apply a wrapped function to a parser.
*/
let apply: 'a 'b. (t('a => 'b), t('a)) => t('b) =
  (Parser(pf), Parser(pa)) => {
    Parser(
      posString =>
        pf(posString)
        |> Relude.Result.flatMap(({result: f, suffix: s1}) =>
             pa(s1)
             |> Relude.Result.flatMap(({result: a, suffix: s2}) =>
                  Ok({result: f(a), suffix: s2})
                )
           ),
    );
  };

module Apply: BsAbstract.Interface.APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};
include Relude_Extensions_Apply.ApplyExtensions(Apply);

/**
Lift a pure value into a parser.
*/
let pure: 'a. 'a => t('a) =
  a => Parser(posString => Belt.Result.Ok({result: a, suffix: posString}));

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

/**
Attempts to run a parser, and if it fails, attempts the other parser.
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

module Alt: BsAbstract.Interface.ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};
include Relude_Extensions_Alt.AltExtensions(Alt);

/**
Monadic bind for sequencing parsers
*/
let bind: 'a 'b. (t('a), 'a => t('b)) => t('b) =
  (Parser(pa), aToPB) =>
    Parser(
      posString =>
        pa(posString)
        |> Relude.Result.flatMap(({result: a, suffix: s1}) => {
             let Parser(pb) = aToPB(a);
             pb(s1);
           }),
    );

module Monad: BsAbstract.Interface.MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};
include Relude_Extensions_Monad.MonadExtensions(Monad);

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
In case of error, the default behavior is to backtrack if no input was consumed.

`tries` backtracks even if input was consumed
*/
let tries: 'a. t('a) => t('a) =
  (Parser(pa)) =>
    Parser(
      ({pos} as posString) =>
        pa(posString) |> Relude.Result.mapError(({error}) => {pos, error}),
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
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);
  include Relude_Extensions_Alt.AltInfix(Alt);
  let (<?>) = flipWithError;
  let (<&>) = tuple2;
};

// Bring all the useful operators into scope
let ((<$>), (<#>), (<*>), ( <* ), ( *> ), (>>=), (<|>), (<?>)) =
  Infix.((<$>), (<#>), (<*>), ( <* ), ( *> ), (>>=), (<|>), (<?>));

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
Attempts to run a parser 0 or more times to produce a list of results.

TODO: not stack safe - purescript-string-parsers uses manyRec the MonadRec version
*/
let rec many: 'a. t('a) => t(list('a)) =
  pa => {
    pa >>= (a => Relude.List.cons(a) <$> many(pa)) <|> pure([]);
  };

/**
Attempts to run a parser 1 or more times to produce a non-empty-list of results.
*/
let many1: 'a. t('a) => t(Relude.Nel.t('a)) =
  pa => Relude.Nel.make <$> pa <*> many(pa);

/**
Attempts to run a parser the given number of times

TODO: not stack safe
*/
let rec times: 'a. (int, t('a)) => t(list('a)) =
  (count, pa) =>
    if (count <= 0) {
      pure([]);
    } else {
      Relude.List.cons <$> pa <*> times(count - 1, pa);
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
let timesAtLeast: 'a. (int, t('a)) => t(list('a)) =
  (min, pa) => Relude.List.concat <$> times(min, pa) <*> many(pa);

/**
Attempts to run a parser as many times as possible up to `max` times (inclusive).

TODO: not stack safe
*/
let rec timesAtMost: 'a. (int, t('a)) => t(list('a)) =
  (max, pa) =>
    if (max == 0) {
      pure([]);
    } else {
      Relude.List.concat
      <$> (pa <#> (a => [a]) <|> pure([]))
      <*> timesAtMost(max - 1, pa);
    };

/**
Attempts to run a parser at least min (inclusive) and up to max times (inclusive) and returns the results in a list.

E.g. if you want to parse 2 to 5 digits, use: `timesMinMax(2, 5, anyDigit)`
*/
let timesMinMax: 'a. (int, int, t('a)) => t(list('a)) =
  (min, max, pa) => {
    Relude.List.concat <$> times(min, pa) <*> timesAtMost(max - min, pa);
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
let optional: 'a. t('a) => t(option('a)) =
  p => orDefault(None, Relude.Option.some <$> p);

/**
Parses 0 or more separated values.
*/
let rec sepBy: 'a 'sep. (t('sep), t('a)) => t(list('a)) =
  (ps, pa) => {
    Relude.Nel.toList <$> sepBy1(ps, pa) <|> pure([]);
  }

/**
Parses 1 or more separated values.
*/
and sepBy1: 'a 'sep. (t('sep), t('a)) => t(Relude.Nel.t('a)) =
  (ps, pa) => {
    pa
    >>= (
      h => {
        many(ps *> pa) <#> (t => Relude.Nel.make(h, t));
      }
    );
  };

/**
Parses 0 or more separated values, optionally ending with a separator
*/
let rec sepByOptEnd: 'a 'sep. (t('sep), t('a)) => t(list('a)) =
  (ps, pa) => Relude.Nel.toList <$> sepByOptEnd1(ps, pa) <|> pure([])

/**
Parses 1 or more separated values, optionally ending with a separator
*/
and sepByOptEnd1: 'a 'sep. (t('sep), t('a)) => t(Relude.Nel.t('a)) =
  (ps, pa) => {
    pa
    >>= (
      h => {
        ps
        >>= (
          _ => {
            sepByOptEnd(ps, pa) <#> (t => Relude.Nel.make(h, t));
          }
        )
        <|> pure(Relude.Nel.pure(h));
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
let sepByWithEnd1: 'a 'sep. (t('sep), t('a)) => t(Relude.Nel.t('a)) =
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
  ps => Relude.List.foldLeft((<|>), fail("Nothing to parse"), ps);

/**
Parses 0 or more values up until an end value
 */
let rec manyUntil: 'a 'terminator. (t('terminator), t('a)) => t(list('a)) =
  (pt, pa) => pt *> pure([]) <|> (Relude.Nel.toList <$> many1Until(pt, pa))

/**
Parses 1 or more values up until an end value

TODO: not stack safe
 */
and many1Until:
  'a 'terminator.
  (t('terminator), t('a)) => t(Relude.Nel.t('a))
 =
  (pt, pa) =>
    pa
    >>= (
      a => {
        pt
        <#> (_ => Relude.Nel.pure(a))
        <|> (many1Until(pt, pa) <#> (nel => Relude.Nel.cons(a, nel)));
      }
    );

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
      if (pos < Relude.String.length(str)) {
        Error({pos, error: ParseError("Expected EOF")});
      } else {
        Ok({result: (), suffix: posString});
      },
  );

/**
Matches any character
*/
let anyChar: t(string) =
  Parser(
    ({pos, str}) =>
      switch (Relude.String.charAt(pos, str)) {
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
Matches any digit 0-9 and converts it to an int
*/
let anyDigitAsInt: t(int) =
  tries(
    anyChar
    >>= (
      c =>
        c
        |> Relude.String.toInt
        |> Relude.Option.foldLazy(
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
Matches any string
 */
let anyStr: t(string) = many(anyChar) <#> Relude.List.String.join;

/**
Matches any non-empty string
 */
let anyNonEmptyStr: t(string) =
  many1(anyChar) <#> (Relude.Nel.toList >> Relude.List.String.join);

/**
Matches any non-empty string of digits
 */
let anyNonEmptyDigits: t(string) =
  many1(anyDigit) <#> (Relude.Nel.toList >> Relude.List.String.join);

/**
Matches the given string (case-sensitive)
*/
let str: string => t(string) =
  toMatch =>
    Parser(
      ({pos, str}) => {
        let matchLength = Relude.String.length(toMatch);
        let slice = Relude.String.slice(pos, pos + matchLength, str);
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
        let matchLength = Relude.String.length(toMatch);
        let slice = Relude.String.slice(pos, pos + matchLength, str);
        if (Relude.String.toLowerCase(slice)
            == Relude.String.toLowerCase(toMatch)) {
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
  whitelist => anyOf(Relude.List.map(str, whitelist));

/**
Matches any of the given strings (case-insensitive)
 */
let anyOfStrIgnoreCase: list(string) => t(string) =
  whitelist => anyOf(Relude.List.map(strIgnoreCase, whitelist));

/**
Matches any amoutn of whitespace and returns each ws char in a list
*/
let wsList: t(list(string)) = many(anyOfStr([" ", "\t", "\r", "\n"]));

/**
Matches any amount of whitespace, and returns it as a single string
 */
let wsStr: t(string) = Relude.List.String.join <$> wsList;

/**
Matches any amount of whitespace, and ignores it (returns unit)
*/
let ws: t(unit) = void(wsList);

/**
Matches any char except for any of the given chars (case-sensitive)
 */
let anyCharNotIn: list(string) => t(string) =
  blacklist => anyCharBy(c => !(blacklist |> Relude.List.String.contains(c)));

/**
Matches any char except for any of the given chars (case-insensitive)
 */
let anyCharNotInIgnoreCase: list(string) => t(string) =
  blacklist => {
    let blacklistLower =
      blacklist |> Relude.List.map(Relude.String.toLowerCase);
    anyCharBy(c =>
      !(
        blacklistLower
        |> Relude.List.String.contains(Relude.String.toLowerCase(c))
      )
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
let anyPositiveInt: t(int) =
  str("0")
  <#> int_of_string
  <|> (
    anyNonZeroDigit
    >>= (
      nonZeroDigit =>
        many(anyDigit)
        <#> Relude.List.String.join
        <#> (otherDigits => int_of_string(nonZeroDigit ++ otherDigits)) // int_of_string is unsafe, but we are relatively sure we have a valid int string here, so we'll allow it
    )
  );

/**
Matches a "-" negative sign followed by a digit 1-9, followed by any other digits
 */
let anyNegativeInt: t(int) =
  str("-")
  *> anyPositiveInt
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
let anyPositiveShort: t(int) =
  tries(
    anyPositiveInt
    |> filter(i => i <= 255)
    <?> "Expected a positive short (0 - 255)",
  );

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

let regex: Js.Re.t => t(string) =
  regex =>
    Parser(
      ({pos, str}) => {
        // Get the string value of the regex, and make sure it starts with ^ so we only match the next parse position, and not later in the input
        let flags = Js.Re.flags(regex);
        let source = Js.Re.source(regex);
        let caretSource =
          if (Relude.String.startsWith("^", source)) {
            source;
          } else {
            "^" ++ source;
          };
        let regexFinal = Js.Re.fromStringWithFlags(caretSource, ~flags);
        let input = Relude.String.sliceToEnd(pos, str);
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
          Relude.Array.head(captures)
          |> Relude.Option.flatMap(Js.Nullable.toOption)
          |> Relude.Option.foldLazy(
               () => Belt.Result.Error({error: parseError(), pos}),
               match =>
                 Belt.Result.Ok({
                   result: match,
                   suffix: {
                     str,
                     pos: pos + Relude.String.length(match),
                   },
                 }),
             );
        };
      },
    );

/**
Matches a string which matches the given regular expression
*/
let regexStr = (~flags: string="", regexString: string): t(string) =>
  regex(Js.Re.fromStringWithFlags(regexString, ~flags));

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