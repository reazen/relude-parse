module Pos = {
  type t = int;
};

module PosString = {
  type t = {
    pos: Pos.t,
    str: string,
  };
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
let unParser: 'a. (t('a), PosString.t) => Belt.Result.t(success('a), error) =
  (Parser(p)) => p;

/**
Runs a parser to produce either a value or error.
*/
let runParser: 'a. (t('a), string) => Belt.Result.t('a, ParseError.t) =
  (Parser(p), input) => {
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

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);
  include Relude_Extensions_Alt.AltInfix(Alt);
};

// Bring all the useful operators into scope
let ((<$>), (<#>), (<*>), ( <* ), ( *> ), (>>=), (<|>)) =
  Infix.((<$>), (<#>), (<*>), ( <* ), ( *> ), (>>=), (<|>));

/**
Lifts an error message into a parser that will fail with the given message.
*/
let fail: 'a. string => t('a) =
  message => Parser(({pos}) => Error({pos, error: ParseError(message)}));

/**
Tries a parser, and if it fails, the position is backtracked to the previous location.
*/
let tries: 'a. t('a) => t('a) =
  (Parser(pa)) =>
    Parser(
      ({pos} as posString) =>
        pa(posString) |> Relude.Result.mapError(({error}) => {pos, error}),
    );

////////////////////////////////////////////////////////////////////////////////
// Combinators
////////////////////////////////////////////////////////////////////////////////

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

// TODO: not stack safe - purescript-string-parsers uses manyRec the MonadRec version
/**
Attempts to run a parser 0 or more times to produce a list of results.
*/
let rec many: 'a. t('a) => t(list('a)) =
  parser => Relude.List.cons <$> parser <*> many(parser);

/**
Attempts to run a parser 1 or more times to produce a non-empty-list of results.
*/
let many1: 'a. t('a) => t(Relude.Nel.t('a)) =
  parser => Relude.Nel.make <$> parser <*> many(parser);

/**
Attempts a parser, and if it fails, provide a custom error message
*/
let withError: 'a. (t('a), string) => t('a) =
  (p, message) => p <|> fail(message);

/**
Infix operator for withError (e.g. many(anyChar) <?> "Expected many chars")
*/
let (<?>) = withError;

/**
Attempts to parse an opening delimiter, a value, and a closing delimiter, producing only the value.
*/
let between: 'a 'o 'c. (t('o), t('c), t('a)) => t('a) =
  (po, pc, pa) => po *> pa <* pc;

// option
/**
Attempts a parser, and if it fails, return the given default value (in a parser)
 */
let orDefault: 'a. ('a, t('a)) => t('a) =
  (default, p) => p <|> pure(default);

// optional
/**
Attempts a parser to consume some input and ignores failures.
*/
let orUnit: 'a. t('a) => t(unit) = p => p >>= (_ => pure()) <|> pure();

// optionMaybe
/**
Attempts a parser, and converts any errors into None, and wraps successful values in Some.
*/
let optional: 'a. t('a) => t(option('a)) =
  p => orDefault(None, Relude.Option.some <$> p);

/**
Parses 0 or more separated values.
*/
let rec sepBy: 'a 's. (t('a), t('s)) => t(list('a)) =
  (pa, ps) => {
    Relude.Nel.toList <$> sepBy1(pa, ps) <|> pure([]);
  }

/**
Parses 1 or more separated values.
*/
and sepBy1: 'a 's. (t('a), t('s)) => t(Relude.Nel.t('a)) =
  (pa, ps) => {
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
let rec sepEndBy: 'a 's. (t('a), t('s)) => t(list('a)) =
  (pa, ps) => Relude.Nel.toList <$> sepEndBy1(pa, ps) <|> pure([])

/**
Parses 1 or more separated values, optionally ending with a separator
*/
and sepEndBy1: 'a 's. (t('a), t('s)) => t(Relude.Nel.t('a)) =
  (pa, ps) => {
    pa
    >>= (
      h => {
        ps
        >>= (
          _ => {
            sepEndBy(pa, ps) <#> (t => Relude.Nel.make(h, t));
          }
        )
        <|> pure(Relude.Nel.pure(h));
      }
    );
  };

/**
Parses 0 or more separated values, ending with a separator
 */
let endBy: 'a 's. (t('a), t('s)) => t(list('a)) =
  (pa, ps) => many(pa <* ps);

/**
Parses 1 or more separated values, ending with a separator
 */
let endBy1: 'a 's. (t('a), t('s)) => t(Relude.Nel.t('a)) =
  (pa, ps) => many1(pa <* ps);

/**
Parses 0 or more values separated by a right-associative operator.
 */
let rec chainr: 'a. (t('a), t(('a, 'a) => 'a), 'a) => t('a) =
  (pa, pf, a) => chainr1(pa, pf) <|> pure(a)

/**
Parses 1 or more values separated by a right-associative operator.
 */
and chainr1: 'a. (t('a), t(('a, 'a) => 'a)) => t('a) =
  (pa, pf) => pa >>= (a => chainr1'(pa, pf, a))

/**
Parses 1 or more values separated by a right-associative operator.
 */
and chainr1': 'a. (t('a), t(('a, 'a) => 'a), 'a) => t('a) =
  (pa, pf, a) =>
    pf >>= (f => chainr1(pa, pf) <#> (a2 => f(a, a2))) <|> pure(a);

/**
Parses 0 or more values separated by a left-associative operator.
 */
let rec chainl: 'a. (t('a), t(('a, 'a) => 'a), 'a) => t('a) =
  (pa, pf, a) => chainl1(pa, pf) <|> pure(a)

/**
Parses 1 or more values separated by a left-associative operator.
 */
and chainl1: 'a. (t('a), t(('a, 'a) => 'a)) => t('a) =
  (pa, pf) => pa >>= (a => chainl1'(pa, pf, a))

/**
Parses 1 or more values separated by a left-associative operator.
 */
and chainl1': 'a. (t('a), t(('a, 'a) => 'a), 'a) => t('a) =
  (pa, pf, a) =>
    pf >>= (f => pa >>= (a2 => chainl1'(pa, pf, f(a, a2)))) <|> pure(a);

/**
Parses a value using any of the given parsers (first successful wins from left-to-right)
*/
let anyOf: 'a. list(t('a)) => t('a) =
  ps => Relude.List.foldLeft((<|>), fail("Nothing to parse"), ps);

/**
Parses 0 or more values up until an end value
 */
let rec manyUntil: 'a 'e. (t('a), t('e)) => t(list('a)) =
  (pa, pe) => pe *> pure([]) <|> (Relude.Nel.toList <$> many1Until(pa, pe))

/**
Parses 1 or more values up until an end value

TODO: not stack safe
 */
and many1Until: 'a 'e. (t('a), t('e)) => t(Relude.Nel.t('a)) =
  (pa, pe) =>
    pa
    >>= (
      a => {
        pe
        <#> (_ => Relude.Nel.pure(a))
        <|> (many1Until(pa, pe) <#> (nel => Relude.Nel.cons(a, nel)));
      }
    );

////////////////////////////////////////////////////////////////////////////////
// Text parsers
////////////////////////////////////////////////////////////////////////////////

let eof: t(unit) =
  Parser(
    ({pos, str} as posString) =>
      if (pos < Relude.String.length(str)) {
        Error({pos, error: ParseError("Expected EOF")});
      } else {
        Ok({result: (), suffix: posString});
      },
  );

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
          error: ParseError("Unexpected EOF when trying to parse any char"),
        })
      },
  );