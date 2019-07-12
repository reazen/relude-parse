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

let unParser: 'a. (t('a), PosString.t) => Belt.Result.t(success('a), error) =
  (Parser(p)) => p;

let runParser: 'a. (t('a), string) => Belt.Result.t('a, ParseError.t) =
  (Parser(p), input) => {
    let result = p({str: input, pos: 0});
    switch (result) {
    | Ok({result}) => Ok(result)
    | Error({error}) => Error(error)
    };
  };

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

let pure: 'a. 'a => t('a) =
  a => Parser(posString => Belt.Result.Ok({result: a, suffix: posString}));

module Applicative: BsAbstract.Interface.APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};
include Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

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

let fail: 'a. string => t('a) =
  message => Parser(({pos}) => Error({pos, error: ParseError(message)}));

let tries: 'a. t('a) => t('a) =
  (Parser(pa)) =>
    Parser(
      ({pos} as posString) =>
        pa(posString) |> Relude.Result.mapError(({error}) => {pos, error}),
    );