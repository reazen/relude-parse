open Relude.Globals;
module Parser = ReludeParse_Parser;

module AreaCode = {
  type t =
    | AreaCode(int, int, int);

  let parser =
    Parser.(
      (
        opt(str("(")),
        filter(v => v != 1, anyDigitAsInt),
        times2(anyDigitAsInt),
        opt(str(")")),
      )
      |> mapTuple4((_, a, (b, c), _) => AreaCode(a, b, c))
    );
};

module Exchange = {
  type t =
    | Exchange(int, int, int);

  // TODO: I think this should disallow 1 as the first digit as well as the
  // pattern x11
  let parser =
    Parser.(
      times3(anyDigitAsInt) |> map(((a, b, c)) => Exchange(a, b, c))
    );
};

module Line = {
  type t =
    | Line(int, int, int, int);

  let parser =
    Parser.(
      times4(anyDigitAsInt) |> map(((a, b, c, d)) => Line(a, b, c, d))
    );
};

type t =
  | NanpPhone(AreaCode.t, Exchange.t, Line.t);

let make = (areaCode, exchange, line) => NanpPhone(areaCode, exchange, line);

let toDigits =
    (NanpPhone(AreaCode(a, b, c), Exchange(d, e, f), Line(g, h, i, j))) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
);

let sep = Parser.anyOfStr(["-", "."]);

let parser: Parser.t(t) =
  Parser.(
    make
    <$> ws
    *> opt(str("+") *> str("1") <|> str("1"))
    *> ws
    *> AreaCode.parser
    <* opt(sep)
    <* ws
    <*> Exchange.parser
    <* opt(sep)
    <* ws
    <*> Line.parser
    <* ws
    <* eof
  );

let parse = str => Parser.runParser(str, parser);

let parseOption = parse >> Result.getOk;

let unsafeFromString = str =>
  parse(str)
  |> Result.fold(e => failwith(ReludeParse_Parser.ParseError.show(e)), id);
