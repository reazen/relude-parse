open Relude.Globals;
module Parser = ReludeParse_Parser;

type t =
  | IPv4(int, int, int, int);

let show: t => string =
  (IPv4(a, b, c, d)) =>
    Int.[show(a), show(b), show(c), show(d)] |> List.String.joinWith(".");

let toTuple: t => (int, int, int, int) =
  (IPv4(a, b, c, d)) => (a, b, c, d);

let parser: Parser.t(t) =
  Parser.(
    anyPositiveShort <* str("."),
    anyPositiveShort <* str("."),
    anyPositiveShort <* str("."),
    anyPositiveShort,
  )
  |> Parser.mapTuple4((a, b, c, d) => IPv4(a, b, c, d));

let parse: string => Belt.Result.t(t, Parser.ParseError.t) =
  str => Parser.runParser(str, parser);

let parseOption: string => option(t) = parse >> Result.getOk;

let unsafeFromString: string => t =
  str =>
    Result.fold(e => failwith(Parser.ParseError.show(e)), id, parse(str));

let unsafeFromInts: (int, int, int, int) => t =
  (first, second, third, fourth) =>
    unsafeFromString(show(IPv4(first, second, third, fourth)));
