open Relude.Globals;
module Parser = ReludeParse_Parser;

type t =
  | UUID(string);

let unsafeFromParts = (a, b, c, d, e) =>
  UUID(List.String.joinWith("-", [a, b, c, d, e]));

let show: t => string = (UUID(str)) => str;

let hexDigits: int => Parser.t(string) =
  count => Parser.(times(count, anyHexDigit) <$$> List.String.join);

let hyphen = Parser.str("-");

let parser: Parser.t(t) =
  Parser.(
    unsafeFromParts
    <$> hexDigits(8)
    <* hyphen
    <*> hexDigits(4)
    <* hyphen
    <*> hexDigits(4)
    <* hyphen
    <*> hexDigits(4)
    <* hyphen
    <*> hexDigits(12)
  );

let parse: string => Belt.Result.t(t, Parser.ParseError.t) =
  str => Parser.runParser(str, parser);

let parseOption: string => option(t) = parse >> Result.getOk;

let unsafeFromString: string => t =
  str =>
    Result.fold(e => failwith(Parser.ParseError.show(e)), id, parse(str));
