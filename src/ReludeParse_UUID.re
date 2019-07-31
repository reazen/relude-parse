module P = ReludeParse_Parser;
open P;

type t =
  | UUID(string);

let show: t => string = (UUID(str)) => str;

let hexDigits: int => P.t(string) =
  count => times(count, anyHexDigit) <#> Relude.List.String.join;

let hyphen = str("-");

let parser: P.t(t) =
  (
    (a, b, c, d, e) => UUID(a ++ "-" ++ b ++ "-" ++ c ++ "-" ++ d ++ "-" ++ e)
  )
  <$> hexDigits(8)
  <* hyphen
  <*> hexDigits(4)
  <* hyphen
  <*> hexDigits(4)
  <* hyphen
  <*> hexDigits(4)
  <* hyphen
  <*> hexDigits(12);