module P = ReludeParse_Parser;
open P;

type t =
  | IPv4(int, int, int, int);

let unsafeFromInts = (a, b, c, d) => IPv4(a, b, c, d);

let show: t => string =
  (IPv4(first, second, third, fourth)) =>
    string_of_int(first)
    ++ "."
    ++ string_of_int(second)
    ++ "."
    ++ string_of_int(third)
    ++ "."
    ++ string_of_int(fourth);

let parser: P.t(t) = {
  (
    anyPositiveShort <* str("."),
    anyPositiveShort <* str("."),
    anyPositiveShort <* str("."),
    anyPositiveShort,
  )
  |> P.mapTuple4(unsafeFromInts);
};