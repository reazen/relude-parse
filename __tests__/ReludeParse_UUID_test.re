open Jest;
open Expect;
module P = ReludeParse.Parser;

module UUID = ReludeParse.UUID;

let (testParse, testParseFail) =
  ReludeParse_Parser_test.(testParse, testParseFail);

describe("ReludeParse_UUID", () => {
  testAll(
    "show",
    [
      (
        UUID.UUID("00000000-0000-0000-0000-000000000000"),
        "00000000-0000-0000-0000-000000000000",
      ),
    ],
    ((input, exp)) =>
    expect(UUID.show(input)) |> toEqual(exp)
  );

  testAll(
    "valid",
    [
      (
        "00000000-0000-0000-0000-000000000000",
        UUID.UUID("00000000-0000-0000-0000-000000000000"),
      ),
      (
        "01234567-89ab-cdef-aaaa-0000aaaaffff",
        UUID.UUID("01234567-89ab-cdef-aaaa-0000aaaaffff"),
      ),
    ],
    ((str, exp)) =>
    testParse(UUID.parser, str, exp, {pos: 36, str})
  );

  testAll(
    "invalid",
    [
      ("", 0),
      ("x", 0),
      ("00000000-1111-2222-3333-44444444444g", 35),
      ("00000000-1111-2222-3333-44444444444", 35),
      ("0000000-1111-2222-3333-444444444444", 7),
    ],
    ((str, pos)) =>
    testParseFail(UUID.parser, str, pos)
  );
});