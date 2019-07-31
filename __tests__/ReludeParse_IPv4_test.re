open Jest;
open Expect;

module P = ReludeParse.Parser;
module IPv4 = ReludeParse.IPv4;

let (testParse, testParseFail) =
  ReludeParse_Parser_test.(testParse, testParseFail);

describe("ReludeParse_IPv4", () => {
  test("show", () =>
    expect(IPv4.show(IPv4(127, 0, 0, 1))) |> toEqual("127.0.0.1")
  );

  testAll(
    "valid",
    [
      ("0.0.0.0", IPv4.unsafeFromInts(0, 0, 0, 0), 7),
      ("0.1.2.3", IPv4.unsafeFromInts(0, 1, 2, 3), 7),
      ("3.2.1.0", IPv4.unsafeFromInts(3, 2, 1, 0), 7),
      ("127.0.0.1", IPv4.unsafeFromInts(127, 0, 0, 1), 9),
      ("255.255.255.255", IPv4.unsafeFromInts(255, 255, 255, 255), 15),
    ],
    ((str, exp, pos)) =>
    testParse(IPv4.parser, str, exp, {pos, str})
  );

  testAll(
    "invalid",
    [
      ("", 0),
      ("x", 0),
      ("255.255.255", 11),
      ("255.255.255.", 12),
      ("1.2.a.3", 4),
      ("255.257.0.1", 4),
    ],
    ((str, pos)) =>
    testParseFail(IPv4.parser, str, pos)
  );
});