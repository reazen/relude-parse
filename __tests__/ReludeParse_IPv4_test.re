open Jest;
open Expect;

module IPv4 = ReludeParse.IPv4;

let (testParse, testParseFail) =
  ReludeParse_Parser_test.(testParse, testParseFail);

describe("ReludeParse_IPv4", () => {
  test("show", () =>
    expect(IPv4.show(IPv4(127, 0, 0, 1))) |> toEqual("127.0.0.1")
  );

  test("localhost", () =>
    testParse(
      IPv4.parser,
      "127.0.0.1",
      IPv4(127, 0, 0, 1),
      {pos: 9, str: "127.0.0.1"},
    )
  );

  test("invalid", () =>
    testParseFail(
      IPv4.parser,
      "255.257.0.1",
      4
    )
  );
});