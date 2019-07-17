open Jest;
open Expect;

module P = ReludeParse.Parser;
open P.Infix;

let testParse: 'a. (P.t('a), string, 'a, P.PosString.t) => assertion =
  (parser, input, expectedResult, expectedSuffix) => {
    switch (P.unParser({pos: 0, str: input}, parser)) {
    | Ok({result, suffix}) =>
      expect((result, suffix)) |> toEqual((expectedResult, expectedSuffix))
    | Error({pos, error: ParseError(message)}) =>
      fail(
        "Parser should have succeeded for input: "
        ++ input
        ++ ": "
        ++ message
        ++ " at pos: "
        ++ string_of_int(pos),
      )
    };
  };

let testParseFail: 'a. (P.t('a), string, P.Pos.t) => assertion =
  (parser, input, expectedPos) => {
    switch (P.unParser({pos: 0, str: input}, parser)) {
    | Ok(_) => fail("Parser should have failed for input: " ++ input)
    | Error({pos, error: ParseError(_message)}) =>
      expect(pos) |> toEqual(expectedPos)
    };
  };

let testParseFailWithMessage:
  'a.
  (P.t('a), string, P.Pos.t, string) => assertion
 =
  (parser, input, expectedPos, expectedMessage) => {
    switch (P.unParser({pos: 0, str: input}, parser)) {
    | Ok(_) => fail("Parser should have failed for input: " ++ input)
    | Error({pos, error: ParseError(message)}) =>
      expect((pos, message)) |> toEqual((expectedPos, expectedMessage))
    };
  };

describe("ReludeParse_Parser", () => {
  test("runParser success", () =>
    expect(P.runParser("9", P.anyDigit)) |> toEqual(Belt.Result.Ok("9"))
  );

  test("runParser failure", () => {
    let expectedError: P.ParseError.t =
      ParseError("Expected a digit, but found character 'a'");
    expect(P.runParser("a", P.anyDigit))
    |> toEqual(Belt.Result.Error(expectedError));
  });

  test("pure", () => {
    let actual: Belt.Result.t(P.success(int), P.error) =
      P.pure(42) |> P.unParser({pos: 0, str: "whatever"});
    let expected: Belt.Result.t(P.success(int), P.error) =
      Ok({
        result: 42,
        suffix: {
          pos: 0,
          str: "whatever",
        },
      });
    expect(actual) |> toEqual(expected);
  });

  test("fail", () => {
    let actual = P.fail("Fail!") |> P.unParser({pos: 0, str: "whatever"});
    let expected: Belt.Result.t(P.success(_), P.error) =
      Belt.Result.Error({error: ParseError("Fail!"), pos: 0});
    expect(actual) |> toEqual(expected);
  });

  test("tries success", () =>
    testParse(
      P.tries(P.times(3, P.anyDigit)),
      "012",
      ["0", "1", "2"],
      {pos: 3, str: "012"},
    )
  );

  test("without tries, error position is position of error", () =>
    testParseFail(P.times(3, P.anyDigit), "01a", 2)
  );

  test("with tries, error position is the starting position", () =>
    testParseFail(P.tries(P.times(3, P.anyDigit)), "01a", 0)
  );

  test("withError", () =>
    testParseFailWithMessage(
      P.withError("Fail!", P.anyDigit),
      "a",
      0,
      "Fail!",
    )
  );

  test("flipWithError", () =>
    testParseFailWithMessage(
      P.flipWithError(P.anyDigit, "Fail!"),
      "a",
      0,
      "Fail!",
    )
  );

  test("<?> (withError operator)", () =>
    testParseFailWithMessage(P.anyDigit <?> "Fail!", "a", 0, "Fail!")
  );

  test("map/<$>/<#>", () =>
    testParse(P.anyDigit <#> int_of_string, "9", 9, {pos: 1, str: "9"})
  );

  test("apply/<*>", () =>
    testParse(
      P.pure(int_of_string) <*> P.anyDigit,
      "9",
      9,
      {pos: 1, str: "9"},
    )
  );

  test("<&> (tuple2 operator)", () =>
    testParse(
      P.anyDigit <&> P.anyDigit,
      "01",
      ("0", "1"),
      {pos: 2, str: "01"},
    )
  );

  test("mapN (applicative extensions)", () =>
    testParse(
      P.map3((a, b, c) => (a, b, c), P.anyDigit, P.anyDigit, P.anyDigit),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  );

  test("tupleN (applicative extensions)", () =>
    testParse(
      P.tuple3(P.anyDigit, P.anyDigit, P.anyDigit),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  );

  test("mapTupleN (applicative extensions)", () =>
    testParse(
      (P.anyDigit, P.anyDigit, P.anyDigit)
      |> P.mapTuple3((a, b, c) => (a, b, c)),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  );

  test("*>", () =>
    testParse(
      P.anyDigit *> P.anyDigit <* P.eof,
      "12",
      "2",
      {pos: 2, str: "12"},
    )
  );

  test("<*", () =>
    testParse(
      P.anyDigit <* P.anyDigit <* P.eof,
      "12",
      "1",
      {pos: 2, str: "12"},
    )
  );

  test("bind/flatMap/>>=", () => {
    let twoDigits = P.anyDigit >>= (d1 => P.anyDigit <#> (d2 => (d1, d2)));
    testParse(twoDigits, "01", ("0", "1"), {pos: 2, str: "01"});
  });

  test(">>= nested", () => {
    let p =
      P.anyDigit
      >>= (
        a =>
          P.anyDigit
          >>= (b => P.anyDigit >>= (c => P.anyDigit <#> (d => (a, b, c, d))))
      );
    testParse(p, "0123", ("0", "1", "2", "3"), {pos: 4, str: "0123"});
  });

  test(">>= conditional", () => {
    let p = P.anyDigitAsInt >>= (count => P.times(count, P.anyChar));
    testParse(p, "3abcdef", ["a", "b", "c"], {pos: 4, str: "3abcdef"});
  });

  test("alt/<|> first success", () =>
    testParse(P.anyDigit <|> P.anyAlpha, "2", "2", {str: "2", pos: 1})
  );

  test("alt/<|> second success", () =>
    testParse(P.anyDigit <|> P.anyAlpha, "a", "a", {str: "a", pos: 1})
  );

  test("alt/<|> failure", () =>
    testParseFail(P.anyDigit <|> P.anyAlpha, "!", 0)
  );

  test("alt/<|> failure 2", () =>
    testParseFail(
      P.str("abc") <|> P.str("def") <|> P.str("ghi"),
      "!!!!",
      0,
    )
  );

  test("lookAhead preserves the position", () =>
    testParse(
      P.lookAhead(P.times(3, P.anyDigit)),
      "012",
      ["0", "1", "2"],
      {str: "012", pos: 0},
    )
  );

  test("lookAhead preserves the position mid-parse", () =>
    testParse(
      P.times(3, P.anyDigit)
      >>= (
        first =>
          P.lookAhead(
            P.times(3, P.anyDigit) <#> (second => (first, second)),
          )
      ),
      "012345",
      (["0", "1", "2"], ["3", "4", "5"]),
      {str: "012345", pos: 3},
    )
  );

  test("lookAhead failure", () =>
    testParseFail(P.lookAhead(P.anyDigit), "a", 0)
  );

  test("many success", () =>
    testParse(
      P.many(P.anyDigit),
      "0123456789",
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
      {pos: 10, str: "0123456789"},
    )
  );

  test("many non-spaces", () =>
    testParse(
      P.many(P.str("a")),
      "aaa",
      ["a", "a", "a"],
      {pos: 3, str: "aaa"},
    )
  );

  test("many spaces", () =>
    testParse(
      P.many(P.str(" ")),
      "   ",
      [" ", " ", " "],
      {pos: 3, str: "   "},
    )
  );

  test("many with eof success", () =>
    testParse(
      P.many(P.anyDigit) <* P.eof,
      "0123456789",
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
      {pos: 10, str: "0123456789"},
    )
  );

  test("many with eof failure", () =>
    testParseFail(P.many(P.anyDigit) <* P.eof, "0123456789abc", 10)
  );

  test("many with extra success", () =>
    testParse(
      P.many(P.anyDigit),
      "0123456789abc",
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
      {pos: 10, str: "0123456789abc"},
    )
  );

  test("many with empty success", () =>
    testParse(P.many(P.anyDigit), "", [], {pos: 0, str: ""})
  );

  test("many with non-match success", () =>
    testParse(P.many(P.anyDigit), "abc", [], {pos: 0, str: "abc"})
  );

  test("many1 success", () =>
    testParse(
      P.many1(P.anyDigit),
      "0123456789",
      Relude.Nel.make("0", ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
      {pos: 10, str: "0123456789"},
    )
  );

  test("many1 with eof success", () =>
    testParse(
      P.many1(P.anyDigit) <* P.eof,
      "0123456789",
      Relude.Nel.make("0", ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
      {pos: 10, str: "0123456789"},
    )
  );

  test("many1 with eof failure", () =>
    testParseFail(P.many1(P.anyDigit) <* P.eof, "0123456789abc", 10)
  );

  test("many1 with extra success", () =>
    testParse(
      P.many1(P.anyDigit),
      "0123456789abc",
      Relude.Nel.make("0", ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
      {pos: 10, str: "0123456789abc"},
    )
  );

  test("many1 with empty failure", () =>
    testParseFail(P.many1(P.anyDigit), "", 0)
  );

  test("many1 with non-match failure", () =>
    testParseFail(P.many1(P.anyDigit), "abc", 0)
  );

  test("times 0 empty string", () =>
    testParse(P.times(0, P.anyDigit), "", [], {pos: 0, str: ""})
  );

  test("times 0 valid string", () =>
    testParse(P.times(0, P.anyDigit), "123", [], {pos: 0, str: "123"})
  );

  test("times 0 invalid string", () =>
    testParse(P.times(0, P.anyDigit), "abc", [], {pos: 0, str: "abc"})
  );

  test("times 1 empty string", () =>
    testParseFail(P.times(1, P.anyDigit), "", 0)
  );

  test("times 1 valid string", () =>
    testParse(P.times(1, P.anyDigit), "123", ["1"], {pos: 1, str: "123"})
  );

  test("times 1 invalid string", () =>
    testParseFail(P.times(1, P.anyDigit), "abc", 0)
  );

  test("times 2 empty string", () =>
    testParseFail(P.times(2, P.anyDigit), "", 0)
  );

  test("times 2 valid string", () =>
    testParse(
      P.times(2, P.anyDigit),
      "123",
      ["1", "2"],
      {pos: 2, str: "123"},
    )
  );

  test("times 2 invalid string", () =>
    testParseFail(P.times(2, P.anyDigit), "abc", 0)
  );

  test("times 2 invalid string 2", () =>
    testParseFail(P.times(2, P.anyDigit), "0ab", 1)
  );

  test("times2", () =>
    testParse(P.times2(P.anyDigit), "12", ("1", "2"), {pos: 2, str: "12"})
  );

  test("times3", () =>
    testParse(
      P.times3(P.anyDigit),
      "123",
      ("1", "2", "3"),
      {pos: 3, str: "123"},
    )
  );

  test("times4", () =>
    testParse(
      P.times4(P.anyDigit),
      "1234",
      ("1", "2", "3", "4"),
      {pos: 4, str: "1234"},
    )
  );

  test("times5", () =>
    testParse(
      P.times5(P.anyDigit),
      "12345",
      ("1", "2", "3", "4", "5"),
      {pos: 5, str: "12345"},
    )
  );

  test("timesMinMax exact min", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "01abc",
      ["0", "1"],
      {pos: 2, str: "01abc"},
    )
  );

  test("timesMinMax exact max", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "0123abc",
      ["0", "1", "2", "3"],
      {pos: 4, str: "0123abc"},
    )
  );

  test("timesMinMax over max", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "0123456789",
      ["0", "1", "2", "3", "4"],
      {pos: 5, str: "0123456789"},
    )
  );

  test("timesMinMax partial", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "012abc",
      ["0", "1", "2"],
      {pos: 3, str: "012abc"},
    )
  );

  test("timesMinMax failure", () =>
    testParseFail(P.anyDigit |> P.timesMinMax(2, 5), "0abc", 1)
  );

  test("timesAtLeast exact min", () =>
    testParse(
      P.anyDigit |> P.timesAtLeast(2),
      "01abc",
      ["0", "1"],
      {pos: 2, str: "01abc"},
    )
  );

  test("timesAtLeast full", () =>
    testParse(
      P.anyDigit |> P.timesAtLeast(2),
      "01234abc",
      ["0", "1", "2", "3", "4"],
      {pos: 5, str: "01234abc"},
    )
  );

  test("timesAtLeast fail", () =>
    testParseFail(P.anyDigit |> P.timesAtLeast(2), "0abc", 1)
  );

  test("timesAtMost empty", () =>
    testParse(P.anyDigit |> P.timesAtMost(3), "", [], {pos: 0, str: ""})
  );

  test("timesAtMost partial", () =>
    testParse(
      P.anyDigit |> P.timesAtMost(3),
      "01abc",
      ["0", "1"],
      {pos: 2, str: "01abc"},
    )
  );

  test("timesAtMost exact max", () =>
    testParse(
      P.anyDigit |> P.timesAtMost(3),
      "012abc",
      ["0", "1", "2"],
      {pos: 3, str: "012abc"},
    )
  );

  test("timesAtMost over max", () =>
    testParse(
      P.anyDigit |> P.timesAtMost(3),
      "01234abc",
      ["0", "1", "2"],
      {pos: 3, str: "01234abc"},
    )
  );

  test("between simple", () =>
    testParse(
      P.between(
        P.leftParen,
        P.rightParen,
        P.many(P.anyDigit) <#> Relude.List.String.join,
      ),
      "(123)",
      "123",
      {pos: 5, str: "(123)"},
    )
  );

  test("between extra spaces 1", () =>
    testParse(
      P.between(
        P.leftParen,
        P.rightParen,
        P.wsStr *> P.anyNonEmptyDigits <* P.wsStr,
      ),
      "( 123  )",
      "123",
      {pos: 8, str: "( 123  )"},
    )
  );

  test("between extra spaces 2", () =>
    testParse(
      P.between(
        P.ws *> P.leftParen <* P.ws,
        P.ws *> P.rightParen <* P.ws,
        P.anyNonEmptyDigits,
      ),
      "( 123  )",
      "123",
      {pos: 8, str: "( 123  )"},
    )
  );

  test("orDefault success", () =>
    testParse(P.anyDigit |> P.orDefault("!"), "9", "9", {pos: 1, str: "9"})
  );

  test("orDefault default", () =>
    testParse(
      P.anyDigit |> P.orDefault("!"),
      "x",
      "!",
      {pos: 0, str: "x"} // TODO: not sure if pos should be advanced here?
    )
  );

  // orUnit

  // optional

  // sepBy1

  // sepBy

  // sepEndBy

  // sepEndBy1

  // endBy

  // endBy1

  // chainr1

  // chainr1'

  // chainr

  // chainl1'

  // chainl1

  // chainl

  // anyOf

  // many1Until

  // manyUntil

  test("eof empty string", () =>
    testParse(P.eof, "", (), {pos: 0, str: ""})
  );

  test("eof non-empty success", () =>
    testParse(P.anyChar <* P.eof, "a", "a", {pos: 1, str: "a"})
  );

  test("eof non-empty fail", () =>
    testParseFail(P.anyChar <* P.eof, "ab", 1)
  );

  let goodChars = ["a", "0", "!", ",", " "];
  let badChars = [""];

  testAll("anyChar success", goodChars, input =>
    testParse(P.anyChar, input, input, {pos: 1, str: input})
  );

  testAll("anyChar fail", badChars, input =>
    testParseFail(P.anyChar, input, 0)
  );

  let goodDigits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
  let badDigits = ["a", "!", ",", " "];

  testAll("anyDigitAsInt success", goodDigits, input =>
    testParse(
      P.anyDigitAsInt,
      input,
      int_of_string(input),
      {pos: 1, str: input},
    )
  );

  testAll("anyDigitAsInt fail", badDigits, input =>
    testParseFail(P.anyDigitAsInt, input, 0)
  );

  testAll("anyDigit success", goodDigits, input =>
    testParse(P.anyDigit, input, input, {pos: 1, str: input})
  );

  testAll("anyDigit fail", badDigits, input =>
    testParseFail(P.anyDigit, input, 0)
  );

  test("anyStr", () =>
    pass
  );

  test("anyNonEmptyStr", () =>
    pass
  );

  test("str empty", () =>
    testParse(P.str(""), "", "", {pos: 0, str: ""})
  );

  test("str simple", () =>
    testParse(P.str("x"), "x", "x", {pos: 1, str: "x"})
  );

  test("str multiple", () =>
    testParse(P.str("abc"), "abcdef", "abc", {pos: 3, str: "abcdef"})
  );

  test("str failure", () =>
    testParseFail(P.str("xxx"), "abcdef", 0)
  );

  test("strIgnoreCase", () =>
    pass
  );

  test("anyCharBy", () =>
    pass
  );

  test("anyOfStr first", () =>
    testParse(P.anyOfStr(["a", "b", "c"]), "a", "a", {pos: 1, str: "a"})
  );

  test("anyOfStr second", () =>
    testParse(P.anyOfStr(["a", "b", "c"]), "b", "b", {pos: 1, str: "b"})
  );

  test("anyOfStr third", () =>
    testParse(P.anyOfStr(["a", "b", "c"]), "c", "c", {pos: 1, str: "c"})
  );

  test("anyOfStr failure", () =>
    testParseFail(P.anyOfStr(["a", "b", "c"]), "d", 0)
  );

  test("anyOfStr many", () =>
    testParse(
      P.many(P.anyOfStr(["a", "b", "c"])),
      "abcd",
      ["a", "b", "c"],
      {pos: 3, str: "abcd"},
    )
  );

  test("anyOfStrIgnoreCase", () =>
    pass
  );

  test("wsList empty", () =>
    testParse(P.wsList, "", [], {str: "", pos: 0})
  );

  test("wsList single", () =>
    testParse(P.wsList, " ", [" "], {str: " ", pos: 1})
  );

  test("wsList multiple", () =>
    testParse(
      P.wsList,
      "   \t \r \n",
      [" ", " ", " ", "\t", " ", "\r", " ", "\n"],
      {str: "   \t \r \n", pos: 8},
    )
  );

  test("wsStr empty", () =>
    testParse(P.wsStr, "", "", {str: "", pos: 0})
  );

  test("wsStr single", () =>
    testParse(P.wsStr, " ", " ", {str: " ", pos: 1})
  );

  test("wsStr multiple", () =>
    testParse(
      P.wsStr,
      "   \t \r \n",
      "   \t \r \n",
      {str: "   \t \r \n", pos: 8},
    )
  );

  test("ws empty", () =>
    testParse(P.ws, "", (), {str: "", pos: 0})
  );

  test("ws single", () =>
    testParse(P.ws, " ", (), {str: " ", pos: 1})
  );

  test("ws multiple", () =>
    testParse(P.ws, "   \t \r \n", (), {str: "   \t \r \n", pos: 8})
  );

  test("anyCharNotIn", () =>
    pass
  );

  test("anyCharNotInIgnoreCase", () =>
    pass
  );

  test("anyCharInRange", () =>
    pass
  );

  test("anyLowerCaseChar", () =>
    pass
  );

  test("anyUpperCaseChar", () =>
    pass
  );

  test("anyAlpha", () =>
    pass
  );

  test("anyAlphaOrDigit", () =>
    pass
  );

  test("leftParen", () =>
    testParse(P.leftParen, "(", "(", {pos: 1, str: "("})
  );

  test("rightParen", () =>
    testParse(P.rightParen, ")", ")", {pos: 1, str: ")"})
  );

  test("betweenParens", () =>
    testParse(
      P.betweenParens(P.anyNonEmptyDigits),
      "(  456  )",
      "456",
      {pos: 9, str: "(  456  )"},
    )
  );

  test("leftCurly", () =>
    testParse(P.leftCurly, "{", "{", {pos: 1, str: "{"})
  );

  test("rightCurly", () =>
    testParse(P.rightCurly, "}", "}", {pos: 1, str: "}"})
  );

  test("betweenCurlies", () =>
    testParse(
      P.betweenCurlies(P.anyNonEmptyDigits),
      "{  456  }",
      "456",
      {pos: 9, str: "{  456  }"},
    )
  );

  test("leftSquare", () =>
    testParse(P.leftSquare, "[", "[", {pos: 1, str: "["})
  );

  test("rightSquare", () =>
    testParse(P.rightSquare, "]", "]", {pos: 1, str: "]"})
  );

  test("betweenSquares", () =>
    testParse(
      P.betweenSquares(P.anyNonEmptyDigits),
      "[  456  ]",
      "456",
      {pos: 9, str: "[  456  ]"},
    )
  );

  test("leftAngle", () =>
    testParse(P.leftAngle, "<", "<", {pos: 1, str: "<"})
  );

  test("rightAngle", () =>
    testParse(P.rightAngle, ">", ">", {pos: 1, str: ">"})
  );

  test("betweenAngles", () =>
    testParse(
      P.betweenAngles(P.anyNonEmptyDigits),
      "<  456  >",
      "456",
      {pos: 9, str: "<  456  >"},
    )
  );

  test("delimited", () =>
    testParse(
      P.delimited(","),
      "a, b,  c, d   d, ee e, f ",
      ["a", "b", "c", "d   d", "ee e", "f"],
      {pos: 20, str: ""},
    )
  );
});