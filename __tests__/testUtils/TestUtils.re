open Jest;
open Expect;
module P = ReludeParse.Parser;

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
