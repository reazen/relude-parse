open Jest;
open Expect;

let jestFail: string => assertion = fail;
let parseFail: 'a. string => ReludeParse.Parser.t('a) = ReludeParse.Parser.fail;

describe("Tutorial", () => {
  // This code below is referenced in the README

  test("Running a parser normally (success)", () =>
    ReludeParse.Parser.(
      anyDigit |> runParser("3") |> expect |> toEqual(Belt.Result.Ok("3"))
    )
  );

  test("Running a parser normally (failure)", () =>
    ReludeParse.Parser.(
      anyDigit
      |> runParser("!")
      |> expect
      |> toEqual(
           Belt.Result.Error(
             ParseError.ParseError(
               "Expected a digit, but found character '!'",
             ),
           ),
         )
    )
  );

  test("Running a parser verbosely (success)", () =>
    ReludeParse.Parser.(
      anyDigit
      |> unParser({pos: 0, str: "3"})
      |> Relude.Result.fold(
           ({error: ParseError(message)}) =>
             jestFail("Parse should have succeeded: " ++ message),
           success =>
             expect(success)
             |> toEqual({
                  result: "3",
                  suffix: {
                    pos: 1,
                    str: "3",
                  },
                }),
         )
    )
  );

  test("mapping the result of a parser - unsafe & impure example", () =>
    ReludeParse.Parser.(
      many1(anyDigit)
      <#> Relude.Nel.foldLeft((acc, v) => acc ++ v, "")
      <#> int_of_string  // DANGER!
      |> runParser("12345")
      |> expect
      |> toEqual(Belt.Result.Ok(12345))
    )
  );

  test("mapping the result of a parser - safe & pure example", () =>
    ReludeParse.Parser.(
      many1(anyDigit)
      <* eof
      <#> Relude.Nel.foldLeft((acc, v) => acc ++ v, "")
      >>= (
        digitString =>
          Relude.Int.fromString(digitString)
          |> Relude.Option.foldLazy(
               () =>
                 parseFail(
                   "Failed to conver digit string to an int: " ++ digitString,
                 ),
               pure,
             )
      )
      |> runParser("12345")
      |> expect
      |> toEqual(Belt.Result.Ok(12345))
    )
  );

  test("tuple2 example", () =>
    ReludeParse.Parser.(
      tuple2(anyDigit, anyDigit)
      |> runParser("12")
      |> expect
      |> toEqual(Belt.Result.Ok(("1", "2")))
    )
  );

  test("map2 example", () =>
    ReludeParse.Parser.(
      map2((a, b) => a + b, anyDigitAsInt, anyDigitAsInt)
      |> runParser("12")
      |> expect
      |> toEqual(Belt.Result.Ok(3))
    )
  );

  test("mapTuple2 example", () =>
    ReludeParse.Parser.(
      (anyDigitAsInt, anyDigitAsInt)
      |> mapTuple2((a, b) => a + b)
      |> runParser("12")
      |> expect
      |> toEqual(Belt.Result.Ok(3))
    )
  );

  test("*> example", () =>
    ReludeParse.Parser.(
      ws
      *> anyDigit
      |> runParser("    3")
      |> expect
      |> toEqual(Belt.Result.Ok("3"))
    )
  );

  test("<* example", () =>
    ReludeParse.Parser.(
      ws
      *> anyDigit
      <* ws
      <* eof
      |> runParser("    3   ")
      |> expect
      |> toEqual(Belt.Result.Ok("3"))
    )
  );

  test("<$> <*> example", () => {
    let add3 = (a, b, c) => a + b + c;
    ReludeParse.Parser.(
      add3
      <$> anyDigitAsInt
      <*> anyDigitAsInt
      <*> anyDigitAsInt
      |> runParser("123")
      |> expect
      |> toEqual(Belt.Result.Ok(6))
    );
  });

  test("pure example", () =>
    ReludeParse.Parser.(
      pure(3)
      |> runParser("abcdef")
      |> expect
      |> toEqual(Belt.Result.Ok(3))
    )
  );

  test("Sequence example", () =>
    ReludeParse.Parser.(
      anyDigitAsInt
      |> flatMap(count => anyAlpha |> times(count) <* eof)
      |> map(chars => Relude.List.String.join(chars))
      |> runParser("3abc")
      |> expect
      |> toEqual(Belt.Result.Ok("abc"))
    )
  );

  test("Sequence operator example", () =>
    ReludeParse.Parser.(
      anyDigitAsInt
      >>= (count => times(count, anyAlpha) <* eof)
      <#> Relude.List.String.join
      |> runParser("3abc")
      |> expect
      |> toEqual(Belt.Result.Ok("abc"))
    )
  );

  test("Sequence with fail example", () =>
    ReludeParse.Parser.(
      anyDigitAsInt
      >>= (
        count =>
          if (count >= 5) {
            ReludeParse.Parser.fail("The count cannot be >= 5");
          } else {
            times(count, anyAlpha) <* eof;
          }
      )
      <#> Relude.List.String.join
      |> runParser("9abc")
      |> expect
      |> toEqual(
           Belt.Result.Error(
             ParseError.ParseError("The count cannot be >= 5"),
           ),
         )
    )
  );

  test("<|> left example", () =>
    ReludeParse.Parser.(
      anyDigit
      <|> anyAlpha
      |> runParser("9")
      |> expect
      |> toEqual(Belt.Result.Ok("9"))
    )
  );

  test("<|> right example", () =>
    ReludeParse.Parser.(
      anyDigit
      <|> anyAlpha
      |> runParser("c")
      |> expect
      |> toEqual(Belt.Result.Ok("c"))
    )
  );

  test("<|> error example", () =>
    ReludeParse.Parser.(
      anyDigit
      <|> anyAlpha
      |> runParser("!")
      |> expect
      |> toEqual(
           Belt.Result.Error(
             ParseError.ParseError(
               "Expected any alphabet letter (upper or lowercase)",
             ),
           ),
         )
    )
  );

  test("<|> with tries example", () =>
    ReludeParse.Parser.(
      tries(anyDigit *> anyDigit)
      <|> anyDigit
      *> anyAlpha
      |> runParser("9a")
      |> expect
      |> toEqual(Belt.Result.Ok("a"))
    )
  );

  test("Custom error example", () =>
    ReludeParse.Parser.(
      many1(anyDigit)
      <?> "Expected one or more digits"
      |> runParser("abc")
      |> expect
      |> toEqual(
           Belt.Result.Error(
             ParseError.ParseError("Expected one or more digits"),
           ),
         )
    )
  );

  test("IPv4 example 1", () =>
    ReludeParse.Parser.(
      (
        anyPositiveShort <* str("."),
        anyPositiveShort <* str("."),
        anyPositiveShort <* str("."),
        anyPositiveShort,
      )
      |> mapTuple4(ReludeParse.IPv4.unsafeFromInts)
      |> runParser("127.0.0.1")
      |> Relude.Result.map(ReludeParse.IPv4.toTuple)
      |> expect
      |> toEqual(Belt.Result.Ok((127, 0, 0, 1)))
    )
  );

  test("IPv4 example 2", () =>
    ReludeParse.Parser.(
      anyPositiveShort
      >>= (
        a =>
          str(".")
          >>= (
            _ =>
              anyPositiveShort
              >>= (
                b =>
                  str(".")
                  >>= (
                    _ =>
                      anyPositiveShort
                      >>= (
                        c =>
                          str(".")
                          >>= (
                            _ =>
                              anyPositiveShort
                              <#> (
                                d =>
                                  ReludeParse.IPv4.unsafeFromInts(a, b, c, d)
                              )
                          )
                      )
                  )
              )
          )
      )
      |> runParser("127.0.0.1")
      |> Relude.Result.map(ReludeParse.IPv4.toTuple)
      |> expect
      |> toEqual(Belt.Result.Ok((127, 0, 0, 1)))
    )
  );

  test("IPv4 example 3", () =>
    ReludeParse.Parser.(
      ReludeParse.IPv4.unsafeFromInts
      <$> anyPositiveShort
      <* str(".")
      <*> anyPositiveShort
      <* str(".")
      <*> anyPositiveShort
      <* str(".")
      <*> anyPositiveShort
      |> runParser("127.0.0.1")
      |> Relude.Result.map(ReludeParse.IPv4.toTuple)
      |> expect
      |> toEqual(Belt.Result.Ok((127, 0, 0, 1)))
    )
  );

  test("IPv4 example 4", () =>
    ReludeParse.Parser.(
      anyPositiveShort
      |> sepBy(str("."))
      >>= (
        shorts =>
          switch (shorts) {
          | [a, b, c, d] =>
            pure(ReludeParse.IPv4.unsafeFromInts(a, b, c, d))
          | _ => parseFail("Expected exactly 4 shorts separated by .")
          }
      )
      |> runParser("127.0.0.1")
      |> Relude.Result.map(ReludeParse.IPv4.toTuple)
      |> expect
      |> toEqual(Belt.Result.Ok((127, 0, 0, 1)))
    )
  );

  test("CSV example", () => {
    let fieldParser =
      ReludeParse.Parser.(
        manyUntilPeek(str(",") |> orEOF, anyChar) <#> Relude.List.String.join
      );

    let csvParser = ReludeParse.Parser.(fieldParser |> sepBy(str(",")));

    let actual = csvParser |> ReludeParse.runParser("abc,def");
    let expected = Belt.Result.Ok(["abc", "def"]);
    expect(actual) |> toEqual(expected);
  });
});