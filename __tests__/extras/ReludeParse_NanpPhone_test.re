open Jest;
open Expect;
open Relude.Globals;
module NanpPhone = ReludeParse.NanpPhone;

describe("NanpPhone", () => {
  let parse = str => NanpPhone.parse(str) |> Result.map(NanpPhone.toDigits);
  let tuple3334445555 = (3, 3, 3, 4, 4, 4, 5, 5, 5, 5);

  test("xxx-yyy-zzzz (separated by hyphens, succeeds)", () =>
    expect(parse("333-444-5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("(xxx) yyy-zzzz (area code parens, succeeds)", () =>
    expect(parse("(333) 444-5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("333.444.5555 (dots as separator, succeeds)", () =>
    expect(parse("333.444.5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("333 444 5555 (space as separator, succeeds)", () =>
    expect(parse("333 444 5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("3334445555 (no separator, succeeds)", () =>
    expect(parse("3334445555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("1 (333) 444-5555 (country code: 1, succeeds)", () =>
    expect(parse("1 (333) 444-5555"))
    |> toEqual(Result.ok(tuple3334445555))
  );

  test("+13334445555 (leading +1 country code, succeeds)", () =>
    expect(parse("+13334445555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("empty string (fails)", () =>
    expect(NanpPhone.parse("") |> Result.isError) |> toEqual(true)
  );

  // TODO: invalid characters, e.g. exchange 911

  test("(123) 444-5555 (area code starts with 1, fails)", () =>
    expect(parse("(123) 444-5555") |> Result.isError) |> toEqual(true)
  );

  test("+11234445555 (country and area code both 1, fails)", () =>
    expect(parse("+11234445555") |> Result.isError) |> toEqual(true)
  );

  test("+23334445555 (non-1 country code, fails)", () =>
    expect(parse("+23334445555") |> Result.isError) |> toEqual(true)
  );

  test("33344455556 (11 characters, no valid country code, fails)", () =>
    expect(parse("33344455556") |> Result.isError) |> toEqual(true)
  );

  test("133344455556 (12 characters, with valid country code, fails)", () =>
    expect(parse("133344455556") |> Result.isError) |> toEqual(true)
  );

  test("too few characters (fails)", () =>
    expect(NanpPhone.parse("333444") |> Result.isError) |> toEqual(true)
  );

  test("parseOption (success)", () =>
    expect(NanpPhone.parseOption("333.444.5555"))
    |> toEqual(Some(NanpPhone.unsafeFromString("333.444.5555")))
  );

  test("parseOption (fails)", () =>
    expect(NanpPhone.parseOption("1234567890")) |> toEqual(None)
  );

  test("unsafeFromString (throws on failure)", () =>
    expect(() =>
      NanpPhone.unsafeFromString("")
    ) |> toThrow
  );

  testAll(
    "show",
    [
      (NanpPhone.Local, "754-3010"),
      (Domestic, "303-754-3010"),
      (DomesticShort, "3037543010"),
      (DomesticParen, "(303) 754-3010"),
      (International, "+1-303-754-3010"),
      (InternationalShort, "+13037543010"),
    ],
    ((format, expected)) => {
      let phone = NanpPhone.unsafeFromString("3037543010");
      expect(NanpPhone.show(~format, phone)) |> toEqual(expected);
    },
  );
});