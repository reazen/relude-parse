open Jest;
open Expect;

module P = ReludeParse.Parser;
open P;

module IPv6 = ReludeParse.IPv6;

let (testParse, testParseFail) =
  ReludeParse_Parser_test.(testParse, testParseFail);

describe("ReludeParse_IPv6", () => {
  testAll(
    "show",
    [
      (IPv6.loopback, "::1"),
      (IPv6.unsafeFromInts(15, 0, 0, 0, 0, 0, 0, 1), "f:0:0:0:0:0:0:1"),
      (IPv6.unsafeFromInts(16, 0, 0, 0, 0, 0, 0, 1), "10:0:0:0:0:0:0:1"),
      (IPv6.unsafeFromInts(17, 0, 0, 0, 0, 0, 0, 255), "11:0:0:0:0:0:0:ff"),
    ],
    ((ip, str)) =>
    expect(IPv6.show(ip)) |> toEqual(str)
  );

  testAll(
    "valid",
    [
      ("::1", IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 1), 3),
      ("0:0:0:0:0:0:0:0", IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 0), 15),
      ("0:0:0:0:0:0:0:1", IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 1), 15),
      ("1:0:0:0:0:0:0:0", IPv6.unsafeFromInts(1, 0, 0, 0, 0, 0, 0, 0), 15),
      (
        "00:00:00:00:00:00:00:01",
        IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 1),
        23,
      ),
      (
        "000:000:000:000:000:000:000:001",
        IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 1),
        31,
      ),
      (
        "0000:0000:0000:0000:0000:0000:0000:0001",
        IPv6.unsafeFromInts(0, 0, 0, 0, 0, 0, 0, 1),
        39,
      ),
      (
        "2001:0db8:0000:0000:0000:ff00:0042:8329",
        IPv6.unsafeFromInts(8193, 3512, 0, 0, 0, 65280, 66, 33577),
        39,
      ),
      (
        "2001:db8:0:0:0:ff00:42:8329",
        IPv6.unsafeFromInts(8193, 3512, 0, 0, 0, 65280, 66, 33577),
        27,
      ),
      (
        "2001:db8::ff00:42:8329",
        IPv6.unsafeFromInts(8193, 3512, 0, 0, 0, 65280, 66, 33577),
        22,
      ),
      ("2001::8329", IPv6.unsafeFromInts(8193, 0, 0, 0, 0, 0, 0, 33577), 10),
      (
        "ffff::ffff",
        IPv6.unsafeFromInts(65535, 0, 0, 0, 0, 0, 0, 65535),
        10,
      ),
      (
        "ffff:ffff::ffff",
        IPv6.unsafeFromInts(65535, 65535, 0, 0, 0, 0, 0, 65535),
        15,
      ),
      (
        "ffff:ffff:ffff::ffff",
        IPv6.unsafeFromInts(65535, 65535, 65535, 0, 0, 0, 0, 65535),
        20,
      ),
      (
        "ffff:ffff:ffff:ffff::ffff",
        IPv6.unsafeFromInts(65535, 65535, 65535, 65535, 0, 0, 0, 65535),
        25,
      ),
      (
        "ffff:ffff:ffff:ffff:ffff::ffff",
        IPv6.unsafeFromInts(65535, 65535, 65535, 65535, 65535, 0, 0, 65535),
        30,
      ),
      (
        "ffff:ffff:ffff:ffff:ffff:ffff::ffff",
        IPv6.unsafeFromInts(
          65535,
          65535,
          65535,
          65535,
          65535,
          65535,
          0,
          65535,
        ),
        35,
      ),
      (
        "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",
        IPv6.unsafeFromInts(
          65535,
          65535,
          65535,
          65535,
          65535,
          65535,
          65535,
          65535,
        ),
        39,
      ),
      (
        "ffff::ffff:ffff:ffff:ffff:ffff:ffff",
        IPv6.unsafeFromInts(
          65535,
          0,
          65535,
          65535,
          65535,
          65535,
          65535,
          65535,
        ),
        35,
      ),
      (
        "ffff::ffff:ffff:ffff:ffff:ffff",
        IPv6.unsafeFromInts(65535, 0, 0, 65535, 65535, 65535, 65535, 65535),
        30,
      ),
      (
        "ffff::ffff:ffff:ffff:ffff",
        IPv6.unsafeFromInts(65535, 0, 0, 0, 65535, 65535, 65535, 65535),
        25,
      ),
      (
        "ffff::ffff:ffff:ffff",
        IPv6.unsafeFromInts(65535, 0, 0, 0, 0, 65535, 65535, 65535),
        20,
      ),
      (
        "ffff::ffff:ffff",
        IPv6.unsafeFromInts(65535, 0, 0, 0, 0, 0, 65535, 65535),
        15,
      ),
    ],
    ((str, exp, pos)) =>
    testParse(IPv6.parser <* eof, str, exp, {pos, str})
  );

  testAll(
    "invalid",
    [
      ("", 0),
      ("x", 0),
      ("255.255.255", 3),
      ("255.255.255.", 3),
      ("1.2.a.3", 1),
      ("255.257.0.1", 3),
    ],
    ((str, pos)) =>
    testParseFail(IPv6.parser <* eof, str, pos)
  );

  test("parseOption (success)", () =>
    expect(IPv6.parseOption("ffff::ffff:ffff:ffff"))
    |> toEqual(
         Some(IPv6.unsafeFromInts(65535, 0, 0, 0, 0, 65535, 65535, 65535)),
       )
  );

  test("parseOption (failure)", () =>
    expect(IPv6.parseOption("z:z:z:z:z:z:z:z")) |> toEqual(None)
  );

  test("unsafeFromString (throws)", () =>
    expect(() =>
      IPv6.unsafeFromString("x")
    ) |> toThrow
  );
});
