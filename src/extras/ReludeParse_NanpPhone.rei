module AreaCode: {
  /**
   * An area code is the first 3 digits after the country code.
   */
  type t;
  let parser: ReludeParse_Parser.t(t);
};

module Exchange: {
  /**
   * The exchange is the second set of 3 digits.
   */
  type t;
  let parser: ReludeParse_Parser.t(t);
};

module Line: {
  /**
   * The line is the final 4 digits in a phone number
   */
  type t;
  let parser: ReludeParse_Parser.t(t);
};

/**
 * A phone number, following North American Numbering Plan conventions (used by
 * the US, Canada, and others). No country code is included because all NANP
 * countries begin with 1 (often implied without being specified).
 */
type t =
  | NanpPhone(AreaCode.t, Exchange.t, Line.t);

/**
 * Construct an NANP phone number from its 3 parts. The constructor itself is
 * safe, but the individual parts can only be produced through validation.
 */
let make: (AreaCode.t, Exchange.t, Line.t) => t;

/**
 * Fully destructure a phone number to its 10 digits (beginning with the area
 * code, without the country prefix).
 */
let toDigits: t => (int, int, int, int, int, int, int, int, int, int);

/**
 * A parser capable of producing a phone number from a string. The parser aims
 * to be flexible, allowing (and discarding) a country prefix of 1 (optionally
 * including a leading +), parentheses around the area code, and space, hyphen,
 * or period characters separating the parts.
 */
let parser: ReludeParse_Parser.t(t);

/**
 * Attempt to produce a valid phone number from a given string, possibly failing
 * with a ParseError.
 */
let parse: string => Belt.Result.t(t, ReludeParse_Parser.ParseError.t);

/**
 * Attempt to produce a valid phone number from a given string, discarding the
 * error and returning a simple `option`.
 */
let parseOption: string => option(t);

/**
 * Attempt to produce a valid phone number from a given string, raising an
 * exception if the value is invalid. While this may be useful for testing, it
 * should be avoided in production code.
 */
let unsafeFromString: string => t;

/**
 * Formats for converting a `NanpPhone` to a string
 * 
 * `Local`              is a format like "754-3010" (no area code)
 * `Domestic`           is a format like "303-754-3010"
 * `DomesticShort`      is a format like "3037543010"
 * `DomesticParen`      is a format like "(303) 754-3010"
 * `International`      is a format like "+1-303-754-3010"
 * `InternationalShort` is a format like "+13037543010"
 */
type format =
  | Local
  | Domestic
  | DomesticShort
  | DomesticParen
  | International
  | InternationalShort;

/**
 * Converts a `NanpPhone.t` to a string using the given format
 */
let show: (~format: format=?, t) => string;