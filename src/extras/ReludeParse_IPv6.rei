/**
 * A type representing v6 of the IP address standard.
 */
type t;

/**
 * The typed equivalent of the IPv6 address "0:0:0:0:0:0:0:1" or in its short
 * form: "::1".
 */
let loopback: t;

/**
 * Print an `IPv6.t` value as a human-readable string (8 hexadecimal values,
 * separated by a colon, with `loopback` special-cased to "::1").
 */
let show: t => string;

/**
 * A `Parser.t` that can produce IPv6 values.
 */
let parser: ReludeParse_Parser.t(t);

/**
 * Attempt to parse a given `string` into an `IPv6.t`, possibly failing with a
 * ParseError.
 */
let parse: string => Belt.Result.t(t, ReludeParse_Parser.ParseError.t);

/**
 * Attempt to parse a given `string`, discarding the ParseError and returning a
 * simple option.
 */
let parseOption: string => option(t);

/**
 * Attempt to convert a string to an `IPv6.t`, raising an exception if parsing
 * fails. This may be useful when you have hard-coded values known to be good,
 * but it should be avoided in most production code.
 */
let unsafeFromString: string => t;

/**
 * Attempt to convert 8 `int` values to an IPv6, raising an exception if any of
 * the ints is out of the appropriate hexadecimal range. Avoid this in
 * production code.
 */
let unsafeFromInts: (int, int, int, int, int, int, int, int) => t;
