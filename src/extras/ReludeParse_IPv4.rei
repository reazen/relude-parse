/**
 * IPv4 represents an address that matches the pattern `127.0.0.1`. The four
 * ints have to be positive, in the range 0...255, and they are separated by a
 * period.
 */
type t;

/**
 * Turn an `IPv4.t` value into a string, e.g. "127.0.0.1".
 */
let show: t => string;

/**
 * Destructure an `IPv4.t` value into its 4 integer parts, returning e.g.
 * `(127.0.0.1)`.
 */
let toTuple: t => (int, int, int, int);

/**
 * A `Parser.t` that can produce IPv4 values.
 */
let parser: ReludeParse_Parser.t(t);

/**
 * Attempt to parse a given `string` into an `IPv4.t`, possibly failing with a
 * ParseError.
 */
let parse: string => Belt.Result.t(t, ReludeParse_Parser.ParseError.t);

/**
 * Attempt to parse a given `string`, discarding the ParseError and returning a
 * simple option.
 */
let parseOption: string => option(t);

/**
 * Attempt to convert a string to an `IPv4.t`, raising an exception if parsing
 * fails. This may be useful when working with hard-coded IPv4 values that are
 * known to be good (e.g. when writing tests), but you should actively avoid
 * this in production code.
 */
let unsafeFromString: string => t;

/**
 * Attempt to convert 4 `int` values to an IPv4, raising an exception if any of
 * the ints is out of the appropriate range. Avoid this in production code.
 */
let unsafeFromInts: (int, int, int, int) => t;
