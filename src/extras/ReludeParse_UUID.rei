/**
 * UUID.t represents a universally unique identifier, made up of 32 hexadecimal
 * digits in the format `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`. Its
 * implementation is hidden, but it can be constructed via a validating parser,
 * and it can be turned into a string with hyphens in the appropriate places.
 */
type t;

/**
 * Turn a `UUID.t` into a valid string representation, including hyphens.
 */
let show: t => string;

/**
 * A `Parser.t` that can produce UUID values.
 */
let parser: ReludeParse_Parser.t(t);

/**
 * Attempt to parse a given `string` into a `UUID.t`, possibly failing with a
 * ParseError.
 */
let parse: string => Belt.Result.t(t, ReludeParse_Parser.ParseError.t);

/**
 * Attempt to parse a given `string`, discarding the ParseError and returning a
 * simple `option(t)`
 */
let parseOption: string => option(t);

/**
 * Attempt to convert a string to a `UUID.t`, raising an exception if parsing
 * fails. This may be useful when working with hard-coded UUIDs that are known
 * to be good (e.g. when writing tests), but you should actively avoid this in
 * production code.
 */
let unsafeFromString: string => t;
