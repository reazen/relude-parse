// Core parser
module Parser = ReludeParse_Parser;

// Alias a few things at the top-level
let runParser = Parser.runParser;
let unParser = Parser.unParser;

// Extras
module IPv4 = ReludeParse_IPv4;
module IPv6 = ReludeParse_IPv6;
module NanpPhone = ReludeParse_NanpPhone;
module UUID = ReludeParse_UUID;
