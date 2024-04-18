# interpreterbook
Repo for <interpreterbook.com>

Note to future me:

Expression parsers start on the first token of the expression and end on the last token, not after it.
`parse_statement` ends *after* the semicolon, that is on the first token of the next statement.

Some ideas for future me:
- Add line numbers to lexer and pass through to evaluation for nice error messages
- Implement rust's `as` syntax and disallow implicit typecasting
- Turn let statements and return statements into expressions
- Let every block expression have its own scope rather than just functions
- Is it possible/feasible to check for existence of variables used in function when it is defined, rather than when
it is executed?
- GC?
- Error handling in lexer
    - No closing apostrophe for Str
- Don't allow a variable being set to Null
- Rework infix operator evaluation to more easily allow operators of different types
    - Have a function for each operator which matches types, rather than each type combinations which matches operator
- Implement fuzzy string matching operator?
    - More of a DSA exercise than a build-an-interpreter exercise
- Allow passing multiple indices at once separated by commas
