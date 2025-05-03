# ocaml-func
An OCaml library that provides an AST and a battle-hardened parser for [FunC](https://docs.ton.org/v3/documentation/smart-contracts/func/overview).

## Usage
The library exposes two main components:
1. The AST definition: [ast.ml](./src/func/ast.ml)
2. The `parse` function that takes the filepath and parses it: [func.ml](./src/func/func.ml)

See the complete usage example that parses FunC code to AST: [driver.ml](./src/bin/driver.ml).

## Limitations
The parser successfully handles all real-world FunC contracts used in our tests, but some edge cases may arise due to the language's design.

If parsing fails, consider:
1. Migrate to [a properly designed language](https://tact-lang.org)
2. Rewrite unparsable code
3. [Submit a pull request](https://github.com/nowarp/ocaml-func/pulls) with a fix

## See also
- **[nowarp.io](https://nowarp.io)**: We do Security stuff in the TON ecosystem
- **[Community Chat](https://t.me/tonsec_chat)**: Where FunC tooling questions are on-topic
