# proc-easy

[![crates](https://img.shields.io/crates/v/proc-easy.svg?style=for-the-badge&label=proc-easy)](https://crates.io/crates/proc-easy)
[![docs](https://img.shields.io/badge/docs.rs-proc--easy-66c2a5?style=for-the-badge&labelColor=555555&logoColor=white)](https://docs.rs/proc-easy)
[![actions](https://img.shields.io/github/workflow/status/zakarumych/proc-easy/badge/master?style=for-the-badge)](https://github.com/zakarumych/proc-easy/actions?query=workflow%3ARust)
[![MIT/Apache](https://img.shields.io/badge/license-MIT%2FApache-blue.svg?style=for-the-badge)](COPYING)
![loc](https://img.shields.io/tokei/lines/github/zakarumych/proc-easy?style=for-the-badge)

Macros to make writing proc-macro crates easy.

This crate provides mainly macros and supporting types and traits
to reduce amount of boilerplate required for working with [`syn`].

Currently most of the macros are targeted to construct types
that then can be parses to configure proc-macro and proc-derive-macro implementation.

[`easy_token!`] - defines new custom token from ident. To be used in other structs.

[`easy_parse!`] - defines struct or enum that can be parsed and peeked from stream.

[`easy_argument!`] - defines struct with a token as a name and the rest to be parsed as-is.

[`easy_argument_group!`] - defines a group of arguments as enum of arguments.

[`easy_argument_tuple!`] - specialized version of [`easy_argument!`] that parses fields starting from 2nd as [`EasyArgumentField`]s inside parenthesis and in any order.

[`easy_argument_value!`] - specialized version of [`easy_argument!`] for 2 field structs. It defines 2nd field as a value that can be parsed after `=` token or inside parenthesis.

[`easy_separated!`] - defines struct that parses fields as [`EasyArgumentField`]s in any order. Does not accept trailing punctuation.

[`easy_terminated!`] - defines struct that parses fields as [`EasyArgumentField`]s in any order. Accepts trailing punctuation. Parses whole stream.

[`easy_attributes!`] - defines struct that parses fields as [`EasyArgumentField`]s from a slice of [`Attribute`]s with specified namespace.

[`EasyArgumentField`] is implemented for types defined with [`easy_token!`], [`easy_argument!`], [`easy_argument_tuple!`], [`easy_argument_value!`] and [`easy_argument_group!`] possibly wrapped in [`Option`] or [`Vec`].

[`easy_token!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_token.html
[`easy_parse!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_parse.html
[`easy_argument!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_argument.html
[`easy_argument_group!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_argument_group.html
[`easy_argument_tuple!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_argument_tuple.html
[`easy_argument_value!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_argument_value.html
[`easy_separated!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_separated.html
[`easy_terminated!`]:  https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_terminated.html
[`easy_attributes!`]: https://docs.rs/proc-easy/0.1.0/proc_easy/macro.easy_attributes.html
[`EasyArgumentField`]: https://docs.rs/proc-easy/latest/proc_easy/trait.EasyArgumentField.html
[`Attribute`]: https://docs.rs/syn/latest/syn/struct.Attribute.html
[`Option`]: https://doc.rust-lang.org/std/option/enum.Option.html
[`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
[`syn`]: https://docs.rs/syn
## License

Licensed under either of

* Apache License, Version 2.0, ([license/APACHE](license/APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([license/MIT](license/MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
