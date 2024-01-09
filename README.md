# fuzzy-select [![CI Status][ci-badge]][ci-url] [![Crates.io][crates-badge]][crates-url] [![Docs][docs-badge]][docs-url] ![License: MIT OR Apache-2.0][license-badge] ![Rust Version: 1.75.0][rust-version-badge]

[ci-badge]: https://github.com/knutwalker/fuzzy-select/actions/workflows/checks.yml/badge.svg
[ci-url]: https://github.com/knutwalker/fuzzy-select
[crates-badge]: https://img.shields.io/crates/v/fuzzy-select?style=shield
[crates-url]: https://crates.io/crates/fuzzy-select
[docs-badge]: https://img.shields.io/badge/docs-latest-blue.svg?style=shield
[docs-url]: https://docs.rs/fuzzy-select
[license-badge]: https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg?style=shield
[rust-version-badge]: https://img.shields.io/badge/rustc-1.75.0-orange.svg?style=shield


### A fuzzy select prompt for the terminal.

This crate is a library for creating a fuzzy select prompt for the terminal. It uses [nucleo][__link0] as its fuzzy matching engine. The prompt is very simple and not very configurable.


### Usage

Add the following to your `Cargo.toml`:


```toml
[dependencies]
fuzzy-select = "0.1"
```


### Example


```rust
use fuzzy_select::FuzzySelect;

let options = vec!["foo", "bar", "baz"];
let selected = FuzzySelect::new()
    .with_prompt("Select something")
    .with_options(options)
    .select()?;

println!("Selected: {:?}", selected);
```



## License

fuzzy-select is licensed under either of the following, at your option:

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT License ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

---
 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEGxcMnTtl_oCHG0zCht4siKMHG6CDzHC9NHtAG2TRLBwUwaKlYXKEG_yFPXU1peM0GxBFpDN0Z7gaG7cKuARoeC2vGy293UqdVxw2YWSBgmZudWNsZW9lMC4zLjA
 [__link0]: https://crates.io/crates/nucleo/0.3.0
