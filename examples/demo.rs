#![allow(unused_crate_dependencies)]

use fuzzy_select::FuzzySelect;

fn main() {
    let items = vec!["Hello there", "General Kenobi", "You are a bold one"];

    let item = FuzzySelect::new()
        .with_prompt("Select your destiny:")
        .with_options(items)
        .with_query("o")
        .with_initial_selection(2)
        .select()
        .expect("Failed to create FuzzySelect");

    println!("You have chosen: {item:?}");
}
