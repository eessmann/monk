//! Run with `cargo miri test -p monk-runtime --test semantic_miri`.
//! Reinclude pure modules so Miri does not execute the native pre-main hook.
#![feature(pattern_types, pattern_type_macro)]
#![allow(incomplete_features, internal_features, dead_code)]
#[path = "../src/abi2.rs"]
mod abi2;
#[path = "../src/semantics/mod.rs"]
mod semantics;
#[path = "../src/types.rs"]
mod types;
