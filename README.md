# type-mapper

A Rust procedural macro crate for powerful type-level pattern matching and transformation. This crate is particularly useful for macro authors who need to perform complex type manipulations at compile time.

## Features

- Type pattern matching with wildcards
- Lifetime handling and manipulation
- Recursive type transformations
- Support for generic type parameters
- Path-aware type matching

## Limitations

The macro matches on token patterns rather than full types. It performs no
validation of generic parameters or type identity.

For example, you cannot tell whether a type named `Ordering` was imported from
`std::cmd::Ordering` or `std::sync::atomic::Ordering`.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
type-mapper = "0.x.y"
```

## Examples

### Basic Type Matching

The crate provides a `assert_type_matches!` and `assert_type_not_matches!` macros for testing type matches.

```rust
use type_mapper::{assert_type_matches, assert_type_not_matches, map_types};

// Simple type matching
assert_type_matches!(Vec<u8>, Vec<u8>);
assert_type_matches!(Vec<u8>, Vec);
assert_type_matches!(Vec<u8>, _T <u8>);
assert_type_matches!(Vec<u8>, Vec<_>);

// Wildcard matching
assert_type_matches!(u8, _);
assert_type_matches!(u8, _T);
assert_type_matches!(u8, _T<>);
assert_type_not_matches!(u8, _T<_U>);

// Type mismatch
assert_type_not_matches!(Vec<u8>, Vec<u16>);
assert_type_not_matches!(u8, u16);
assert_type_not_matches!(std::sync::MutexGuard<'a, u8>, std::sync::MutexGuard<'_>);
```

### Lifetime Handling

```rust
use type_mapper::map_types;
use static_assertions::assert_type_eq_all;

/// Make a slice type static.
macro_rules! make_slice_static {
    ($ty:ty) => {
        map_types!(
            match $ty {
                &'_ [_T] => &'static [_T],
            }
        )
    }
}

assert_type_eq_all!(make_slice_static!(&'a [u8]), &'static [u8]);
assert_type_eq_all!(make_slice_static!(&'static [u8]), &'static [u8]);

```

### Recursive Type Matching

Using the internal `recurse!` macro, you can match on types recursively.

```rust
use type_mapper::map_types;
use static_assertions::assert_type_eq_all;

/// Unwrap an Option type recursively.
macro_rules! unwrap_option {
    ($ty:ty) => {
        map_types!(
            match $ty {
                Option<_T> => recurse!(_T),
                _T => _T,
            }
        )
    }
}

assert_type_eq_all!(unwrap_option!(Option<u8>), u8);
assert_type_eq_all!(unwrap_option!(Option<Option<u8>>), u8);
assert_type_eq_all!(unwrap_option!(Option<Option<Option<u8>>>), u8);
```

## Pattern Matching Syntax

The crate supports several pattern matching features:

- `_` - Wildcard for any type
- `_T` - Named wildcard that can be referenced in the result
- `_T<T>` - Generic type matching
- `_T<>` - Generic type matching with no generics (WARNING: rustfmt may eat the `<>` if you don't use `#[rustfmt::skip]`)
- `'_` - Wildcard lifetime (will also match no lifetime in a reference)
- `'_A` - Named wildcard lifetime (will also match no lifetime in a reference)
- `'a` - Named lifetime matching
- `recurse!(_T)` - Recursive type transformation

## Use Cases

This crate is particularly useful for:

1. Macro authors who need to transform types
2. Creating type-level DSLs
3. Implementing complex type-level logic
4. Generic type manipulation
5. Lifetime manipulation in macros

## License

MIT License
