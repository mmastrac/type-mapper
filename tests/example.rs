#![allow(unused)]

use static_assertions::{assert_type_eq_all, const_assert, const_assert_eq};
use type_mapper::{assert_type_matches, assert_type_not_matches, map_types};

#[rustfmt::skip] // Don't eat the <>
mod match_tests {
    use super::*;

    assert_type_matches!(u8, _);                  // matches wildcard
    assert_type_matches!(u8, _T);                 // matches named wildcard
    assert_type_matches!(u8, _T<>);               // matches wildcard with no generics

    assert_type_not_matches!(u8, u16);            // type mismatch
    assert_type_not_matches!(u8, _T<u8>);         // u8 has not generics


    assert_type_matches!([u8; 1], _);             // matches wildcard
    assert_type_matches!([u8; 1], _T);            // matches named wildcard
    assert_type_matches!([u8; 1], _T<>);          // matches wildcard with no generics

    assert_type_not_matches!([u8; 1], [u8; 2]);   // type mismatch
    assert_type_not_matches!([u8; 1], u8);        // type mismatch
    assert_type_not_matches!([u8; 1], _T<u8>);    // u8 has no generics


    assert_type_matches!(Vec<u8>, Vec<u8>); // matches exact type
    assert_type_matches!(Vec<u8>, Vec);     // matches naked type
    assert_type_matches!(Vec<u8>, _T<u8>);  // matches wildcard with generics
    assert_type_matches!(Vec<u8>, Vec<_>);  // matches type with wildcard generics

    assert_type_not_matches!(Vec<u8>, Vec<u16>); // type mismatch
    assert_type_not_matches!(Vec<u8>, Vec<>);    // no generics


    assert_type_matches!(std::sync::MutexGuard<u8>, _T);                        // matches wildcard
    assert_type_matches!(std::sync::MutexGuard, _T);                            // matches wildcard
    assert_type_matches!(std::sync::MutexGuard<u8>, _T<u8>);                    // matches wildcard with generics
    assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'a, u8>);            // matches wildcard with lifetime and generics
    assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, u8>);            // matches wildcard with lifetime and generics
    assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, _>);             // matches wildcard with wildcard generics
    assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, __>);            // matches wildcard with wildcard generics
    assert_type_matches!(std::sync::MutexGuard<'a, u8>, std::sync::MutexGuard); // matches naked type

    assert_type_not_matches!(std::sync::MutexGuard<'a, u8>, std::sync::MutexGuard<'b, u8>); // lifetime mismatch
    assert_type_not_matches!(std::sync::MutexGuard, std::sync::MutexGuard<'b, u8>);         // naked type won't match generic type
    assert_type_not_matches!(std::sync::MutexGuard<'a, u8>, std::sync::MutexGuard<'_>);     // missing generic
    assert_type_not_matches!(std::sync::MutexGuard<'a, u8>, std::sync::MutexGuard<>);       // no generics
}

/// Wildcard, unnamed type
static TEST_WILDCARD_UNNAMED: map_types!(
    match Vec<T> {
        Vec<_> => u8,
        _ => u16,
    }
) = 1_u8;

/// Wildcard lifetime
static TEST_WILDCARD_LIFETIME: map_types!(
    match &'a [T] {
        &'_ [T] => u8,
        _ => u16,
    }
) = 1_u8;

/// Named lifetime
static TEST_NAMED_LIFETIME: map_types!(
    match &'a [T] {
        &'a [T] => u8,
        _ => u16,
    }
) = 1_u8;

/// Named wildcard
static TEST_NAMED_WILDCARD: map_types!(
    match Vec<u8> {
        Vec<_V> => _V,
        _ => u16,
    }
) = 1_u8;

/// Named wildcard
static TEST_NAMED_PATH_WILDCARD: map_types!(
    match std::vec::Vec<u8> {
        std::vec::Vec<_V> => _V,
        _ => u16,
    }
) = 1_u8;

/// Recursion
static TEST_RECURSION: map_types!(
    match Vec<u8> {
        Vec<_V> => recurse!(_V),
        u8 => u8,
        _ => u16,
    }
) = 1_u8;

#[macro_export]
macro_rules! remove_lifetime {
    ($ty:ty) => {
        map_types!(
            match $ty {
                _T<'_> => _T,
                _T<'_, _G> => _T<_G>,
                &'_ _T => &'_ _T,
                _T => _T,
            }
        )
    }
}

assert_type_eq_all!(
    remove_lifetime!(std::sync::MutexGuard<u8>),
    remove_lifetime!(std::sync::MutexGuard<'_, u8>),
    remove_lifetime!(std::sync::MutexGuard<'a, u8>),
    std::sync::MutexGuard<u8>
);

#[macro_export]
macro_rules! remove_lifetime_named {
    ($lt:lifetime, $ty:ty) => {
        map_types!(
            match $ty {
                _T<'a, _G> => _T<_G>,
                _T<'_A> => _T<'_A>,
                _T<'_A, _G> => _T<'_A, _G>,
                _T => _T,
            }
        )
    }
}

assert_type_eq_all!(
    remove_lifetime_named!('a, std::sync::MutexGuard<'a, u8>),
    remove_lifetime_named!('a, std::sync::MutexGuard<'_, u8>), // '_ is kept, but equivalent to the removed lifetime
    std::sync::MutexGuard<u8>
);

assert_type_eq_all!(
    remove_lifetime_named!('a, std::sync::MutexGuard<'static, u8>),
    std::sync::MutexGuard<'static, u8>
);

#[macro_export]
macro_rules! lifetime_to_static {
    ($ty:ty) => {
        map_types!(
            match $ty {
                _T<'_, _G> => _T<'static, _G>,
                &'_ _T => &'static _T,
                _T => _T
            }
        )
    }
}

assert_type_eq_all!(
    lifetime_to_static!(std::sync::MutexGuard<'_, u8>),
    lifetime_to_static!(std::sync::MutexGuard<'a, u8>),
    std::sync::MutexGuard<'static, u8>,
);

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

assert_type_eq_all!(make_slice_static!(&[u8]), &'static [u8]);
assert_type_eq_all!(make_slice_static!(&'a [u8]), &'static [u8]);
assert_type_eq_all!(make_slice_static!(&'static [u8]), &'static [u8]);
