#![allow(unused)]

use type_mapper::{assert_type_matches, map_types};
use static_assertions::assert_type_eq_all;

assert_type_matches!(Vec<u8>, Vec<u8>);
assert_type_matches!(Vec<u8>, Vec);
assert_type_matches!(Vec<u8>, _T <u8>);
assert_type_matches!(Vec<u8>, Vec<_>);

assert_type_matches!(u8, _);
assert_type_matches!(u8, _T);
assert_type_matches!(u8, _T<>);

assert_type_matches!(std::sync::MutexGuard<u8>, _T);
assert_type_matches!(std::sync::MutexGuard, _T);
assert_type_matches!(std::sync::MutexGuard<u8>, _T<u8>);
assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'a, u8>);
assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, u8>);
assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, _>);
assert_type_matches!(std::sync::MutexGuard<'a, u8>, _T<'_, __>);

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

assert_type_eq_all!(make_slice_static!(&'a [u8]), &'static [u8]);
assert_type_eq_all!(make_slice_static!(&'static [u8]), &'static [u8]);
