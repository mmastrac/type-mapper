#![doc = include_str!("../README.md")]
use std::collections::HashMap;

use proc_macro::{TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{braced, parse::{Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, token, GenericArgument, PathArguments, Token};

struct TypeMatch {
    match_token: Token![match],
    match_type: syn::Type,
    brace_token: token::Brace,
    arms: Punctuated<TypeMatchArm, Token![,]>,
}

struct TypeMatchArm {
    pattern: syn::Type,
    fat_arrow: Token![=>],
    result: syn::Type,
}

impl Parse for TypeMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(TypeMatch {
            match_token: input.parse()?,
            match_type: input.parse()?,
            brace_token: braced!(content in input),
            arms: content.parse_terminated(TypeMatchArm::parse, Token![,])?,
        })
    }
}

impl Parse for TypeMatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(TypeMatchArm {
            pattern: input.parse()?,
            fat_arrow: input.parse()?,
            result: input.parse()?,
        })
    }
}

#[derive(Default, Clone)]
struct Wildcards {
    wildcards: HashMap<String, proc_macro2::TokenStream>,
}

impl Wildcards {
    fn track_wildcard(&mut self, arg: &str, input: &impl ToTokens) {
        self.wildcards.insert(arg.to_string(), input.to_token_stream());
    }
}

/// Attempts to match the input type with the pattern type. If there's a match, returns the templated generics:
/// 
///  - `_`, `_X` are wildcard types (optionally named)
///  - `__`, `__X` are multi-generic wildcards (optionally named)
///  - `'_`, `'_X` are lifetime wildcards (optionally named)
fn match_type(input: &syn::Type, pattern: &syn::Type) -> Result<Wildcards, &'static str> {
    match_type_recursive(input, pattern, &mut Wildcards::default())
}

/// Attempts to match the input type with the pattern type. If there's a match, returns the templated generics:
/// 
///  - `_`, `_X` are wildcard types (optionally named)
///  - `__`, `__X` are multi-generic wildcards (optionally named)
///  - `'_`, `'_X` are lifetime wildcards (optionally named)
fn match_type_recursive(mut input: &syn::Type, mut pattern: &syn::Type, wildcards: &mut Wildcards) -> Result<Wildcards, &'static str> {
    #![cfg_attr(test, deny(non_exhaustive_omitted_patterns))]

    while let Group(grouped_input) = input {
        input = &grouped_input.elem;
    }

    while let Group(grouped_pattern) = pattern {
        pattern = &grouped_pattern.elem;
    }

    use syn::Type::*;
    match (input, pattern) {
        (input, Infer(_)) => {
            wildcards.track_wildcard("_", input);
            Ok(wildcards.clone())
        }

        (Array(input), Array(pattern)) => {
            if input.len.to_token_stream().to_string() != pattern.len.to_token_stream().to_string() {
                Err("Array length mismatch")
            } else {
                match_type_recursive(&input.elem, &pattern.elem, wildcards)
            }
        }
        (BareFn(input), BareFn(pattern)) => {
            if input.inputs.len() != pattern.inputs.len() {
                Err("Function argument length mismatch")
            } else {
                for (input_arg, pattern_arg) in input.inputs.iter().zip(pattern.inputs.iter()) {
                    match_type_recursive(&input_arg.ty, &pattern_arg.ty, wildcards)?;
                }
                Ok(wildcards.clone())
            }
        }
        (Group(input), Group(pattern)) => {
            panic!("Groups should not exist at this point");
        }
        (ImplTrait(input), ImplTrait(pattern)) => {
            panic!("ImplTrait types not supported: {:?} {:?}", input.to_token_stream().to_string(), pattern.to_token_stream().to_string());
        }
        (Macro(input), Macro(pattern)) => {
            panic!("Macro types not supported: {:?} {:?}", input.to_token_stream().to_string(), pattern.to_token_stream().to_string());
        }
        (Never(_), Never(_)) => {
            Ok(wildcards.clone())
        }
        (Paren(input), Paren(pattern)) => {
            match_type_recursive(&input.elem, &pattern.elem, wildcards)
        }
        (Path(input_path), Path(pattern_path)) => {
            match_type_path(&input,&input_path.path, &pattern_path.path, wildcards)
        }
        (Ptr(input), Ptr(pattern)) => {
            match_type_recursive(&input.elem, &pattern.elem, wildcards)
        }
        (Reference(input), Reference(pattern)) => {
            match_type_recursive(&input.elem, &pattern.elem, wildcards)
        }
        (Slice(input), Slice(pattern)) => {
            match_type_recursive(&input.elem, &pattern.elem, wildcards)
        }
        (TraitObject(input), TraitObject(pattern)) => {
            panic!("TraitObject types not supported: {:?} {:?}", input.to_token_stream().to_string(), pattern.to_token_stream().to_string());
        }
        (Tuple(input), Tuple(pattern)) => {
            if input.elems.len() != pattern.elems.len() {
                Err("Tuple length mismatch")
            } else {
                for (input_arg, pattern_arg) in input.elems.iter().zip(pattern.elems.iter()) {
                    match_type_recursive(&input_arg, &pattern_arg, wildcards)?;
                }
                Ok(wildcards.clone())
            }
        }
        (Verbatim(input), Verbatim(pattern)) => {
            panic!("Verbatim types not supported: {:?} {:?}", input.to_token_stream().to_string(), pattern.to_token_stream().to_string());
        }
        _ => {
            Err("Type shapes are not the same")
        }
    }
}

/// The core of the type matching logic. Most of the interesting matches happen here.
/// 
/// Some examples:
/// 
///  - _T matches every type, with or without generics
///  - _T<> matches every type, without generics
///  - _T<_> matches every type, with a single generic
///  - _T<__> matches every type with any number of generics (0..infinity)
///  - _T<'_, _> matches any type with one lifetime parameter and one generic
///  - _T<___> matches any number of lifetime parameters and generics (0..infinity)
fn match_type_path(full_input: &syn::Type, input: &syn::Path, pattern: &syn::Path, wildcards: &mut Wildcards) -> Result<Wildcards, &'static str> {
    let mut is_wildcard = false;
    if pattern.segments.len() == 1 {
        if let Some(first) = pattern.segments.first() {
            if first.ident.to_string().starts_with('_') {
                is_wildcard = true;
            }
        }
    }

    if is_wildcard {
        // In a wildcard match, we only care about the final path segment args.
        let input_args = &input.segments.last().as_ref().unwrap().arguments;
        let pattern_args = &pattern.segments.last().as_ref().unwrap().arguments;

        let mut input = input.clone();

        if !matches!(pattern_args, PathArguments::None) {
            input.segments.last_mut().unwrap().arguments = PathArguments::None;
        }

        wildcards.track_wildcard(&pattern.segments.first().unwrap().ident.to_string(), &input);

        match_type_path_args(input_args, pattern_args, wildcards)
    } else {
        if input.segments.len() != pattern.segments.len() {
            Err("Path segment lengths are not the same")
        } else {
            for (input_segment, pattern_segment) in input.segments.iter().zip(pattern.segments.iter()) {
                match_type_path_args(&input_segment.arguments, &pattern_segment.arguments, wildcards)?;
            }
            Ok(wildcards.clone())
        }
    }
}

/// Matches the arguments of a path ie: `<...>`.
fn match_type_path_args(input: &PathArguments, pattern: &PathArguments, wildcards: &mut Wildcards) -> Result<Wildcards, &'static str> {
    match (&input, &pattern) {
        // Always match if the pattern is empty, but still capture wildcards.
        (_, PathArguments::None) => {},
        // If the pattern is empty <>, we match if the input is empty as well.
        (PathArguments::None, PathArguments::AngleBracketed(args)) if args.args.len() == 0 => {},

        (PathArguments::AngleBracketed(input_args), PathArguments::AngleBracketed(pattern_args)) => {
            if input_args.args.len() != pattern_args.args.len() {
                return Err("Path argument lengths are not the same");
            }
            for (input_arg, pattern_arg) in input_args.args.iter().zip(pattern_args.args.iter()) {
                match (input_arg, pattern_arg) {
                    (GenericArgument::Type(input_arg), GenericArgument::Type(pattern_arg)) => {
                        match_type_recursive(&input_arg, &pattern_arg, wildcards)?;
                    }
                    (GenericArgument::Lifetime(input_arg), GenericArgument::Lifetime(pattern_arg)) => {
                        if pattern_arg.ident.to_string() != "_" {
                            if input_arg.ident.to_string() != pattern_arg.ident.to_string() {
                                return Err("Lifetime mismatch");
                            }
                        } else {
                            wildcards.track_wildcard(&pattern_arg.ident.to_string(), &pattern_arg.ident);
                        }
                    }
                    _ => {
                        if input_arg.to_token_stream().to_string() != pattern_arg.to_token_stream().to_string() {
                            return Err("Path argument types are not the same");
                        }
                    }
                }
            }
        }
        (_, PathArguments::Parenthesized(..)) => panic!("Unsupported parenthesized arguments: {:?}", input.to_token_stream().to_string()),
        _ => {
            return Err("Path arguments are not the same");
        }
    }
    Ok(wildcards.clone())
}

fn render(result: syn::Type, matched: &Wildcards, input: &TypeMatch) -> proc_macro2::TokenStream {
    use syn::Type::*;
    match result {
        Path(path) => {
            let mut out = proc_macro2::TokenStream::new();
            let mut is_wildcard = false;
            if path.path.segments.len() == 1 {
                if let Some(first) = path.path.segments.first() {
                    if first.ident.to_string().starts_with('_') {
                        if let Some(wildcard) = matched.wildcards.get(&first.ident.to_string()) {
                            out.extend(wildcard.clone());
                            is_wildcard = true;
                        }
                    }
                }
            }

            if is_wildcard {
                out.extend(render_path_args(path.path.segments.last().unwrap().arguments.clone(), matched, input));
            } else {
                for segment in path.path.segments {
                    out.extend(segment.ident.to_token_stream());
                    out.extend(render_path_args(segment.arguments, matched, input));
                }
            }

            return out;
        }
        Reference(reference) => {
            let mut out = proc_macro2::TokenStream::new();
            out.extend(quote! { &});
            if let Some(mutability) = reference.mutability {
                out.extend(mutability.into_token_stream());
            }
            if let Some(lifetime) = reference.lifetime {
                if lifetime.ident.to_string().starts_with("_") && lifetime.ident != "_" {
                    out.extend(matched.wildcards.get(&lifetime.ident.to_string()).unwrap().clone());
                } else {
                    out.extend(lifetime.into_token_stream());
                }
            }
            out.extend(render(*reference.elem, matched, input));
            return out;
        }
        Macro(macro_type) => {
            if macro_type.mac.path.segments.len() == 1 {
                if let Some(first) = macro_type.mac.path.segments.first() {
                    if first.ident == "recurse" {
                        let recurse_type = syn::parse2::<syn::Type>(macro_type.mac.tokens.clone()).expect("Recursive call failed");
                        let rendered = render(recurse_type, &matched, input);
                        let recurse_type = syn::parse::<syn::Type>(rendered.into()).expect("Recursive call failed");

                        for arm in &input.arms {
                            if let Ok(matched) = match_type(&recurse_type, &arm.pattern) {
                                let result: proc_macro2::TokenStream = render(arm.result.clone(), &matched, input).into();
                                return proc_macro2::TokenStream::from(quote! { #result });
                            }
                        }
                        panic!("No recursive match found for {:?}", recurse_type.to_token_stream().to_string());
                    }
                }
            }
            panic!("Unhandled macro: {:?}", macro_type.mac.path.to_token_stream().to_string());
        }
        _ => {
            panic!("Unhandled type: {:?}", result.to_token_stream().to_string());
        },
    }
}

fn render_path_args(args: PathArguments, matched: &Wildcards, input: &TypeMatch) -> proc_macro2::TokenStream {
    let mut out = proc_macro2::TokenStream::new();
    match args {
        PathArguments::None => {
        }
        PathArguments::AngleBracketed(args) => {
            out.extend(quote! { <});
            for arg in args.args {
                match arg {
                    GenericArgument::Type(arg) => {
                        out.extend(render(arg, matched, input));
                    }
                    GenericArgument::Lifetime(arg) => {
                        if arg.ident.to_string().starts_with("_") && arg.ident != "_" {
                            out.extend(matched.wildcards.get(&arg.ident.to_string()).unwrap().clone());
                        } else {
                            out.extend(arg.into_token_stream());
                        }
                    }
                    arg => {
                        out.extend(arg.into_token_stream());
                    }
                }
                out.extend(quote! { ,});
            }
            out.extend(quote! { >});
        }
        _ => {
            panic!("Unhandled path arguments: {:?}", args.to_token_stream().to_string());
        }
    }
    out
}

/// Matches something like this:
/// 
/// ```
/// use type_mapper::map_types;
/// 
/// let x: map_types!(
///     match Vec<T> {
///         Vec<_> => u8,
///         _ => u16,
///     }
/// ) = 1_u8;
/// ```
#[proc_macro]
pub fn map_types(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as TypeMatch);

    let mut out = String::new();
    for arm in &input.arms {
        out.push_str(&arm.pattern.to_token_stream().to_string());

        match match_type(&input.match_type, &arm.pattern) {
            Ok(matched) => {
                let result: proc_macro2::TokenStream = render(arm.result.clone(), &matched, &input).into();
                return TokenStream::from(quote! { #result });
            }
            Err(e) => {
                out.push_str(&format!(": No match: {e}\n"));
            }
        }
    }

    panic!("No match found for {:?}\n{}", input.match_type.to_token_stream().to_string(), out);
}

struct AssertTypeMatches {
    input_type: syn::Type,
    comma: Token![,],
    expected_type: syn::Type,
    message: Option<syn::LitStr>,
}

impl Parse for AssertTypeMatches {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(AssertTypeMatches {
            input_type: input.parse()?,
            comma: input.parse()?,
            expected_type: input.parse()?,
            message: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn assert_type_matches(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as AssertTypeMatches);

    match match_type(&input.input_type, &input.expected_type) {
        Err(e) => {
            if let Some(message) = input.message {
                panic!("{}", message.value());
            } else {
                panic!("Type mismatch: {:?} !~ {:?}: {e}", input.input_type.to_token_stream().to_string(), input.expected_type.to_token_stream().to_string());
            }
        }
        Ok(_) => {
            TokenStream::new()
        }
    }
}

#[proc_macro]
pub fn recurse(_: TokenStream) -> TokenStream {
    panic!("Don't use this macro directly, use `map_types!` instead");
}
