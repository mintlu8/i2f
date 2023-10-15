//! Macros that converts number literals into int or float literals.
//!
//! In addition to literals, we also accept negative literals `-literal`.
//! # Examples
//! ```
//! use i2f::*;
//! assert_eq!(i2f!(90), 90.0);
//! assert_eq!(i2f!(-462), -462.0);
//! assert_eq!(i2f!(3.14), 3.14);
//! assert_eq!(f2i!(3), 3);
//! assert_eq!(f2i!(3.0), 3);
//! assert_eq!(f2i!(-12.0), -12);
//! assert_eq!(trunc!(3.14), 3);
//! assert_eq!(trunc!(-3.14), -3);
//! ```
use proc_macro::{TokenStream, TokenTree, Span, Literal};
use proc_macro_error::{proc_macro_error, abort};

fn parse_float(lit: Literal) -> TokenTree{
    let span = lit.span();
    if let Ok(x) = litrs::IntegerLit::try_from(lit.clone()) {
        let value: i128 = match x.value() {
            Some(x) => x,
            None => abort!(span, "Integer overflow: {}.", lit)
        };
        let mut lit = TokenTree::Literal(Literal::f64_unsuffixed(value as f64));
        lit.set_span(span);
        lit
    } else if let Ok(_) = litrs::FloatLit::try_from(lit.clone()) {
        TokenTree::Literal(lit)
    } else {
        abort!(span, "Expected an integer or a float, found {}.", lit)
    }
}

fn parse_int(lit: Literal, checked: bool) -> TokenTree{
    use std::str::FromStr;
    let span = lit.span();
    if let Ok(_) = litrs::IntegerLit::try_from(lit.clone()) {
        TokenTree::Literal(lit)
    } else if let Ok(x) = litrs::FloatLit::try_from(lit.clone()) {
        let value: f64 = match f64::from_str(x.number_part()) {
            Ok(x) => x,
            Err(e) => abort!(lit.span(), "Parsing Failed: {}.", e)
        };
        if checked && value.fract() != 0.0 {
            abort!(span, "Not an integer, maybe use 'trunc!'?.")
        }
        let mut lit = TokenTree::Literal(Literal::i128_unsuffixed(value.trunc() as i128));
        lit.set_span(span);
        lit
    } else {
        abort!(span, "Expected an integer or a float, found {}.", lit)
    }
}

/// Convert number literals to floating point literals.
/// 
/// ```
/// # use i2f::i2f;
/// assert_eq!(i2f!(42), 42.0);
/// assert_eq!(i2f!(42.0), 42.0);
/// assert_eq!(i2f!(-98), -98.0);
/// assert_eq!(i2f!(-98.0), -98.0);
/// ```
#[proc_macro]
#[proc_macro_error]
pub fn i2f(tokens: TokenStream) -> TokenStream {
    let mut iter = tokens.into_iter();
    let first = iter.next();
    match first {
        Some(TokenTree::Literal(lit)) => {
            if let Some(tt) = iter.next() {
                abort!(tt.span(), "Expected an integer or a float, found more.")
            }
            [parse_float(lit)].into_iter().collect()
        },
        Some(TokenTree::Punct(p)) if p.as_char() == '-' => {
            let neg = TokenTree::Punct(p);
            match iter.next() {
                Some(TokenTree::Literal(lit)) => {
                    if let Some(tt) = iter.next() {
                        abort!(tt.span(), "Expected an integer or a float, found more.")
                    }
                    [neg, parse_float(lit)].into_iter().collect()
                },
                Some(tt) => abort!(tt.span(), "Expected an integer or a float, found {}.", tt),
                None => abort!(Span::call_site(), "Expected an integer or a float.")
            }
        }
        Some(tt) => abort!(tt.span(), "Expected '-', integer or float, found {}.", tt),
        None => abort!(Span::call_site(), "Expected '-', integer or float.")
    }
}

/// Convert number literals to integer literals.
/// 
/// ```
/// # use i2f::*;
/// assert_eq!(f2i!(42), 42);
/// assert_eq!(f2i!(42.0), 42);
/// assert_eq!(f2i!(-98), -98);
/// assert_eq!(f2i!(-98.0), -98);
/// ```
/// 
/// This fails if the number is not actually an integer.
/// 
/// ```compile_fail
/// # use i2f::*;
/// f2i!(42.65)
/// ```
/// 
/// ```compile_fail
/// # use i2f::*;
/// f2i!(-23.12)
/// ```
#[proc_macro]
#[proc_macro_error]
pub fn f2i(tokens: TokenStream) -> TokenStream {
    let mut iter = tokens.into_iter();
    let first = iter.next();
    match first {
        Some(TokenTree::Literal(lit)) => {
            if let Some(tt) = iter.next() {
                abort!(tt.span(), "Expected an integer or a float, found more.")
            }
            [parse_int(lit, true)].into_iter().collect()
        },
        Some(TokenTree::Punct(p)) if p.as_char() == '-' => {
            let neg = TokenTree::Punct(p);
            match iter.next() {
                Some(TokenTree::Literal(lit)) => {
                    if let Some(tt) = iter.next() {
                        abort!(tt.span(), "Expected an integer or a float, found more.")
                    }
                    [neg, parse_int(lit, true)].into_iter().collect()
                },
                Some(tt) => abort!(tt.span(), "Expected an integer or a float, found {}.", tt),
                None => abort!(Span::call_site(), "Expected an integer or a float.")
            }
        }
        Some(tt) => abort!(tt.span(), "Expected an integer or a float, found {}.", tt),
        None => abort!(Span::call_site(), "Expected an integer or a float.")
    }
}

/// Convert number literals to integer literals.
/// 
/// ```
/// # use i2f::*;
/// assert_eq!(trunc!(42), 42);
/// assert_eq!(trunc!(42.0), 42);
/// assert_eq!(trunc!(-98), -98);
/// assert_eq!(trunc!(-98.0), -98);
/// ```
/// 
/// This converts by truncating.
/// ```
/// # use i2f::*;
/// assert_eq!(trunc!(42.65), 42);
/// assert_eq!(trunc!(-23.12), -23);
/// ```
/// 
#[proc_macro]
#[proc_macro_error]
pub fn trunc(tokens: TokenStream) -> TokenStream {
    let mut iter = tokens.into_iter();
    let first = iter.next();
    match first {
        Some(TokenTree::Literal(lit)) => {
            if let Some(tt) = iter.next() {
                abort!(tt.span(), "Expected an integer or a float, found more.")
            }
            [parse_int(lit, false)].into_iter().collect()
        },
        Some(TokenTree::Punct(p)) if p.as_char() == '-' => {
            let neg = TokenTree::Punct(p);
            match iter.next() {
                Some(TokenTree::Literal(lit)) => {
                    if let Some(tt) = iter.next() {
                        abort!(tt.span(), "Expected an integer or a float, found more.")
                    }
                    [neg, parse_int(lit, false)].into_iter().collect()
                },
                Some(tt) => abort!(tt.span(), "Expected an integer or a float, found {}.", tt),
                None => abort!(Span::call_site(), "Expected an integer or a float.")
            }
        }
        Some(tt) => abort!(tt.span(), "Expected an integer or a float, found {}.", tt),
        None => abort!(Span::call_site(), "Expected an integer or a float.")
    }
}