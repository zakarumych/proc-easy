//!
//! Macros to make writing proc-macro crates easy.
//!
//! This crate provides mainly macros and supporting types and traits
//! to reduce amount of boilerplate required for working with [`syn`].
//!
//! Currently most of the macros are targeted to construct types
//! that then can be parses to configure proc-macro and proc-derive-macro implementation.
//!
//! [`easy_token!`] - defines new custom token from ident. To be used in other structs.
//!
//! [`easy_parse!`] - defines struct or enum that can be parsed and peeked from stream.
//!
//! [`easy_argument!`] - defines struct with a token as a name and the rest to be parsed as-is.
//!
//! [`easy_argument_group!`] - defines a group of arguments as enum of arguments.
//!
//! [`easy_argument_tuple!`] - specialized version of [`easy_argument!`] that parses fields starting from 2nd as [`EasyArgumentField`]s inside parenthesis and in any order.
//!
//! [`easy_argument_value!`] - specialized version of [`easy_argument!`] for 2 field structs. It defines 2nd field as a value that can be parsed after `=` token or inside parenthesis.
//!
//! [`easy_separated!`] - defines struct that parses fields as [`EasyArgumentField`]s in any order. Does not accept trailing punctuation.
//!
//! [`easy_terminated!`] - defines struct that parses fields as [`EasyArgumentField`]s in any order. Accepts trailing punctuation. Parses whole stream.
//!
//! [`easy_attributes!`] - defines struct that parses fields as [`EasyArgumentField`]s from a slice of [`Attribute`]s with specified namespace.
//!
//! [`EasyArgumentField`] is implemented for types defined with [`easy_token!`], [`easy_argument!`], [`easy_argument_tuple!`], [`easy_argument_value!`] and [`easy_argument_group!`] possibly wrapped in [`Option`] or [`Vec`].
//!

#![deny(missing_docs)]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]

use std::{marker::PhantomData, ops::Deref};

use proc_macro2::Span;

use syn::{
    parse::{Lookahead1, Parse, ParseStream},
    spanned::Spanned,
    token::{Paren, Token},
    Attribute,
};

#[doc(hidden)]
pub mod private {
    pub use std::{
        concat, default::Default, format, mem::discriminant, option::Option, result::Result,
        string::String, stringify, vec::Vec,
    };

    pub use bool;
    pub use proc_macro2::Span;
    pub use syn::{
        custom_keyword, parenthesized,
        parse::{Lookahead1, Parse, ParseStream},
        punctuated::Punctuated,
        spanned::Spanned,
        token::{Comma, Eq, Paren},
        Attribute, Error, Ident,
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! easy_replace_tokens {
    ($with:tt, $($t:tt)*) => {
        $with
    };
}

/// Peekable and parsable single token.
///
/// [`easy_token!`] macro produces types that implement this trait.
pub trait EasyToken: EasyPeek + Parse + Spanned {
    /// Display the token for the user.
    fn display() -> &'static str;
}

/// Defines a type with specified name that implement [`EasyToken`] and can be parsed from that name ident.
#[macro_export]
macro_rules! easy_token {
    ($name:ident) => {
        $crate::private::custom_keyword!($name);

        impl $crate::EasyToken for $name {
            #[inline]
            fn display() -> &'static str {
                $crate::private::concat!("`", $crate::private::stringify!($name), "`")
            }
        }
    };
}

/// Provides interface for peeking first token before parsing.
///
/// May be implemented for complex structures by peeking first field.
pub trait EasyPeek: Parse {
    /// Peek head token before parsing.
    fn peek(lookahead1: &Lookahead1) -> bool;

    /// Peek head token before parsing.
    fn peek_stream(stream: ParseStream) -> bool;
}

impl<T> EasyPeek for T
where
    T: Token + Parse,
{
    #[inline]
    fn peek(lookahead1: &Lookahead1) -> bool {
        lookahead1.peek(|v| -> T { match v {} })
    }

    #[inline]
    fn peek_stream(stream: ParseStream) -> bool {
        stream.peek(|v| -> T { match v {} })
    }
}

macro_rules! easy_syn_token {
    ($token:tt) => {
        impl EasyToken for syn::Token![$token] {
            #[inline]
            fn display() -> &'static str {
                $crate::private::concat!("`", $crate::private::stringify!($token), "`")
            }
        }
    };
}

easy_syn_token![abstract];
easy_syn_token![as];
easy_syn_token![async];
easy_syn_token![auto];
easy_syn_token![await];
easy_syn_token![become];
easy_syn_token![box];
easy_syn_token![break];
easy_syn_token![const];
easy_syn_token![continue];
easy_syn_token![crate];
easy_syn_token![default];
easy_syn_token![do];
easy_syn_token![dyn];
easy_syn_token![else];
easy_syn_token![enum];
easy_syn_token![extern];
easy_syn_token![final];
easy_syn_token![fn];
easy_syn_token![for];
easy_syn_token![if];
easy_syn_token![impl];
easy_syn_token![in];
easy_syn_token![let];
easy_syn_token![loop];
easy_syn_token![macro];
easy_syn_token![match];
easy_syn_token![mod];
easy_syn_token![move];
easy_syn_token![mut];
easy_syn_token![override];
easy_syn_token![priv];
easy_syn_token![pub];
easy_syn_token![ref];
easy_syn_token![return];
easy_syn_token![Self];
easy_syn_token![self];
easy_syn_token![static];
easy_syn_token![struct];
easy_syn_token![super];
easy_syn_token![trait];
easy_syn_token![try];
easy_syn_token![type];
easy_syn_token![typeof];
easy_syn_token![union];
easy_syn_token![unsafe];
easy_syn_token![unsized];
easy_syn_token![use];
easy_syn_token![virtual];
easy_syn_token![where];
easy_syn_token![while];
easy_syn_token![yield];
easy_syn_token![+];
easy_syn_token![+=];
easy_syn_token![&];
easy_syn_token![&&];
easy_syn_token![&=];
easy_syn_token![@];
easy_syn_token![!];
easy_syn_token![^];
easy_syn_token![^=];
easy_syn_token![:];
easy_syn_token![::];
easy_syn_token![,];
easy_syn_token![/];
easy_syn_token![/=];
easy_syn_token![$];
easy_syn_token![.];
easy_syn_token![..];
easy_syn_token![...];
easy_syn_token![..=];
easy_syn_token![=];
easy_syn_token![==];
easy_syn_token![>=];
easy_syn_token![>];
easy_syn_token![<=];
easy_syn_token![<];
easy_syn_token![*=];
easy_syn_token![!=];
easy_syn_token![|];
easy_syn_token![|=];
easy_syn_token![||];
easy_syn_token![#];
easy_syn_token![?];
easy_syn_token![->];
easy_syn_token![<-];
easy_syn_token![%];
easy_syn_token![%=];
easy_syn_token![=>];
easy_syn_token![;];
easy_syn_token![<<];
easy_syn_token![<<=];
easy_syn_token![>>];
easy_syn_token![>>=];
easy_syn_token![*];
easy_syn_token![-];
easy_syn_token![-=];
easy_syn_token![~];
easy_syn_token![_];

/// HACK around trivial bounds error.
#[doc(hidden)]
#[allow(missing_debug_implementations)]
pub struct EasyPeekHack<'a, T>(&'a PhantomData<T>);

impl<'a, T> Parse for EasyPeekHack<'a, T> {
    fn parse(_: ParseStream) -> syn::Result<Self> {
        panic!("This function must not be called");
    }
}

impl<'a, T> EasyPeek for EasyPeekHack<'a, T>
where
    T: EasyPeek,
{
    fn peek(lookahead1: &Lookahead1) -> bool {
        T::peek(lookahead1)
    }

    fn peek_stream(stream: ParseStream) -> bool {
        T::peek_stream(stream)
    }
}

/// Parses inner type parenthesized.
/// Implements [`EasyPeek`] and peeks opening parenthesis.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EasyParenthesized<T>(pub T);

impl<T> Deref for EasyParenthesized<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Spanned for EasyParenthesized<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl<T> EasyPeek for EasyParenthesized<T>
where
    T: Parse,
{
    #[inline]
    fn peek(lookahead1: &Lookahead1) -> bool {
        lookahead1.peek(syn::token::Paren)
    }

    #[inline]
    fn peek_stream(stream: ParseStream) -> bool {
        stream.peek(syn::token::Paren)
    }
}

impl<T> Parse for EasyParenthesized<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);
        let inner = T::parse(&content)?;
        if content.is_empty() {
            Ok(EasyParenthesized(inner))
        } else {
            Err(content.error("Expected closing parentheses"))
        }
    }
}

/// Parses inner type braced.
/// Implements [`EasyPeek`] and peeks opening brace.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EasyBraced<T>(pub T);

impl<T> Deref for EasyBraced<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Spanned for EasyBraced<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl<T> EasyPeek for EasyBraced<T>
where
    T: Parse,
{
    #[inline]
    fn peek(lookahead1: &Lookahead1) -> bool {
        lookahead1.peek(syn::token::Brace)
    }

    #[inline]
    fn peek_stream(stream: ParseStream) -> bool {
        stream.peek(syn::token::Brace)
    }
}

impl<T> Parse for EasyBraced<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::braced!(content in input);
        let inner = T::parse(&content)?;
        if content.is_empty() {
            Ok(EasyBraced(inner))
        } else {
            Err(content.error("Expected closing parentheses"))
        }
    }
}

/// Parses inner type bracketed.
/// Implements [`EasyPeek`] and peeks opening bracket.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EasyBracketed<T>(pub T);

impl<T> Deref for EasyBracketed<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Spanned for EasyBracketed<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl<T> EasyPeek for EasyBracketed<T>
where
    T: Parse,
{
    #[inline]
    fn peek(lookahead1: &Lookahead1) -> bool {
        lookahead1.peek(syn::token::Bracket)
    }

    #[inline]
    fn peek_stream(stream: ParseStream) -> bool {
        stream.peek(syn::token::Bracket)
    }
}

impl<T> Parse for EasyBracketed<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::bracketed!(content in input);
        let inner = T::parse(&content)?;
        if content.is_empty() {
            Ok(EasyBracketed(inner))
        } else {
            Err(content.error("Expected closing parentheses"))
        }
    }
}

/// Similar to [`syn::punctuated::Punctuated`] but implements [`Parse`] and optionally [`EasyPeek`].
///
/// Parses one or more occurrences of T separated by punctuation of type P, not accepting trailing punctuation.
/// Parsing continues as long as punctuation P is present at the head of the stream. This method returns upon parsing a T and observing that it is not followed by a P, even if there are remaining tokens in the stream.
#[derive(Clone, Debug)]
pub struct EasySeparated<T, P = syn::Token![,]> {
    items: Vec<T>,
    punctuation: PhantomData<P>,
}

impl<T, P> Deref for EasySeparated<T, P> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.items
    }
}

impl<T, P> Parse for EasySeparated<T, P>
where
    T: Parse,
    P: EasyPeek,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();

        let item = input.parse::<T>()?;
        items.push(item);

        while P::peek_stream(input) {
            let _ = input.parse::<P>()?;
            let item = input.parse::<T>()?;
            items.push(item);
        }

        Ok(EasySeparated {
            items,
            punctuation: PhantomData,
        })
    }
}

impl<T, P> EasyPeek for EasySeparated<T, P>
where
    T: EasyPeek,
    P: EasyPeek,
{
    fn peek(lookahead1: &Lookahead1) -> bool {
        T::peek(lookahead1)
    }

    fn peek_stream(stream: ParseStream) -> bool {
        T::peek_stream(stream)
    }
}

/// Similar to [`syn::punctuated::Punctuated`] but implements [`Parse`].
///
/// Parses zero or more occurrences of T separated by punctuation of type P, with optional trailing punctuation.
/// Parsing continues until the end of this parse stream. The entire content of this parse stream must consist of T and P.
#[derive(Clone, Debug)]
pub struct EasyTerminated<T, P = syn::Token![,]> {
    items: Vec<T>,
    punctuation: PhantomData<P>,
}

impl<T, P> Deref for EasyTerminated<T, P> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.items
    }
}

impl<T, P> Parse for EasyTerminated<T, P>
where
    T: Parse,
    P: EasyPeek,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();

        if input.is_empty() {
            return Ok(EasyTerminated {
                items,
                punctuation: PhantomData,
            });
        }

        let item = input.parse::<T>()?;
        items.push(item);

        loop {
            if input.is_empty() {
                break;
            }
            let _ = input.parse::<P>()?;
            if input.is_empty() {
                break;
            }
            let item = input.parse::<T>()?;
            items.push(item);
        }

        Ok(EasyTerminated {
            items,
            punctuation: PhantomData,
        })
    }
}

/// Similar to [`Option`] but implements [`Parse`] when `T` implements [`EasyPeek`]
///
/// If stream doesn't start as `T` then no tokens are consumed and [`EasyMaybe::Nothing`] is returned.
/// Otherwise `T` is parsed and returned wrapped in [`EasyMaybe::Just`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EasyMaybe<T> {
    /// Nothing at all
    Nothing,

    /// Just a value
    Just(T),
}

impl<T> Default for EasyMaybe<T> {
    fn default() -> Self {
        EasyMaybe::Nothing
    }
}

impl<T> Parse for EasyMaybe<T>
where
    T: EasyPeek,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if T::peek_stream(input) {
            T::parse(input).map(EasyMaybe::Just)
        } else {
            Ok(EasyMaybe::Nothing)
        }
    }
}

/// Either a value preceded with `=` or parenthesized value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EasySubArgument<V, T = V> {
    /// Simple value
    Value(V),

    /// Tuple sub-fields
    Tuple(T),
}

impl<V, T> Parse for EasySubArgument<V, T>
where
    V: Parse,
    T: Parse,
{
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        let lookahead1 = stream.lookahead1();
        if lookahead1.peek(syn::Token![=]) {
            stream.parse::<syn::Token![=]>()?;
            Ok(EasySubArgument::Value(stream.parse::<V>()?))
        } else if lookahead1.peek(Paren) {
            let content;
            syn::parenthesized!(content in stream);
            let arg = content.parse::<T>()?;

            if content.is_empty() {
                Ok(EasySubArgument::Tuple(arg))
            } else {
                Err(content.error("Expected closing parentheses"))
            }
        } else {
            Err(lookahead1.error())
        }
    }
}

impl<V, T> EasyPeek for EasySubArgument<V, T>
where
    V: Parse,
    T: Parse,
{
    fn peek(lookahead1: &Lookahead1) -> bool {
        lookahead1.peek(syn::Token![=]) || lookahead1.peek(Paren)
    }

    fn peek_stream(stream: ParseStream) -> bool {
        stream.peek(syn::Token![=]) || stream.peek(Paren)
    }
}

/// Defines structure or enum and implements [`Parse`] for it.
///
/// Implements [`EasyPeek`] for structs if first field implements [`EasyPeek`].
///
/// For enums, if first variant is prefixed by `!` it becomes a default variant that is parsed if no other variants peeking succeeds.
/// Variants prefixed with `?` are skipped during parsing and peeking.
///
/// For enums [`EasyPeek`] is implemented if enum does not have a default variant.
/// First field in  all non-default non-skipped variants must implement [`EasyPeek`].
///
/// Therefore unit and empty tuple and struct-like variants may be present but must be marked as default or skipped.
#[macro_export]
macro_rules! easy_parse {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident;
    ) => {
        $(#[$meta])*
        $vis struct $name;

        impl $crate::private::Parse for $name {
            fn parse(_input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                $crate::private::Result::Ok($name)
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident ();
    ) => {
        $(#[$meta])*
        $vis struct $name ();

        impl $crate::private::Parse for $name {
            fn parse(_input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                $crate::private::Result::Ok($name ())
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {}
    ) => {
        $(#[$meta])*
        $vis struct $name {}

        impl $crate::private::Parse for $name {
            fn parse(_input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                $crate::private::Result::Ok($name {})
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(#[$hmeta:meta])* $hvis:vis $hname:ident : $htype:ty
            $(, $(#[$fmeta:meta])* $fvis:vis $fname:ident : $ftype:ty)*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $(#[$hmeta])* $hvis $hname: $htype,
            $($(#[$fmeta])* $fvis $fname: $ftype,)*
        }

        impl $crate::private::Parse for $name {
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                #![allow(unused_variables)]

                let $hname = <$htype as $crate::private::Parse>::parse(input)?;

                $(
                    let $fname = <$ftype as $crate::private::Parse>::parse(input)?;
                )*

                $crate::private::Result::Ok($name {
                    $hname,
                    $( $fname, )*
                })
            }
        }

        impl $crate::EasyPeek for $name
        where
            for<'a> $crate::EasyPeekHack<'a, $htype>: $crate::EasyPeek,
        {
            #[inline]
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$crate::EasyPeekHack<'static, $htype> as $crate::EasyPeek>::peek(lookahead1)
            }

            #[inline]
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$crate::EasyPeekHack<'static, $htype> as $crate::EasyPeek>::peek_stream(stream)
            }
        }
    };

    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            $(#[$hmeta:meta])* $hvis:vis $htype:ty
            $(, $(#[$fmeta:meta])* $fvis:vis $ftype:ty)*
            $(,)?
        );
    ) => {
        $(#[$meta])*
        $vis struct $name (
            $(#[$hmeta])* $hvis $htype,
            $($(#[$fmeta])* $fvis $ftype,)*
        );

        impl $crate::private::Parse for $name {
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                #![allow(unused_variables)]

                $crate::private::Result::Ok($name (
                    <$htype as $crate::private::Parse>::parse(input)?,
                    $(
                        <$ftype as $crate::private::Parse>::parse(input)?,
                    )*
                ))
            }
        }

        impl $crate::EasyPeek for $name
        where
            for<'a> $crate::EasyPeekHack<'a, $htype>: $crate::EasyPeek,
        {
            #[inline]
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$crate::EasyPeekHack<'static, $htype> as $crate::EasyPeek>::peek(lookahead1)
            }

            #[inline]
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$crate::EasyPeekHack<'static, $htype> as $crate::EasyPeek>::peek_stream(stream)
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $(#[$vdmeta:meta])* ! $vdname:ident $( ( $($vdt:tt)* ) )? $( { $($vdr:tt)* } )?,
            $($(#[$vmeta:meta])* $( ? $vsname:ident )? $( $vname:ident )? $( ( $($vt:tt)* ) )? $( { $($vr:tt)* } )?, )*
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $(#[$vdmeta])* $vdname $( ( $($vdt)* ) )? $( { $($vdr)* } )?,
            $( $(#[$vmeta])* $( $vsname )? $( $vname )? $( ( $($vt)* ) )? $( { $($vr)* } )?, )*
        }

        impl $crate::private::Parse for $name {
            #[inline]
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                #![allow(unused_variables)]
                let lookahead1 = input.lookahead1();
                $(
                    $crate::easy_parse_enum_variant!{parse lookahead1 input $name $( $vname )? $( ( $($vt)* ) )? $( { $($vr)* } )?}
                )*

                $crate::easy_parse_enum_variant!{parse_default input $name $vdname $( ( $($vdt)* ) )? $( { $($vdr)* } )?}
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $($(#[$vmeta:meta])* $( ? $vsname:ident )? $( $vname:ident )? $( ( $($vt:tt)* ) )? $( { $($vr:tt)* } )?, )*
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $( $(#[$vmeta])* $( $vsname )? $( $vname )? $( ( $($vt)* ) )? $( { $($vr)* } )? ),*
        }

        impl $crate::private::Parse for $name {
            #[inline]
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                #![allow(unused_variables)]
                let lookahead1 = input.lookahead1();

                $(
                    $crate::easy_parse_enum_variant!{parse lookahead1 input $name $( $vname )? $( ( $($vt)* ) )? $( { $($vr)* } )?}
                )*

                $crate::private::Result::Err(lookahead1.error())
            }
        }

        impl $crate::EasyPeek for $name {
            #[inline]
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                $(
                    $crate::easy_parse_enum_variant!{peek lookahead1 $($vname)? $( ( $($vt)* ) )? $( { $($vr)* } )?}
                )*
                false
            }

            #[inline]
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                $(
                    $crate::easy_parse_enum_variant!{peek_stream stream $($vname)? $( ( $($vt)* ) )? $( { $($vr)* } )?}
                )*
                false
            }
        }
    };
}

/// Non-public macro used by [`easy_parse!`].
#[doc(hidden)]
#[macro_export]
macro_rules! easy_parse_enum_variant {
    (parse $lookahead1:ident $stream:ident $name:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {};
    (parse $lookahead1:ident $stream:ident $name:ident $vname:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {
        if <$vptype as $crate::EasyPeek>::peek(&$lookahead1) {
            return $crate::private::Result::Ok($name::$vname(
                <$vptype as $crate::private::Parse>::parse($stream)?,
                $( <$vftype as $crate::private::Parse>::parse($stream)?, )*
            ));
        }
    };
    (peek $lookahead1:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {};
    (peek $lookahead1:ident $vname:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {
        if <$vptype as $crate::EasyPeek>::peek($lookahead1) {
            return true;
        }
    };

    (peek_stream $stream:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {};
    (peek_stream $stream:ident $vname:ident ( $(#[$vpmeta:meta])* $vptype:ty $(, $(#[$vfmeta:meta])* $vftype:ty )* $(,)? )) => {
        if <$vptype as $crate::EasyPeek>::peek_stream($stream) {
            return true;
        }
    };

    (parse $lookahead1:ident $stream:ident $name:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {};
    (parse $lookahead1:ident $stream:ident $name:ident $vname:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {
        if <$vptype as $crate::EasyPeek>::peek(&$lookahead1) {
            let $vpname =  <$vptype as $crate::private::Parse>::parse($stream)?;
            $( let $vfname = <$vftype as $crate::private::Parse>::parse($stream)?; )*

            return $crate::private::Result::Ok($name :: $vname {
                $vpname,
                $($vfname,)*
            })
        }
    };
    (peek $lookahead1:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {};
    (peek $lookahead1:ident $vname:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {
        if <$vptype as $crate::EasyPeek>::peek($lookahead1) {
            return true;
        }
    };
    (peek_stream $stream:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {};
    (peek_stream $stream:ident $vname:ident { $(#[$vpmeta:meta])* $vpname:ident : $vptype:ty $(, $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty )* $(,)? }) => {
        if <$vptype as $crate::EasyPeek>::peek_stream($stream) {
            return true;
        }
    };

    (parse_default $stream:ident $name:ident $vname:ident) => {
        $crate::private::Result::Ok( $name::$vname )
    };
    (parse_default $stream:ident $name:ident $vname:ident ( $( $(#[$vfmeta:meta])* $vftype:ty ),* $(,)? )) => {
        $crate::private::Result::Ok( $name::$vname ( $( <$vftype as $crate::private::Parse>::parse($stream)?, )* ))
    };
    (parse_default $stream:ident $name:ident $vname:ident { $( $(#[$vfmeta:meta])* $vfname:ident : $vftype:ty),* $(,)? }) => {
        $crate::private::Result::Ok( $name::$vname { $( $vfname: <$vftype as $crate::private::Parse>::parse($stream)?, )* })
    };
}

/// Trait for parsable arguments.
/// Arguments have a token as a name that is used for peeking.
/// Name of the argument can be displayed for user in some error cases.
pub trait EasyArgument: EasyPeek {
    /// Returns attribute name for display purposes.
    fn name_display() -> &'static str;

    /// Returns attribute name for display purposes.
    fn name_span(&self) -> Span;
}

impl<T> EasyArgument for T
where
    T: EasyToken,
{
    fn name_display() -> &'static str {
        <T as EasyToken>::display()
    }

    fn name_span(&self) -> Span {
        <T as Spanned>::span(self)
    }
}

/// Defines argument structure.
///
/// First field is the argument name. It must implement [`EasyToken`].
/// It must be used to for [`EasyPeek`] implementation.
///
/// The rest of the fields are parsed in order.
///
/// In case of errors, such as missing argument or unexpected duplicates, argument's name and span will be used.
#[macro_export]
macro_rules! easy_argument {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(#[$nmeta:meta])* $nvis:vis $nname:ident: $ntype:ty
            $(, $(#[$fmeta:meta])* $fvis:vis $fname:ident: $ftype:ty)*
            $(,)?
        }
    ) => {
        $crate::easy_parse!{
            $(#[$meta])*
            $vis struct $name {
                $(#[$nmeta])* $nvis $nname: $ntype,
                $($(#[$fmeta])* $fvis $fname: $ftype,)*
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.$nname)
            }
        }
    };
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            $(#[$nmeta:meta])* $nvis:vis $ntype:ty
            $(, $(#[$fmeta:meta])* $fvis:vis $ftype:ty)*
            $(,)?
        );
    ) => {
        $crate::easy_parse!{
            $(#[$meta])*
            $vis struct $name (
                $(#[$nmeta])* $nvis $ntype,
                $( $(#[$fmeta])* $fvis $ftype,)*
            );
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.0)
            }
        }
    };
}

/// Defines argument structure.
///
/// First field is the argument name. It must implement [`EasyToken`].
/// It must be used to for [`EasyPeek`] implementation.
///
/// The rest of the fields must be [`EasyArgumentField`] and are expected to be in parenthesized and parsed in any order.
///
/// If name is not followed by parentheses, all fields are missing (which may be not an error for [`Option`] and [`Vec`] fields).
///
/// In case of errors, such as missing argument or unexpected duplicates, argument's name and span will be used.
#[macro_export]
macro_rules! easy_argument_tuple {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(#[$nmeta:meta])* $nvis:vis $nname:ident: $ntype:ty
            $(, $(#[$fmeta:meta])* $fvis:vis $fname:ident: $ftype:ty)*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $(#[$nmeta])* $nvis $nname: $ntype,
            $($(#[$fmeta])* $fvis $fname: $ftype,)*
        }

        impl $crate::EasyPeek for $name {
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek(lookahead1)
            }
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek_stream(stream)
            }
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let $nname = stream.parse::<$ntype>()?;

                $(let mut $fname = $crate::private::Option::None;)*

                if stream.peek($crate::private::Paren) {
                    let content;
                    $crate::private::parenthesized!(content in stream);

                    loop {
                        if content.is_empty() {
                            break;
                        }
                        let lookahead1 = content.lookahead1();
                        $(
                            match &mut $fname {
                                $crate::private::Option::None => {
                                    if let $crate::private::Option::Some(value) = <$ftype as $crate::EasyArgumentField>::try_parse(&lookahead1, &content)? {
                                        $fname = $crate::private::Option::Some(value);
                                        if content.is_empty() {
                                            break;
                                        }
                                        content.parse::<$crate::private::Comma>()?;
                                        continue
                                    }
                                }
                                $crate::private::Option::Some($fname) => {
                                    if <$ftype as $crate::EasyArgumentField>::try_extend($fname, &lookahead1, &content)? {
                                        if content.is_empty() {
                                            break;
                                        }
                                        content.parse::<$crate::private::Comma>()?;
                                        continue
                                    }
                                }
                            }
                        )*
                        return $crate::private::Result::Err(lookahead1.error());
                    }
                }

                $crate::private::Result::Ok($name {
                    $nname,
                    $(
                        $fname: match $fname {
                            $crate::private::Option::None => <$ftype as $crate::EasyArgumentField>::missing().map_err(|msg| stream.error(msg))?,
                            $crate::private::Option::Some($fname) => $fname,
                        },
                    )*
                })
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.$nname)
            }
        }
    };
}

/// Defines argument structure with exactly two fields.
///
/// First field is the argument name. It must implement [`EasyToken`].
/// It must be used to for [`EasyPeek`] implementation.
///
/// Another field is a value. It is parsed preceded by `=` token or inside parentheses.
///
/// If name is not followed by `=` or parentheses, value field is missing (which may be not an error for [`Option`] and [`Vec`] field).
///
/// In case of errors, such as missing argument or unexpected duplicates, argument's name and span will be used.
#[macro_export]
macro_rules! easy_argument_value {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(#[$nmeta:meta])* $nvis:vis $nname:ident: $ntype:ty,
            $(#[$vmeta:meta])* $vvis:vis $vname:ident: $vtype:ty
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $(#[$nmeta])* $nvis $nname: $ntype,
            $(#[$vmeta])* $vvis $vname: $vtype,
        }

        impl $crate::EasyPeek for $name {
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek(lookahead1)
            }
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek_stream(stream)
            }
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let $nname = stream.parse::<$ntype>()?;

                let lookahead1 = stream.lookahead1();
                if lookahead1.peek($crate::private::Eq) {
                    let _ = stream.parse::<$crate::private::Eq>()?;
                    let $vname = <$vtype as $crate::private::Parse>::parse(stream)?;
                    $crate::private::Result::Ok($name {
                        $nname,
                        $vname,
                    })
                } else if lookahead1.peek($crate::private::Paren) {
                    let content;
                    $crate::private::parenthesized!(content in stream);
                    let $vname = <$vtype as $crate::private::Parse>::parse(&content)?;
                    if content.is_empty() {
                        $crate::private::Result::Ok($name {
                            $nname,
                            $vname,
                        })
                    } else {
                        $crate::private::Result::Err(content.error("Expected closing parentheses"))
                    }
                } else {
                    $crate::private::Result::Err(lookahead1.error())
                }
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.$nname)
            }
        }
    };

    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(#[$nmeta:meta])* $nvis:vis $nname:ident: $ntype:ty,
            $(#[$vmeta:meta])* ? $vvis:vis $vname:ident: $vtype:ty
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $(#[$nmeta])* $nvis $nname: $ntype,
            $(#[$vmeta])* $vvis $vname: $vtype,
        }

        impl $crate::EasyPeek for $name {
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek(lookahead1)
            }
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek_stream(stream)
            }
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let $nname = stream.parse::<$ntype>()?;

                if stream.is_empty() {
                    $crate::private::Result::Ok($name {
                        $nname,
                        $vname: <$vtype as $crate::private::Default>::default(),
                    })
                } else {
                    let _ = stream.parse::<$crate::private::Eq>()?;
                    let $vname = <$vtype as $crate::private::Parse>::parse(stream)?;

                    $crate::private::Result::Ok($name {
                        $nname,
                        $vname,
                    })
                }
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.$nname)
            }
        }
    };


    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            $(#[$nmeta:meta])* $nvis:vis $ntype:ty,
            $(#[$vmeta:meta])* $vvis:vis $vtype:ty
            $(,)?
        );
    ) => {
        $(#[$meta])*
        $vis struct $name (
            $(#[$nmeta])* $nvis $ntype,
            $(#[$vmeta])* $vvis $vtype,
        );

        impl $crate::EasyPeek for $name {
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek(lookahead1)
            }
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek_stream(stream)
            }
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let name = stream.parse::<$ntype>()?;

                let _ = stream.parse::<$crate::private::Eq>()?;
                let value = <$vtype as $crate::private::Parse>::parse(stream)?;

                $crate::private::Result::Ok($name(name, value))
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.0)
            }
        }
    };

    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            $(#[$nmeta:meta])* $nvis:vis $nname:ident: $ntype:ty,
            $(#[$vmeta:meta])* ? $vvis:vis $vname:ident: $vtype:ty
            $(,)?
        );
    ) => {
        $(#[$meta])*
        $vis struct $name (
            $(#[$nmeta])* $nvis $ntype,
            $(#[$vmeta])* $vvis $vtype,
        );

        impl $crate::EasyPeek for $name {
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek(lookahead1)
            }
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                <$ntype as $crate::EasyPeek>::peek_stream(stream)
            }
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let name = stream.parse::<$ntype>()?;

                if stream.is_empty() {
                    $crate::private::Result::Ok($name(name, <$vtype as $crate::private::Default>::default()))
                } else {
                    let _ = stream.parse::<$crate::private::Eq>()?;
                    let value = <$vtype as $crate::private::Parse>::parse(stream)?;

                    $crate::private::Result::Ok($name(name, value))
                }
            }
        }

        impl $crate::EasyArgument for $name {
            fn name_display() -> &'static str {
                <$ntype as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$ntype as $crate::private::Spanned>::span(&self.0)
            }
        }
    };
}

/// Trait that should be implemented for enums of where each variant is an attribute.
/// It is also auto-implemented for all bare attributes.
/// This trait is used to implement [`EasyArgumentField`] for various types.
pub trait EasyArgumentGroup {
    /// Attempt to parse attribute group.
    /// Returns some attribute when parsing succeeds.
    /// Returns none if attribute peeking returns `false`, signalling that stream contains some other attribute.
    /// Returns error if peeking returns `true` but parsing fails.
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>>
    where
        Self: Sized;

    /// Produces error with appropriate message when the attribute group overlaps another instance.
    /// This is called by certain [`EasyArgumentField`] implementations.
    ///
    /// For example bare [`EasyArgumentGroup`] is used when attributes from group must be specified at most once.
    /// And this method will be called when attribute group is encountered second time.
    fn overlap_error(&self, other: &Self) -> syn::Error;

    /// Produces error with appropriate message when the attribute group is missing.
    /// This is called by certain [`EasyArgumentField`] implementations.
    fn missing_error() -> String;
}

impl<T> EasyArgumentGroup for T
where
    T: EasyArgument,
{
    #[inline]
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        if <T as EasyPeek>::peek(lookahead1) {
            T::parse(stream).map(Some)
        } else {
            Ok(None)
        }
    }

    fn overlap_error(&self, other: &Self) -> syn::Error {
        syn::Error::new(
            other.name_span(),
            format!(
                "{} can be specified at most once",
                <T as EasyArgument>::name_display()
            ),
        )
    }

    fn missing_error() -> String {
        format!(
            "{} attribute is required",
            <T as EasyArgument>::name_display()
        )
    }
}

/// Defines argument group as enum where each variant is argument type.
#[macro_export]
macro_rules! easy_argument_group {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $( $(#[$vmeta:meta])* $vname:ident ( $(#[$vtmeta:meta])* $vtype:ty ) ),*
            $(,)?
        }
    ) => {
        $crate::easy_parse! {
            $(#[$meta])*
            $vis enum $name {
                $( $(#[$vmeta])* $vname ( $(#[$vtmeta])* $vtype ), )*
            }
        }

        impl $name {
            #[inline]
            pub fn name_display(&self) -> &'static str {
                match *self {
                    $( $name :: $vname (_) => <$vtype as $crate::EasyArgument>::name_display(), )*
                }
            }

            #[inline]
            pub fn name_span(&self) -> $crate::private::Span {
                match *self {
                    $( $name :: $vname (ref var) => <$vtype as $crate::EasyArgument>::name_span(var), )*
                }
            }
        }

        impl $crate::EasyArgumentGroup for $name {
            #[inline]
            fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>, $crate::private::Error> {
                #![allow(unused_variables)]

                $(
                    if <$vtype as $crate::EasyPeek>::peek(&lookahead1) {
                        let variant = <$vtype as $crate::private::Parse>::parse(stream)?;
                        return $crate::private::Result::Ok($crate::private::Option::Some($name::$vname(variant)));
                    }
                )*

                $crate::private::Result::Ok($crate::private::Option::None)
            }

            #[inline]
            fn overlap_error(&self, other: &Self) -> $crate::private::Error {
                let msg = if $crate::private::discriminant(self) == $crate::private::discriminant(other) {
                    $crate::private::format!("{} can be specified only once", self.name_display())
                } else {
                    $crate::private::format!("{} is mutually exclusive with {}", self.name_display(), other.name_display())
                };

                $crate::private::Error::new(other.name_span(), msg)
            }

            fn missing_error() -> String {
                $crate::private::format!(
                    $crate::private::concat!("One of {{", $(
                        $crate::easy_replace_tokens!("{}, ", $vname),
                    )* "}} is expected"),
                    $(<$vtype as $crate::EasyArgument>::name_display(),)*
                )
            }
        }
    };
}

/// Trait for types that can be used as fields in easy attributes structures.
/// Fields can parsed and extended when encountered again in parsing stream.
/// If field is never encountered - [`EasyArgumentField::missing`] value will be used.
/// If attribute type or group is not mandatory - wrap it into [`Option`].
pub trait EasyArgumentField {
    /// Attempt to parse attribute field.
    /// Returns some field when parsing succeeds.
    /// Returns none if attribute peeking returns `false`, signalling that stream contains some other attribute.
    /// Returns error if peeking returns `true` but parsing fails.
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>>
    where
        Self: Sized;

    /// Attempt to parse attribute field when it already has been successfully parsed.
    /// Field value should extend itself with newly parsed attribute or return error.
    /// Returns true when parsing and extending succeeds.
    /// Returns false if attribute peeking returns `false`, signalling that stream contains some other attribute.
    /// Returns error if peeking returns `true` but parsing or extending fails.
    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool>;

    /// Called if the attribute field was never parsed.
    /// Returns error if attribute is mandatory.
    /// Otherwise returns an instance that will be used to build attributes structure.
    fn missing() -> Result<Self, String>
    where
        Self: Sized;
}

impl<T> EasyArgumentField for T
where
    T: EasyArgumentGroup,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        <T as EasyArgumentGroup>::try_parse(lookahead1, stream)
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match <T as EasyArgumentGroup>::try_parse(lookahead1, stream)? {
            None => Ok(false),
            Some(other) => Err(self.overlap_error(&other)),
        }
    }

    fn missing() -> Result<Self, String> {
        Err(T::missing_error())
    }
}

impl<T> EasyArgumentField for Option<T>
where
    T: EasyArgumentField,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Option<T>>> {
        match <T as EasyArgumentField>::try_parse(lookahead1, stream) {
            Err(err) => Err(err),
            Ok(Some(t)) => Ok(Some(Some(t))),
            Ok(None) => Ok(None),
        }
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match self {
            None => match <T as EasyArgumentField>::try_parse(lookahead1, stream)? {
                None => Ok(false),
                Some(attr) => {
                    *self = Some(attr);
                    Ok(true)
                }
            },
            Some(attr) => attr.try_extend(lookahead1, stream),
        }
    }

    fn missing() -> Result<Self, String> {
        Ok(None)
    }
}

impl<T> EasyArgumentField for Vec<T>
where
    T: EasyArgumentGroup,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        match <T as EasyArgumentGroup>::try_parse(lookahead1, stream)? {
            None => Ok(None),
            Some(attr) => Ok(Some(vec![attr])),
        }
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match <T as EasyArgumentGroup>::try_parse(lookahead1, stream)? {
            None => Ok(false),
            Some(attr) => {
                self.push(attr);
                Ok(true)
            }
        }
    }

    fn missing() -> Result<Self, String> {
        Ok(Vec::new())
    }
}

/// Defines flags and attribute group and fields for them.
/// Flags can be parsed independently.
/// They can be parsed inside attribute that can contain only one flag.
/// They can be parsed inside attribute that accepts a list of flags if provided.
#[macro_export]
macro_rules! easy_flags {
    ( $(#[$onemeta:meta])* $ovis:vis $one:ident($onekw:ident) $(| $(#[$manymeta:meta])* $mvis:vis $many:ident($manykw:ident))? { $( $(#[$flagmeta:meta])* $flag:ident($flagkw:ident)),* $(,)? }) => {
        $($crate::easy_token!($flagkw);)*
        $crate::easy_token!($onekw);

        $(#[$onemeta])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        $ovis enum $one {
            $( $(#[$flagmeta])* $flag($flagkw), )*
        }

        impl $one {
            #[allow(dead_code)]
            $ovis fn try_parse_terminated<T: $crate::private::Parse>(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<$crate::private::Punctuated<Self, $crate::private::Comma>>, $crate::private::Error> {
                if $( lookahead1.peek($flagkw) || )* false {
                    stream.parse_terminated::<Self, $crate::private::Comma>($crate::private::Parse::parse).map($crate::private::Option::Some)
                } else {
                    $crate::private::Result::Ok($crate::private::Option::None)
                }
            }
        }

        impl $crate::private::Spanned for $one {
            fn span(&self) -> $crate::private::Span {
                match *self {
                    $( $one::$flag (ref flag) => $crate::private::Spanned::span(flag), )*
                }
            }
        }

        impl $crate::EasyArgumentGroup for $one {
            fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>, $crate::private::Error> {
                #![allow(unused_variables)]

                $(
                    if lookahead1.peek($flagkw) {
                        let flag = stream.parse::<$flagkw>()?;
                        return $crate::private::Result::Ok( $crate::private::Option::Some( $one::$flag(flag)) );
                    }
                )*

                $crate::private::Result::Ok($crate::private::Option::None)
            }

            fn overlap_error(&self, other: &Self) -> $crate::private::Error {
                let msg = $crate::private::concat!("Only one flag {", $( $crate::private::stringify!($flagkw), ", ", )* "} is expected");
                $crate::private::Error::new(
                    $crate::private::Spanned::span(other),
                    msg,
                )
            }

            fn missing_error() -> $crate::private::String {
                String::from($crate::private::concat!("One flag of {", $( $crate::private::stringify!($flagkw), ", ", )* "} is expected"))
            }
        }

        impl $crate::private::Parse for $one {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                let lookahead1 = stream.lookahead1();

                match <Self as $crate::EasyArgumentGroup>::try_parse(&lookahead1, stream)? {
                    $crate::private::Option::None => $crate::private::Result::Err(lookahead1.error()),
                    $crate::private::Option::Some(flag) => $crate::private::Result::Ok(flag),
                }
            }
        }

        $(
            $crate::easy_token!($manykw);

            $(#[$manymeta])*
            #[derive(Clone, Debug)]
            #[allow(dead_code)]
            $mvis struct $many {
                pub start_span: $crate::private::Span,
                pub end_span: $crate::private::Span,
                pub flags: $crate::private::Punctuated<$one, $crate::private::Comma>,
            }

            impl $many {
                #[allow(dead_code)]
                pub fn new(span: $crate::private::Span) -> Self {
                    $many {
                        start_span: span,
                        end_span: span,
                        flags: $crate::private::Punctuated::new(),
                    }
                }
            }

            impl $crate::private::Default for $many {
                fn default() -> Self {
                    $many {
                        start_span: $crate::private::Span::call_site(),
                        end_span: $crate::private::Span::call_site(),
                        flags: $crate::private::Punctuated::default(),
                    }
                }
            }

            impl $crate::private::Spanned for $many {
                fn span(&self) -> $crate::private::Span {
                    self.start_span.join(self.end_span).unwrap_or(self.start_span)
                }
            }

            impl $crate::EasyArgumentField for $many {
                fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>, $crate::private::Error> {
                    if lookahead1.peek($onekw) {
                        let one = stream.parse::<$onekw>()?;

                        let content;
                        $crate::private::parenthesized!(content in stream);

                        let flag = content.parse::<$one>()?;

                        if content.is_empty() {
                            $crate::private::Result::Ok($crate::private::Option::Some($many {
                                start_span: $crate::private::Spanned::span(&one),
                                end_span: $crate::private::Spanned::span(&flag),
                                flags: {
                                    let mut flags = $crate::private::Punctuated::new();
                                    flags.push(flag);
                                    flags
                                },
                            }))
                        } else {
                            $crate::private::Result::Err(content.error("Expected closing parentheses"))
                        }
                    } else if lookahead1.peek($manykw) {
                        let many = stream.parse::<$manykw>()?;

                        let content;
                        $crate::private::parenthesized!(content in stream);
                        let flags = content.parse_terminated(<$one as $crate::private::Parse>::parse)?;

                        let end_span = flags.last().map($crate::private::Spanned::span).unwrap_or($crate::private::Spanned::span(&many));

                        $crate::private::Result::Ok($crate::private::Option::Some($many {
                            start_span: $crate::private::Spanned::span(&many),
                            end_span,
                            flags,
                        }))
                    } else {
                        match $one::try_parse_terminated::<$crate::private::Comma>(lookahead1, stream)? {
                            $crate::private::Option::None => $crate::private::Result::Ok($crate::private::Option::None),
                            $crate::private::Option::Some(flags) => {
                                let flag = flags.last().unwrap();
                                let span = $crate::private::Spanned::span(flag);
                                $crate::private::Result::Ok($crate::private::Option::Some($many {
                                    start_span: span,
                                    end_span: span,
                                    flags,
                                }))
                            }
                        }
                    }
                }

                fn try_extend(&mut self, lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::bool, $crate::private::Error> {
                    if lookahead1.peek($onekw) {
                        let _ = stream.parse::<$onekw>()?;

                        let content;
                        $crate::private::parenthesized!(content in stream);

                        let flag = content.parse::<$one>()?;

                        if content.is_empty() {
                            self.end_span = $crate::private::Spanned::span(&flag);
                            self.flags.push(flag);
                            $crate::private::Result::Ok(true)
                        } else {
                            $crate::private::Result::Err(content.error("Expected closing parentheses"))
                        }
                    } else if lookahead1.peek($manykw) {
                        let many = stream.parse::<$manykw>()?;

                        let content;
                        $crate::private::parenthesized!(content in stream);

                        let flags = content.parse_terminated::<_, $crate::private::Comma>(<$one as $crate::private::Parse>::parse)?;

                        self.end_span = flags.last().map($crate::private::Spanned::span).unwrap_or($crate::private::Spanned::span(&many));
                        self.flags.extend(flags);

                        $crate::private::Result::Ok(true)
                    } else {
                        match $one::try_parse_terminated::<$crate::private::Comma>(lookahead1, stream)? {
                            $crate::private::Option::None => $crate::private::Result::Ok(false),
                            $crate::private::Option::Some(flags) => {
                                let flag = flags.last().unwrap();
                                self.end_span = $crate::private::Spanned::span(flag);

                                self.flags.extend(flags);
                                $crate::private::Result::Ok(true)
                            }
                        }
                    }
                }

                fn missing() -> $crate::private::Result<Self, $crate::private::String> {
                    $crate::private::Result::Err(<$one as $crate::EasyArgumentGroup>::missing_error())
                }
            }

            impl $crate::private::Parse for $many {
                fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self, $crate::private::Error> {
                    let lookahead1 = stream.lookahead1();

                    match <Self as $crate::EasyArgumentField>::try_parse(&lookahead1, stream)? {
                        $crate::private::Option::None => $crate::private::Result::Err(lookahead1.error()),
                        $crate::private::Option::Some(flag) => $crate::private::Result::Ok(flag),
                    }
                }
            }
        )?
    };
}

/// Defines structure and implements [`Parse`] for it.
///
/// Fields will be parsed in any order separated by specified punctuation.
/// Parsing continues until the end of this parse stream.
#[macro_export]
macro_rules! easy_terminated {
    (
        @($punct:ident)
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $($(#[$fmeta:meta])* $fvis:vis $fname:ident : $ftype:ty),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $($(#[$fmeta])* $fvis $fname : $ftype,)*
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                $(let $fname = $crate::private::None;)*

                loop {
                    if stream.is_empty() {
                        break;
                    }
                    let lookahead1 = stream.lookahead1();
                    $(
                        match &mut $fname {
                            $crate::private::Option::None => {
                                if let $crate::private::Option::Some(value) = <$ftype as $crate::EasyArgumentField>::try_parse(&lookahead1, stream)? {
                                    $fname = $crate::private::Option::Some(value);
                                    if stream.is_empty() {
                                        break;
                                    }
                                    stream.parse::<$punct>()?;
                                    continue
                                }
                            }
                            $crate::private::Option::Some($fname) => {
                                if <$ftype as $crate::EasyArgumentField>::try_extend($fname, &lookahead1, stream)? {
                                    if stream.is_empty() {
                                        break;
                                    }
                                    stream.parse::<$punct>()?;
                                    continue
                                }
                            }
                        }
                    )*
                    return $crate::private::Result::Err(lookahead1.error());
                }

                $crate::private::Result::Ok($name {
                    $(
                        $fname: match $fname {
                            $crate::private::Option::None => <$ftype as $crate::EasyArgumentField>::missing().map_err(|msg| stream.error(msg))?,
                            $crate::private::Option::Some($fname) => $fname,
                        },
                    )*
                })
            }
        }
    };
}

/// Defines structure and implements [`Parse`] for it.
///
/// Fields will be parsed in any order separated by specified punctuation, not accepting trailing punctuation.
/// Parsing continues as long as punctuation $punct is present at the head of the stream.
#[macro_export]
macro_rules! easy_separated {
    (
        @($punct:ident)
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $($(#[$fmeta:meta])* $fvis:vis $fname:ident : $ftype:ty),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $($(#[$fmeta])* $fvis $fname : $ftype,)*
        }

        impl $crate::private::Parse for $name {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                $(let $fname = $crate::private::None;)*

                loop {
                    if stream.is_empty() {
                        break;
                    }
                    let lookahead1 = stream.lookahead1();
                    $(
                        match &mut $fname {
                            $crate::private::Option::None => {
                                if let $crate::private::Option::Some(value) = <$ftype as $crate::EasyArgumentField>::try_parse(&lookahead1, stream)? {
                                    $fname = $crate::private::Option::Some(value);
                                    if !<$punct as $crate::private::EasyPeek>::peek_stream(stream) {
                                        break;
                                    }
                                    stream.parse::<$punct>()?;
                                    continue
                                }
                            }
                            $crate::private::Option::Some($fname) => {
                                if <$ftype as $crate::EasyArgumentField>::try_extend($fname, &lookahead1, stream)? {
                                    if !<$punct as $crate::private::EasyPeek>::peek_stream(stream) {
                                        break;
                                    }
                                    stream.parse::<$punct>()?;
                                    continue
                                }
                            }
                        }
                    )*
                    return $crate::private::Result::Err(lookahead1.error());
                }

                $crate::private::Result::Ok($name {
                    $(
                        $fname: match $fname {
                            $crate::private::Option::None => <$ftype as $crate::EasyArgumentField>::missing().map_err(|msg| stream.error(msg))?,
                            $crate::private::Option::Some($fname) => $fname,
                        },
                    )*
                })
            }
        }
    };
}

/// Collection of attributes that can be parsed from array of attributes.
/// Can be easily applied to attributes vector parsed by [`syn`].
pub trait EasyAttributes {
    /// Parse attributes array.
    fn parse(attrs: &[Attribute], span: Span) -> syn::Result<Self>
    where
        Self: Sized;

    /// Parse attributes array within specifeid namespace.
    fn parse_in(namespace: &syn::Ident, attrs: &[Attribute], span: Span) -> syn::Result<Self>
    where
        Self: Sized;
}

/// Defines struct and implement [`EasyAttributes`] for it.
///
/// Each field's type must implement [`EasyArgumentField`].
/// Fields are parsed in any order.
#[macro_export]
macro_rules! easy_attributes {
    (
        @($namespace:ident)
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $( $(#[$fmeta:meta])* $fvis:vis $fname:ident : $ftype:ty),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $( $(#[$fmeta])* $fvis $fname : $ftype,)*
        }

        impl $crate::EasyAttributes for $name {
            fn parse(attrs: &[$crate::private::Attribute], span: $crate::private::Span) -> $crate::private::Result<Self, $crate::private::Error> {
                Self::parse_in(&$crate::private::Ident::new($crate::private::stringify!($namespace), $crate::private::Span::call_site()), attrs, span)
            }

            fn parse_in(namespace: &$crate::private::Ident, attrs: &[$crate::private::Attribute], span: $crate::private::Span) -> $crate::private::Result<Self, $crate::private::Error> {
                $(let mut $fname = $crate::private::Option::None;)*

                for attr in attrs {
                    if attr.path.is_ident(namespace) {
                        attr.parse_args_with(|stream: $crate::private::ParseStream| {
                            loop {
                                if stream.is_empty() {
                                    return $crate::private::Result::Ok(());
                                }
                                let lookahead1 = stream.lookahead1();
                                $(
                                    match &mut $fname {
                                        $crate::private::Option::None => {
                                            if let $crate::private::Option::Some(value) = <$ftype as $crate::EasyArgumentField>::try_parse(&lookahead1, stream)? {
                                                $fname = $crate::private::Option::Some(value);
                                                if stream.is_empty() {
                                                    return $crate::private::Result::Ok(());
                                                }
                                                stream.parse::<$crate::private::Comma>()?;
                                                continue;
                                            }
                                        }
                                        $crate::private::Option::Some($fname) => {
                                            if <$ftype as $crate::EasyArgumentField>::try_extend($fname, &lookahead1, stream)? {
                                                if stream.is_empty() {
                                                    return $crate::private::Result::Ok(());
                                                }
                                                stream.parse::<$crate::private::Comma>()?;
                                                continue;
                                            }
                                        }
                                    }
                                )*
                                return $crate::private::Result::Err(lookahead1.error())
                            }
                        })?;
                    }
                }

                $crate::private::Result::Ok($name {
                    $(
                        $fname: match $fname {
                            $crate::private::Option::None => <$ftype as $crate::EasyArgumentField>::missing().map_err(|msg| $crate::private::Error::new(span, msg))?,
                            $crate::private::Option::Some($fname) => $fname,
                        },
                    )*
                })
            }
        }
    };
}

easy_parse! {
    /// Expression that may be a constant or a reference to member field.
    /// Often used pattern is having attribute that refer to a field or constant expression.
    ///
    /// # Examples
    ///
    /// ```
    /// # use {proc_easy::ReferenceExpr, syn::{parse_quote, Token}, quote::{quote, format_ident}};
    /// /// Peeking first `const` token it decides to parse `Expr` variant.
    /// let re: ReferenceExpr = parse_quote!(const 42);
    /// assert_eq!(re, ReferenceExpr::Expr { const_: parse_quote!(const), expr: parse_quote!(42) });
    /// ```
    ///
    /// ```
    /// # use {proc_easy::ReferenceExpr, syn::parse_quote, quote::{quote, format_ident}};
    /// /// Without `const` token it decides to parse `Member` variant.
    /// let re: ReferenceExpr = parse_quote!(foo);
    /// assert_eq!(re, ReferenceExpr::Member { member: format_ident!("foo").into() });
    /// ```
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum ReferenceExpr {
        /// Member reference.
        ! Member {
            ///
            member: syn::Member,
        },
        /// Constant expression
        Expr {
            /// Const token.
            const_: syn::Token![const],

            ///
            expr: syn::Expr,
        },
    }
}

#[cfg(any(test, doc))]
#[allow(missing_docs)]
pub mod examples {
    easy_flags! {
        /// Nya flags documentation
        pub Nya(nya) | pub Nyas(nyas) {
            /// Puk documentation
            Puk(puk),
            /// Pak documentation
            Pak(pak),
        }
    }

    easy_parse! {
        /// Foo
        #[derive(Clone, Debug)]
        pub struct Foo {
            /// Foo
            pub foo: syn::Ident,
        }
    }

    easy_parse! {
        /// Bar
        #[derive(Clone, Copy, Debug)]
        pub struct Bar;
    }

    easy_parse! {
        /// Baz
        #[derive(Clone, Debug)]
        pub struct Baz(pub syn::Member);
    }

    easy_parse! {
        /// Baz
        #[derive(Clone, Debug)]
        pub enum FooBar {
            ! Bar(Bar),
            Foo(Foo),
        }
    }

    easy_token!(arg);

    easy_argument! {
        /// Baz
        #[derive(Clone, Debug)]
        pub struct Arg {
            pub arg: arg,
            pub foobar: FooBar,
        }
    }

    easy_token!(outer);

    easy_argument_tuple! {
        /// Baz
        #[derive(Clone, Debug)]
        pub struct Outer {
            pub outer: outer,
            pub arg: Arg,
        }
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::Span;

    use crate::*;

    #[allow(warnings)]
    enum Option {}

    // #[allow(warnings)]
    // const Some: () = (); // Commented due to bug in `syn`. Fixed by https://github.com/dtolnay/syn/pull/1171
    #[allow(warnings)]
    const None: () = ();

    #[allow(warnings)]
    enum Result {}
    #[allow(warnings)]
    const Ok: () = ();
    #[allow(warnings)]
    const Err: () = ();

    #[allow(warnings)]
    enum Vec {}
    #[allow(warnings)]
    macro_rules! vec {
        () => {
            compile_error!();
        };
    }

    trait Default {}

    #[allow(warnings)]
    enum bool {}

    easy_flags! {
        /// Docs
        pub Nya(nya) |
        /// Docs
        Nyas(nyas) {
            /// Docs
            Puk(puk),
            /// Docs
            Pak(pak),
        }
    }

    easy_token!(foo);
    easy_token!(bar);
    easy_token!(baz);

    easy_token!(a);
    easy_token!(b);

    easy_argument_value! {
        /// Docs
        #[derive(Clone, Debug)]
        pub struct A {
            /// Docs
            pub a: a,

            /// Docs
            pub ident: syn::Ident,
        }
    }

    easy_argument_value! {
        /// Docs
        #[derive(Clone, Debug)]
        pub struct B {
            /// Docs
            pub b: b,

            /// Docs
            pub lit: syn::LitInt,
        }
    }

    easy_argument_tuple! {
        /// Docs
        #[derive(Clone, Debug)]
        pub struct Foo {
            /// Docs
            pub name: foo,
            /// Docs
            pub a: A,
            /// Docs
            pub b: B,
        }
    }

    easy_argument! {
        /// Docs
        #[derive(Clone, Debug)]
        pub struct Bar {
            /// Docs
            pub name: bar,
            /// Docs
            pub ident: syn::Ident,
        }
    }

    easy_argument_group! {
        /// Docs
        #[derive(Clone, Debug)]
        pub enum Group {
            /// Docs
            Foo(
                /// Docs
                Foo
            ),
            /// Docs
            Bar(
                /// Docs
                Bar
            ),
        }
    }

    easy_attributes! {
        @(easy)
        /// Docs
        #[derive(Clone, Default)]
        pub struct Attributes {
            /// Docs
            pub foo: std::option::Option<Foo>,
            /// Docs
            pub bar: std::option::Option<Bar>,
        }
    }

    #[test]
    fn test1() {
        let attrs = quote::quote!(
            #[easy(foo(a = a, b = 42), bar b)]
        );

        let attrs = syn::parse::Parser::parse2(syn::Attribute::parse_outer, attrs).unwrap();

        let bar = Attributes::parse(&attrs, Span::call_site()).unwrap();
        match &bar.foo {
            std::option::Option::Some(Foo {
                a: A { ident, .. },
                b: B { lit, .. },
                ..
            }) => {
                assert_eq!(ident, "a");
                assert_eq!(lit.base10_parse::<u32>().unwrap(), 42);
            }
            _ => panic!(),
        }

        match &bar.bar {
            std::option::Option::Some(bar) => assert_eq!(bar.ident, "b"),
            std::option::Option::None => panic!(),
        }
    }

    #[test]
    fn test_argument_tuple() {
        mod kw {
            easy_token!(clear);
            easy_token!(load);
            easy_token!(store);
            easy_token!(attachment);
        }

        easy_argument_value! {
            pub struct Clear(pub kw::clear, pub ReferenceExpr);
        }

        easy_argument_value! {
            pub struct Load(pub kw::load, pub ReferenceExpr);
        }

        easy_argument_group! {
            pub enum LoadOp {
                Clear(Clear),
                Load(Load),
            }
        }

        easy_argument_value! {
            pub struct Store(pub kw::store, pub ReferenceExpr);
        }

        easy_argument_group! {
            pub enum StoreOp {
                Store(Store),
            }
        }

        easy_argument_tuple! {
            struct AttachmentAttribute {
                attachment: kw::attachment,
                load_op: core::option::Option<LoadOp>,
                store_op: core::option::Option<StoreOp>,
            }
        }

        easy_attributes! {
            @(test_namespace)
            struct FieldAttributes {
                attachment: core::option::Option<AttachmentAttribute>,
            }
        }

        let tokens = quote::quote!(#[test_namespace(attachment(store = const Layout::Present, clear = const ClearColor(0.02, 0.03, 0.03, 1.0)))]);
        let attrs = syn::parse::Parser::parse2(syn::Attribute::parse_outer, tokens).unwrap();

        let attributes = FieldAttributes::parse(&attrs, Span::call_site()).unwrap();
        let attachment_attribute = attributes.attachment.unwrap();

        drop(attachment_attribute.load_op);
        drop(attachment_attribute.store_op);
    }
}
