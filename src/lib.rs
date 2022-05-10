//!
//! Making proc-macro easy.
//!
//!
//!

#![deny(missing_docs)]
#![deny(missing_copy_implementations)]
#![deny(missing_debug_implementations)]

use std::ops::Deref;

use proc_macro2::Span;
use syn::{
    parse::{Lookahead1, Parse, ParseStream},
    spanned::Spanned,
    token::Token,
};

#[doc(hidden)]
pub mod private {
    pub use std::{
        concat, default::Default, format, mem::discriminant, option::Option, stringify, vec::Vec,
    };

    pub use bool;
    pub use proc_macro2::Span;
    pub use syn::{
        custom_keyword, parenthesized,
        parse::{Lookahead1, Parse, ParseStream},
        punctuated::Punctuated,
        spanned::Spanned,
        token::Comma,
        Attribute, Error, Result,
    };
}

/// Peekable, parsable token.
///
/// [`easy_token!`] macro produces types that implement this trait.
pub trait EasyToken: EasyPeek + Parse + Spanned {
    /// Display the token for the user.
    fn display() -> &'static str;
}

/// Produces type with specified name that implement [`EasyToken`] and can be parsed from the name ident.
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

/// Provides interface for peeking tokens before parsing.
/// When implemented for complex structures, peeks first token.
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

/// Parses inner type parenthesized.
/// Implements [`EasyPeek`] and peeks opening parenthesis.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parenthesized<T>(pub T);

impl<T> Deref for Parenthesized<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> EasyPeek for Parenthesized<T>
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

impl<T> Parse for Parenthesized<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        syn::parenthesized!(inner in input);
        T::parse(&inner).map(Parenthesized)
    }
}

/// Parses inner type braced.
/// Implements [`EasyPeek`] and peeks opening brace.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Braced<T>(pub T);

impl<T> Deref for Braced<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> EasyPeek for Braced<T>
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

impl<T> Parse for Braced<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        syn::braced!(inner in input);
        T::parse(&inner).map(Braced)
    }
}

/// Parses inner type bracketed.
/// Implements [`EasyPeek`] and peeks opening bracket.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bracketed<T>(pub T);

impl<T> Deref for Bracketed<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> EasyPeek for Bracketed<T>
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

impl<T> Parse for Bracketed<T>
where
    T: Parse,
{
    #[inline]
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        syn::bracketed!(inner in input);
        T::parse(&inner).map(Bracketed)
    }
}

/// Defines structure or enum and implements [`Parse`] for it
/// Also implements [`EasyPeek`] for structs if first field is prefixed with `@`. That field's type must implement [`EasyPeek`] itself.
/// Implements [`EasyPeek`] for all enums.
/// First field in enum variant must implement [`EasyPeek`].
/// Or if there's no fields in enum variant - it does not participate in parsing.
#[macro_export]
macro_rules! easy_parse {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $(@ $pvis:vis $pname:ident: $ptype:ty,)?
            $($fvis:vis $fname:ident: $ftype:ty),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $($pvis $pname: $ptype,)?
            $($fvis $fname: $ftype,)*
        }

        impl $crate::private::Parse for $name {
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                #![allow(unused_variables)]

                $( let $pname =  <$ptype as $crate::private::Parse>::parse(input)?; )?
                $( let $fname = <$ftype as $crate::private::Parse>::parse(input)?; )*

                $crate::private::Result::Ok($name {
                    $( $pname, )?
                    $( $fname, )*
                })
            }
        }

        $(
            impl $crate::EasyPeek for $name {
                #[inline]
                fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                    <$ptype as $crate::EasyPeek>::peek(lookahead1)
                }

                #[inline]
                fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                    <$ptype as $crate::EasyPeek>::peek_stream(stream)
                }
            }
        )?
    };
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            $(@ $pvis:vis $ptype:ty,)?
            $($fvis:vis $ftype:ty),*
            $(,)?
        )
    ) => {
        $(#[$meta])*
        $vis struct $name (
            $($pvis $ptype,)?
            $($fvis $ftype,)*
        )

        impl $crate::private::Parse for $name {
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                #![allow(unused_variables)]
                Ok($name (
                    $( <$ptype as $crate::private::Parse>::parse(input)?, )?
                    $( <$ftype as $crate::private::Parse>::parse(input)?, )*
                ))
            }
        }

        $(
            impl $crate::EasyPeek for $name {
                #[inline]
                fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                    <$ptype as $crate::EasyPeek>::peek(lookahead1)
                }

                #[inline]
                fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                    <$ptype as $crate::EasyPeek>::peek_stream(stream)
                }
            }
        )?
    };

    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $( $vname:ident $( ( $($vt:tt)* ))? $( { $($vr:tt)* })? ),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $( $vname $( ( $($vt)* ) )? $( { $($vr)* } )?, )*
        }

        impl $crate::private::Parse for $name {
            #[inline]
            fn parse(input: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                #![allow(unused_variables)]
                let lookahead1 = input.lookahead1();
                $(
                    $crate::easy_parse_enum_variant!(parse lookahead1 input $name $vname $( ( $($vt)* ) )? $( { $($vr)* } )?);
                )*

                $crate::private::Result::Err(lookahead1.error())
            }
        }

        impl $crate::EasyPeek for $name {
            #[inline]
            fn peek(lookahead1: &$crate::private::Lookahead1) -> $crate::private::bool {
                $( $crate::easy_parse_enum_variant!(peek lookahead1 $( ( $($vt)* ) )? $( { $($vr)* } )?) || )* false
            }

            #[inline]
            fn peek_stream(stream: $crate::private::ParseStream) -> $crate::private::bool {
                $( $crate::easy_parse_enum_variant!(peek_stream stream $( ( $($vt)* ) )? $( { $($vr)* } )?) || )* false
            }
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! easy_parse_enum_variant {
    (parse  $lookahead1:ident $stream:ident $name:ident $vname:ident) => {};
    (peek $lookahead1:ident) => {
        false
    };
    (peek_stream $stream:ident) => {
        false
    };

    (parse  $lookahead1:ident $stream:ident $name:ident $vname:ident ()) => {};
    (peek $lookahead1:ident ()) => {
        false
    };
    (peek_stream $stream:ident ()) => {
        false
    };

    (parse  $lookahead1:ident $stream:ident $name:ident $vname:ident {}) => {};
    (peek $lookahead1:ident {}) => {
        false
    };
    (peek_stream $stream:ident {}) => {
        false
    };

    (parse  $lookahead1:ident $stream:ident $name:ident $vname:ident ( $vptype:ty $(, $vftype:ty )* $(,)? )) => {
        if <$vptype as $crate::EasyPeek>::peek(&$lookahead1) {
            return $crate::private::Result::Ok($name::$vname(
                <$vptype as $crate::private::Parse>::parse($stream)?,
                $( <$vftype as $crate::private::Parse>::parse($stream)?, )*
            ));
        }
    };
    (peek $lookahead1:ident ( $vptype:ty $(, $vftype:ty )* $(,)? )) => {
        <$vptype as $crate::EasyPeek>::peek($lookahead1)
    };
    (peek_stream $stream:ident ( $vptype:ty $(, $vftype:ty )* $(,)? )) => {
        <$vptype as $crate::EasyPeek>::peek_stream($stream)
    };

    (parse  $lookahead1:ident $stream:ident $name:ident $vname:ident { $vpname:ident : $vptype:ty $(, $vfname:ident : $vftype:ty )* $(,)? }) => {
        if <$vptype as $crate::EasyPeek>::peek(&$lookahead1) {
            let $vpname =  <$vptype as $crate::private::Parse>::parse(input)?;
            $( let $vfname = <$vftype as $crate::private::Parse>::parse(input)?; )*

            return $crate::private::Result::Ok($name :: $vname {
                $vpname,
                $($vfname,)*
            })
        }
    };
    (peek $lookahead1:ident { $vpname:ident : $vptype:ty $(, $vfname:ident : $vftype:ty )* $(,)? }) => {
        <$vptype as $crate::EasyPeek>::peek($lookahead1)
    };
    (peek_stream $stream:ident { $vpname:ident : $vptype:ty $(, $vfname:ident : $vftype:ty )* $(,)? }) => {
        <$vptype as $crate::EasyPeek>::peek_stream($stream)
    };
}

/// Trait for parsable attributes.
/// Attributes should be peekable to choose which attribute to parse.
pub trait EasyAttribute: EasyPeek {
    /// Returns attribute name for display purposes.
    fn name_display() -> &'static str;

    /// Returns attribute name for display purposes.
    fn name_span(&self) -> Span;
}

impl<T> EasyAttribute for T
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

/// Defines attribute structure.
/// First field is a attribute name.
/// It must be [`EasyToken`] implementation that will be used to for [`EasyPeek`] implementation.
/// In case of errors, attribute name and span will be used.
#[macro_export]
macro_rules! easy_attribute {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            @ $nvis:vis $namename:ident: $kw:ty
            $(, $fvis:vis $fname:ident: $ftype:ty)*
            $(,)?
        }
    ) => {
        $crate::easy_parse!{
            $(#[$meta])*
            $vis struct $name {
                @ $nvis $namename: $kw,
                $($fvis $fname: $ftype,)*
            }
        }

        impl $crate::EasyAttribute for $name {
            fn name_display() -> &'static str {
                <$kw as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$kw as $crate::private::Spanned>::span(&self.$namename)
            }
        }
    };

    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident (
            @ $nvis:vis $kw:ty
            $(, $fvis:vis $ftype:ty)*
            $(,)?
        )
    ) => {
        $crate::easy_parse!{
            $(#[$meta])*
            $vis struct $name (
                @ $nvis $kw,
                $($fvis $ftype,)*
            )
        }

        impl $crate::EasyAttribute for $name {
            fn name_display() -> &'static str {
                <$kw as $crate::EasyToken>::display()
            }

            fn name_span(&self) -> $crate::private::Span {
                <$kw as $crate::private::Spanned>::span(&self.0)
            }
        }
    };
}

/// Trait that should be implemented for enums of where each variant is an attribute.
/// It is also auto-implemented for all bare attributes.
/// This trait is used to implement [`EasyAttributeField`] for various types.
pub trait EasyAttributeGroup {
    /// Attempt to parse attribute group.
    /// Returns some attribute when parsing succeeds.
    /// Returns none if attribute peeking returns `false`, signalling that stream contains some other attribute.
    /// Returns error if peeking returns `true` but parsing fails.
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>>
    where
        Self: Sized;

    /// Produces error with appropriate message when the attribute group overlaps another instance.
    /// This is called by certain [`EasyAttributeField`] implementations.
    ///
    /// For example bare [`EasyAttributeGroup`] is used when attributes from group must be specified at most once.
    /// And this method will be called when attribute group is encountered second time.
    fn overlap_error(&self, other: &Self) -> syn::Error;
}

impl<T> EasyAttributeGroup for T
where
    T: EasyAttribute,
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
                <T as EasyAttribute>::name_display()
            ),
        )
    }
}

/// Defines attribute group as enum where each variant is attribute type.
#[macro_export]
macro_rules! easy_attribute_group {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $( $vname:ident (  $vtype:ty ) ),*
            $(,)?
        }
    ) => {
        $crate::easy_parse! {
            $(#[$meta])*
            $vis enum $name {
                $( $vname ($vtype), )*
            }
        }

        impl $name {
            #[inline]
            pub fn name_display(&self) -> &'static str {
                match *self {
                    $( $name :: $vname (_) => <$vtype as $crate::EasyAttribute>::name_display(), )*
                }
            }

            #[inline]
            pub fn name_span(&self) -> $crate::private::Span {
                match *self {
                    $( $name :: $vname (ref var) => <$vtype as $crate::EasyAttribute>::name_span(var), )*
                }
            }
        }

        impl $crate::EasyAttributeGroup for $name {
            #[inline]
            fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>> {
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
        }
    };
}

/// Trait for types that can be used as fields in easy attributes structures.
/// Fields can parsed and extended when encountered again in parsing stream.
/// If field is never encountered - default value will be used.
/// If attribute type or group have no default wrap it into [`Option`].
pub trait EasyAttributeField: Default {
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
}

impl<T> EasyAttributeField for T
where
    T: EasyAttributeGroup + Default,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        <T as EasyAttributeGroup>::try_parse(lookahead1, stream)
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match <T as EasyAttributeGroup>::try_parse(lookahead1, stream)? {
            None => Ok(false),
            Some(other) => Err(self.overlap_error(&other)),
        }
    }
}

impl<T> EasyAttributeField for Option<T>
where
    T: EasyAttributeGroup,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        <T as EasyAttributeGroup>::try_parse(lookahead1, stream).map(Some)
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match <T as EasyAttributeGroup>::try_parse(lookahead1, stream)? {
            None => Ok(false),
            Some(attr) => match self {
                None => {
                    *self = Some(attr);
                    Ok(true)
                }
                Some(some) => Err(some.overlap_error(&attr)),
            },
        }
    }
}

impl<T> EasyAttributeField for Vec<T>
where
    T: EasyAttributeGroup,
{
    fn try_parse(lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<Option<Self>> {
        match <T as EasyAttributeGroup>::try_parse(lookahead1, stream)? {
            None => Ok(None),
            Some(attr) => Ok(Some(vec![attr])),
        }
    }

    fn try_extend(&mut self, lookahead1: &Lookahead1, stream: ParseStream) -> syn::Result<bool> {
        match <T as EasyAttributeGroup>::try_parse(lookahead1, stream)? {
            None => Ok(false),
            Some(attr) => {
                self.push(attr);
                Ok(true)
            }
        }
    }
}

/// Defines flags and attribute group and fields for them.
/// Flags can be parsed independently.
/// They can be parsed inside attribute that can contain only one flag.
/// They can be parsed inside attribute that accepts a list of flags if provided.
#[macro_export]
macro_rules! easy_flags {
    ( $ovis:vis $onekw:ident as $one:ident $([$mvis:vis $manykw:ident as $many:ident])? { $($flag:ident $( = $value:literal)?),* $(,)? }) => {
        $($crate::easy_token!($flag);)*
        $crate::easy_token!($onekw);

        $ovis enum $one {
            $( $flag($flag), )*
        }

        impl $one {
            #[allow(dead_code)]
            $ovis fn try_parse_terminated<T: $crate::private::Parse>(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<$crate::private::Punctuated<Self, $crate::private::Comma>>> {
                if $( lookahead1.peek($flag) || )* false {
                    stream.parse_terminated::<Self, $crate::private::Comma>($crate::private::Parse::parse).map($crate::private::Option::Some)
                } else {
                    $crate::private::Result::Ok($crate::private::Option::None)
                }
            }
        }

        impl $crate::private::Spanned for $one {
            fn span(&self) -> $crate::private::Span {
                match *self {
                    $( $one::$flag (ref flag) => $crate::Spanned::span(flag), )*
                }
            }
        }

        impl $crate::EasyAttributeGroup for $one {
            fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>> {
                #![allow(unused_variables)]

                $(
                    if lookahead1.peek($flag) {
                        let flag = stream.parse::<$flag>()?;
                        return $crate::private::Result::Ok( $crate::private::Option::Some( $one::$flag(flag)) );
                    }
                )*

                $crate::private::Result::Ok($crate::private::Option::None)
            }

            fn overlap_error(&self, other: &Self) -> $crate::private::Error {
                let msg = $crate::private::concat!("Only one flag {", $( $crate::private::stringify!($flag), )* "} is expected");
                $crate::private::Error::new(
                    $crate::private::Spanned::span(other),
                    msg,
                )
            }
        }

        impl $crate::private::Parse for $one {
            fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                let lookahead1 = stream.lookahead1();

                match <Self as $crate::EasyAttributeGroup>::try_parse(&lookahead1, stream)? {
                    $crate::private::Option::None => $crate::private::Result::Err(lookahead1.error()),
                    $crate::private::Option::Some(flag) => $crate::private::Result::Ok(flag),
                }
            }
        }

        $(
            $crate::easy_token!($manykw);

            #[allow(dead_code)]
            $mvis struct $many {
                pub ones: $crate::private::Vec<$onekw>,
                pub manies: $crate::private::Vec<$manykw>,
                pub flags: $crate::private::Punctuated<$one, $crate::private::Comma>,
            }

            impl $crate::private::Default for $many {
                fn default() -> Self {
                    $many {
                        ones: $crate::private::Vec::default(),
                        manies: $crate::private::Vec::default(),
                        flags: $crate::private::Punctuated::default(),
                    }
                }
            }

            impl $crate::EasyAttributeField for $many {
                fn try_parse(lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::Option<Self>> {
                    if lookahead1.peek($onekw) {
                        let mut ones = $crate::private::Vec::new();
                        ones.push(stream.parse::<$onekw>()?);

                        let content;
                        $crate::private::parenthesized!(content in stream);

                        let mut flags = $crate::private::Punctuated::new();
                        flags.push(content.parse::<$one>()?);

                        $crate::private::Result::Ok(Some($many {
                            ones,
                            manies: $crate::private::Vec::default(),
                            flags,
                        }))
                    } else if lookahead1.peek($manykw) {
                        let mut manies = $crate::private::Vec::new();
                        manies.push(stream.parse::<$manykw>()?);

                        let content;
                        $crate::private::parenthesized!(content in stream);
                        let flags = content.parse_terminated(<$one as $crate::private::Parse>::parse)?;

                        $crate::private::Result::Ok($crate::private::Option::Some($many {
                            ones: $crate::private::Vec::new(),
                            manies,
                            flags,
                        }))
                    } else {
                        match $one::try_parse_terminated::<$crate::private::Comma>(lookahead1, stream)? {
                            $crate::private::Option::None => $crate::private::Result::Ok($crate::private::Option::None),
                            $crate::private::Option::Some(flags) => {
                                $crate::private::Result::Ok($crate::private::Option::Some($many {
                                    ones: $crate::private::Vec::new(),
                                    manies: $crate::private::Vec::new(),
                                    flags,
                                }))
                            }
                        }
                    }
                }

                fn try_extend(&mut self, lookahead1: &$crate::private::Lookahead1, stream: $crate::private::ParseStream) -> $crate::private::Result<$crate::private::bool> {
                    if lookahead1.peek($onekw) {
                        self.ones.push(stream.parse::<$onekw>()?);

                        let content;
                        $crate::private::parenthesized!(content in stream);

                        let flag = content.parse::<$one>()?;
                        self.flags.push(flag);

                        $crate::private::Result::Ok(true)
                    } else if lookahead1.peek($manykw) {
                        self.manies.push(stream.parse::<$manykw>()?);

                        let content;
                        $crate::private::parenthesized!(content in stream);
                        let flags = content.parse_terminated::<_, $crate::private::Comma>(<$one as $crate::private::Parse>::parse)?;
                        self.flags.extend(flags);

                        $crate::private::Result::Ok(true)
                    } else {
                        match $one::try_parse_terminated::<$crate::private::Comma>(lookahead1, stream)? {
                            $crate::private::Option::None => $crate::private::Result::Ok(false),
                            $crate::private::Option::Some(flags) => {
                                self.flags.extend(flags);
                                $crate::private::Result::Ok(true)
                            }
                        }
                    }
                }
            }

            impl $crate::private::Parse for $many {
                fn parse(stream: $crate::private::ParseStream) -> $crate::private::Result<Self> {
                    let lookahead1 = stream.lookahead1();

                    match <Self as $crate::EasyAttributeField>::try_parse(&lookahead1, stream)? {
                        $crate::private::Option::None => $crate::private::Result::Err(lookahead1.error()),
                        $crate::private::Option::Some(flag) => $crate::private::Result::Ok(flag),
                    }
                }
            }
        )?
    };
}

/// Collection of attributes that can be parsed from array of attributes.
/// Can be easily applied to field's or type's attributes vector.
pub trait EasyAttributes {
    /// Parse attributes array.
    fn parse(attrs: &[syn::Attribute]) -> syn::Result<Self>
    where
        Self: Sized;
}

/// Defines struct and implement [`EasyAttributes`] for it.
/// Each field's type must implement [`EasyAttributeField`].
#[macro_export]
macro_rules! easy_attributes {
    (
        @($namespace:ident)
        $(#[$meta:meta])*
        $vis:vis struct $name:ident {
            $($fvis:vis $fname:ident : $ftype:ty),*
            $(,)?
        }
    ) => {
        $(#[$meta])*
        $vis struct $name {
            $($fvis $fname : $ftype,)*
        }

        impl $crate::EasyAttributes for $name {
            fn parse(attrs: &[$crate::private::Attribute]) -> $crate::private::Result<Self> {
                $(let mut $fname = $crate::private::Option::None;)*

                for attr in attrs {
                    if attr.path.is_ident(::core::stringify!($namespace)) {
                        attr.parse_args_with(|stream: $crate::private::ParseStream| {
                            let lookahead1 = stream.lookahead1();
                            $(
                                match &mut $fname {
                                    $crate::private::Option::None => {
                                        if let $crate::private::Option::Some(value) = <$ftype as $crate::EasyAttributeField>::try_parse(&lookahead1, stream)? {
                                            $fname = $crate::private::Option::Some(value);
                                            return $crate::private::Result::Ok(())
                                        }
                                    }
                                    $crate::private::Option::Some($fname) => {
                                        if <$ftype as $crate::EasyAttributeField>::try_extend($fname, &lookahead1, stream)? {
                                            return $crate::private::Result::Ok(())
                                        }
                                    }
                                }
                            )*
                            $crate::private::Result::Err(lookahead1.error())
                        })?;
                    }
                }

                $crate::private::Result::Ok($name {
                    $(
                        $fname: match $fname {
                            $crate::private::Option::None => <$ftype as ::core::default::Default>::default(),
                            $crate::private::Option::Some($fname) => $fname,
                        },
                    )*
                })
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::{EasyAttributes, Parenthesized};

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

    #[allow(warnings)]
    enum bool {}

    easy_flags! {
        pub nya as Nya [nyas as Nyas] {
            Puk,
            Pak,
        }
    }

    easy_token!(foo);
    easy_token!(bar);
    easy_token!(baz);

    easy_parse! {
        #[derive(Clone, Debug)]
        pub struct FooInner {
            pub ident: syn::Ident
        }
    }

    easy_parse! {
        #[derive(Clone, Debug)]
        pub struct FooInner2 {
            @ pub eq: syn::Token![=],
            pub ident: syn::Ident
        }
    }

    easy_parse! {
        #[derive(Clone, Debug)]
        pub enum FooVariant {
            Inner(Parenthesized<FooInner>, syn::Token![!]),
            Inner2(FooInner2),
        }
    }

    easy_attribute! {
        #[derive(Clone, Debug)]
        pub struct Foo {
            @pub name: foo,
            pub inner: FooVariant,
        }
    }

    easy_attribute! {
        #[derive(Clone, Debug)]
        pub struct Bar {
            @pub name: bar,
            pub ident: syn::Ident,
        }
    }

    easy_attribute_group! {
        #[derive(Clone, Debug)]
        pub enum Group {
            Foo(Foo),
            Bar(Bar),
        }
    }

    easy_attributes! {
        @(easy)
        #[derive(Clone, Default)]
        pub struct Attributes {
            pub foo: std::option::Option<Foo>,
            pub bar: std::option::Option<Bar>,
        }
    }

    #[test]
    fn test_foo() {
        let attrs = quote::quote!(
            #[easy(foo(a)!)]
            #[easy(bar b)]
        );

        let attrs = syn::parse::Parser::parse2(syn::Attribute::parse_outer, attrs).unwrap();

        let bar = Attributes::parse(&attrs).unwrap();
        match &bar.foo {
            std::option::Option::Some(Foo {
                inner: FooVariant::Inner(inner, _),
                ..
            }) => assert_eq!(inner.ident, "a"),
            std::option::Option::Some(Foo {
                inner: FooVariant::Inner2(inner),
                ..
            }) => assert_eq!(inner.ident, "a"),
            _ => panic!(),
        }

        match &bar.bar {
            std::option::Option::Some(bar) => assert_eq!(bar.ident, "b"),
            std::option::Option::None => panic!(),
        }
    }
}
