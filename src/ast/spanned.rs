//! Spanned wrapper for AST nodes.
//!
//! This module provides the [`Spanned`] wrapper type that associates AST nodes
//! with their source location information. This is essential for error reporting,
//! syntax highlighting, and other tooling that needs to map between the AST
//! and the original source code.

use crate::errors::Span;
use std::fmt;

/// A wrapper that associates an AST node with its source location.
///
/// This struct pairs any AST node with a [`Span`] that indicates where
/// in the source code the node was parsed from. This information is
/// crucial for error reporting, IDE features, and source-to-source
/// transformations.
///
/// # Type Parameters
///
/// * `T` - The type of AST node being wrapped
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: Clone> Spanned<T> {
    /// Extract the inner AST node, discarding span information.
    pub fn into_inner(self) -> T {
        self.node
    }

    /// Transform the inner node while preserving span information.
    ///
    /// This is useful when you need to transform an AST node but want
    /// to keep the original source location.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Transform the inner node with a fallible operation while preserving span information.
    ///
    /// If the transformation fails, the error is returned and the span information
    /// is lost. If it succeeds, the new node is wrapped with the original span.
    pub fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Spanned<U>, E> {
        Ok(Spanned {
            node: f(self.node)?,
            span: self.span,
        })
    }
}

/// Display implementation that forwards to the inner node.
///
/// This allows spanned nodes to be formatted the same way as their
/// inner values, making them transparent for display purposes.
impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}
