//! # Derive Weak
//!
//! Adds derive macro #[derive(Weak)] which creates 'weak' counterpart for the structure. I.e. when original structure
//! contains reference counting pointers ([std::sync::Arc], [std::rc::Rc]), the corresponding weak structure contains the weak variants
//! of these pointers ([std::sync::Weak], [std::rc::Weak]). This is useful when it's inconvenient to store some data under refernce counting
//! pointer, due to performance or ergonomic reasons.
//!

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Attribute, DeriveInput, Ident};

fn is_attr_named_fn<'a>(name: &'a str) -> impl Fn(&Attribute) -> bool + 'a {
    move |attr| {
        if let Some(attr_ident) = attr.path.get_ident() {
            if attr_ident == name {
                return true;
            }
        }
        false
    }
}

fn take_attr<T: Parse>(name: &str, attrs: &mut Vec<Attribute>) -> Option<T> {
    if let Some(pos) = attrs.iter().position(is_attr_named_fn(name)) {
        let attr = attrs.remove(pos);
        Some(attr.parse_args::<T>().unwrap())
    } else {
        None
    }
}

#[proc_macro_derive(Weak, attributes(weak_name, weak))]
pub fn derive_weak(input: TokenStream) -> TokenStream {
    let mut input: DeriveInput = parse_macro_input!(input);
    let weak_ident = take_attr("weak_name", &mut input.attrs).unwrap_or(Ident::new(
        format!("W{}", input.ident).as_str(),
        input.ident.span(),
    ));

    quote! {
        struct #weak_ident {
            foo: usize,
            bar: std::rc::Weak<usize>
        }

        impl Foo {
            pub fn downgrade(&self) -> #weak_ident {
                WFoo {
                    foo: self.foo.clone(),
                    bar: std::rc::Rc::downgrade(&self.bar)
                }
            }
        }

        impl #weak_ident {
            pub fn upgrade(&self) -> Option<Foo> {
                let bar = if let Some(v) = self.bar.upgrade() {
                    v
                } else {
                    return None;
                };
                Some(Foo {
                    foo: self.foo.clone(),
                    bar
                })
            }
        }
    }
    .into()
}
