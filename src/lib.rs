//! # Derive Weak
//!
//! Adds derive macro #[derive(Weak)] which creates 'weak' counterpart for the structure. I.e. when original structure
//! contains reference counting pointers ([std::sync::Arc], [std::rc::Rc]), the corresponding weak structure contains the weak variants
//! of these pointers ([std::sync::Weak], [std::rc::Weak]). This is useful when it's inconvenient to store some data under refernce counting
//! pointer, due to performance or ergonomic reasons.
//!

use proc_macro::TokenStream;
// use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro_derive(Weak, attributes(weak_name, weak))]
pub fn derive_weak(_input: TokenStream) -> TokenStream {
    // let input: DeriveInput = parse_macro_input!(input);
    quote! {
        struct WFoo {
            foo: usize,
            bar: std::rc::Weak<usize>
        }

        impl Foo {
            pub fn downgrade(&self) -> WFoo {
                WFoo {
                    foo: self.foo.clone(),
                    bar: std::rc::Rc::downgrade(&self.bar)
                }
            }
        }

        impl WFoo {
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
