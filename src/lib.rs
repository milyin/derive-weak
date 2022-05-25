//! # Derive Weak
//!
//! Adds derive macro #[derive(Weak)] which creates 'weak' counterpart for the structure. I.e. when original structure
//! contains reference counting pointers ([std::sync::Arc], [std::rc::Rc]), the corresponding weak structure contains the weak variants
//! of these pointers ([std::sync::Weak], [std::rc::Weak]). This is useful when it's inconvenient to store some data under refernce counting
//! pointer, due to performance or ergonomic reasons.
//!

use std::mem::swap;

use quote::quote;
use syn::{
    parse::Parse, parse_macro_input, Attribute, Data::Struct, DataStruct, DeriveInput, Expr,
    Fields, FieldsNamed, Ident, Type, TypePath, Visibility,
};

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

// Replaces type path in ty to new type path, keeping wrapped type if it exists
// I.e. repalace_wrapper_type(std::sync::Arc<Foo>, Weak) -> Weak<Foo>
fn replace_wrapper_type(ty: &mut Type, mut new_type_path: TypePath) {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last_mut() {
            let new_type_path_arguments = &mut new_type_path
                .path
                .segments
                .last_mut()
                .expect("type must not be empty")
                .arguments;
            swap(new_type_path_arguments, &mut last_segment.arguments);
            *ty = Type::Path(new_type_path)
        } else {
            panic!("Path types must not be empty");
        }
    } else {
        panic!("Only path types can be replaced to weak type")
    };
}

fn derive_named_fields_struct(
    vis: Visibility,
    ident: Ident,
    weak_ident: Ident,
    fields_named: FieldsNamed,
) -> proc_macro2::TokenStream {
    let mut fields = Vec::new();
    let mut downgrades = Vec::new();
    let mut upgrade_cmds = Vec::new();
    let mut upgrades = Vec::new();
    for mut field in fields_named.named {
        let ident = field.ident.clone().expect("Named field expected");

        if let Some(weak_type) = take_attr("weak", &mut field.attrs) {
            replace_wrapper_type(&mut field.ty, weak_type);

            //
            // TODO: handle defauls for std Arc, Rc, etc.
            //
            let upgrade_op = take_attr::<Expr>("upgrade", &mut field.attrs)
                .map_or(quote! {self.#ident.upgrade()}, |expr| quote! {#expr});

            let downgrade_op = take_attr::<Expr>("downgrade", &mut field.attrs)
                .map_or(quote! {self.#ident.downgrade()}, |expr| quote! {#expr});

            upgrade_cmds.push(quote! {
                let #ident = if let Some(v) = #upgrade_op {
                    v
                } else {
                    return None;
                };
            });
            upgrades.push(quote! {#ident});
            downgrades.push(quote! { #ident: #downgrade_op });
            fields.push(field)
        } else {
            upgrades.push(quote! {#ident: self.#ident.clone()});
            downgrades.push(quote! { #ident: self.#ident.clone() });
            fields.push(field);
        }
    }

    quote! {
        #vis struct #weak_ident {
            #(#fields,)*
        }

        impl #ident {
            pub fn downgrade(&self) -> #weak_ident {
                #weak_ident {
                    #(#downgrades,)*
                }
            }
        }

        impl #weak_ident {
            pub fn upgrade(&self) -> Option<#ident> {
                #(#upgrade_cmds)*
                Some(#ident {
                    #(#upgrades,)*
                })
            }
        }
    }
}

#[proc_macro_derive(Weak, attributes(weak, upgrade, downgrade))]
pub fn derive_weak(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input: DeriveInput = parse_macro_input!(input);
    let weak_ident = take_attr("weak", &mut input.attrs).unwrap_or(Ident::new(
        format!("W{}", input.ident).as_str(),
        input.ident.span(),
    ));
    match input.data {
        Struct(DataStruct {
            fields: Fields::Named(fields_named),
            ..
        }) => derive_named_fields_struct(input.vis, input.ident, weak_ident, fields_named),
        Struct(DataStruct {
            fields: Fields::Unnamed(_fields_unnamed),
            ..
        }) => {
            todo!()
        }
        Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => {
            todo!()
        }
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => panic!("derive(Weak) not supported for union"),
    }
    .into()
}
