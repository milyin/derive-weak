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
    parse::Parse,
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    visit_mut::{visit_expr_mut, visit_type_mut, visit_type_path_mut, VisitMut},
    Attribute,
    Data::Struct,
    DataStruct, DeriveInput, Expr, Fields, FieldsNamed, GenericArgument, Ident, PathArguments,
    Token, Type, TypePath, Visibility,
};

enum KnownType {
    Rc,
    Arc,
    CArc,
    EArc,
}

fn get_known_type(ty: &Type) -> Option<KnownType> {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last() {
            match last_segment.ident.to_string().as_str() {
                "Rc" => return Some(KnownType::Rc),
                "Arc" => return Some(KnownType::Arc),
                "CArc" => return Some(KnownType::CArc),
                "EArc" => return Some(KnownType::EArc),
            };
        }
    }
    None
}

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

#[derive(Default, Clone)]
struct WeakFieldAttrs {
    weak_type: Option<Type>,
    upgrade_op: Option<Expr>,
    downgrade_op: Option<Expr>,
}

struct WeakDescr {
    weak_type: Type,
    upgrade_op: Expr,
    downgrade_op: Expr,
}

// TODO: currently replaces first occurence of _ placeholder. This can be fixed by addind annotation attribute if necessary
struct ReplaceUnderscoreInType(Option<Type>);

impl VisitMut for ReplaceUnderscoreInType {
    fn visit_type_mut(&mut self, i: &mut Type) {
        if self.0.is_some() {
            if let Type::Infer(_) = i {
                *i = self.0.take().unwrap();
            }
        }
        visit_type_mut(self, i);
    }
}

fn replace_underscore_in_type(dst: &mut Type, src: Type) {
    let mut replacer = ReplaceUnderscoreInType(Some(src));
    visit_type_mut(&mut replacer, dst);
}

struct ReplaceUnderscoreInExpr(Option<Expr>);

impl VisitMut for ReplaceUnderscoreInExpr {
    fn visit_expr_mut(&mut self, i: &mut Expr) {
        if self.0.is_some() {
            if let Expr::Verbatim(_) = *i {
                // TODO: check is there is really '_' in token stream under Verbatim
                *i = self.0.take().unwrap();
            }
        }
        visit_expr_mut(self, i);
    }
}

fn replace_underscore_in_expr(dst: &mut Expr, src: Expr) {
    let mut replacer = ReplaceUnderscoreInExpr(Some(src));
    visit_expr_mut(&mut replacer, dst);
}

impl WeakDescr {
    fn new(known_type: Option<KnownType>) -> Self {
        let (weak_type, upgrade_op, downgrade_op) = match known_type {
            Some(known_type) => match known_type {
                KnownType::Rc => todo!(),
                KnownType::Arc => todo!(),
                KnownType::CArc => todo!(),
                KnownType::EArc => todo!(),
            },
            None => (
                parse_quote! { Weak<_> },
                parse_quote! { _.upgrade() },
                parse_quote! { _.downgrade() },
            ),
        };
        Self {
            weak_type,
            upgrade_op,
            downgrade_op,
        }
    }
}

impl std::ops::AddAssign<WeakFieldAttrs> for WeakFieldAttrs {
    fn add_assign(&mut self, rhs: WeakFieldAttrs) {
        if rhs.weak_type.is_some() {
            self.weak_type = rhs.weak_type
        }
        if rhs.upgrade_op.is_some() {
            self.upgrade_op = rhs.upgrade_op
        }
        if rhs.downgrade_op.is_some() {
            self.downgrade_op = rhs.downgrade_op
        }
    }
}

impl std::ops::AddAssign<WeakFieldAttrs> for WeakDescr {
    fn add_assign(&mut self, rhs: WeakFieldAttrs) {
        if let Some(weak_type) = rhs.weak_type {
            self.weak_type = weak_type
        }
        if let Some(upgrade_op) = rhs.upgrade_op {
            self.upgrade_op = upgrade_op
        }
        if let Some(downgrade_op) = rhs.downgrade_op {
            self.downgrade_op = downgrade_op
        }
    }
}

enum WeakFieldParam {
    Type(Type),
    Upgrade(Expr),
    Downgrade(Expr),
}

impl Parse for WeakFieldParam {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        if ident == "type" {
            Ok(WeakFieldParam::Type(input.parse()?))
        } else if ident == "upgrade" {
            Ok(WeakFieldParam::Upgrade(input.parse()?))
        } else if ident == "downgrade" {
            Ok(WeakFieldParam::Downgrade(input.parse()?))
        } else {
            Err(syn::Error::new(
                input.span(),
                "Unexpected parameter. Values 'type', 'upgrade' and 'downgrade' only allowed",
            ))
        }
    }
}

impl Parse for WeakFieldAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut res = WeakFieldAttrs::default();
        let weak_params = Punctuated::<WeakFieldParam, Token![,]>::parse_separated_nonempty(input)?;
        for param in weak_params {
            match param {
                WeakFieldParam::Type(ty) => res.weak_type = Some(ty),
                WeakFieldParam::Upgrade(expr) => res.upgrade_op = Some(expr),
                WeakFieldParam::Downgrade(expr) => res.downgrade_op = Some(expr),
            }
        }
        Ok(res)
    }
}

fn take_weak_field_attrs(attrs: &mut Vec<Attribute>) -> Option<WeakFieldAttrs> {
    let mut weak_attr_found = false;
    let mut weak_descr = WeakFieldAttrs::default();
    let is_weak_attr = is_attr_named_fn("weak");
    attrs.retain(|attr| {
        if !is_weak_attr(attr) {
            return true;
        } // retain it
        weak_attr_found = true;
        if attr.tokens.is_empty() {
            // empty 'weak' attribute is ok, just remove this attr
            return false;
        }
        let weak_attr_descr = attr.parse_args::<WeakFieldAttrs>().unwrap();
        weak_descr += weak_attr_descr;
        false // handle and remove
    });
    if weak_attr_found {
        Some(weak_descr)
    } else {
        None
    }
}

// Returns wrapped type - for examlpe 'usize' from std::rc::Rc<usize>
// Currently takes first type argument from last type path segment
fn get_wrapped_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last() {
            if let PathArguments::AngleBracketed(angle_bracketed) = last_segment.arguments {
                for argument in &angle_bracketed.args {
                    if let GenericArgument::Type(ty) = argument {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
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
        if let Some(attrs) = take_weak_field_attrs(&mut field.attrs) {
            let mut weak_descr = WeakDescr::new(get_known_type(&field.ty));
            weak_descr += attrs;

            if let Some(wrapped_type) = get_wrapped_type(&field.ty) {
                replace_underscore_in_type(&mut weak_descr.weak_type, wrapped_type.clone());
            }
            replace_underscore_in_expr(&mut weak_descr.downgrade_op, parse_quote! {self.#ident});
            replace_underscore_in_expr(&mut weak_descr.upgrade_op, parse_quote! {self.#ident});

            upgrade_cmds.push(quote! {
                let #ident = if let Some(v) = #(weak_descr.upgrade_op) {
                    v
                } else {
                    return None;
                };
            });
            upgrades.push(quote! {#ident});
            downgrades.push(quote! { #ident: #(weak_descr.downgrade_op) });
            field.ty = weak_descr.weak_type;
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

//
// unused
//

// std::sync::Arc<Foo>, Weak -> Weak<Foo>
fn override_type_path(type_path: &mut TypePath, mut new_type_path: TypePath) {
    if let Some(last_segment) = type_path.path.segments.last_mut() {
        let new_type_path_arguments = &mut new_type_path
            .path
            .segments
            .last_mut()
            .expect("type must not be empty")
            .arguments;
        swap(new_type_path_arguments, &mut last_segment.arguments);
    }
    *type_path = new_type_path;
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

// Replaces ident in last segment of type path
// I.e. replace_last_ident_of_type(std::sync::Arc<Foo>, Weak) -> std::sync::Weak<Foo>
fn replace_last_ident_of_type(ty: &mut Type, name: &str) {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last_mut() {
            last_segment.ident = Ident::new(name, last_segment.ident.span());
        } else {
            panic!("Path types must not be empty");
        }
    } else {
        panic!("Only path types can be replaced to weak type")
    }
}
