//! # Derive Weak
//!
//! Adds derive macro #[derive(Weak)] which creates 'weak' counterpart for the structure. I.e. when original structure
//! contains reference counting pointers ([std::sync::Arc], [std::rc::Rc]), the corresponding weak structure contains the weak variants
//! of these pointers ([std::sync::Weak], [std::rc::Weak]). This is useful when it's inconvenient to store some data under refernce counting
//! pointer, due to performance or ergonomic reasons.
//!
mod param;

use param::{take_params, Param};
use quote::quote;
use syn::{
    parse::ParseStream,
    parse_macro_input, parse_quote,
    visit_mut::{visit_expr_mut, visit_type_mut, VisitMut},
    Data::Struct,
    DataStruct, DeriveInput, Expr, Fields, FieldsNamed, GenericArgument, Ident, LitBool,
    PathArguments, Type, Visibility,
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
            if last_segment.ident == "Rc" {
                return Some(KnownType::Rc);
            } else if last_segment.ident == "Arc" {
                return Some(KnownType::Arc);
            } else if last_segment.ident == "CArc" {
                return Some(KnownType::CArc);
            } else if last_segment.ident == "EArc" {
                return Some(KnownType::EArc);
            }
        }
    }
    None
}

fn get_default_templates(known_type: Option<KnownType>) -> (Type, Expr, Expr) {
    match known_type {
        Some(known_type) => match known_type {
            KnownType::Rc => (
                parse_quote! { std::rc::Weak<_> },
                parse_quote! { _.upgrade() },
                parse_quote! { std::rc::Rc::downgrade(&_) },
            ),
            KnownType::Arc => (
                parse_quote! { std::sync::Weak<_> },
                parse_quote! { _.upgrade() },
                parse_quote! { std::sync::Arc::downgrade(&_) },
            ),
            KnownType::CArc => (
                parse_quote! { async_object::WCArc<_> },
                parse_quote! { _.upgrade() },
                parse_quote! { _.downgrade() },
            ),
            KnownType::EArc => (
                parse_quote! { async_object::WEArc },
                parse_quote! { _.upgrade() },
                parse_quote! { _.downgrade() },
            ),
        },
        None => (
            parse_quote! { Weak<_> },
            parse_quote! { _.upgrade() },
            parse_quote! { _.downgrade() },
        ),
    }
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

enum WeakFieldParam {
    Type(Type),
    Upgrade(Expr),
    Downgrade(Expr),
}

impl Param for WeakFieldParam {
    fn default(name: Ident) -> syn::Result<Self> {
        if name == "type" {
            Ok(WeakFieldParam::Type(parse_quote! { Weak<_> }))
        } else if name == "upgrade" {
            Ok(WeakFieldParam::Upgrade(parse_quote! { _.upgrade() }))
        } else if name == "downgrade" {
            Ok(WeakFieldParam::Downgrade(parse_quote! { _.downgrade() }))
        } else {
            Err(syn::Error::new(
                name.span(),
                "Unexpected parameter. Values 'type', 'upgrade' and 'downgrade' only allowed",
            ))
        }
    }

    fn parse(&mut self, input: ParseStream) -> syn::Result<()> {
        match self {
            WeakFieldParam::Type(ref mut ty) => *ty = input.parse()?,
            WeakFieldParam::Upgrade(ref mut expr) => *expr = input.parse()?,
            WeakFieldParam::Downgrade(ref mut expr) => *expr = input.parse()?,
        };
        Ok(())
    }
}

// Returns wrapped type - for examlpe 'usize' from std::rc::Rc<usize>
// Currently takes first type argument from last type path segment
fn get_wrapped_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last() {
            if let PathArguments::AngleBracketed(ref angle_bracketed) = last_segment.arguments {
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
    struct_ident: Ident,
    weak_ident: Ident,
    auto: bool,
    fields_named: FieldsNamed,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut fields = Vec::new();
    let mut downgrades = Vec::new();
    let mut upgrade_cmds = Vec::new();
    let mut upgrades = Vec::new();
    for mut field in fields_named.named {
        let field_ident = field.ident.clone().expect("Named field expected");
        let known_type = if auto {
            get_known_type(&field.ty)
        } else {
            None
        };
        let params = take_params::<WeakFieldParam>("weak", &mut field.attrs)?;
        //
        // Replace field to it's weak counterpart if:
        // - it's type is known to us (Rc, Arc, etc) and auto mode is on
        // - it's explicitly marked to be replaced by #[weak] attribute
        //
        if known_type.is_some() || params.is_some() {
            //
            // Fill weak type, upgrade and downgrade operations from defaults for known type and and override them from attributes
            //
            let (mut weak_type, mut upgrade_op, mut downgrade_op) =
                get_default_templates(known_type);
            if let Some(params) = params {
                for param in params {
                    match param {
                        WeakFieldParam::Type(ty) => weak_type = ty,
                        WeakFieldParam::Upgrade(expr) => upgrade_op = expr,
                        WeakFieldParam::Downgrade(expr) => downgrade_op = expr,
                    }
                }
            }

            if let Some(wrapped_type) = get_wrapped_type(&field.ty) {
                replace_underscore_in_type(&mut weak_type, wrapped_type.clone());
            }
            replace_underscore_in_expr(&mut downgrade_op, parse_quote! {self.#field_ident});
            replace_underscore_in_expr(&mut upgrade_op, parse_quote! {self.#field_ident});

            upgrade_cmds.push(quote! {
                let #field_ident = if let Some(v) = #upgrade_op {
                    v
                } else {
                    return None;
                };
            });
            upgrades.push(quote! {#field_ident});
            downgrades.push(quote! { #field_ident: #downgrade_op });
            field.ty = weak_type;
            fields.push(field)
        } else {
            upgrades.push(quote! {#field_ident: self.#field_ident.clone()});
            downgrades.push(quote! { #field_ident: self.#field_ident.clone() });
            fields.push(field);
        }
    }
    Ok(quote! {
        #vis struct #weak_ident {
            #(#fields,)*
        }

       impl #struct_ident {
            pub fn downgrade(&self) -> #weak_ident {
                #weak_ident {
                    #(#downgrades,)*
                }
            }
        }

        impl #weak_ident {
            pub fn upgrade(&self) -> Option<#struct_ident> {
                #(#upgrade_cmds)*
                Some(#struct_ident {
                    #(#upgrades,)*
                })
            }

        }
    })
    // eprintln!(
    //     "{:#?}",
    //     quote! {
    //                 #(#downgrades,)*
    //     }
    //     .to_string()
    // );
}

#[derive(Debug)]
enum WeakStructParam {
    Name(Option<Ident>),
    Auto(bool),
}

impl Param for WeakStructParam {
    fn default(name: Ident) -> syn::Result<Self> {
        if name == "name" {
            return Ok(WeakStructParam::Name(None));
        } else if name == "auto" {
            return Ok(WeakStructParam::Auto(true));
        } else {
            Err(syn::Error::new(
                name.span(),
                "Unexpected parameter. Allowed values are 'name' and 'auto'",
            ))
        }
    }
    fn parse(&mut self, input: ParseStream) -> syn::Result<()> {
        match self {
            WeakStructParam::Name(ref mut v) => *v = Some(input.parse()?),
            WeakStructParam::Auto(ref mut v) => {
                let b: LitBool = input.parse()?;
                *v = b.value
            }
        }
        Ok(())
    }
}

#[proc_macro_derive(Weak, attributes(weak))]
pub fn derive_weak(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input: DeriveInput = parse_macro_input!(input);
    let mut weak_ident = Ident::new(format!("W{}", input.ident).as_str(), input.ident.span());
    let mut auto = true;
    if let Some(params) = take_params::<WeakStructParam>("weak", &mut input.attrs).unwrap() {
        for param in params {
            match param {
                WeakStructParam::Name(Some(v)) => weak_ident = v,
                WeakStructParam::Name(None) => (),
                WeakStructParam::Auto(v) => auto = v,
            }
        }
    }

    match input.data {
        Struct(DataStruct {
            fields: Fields::Named(fields_named),
            ..
        }) => derive_named_fields_struct(input.vis, input.ident, weak_ident, auto, fields_named)
            .unwrap(),
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
