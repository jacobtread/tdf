use std::f32::consts::E;

use darling::{usage::GenericsExt, FromAttributes, FromDeriveInput};
use proc_macro::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, Attribute, DataEnum, DataStruct, DeriveInput, Expr, Generics, Ident,
    Lifetime, LifetimeParam, Type,
};

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfFieldAttr {
    pub tag: Expr,
    #[darling(default)]
    pub skip: bool,
}

#[proc_macro_derive(TdfSerialize, attributes(tdf))]
pub fn derive_tdf_serialize(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    match &input.data {
        syn::Data::Struct(data) => derive_tdf_serialize_struct(&input, data),
        syn::Data::Enum(data) => derive_tdf_serialize_repr_enum(&input, data),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_tdf_serialize_struct(input: &DeriveInput, data: &DataStruct) -> TokenStream {
    let ident = &input.ident;
    let generics = &input.generics;
    let where_clause = generics.where_clause.as_ref();

    let field_impls = data
        .fields
        .iter()
        .map(|field| {
            let attr: TdfFieldAttr = TdfFieldAttr::from_attributes(&field.attrs)
                .expect("Failed to parse tdf field attrs");

            (field, attr)
        })
        .filter(|(_, attr)| !attr.skip)
        .map(|(field, attr)| {
            let field_ident: Option<&Ident> = field.ident.as_ref();
            let field_ty = &field.ty;
            let tag = attr.tag;

            // TODO: Validate tags

            quote! {
                Tagged::serialize_raw(w, #tag, <#field_ty>::TYPE);
                <#field_ty as tdf::TdfSerialize>::serialize(&self.#field_ident, w);
            }
        });

    quote! {
        impl #generics TdfSerialize for #ident #generics #where_clause {
            fn serialize<S: TdfSerializer>(&self, w: &mut S) {
                #(#field_impls)*
            }
        }
    }
    .into()
}

fn get_repr_attribute(attrs: &[Attribute]) -> Option<Ident> {
    attrs
        .iter()
        .filter_map(|attr| attr.meta.require_list().ok())
        .find(|value| value.path.is_ident("repr"))
        .map(|attr| {
            let value: Ident = attr.parse_args().expect("Failed to parse repr type");
            value
        })
}

fn derive_tdf_serialize_repr_enum(input: &DeriveInput, _data: &DataEnum) -> TokenStream {
    let ident = &input.ident;
    let repr = get_repr_attribute(&input.attrs)
        .expect("Non-tagged enums require #[repr({ty})] to be specified");

    quote! {
        impl TdfSerializeOwned for #ident {
            fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
                <#repr as tdf::TdfSerializeOwned>::serialize_owned(self as #repr, w);
            }
        }

        impl TdfTyped for #ident {
            const TYPE: TdfType = <#repr as TdfTyped>::TYPE;
        }
    }
    .into()
}

#[proc_macro_derive(TdfDeserialize, attributes(tdf))]
pub fn derive_tdf_deserialize(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    match &input.data {
        syn::Data::Struct(data) => derive_tdf_deserialize_struct(&input, data),
        syn::Data::Enum(data) => derive_tdf_deserialize_enum(&input, data),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_tdf_deserialize_struct(input: &DeriveInput, data: &DataStruct) -> TokenStream {
    let ident = &input.ident;
    let generics = &input.generics;

    let mut lifetimes = input.generics.lifetimes();
    let lifetime = lifetimes.next();

    if lifetimes.next().is_some() {
        panic!(
            "{} has more than one lifetime, cannot derive TdfDeserialize",
            input.ident
        );
    }

    let where_clause = generics.where_clause.as_ref();

    let field_idents = data.fields.iter().filter_map(|field| field.ident.as_ref());

    let field_impls = data
        .fields
        .iter()
        .map(|field| {
            let attr: TdfFieldAttr = TdfFieldAttr::from_attributes(&field.attrs)
                .expect("Failed to parse tdf field attrs");

            (field, attr)
        })
        .filter(|(_, attr)| !attr.skip)
        .map(|(field, attr)| {
            let field_ident = field.ident.as_ref().unwrap();
            let field_ty = &field.ty;
            let tag = attr.tag;

            // TODO: Validate tags

            quote! {
                let #field_ident = r.tag::<#field_ty>(#tag)?;
            }
        });

    let default_lifetime = LifetimeParam::new(Lifetime::new("'_", Span::call_site().into()));

    let lifetime = lifetime.unwrap_or(&default_lifetime);

    quote! {
        impl #generics TdfDeserialize<#lifetime> for #ident #generics #where_clause {
            fn deserialize(r: &mut TdfDeserializer<#lifetime>) -> DecodeResult<Self> {
                #(#field_impls)*

                Ok(Self {
                    #(#field_idents)*
                })
            }
        }

    }
    .into()
}

fn derive_tdf_deserialize_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    let is_tagged = data
        .variants
        .iter()
        .any(|variant| !variant.fields.is_empty());

    if is_tagged {
        derive_tdf_deserialize_tagged_enum(input, data)
    } else {
        derive_tdf_deserialize_repr_enum(input, data)
    }
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumVariantAttr {
    #[darling(default)]
    pub default: bool,
}

fn derive_tdf_deserialize_repr_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    dbg!(&input.attrs);
    let repr = get_repr_attribute(&input.attrs)
        .expect("Non-tagged enums require #[repr({ty})] to be specified");

    let mut default = None;

    let variant_cases: Vec<_> = data
        .variants
        .iter()
        .map(|variant| {
            let attr = TdfEnumVariantAttr::from_attributes(&variant.attrs)
                .expect("Failed to parse tdf enum variant attrs");
            (variant, attr)
        })
        .filter(|(variant, attr)| {
            if !attr.default {
                return true;
            }

            if default.is_some() {
                panic!("Cannot have more than one default variant");
            }

            let ident = &variant.ident;

            default = Some(quote!(_ => Self::#ident));

            false
        })
        .map(|(variant, _attr)| {
            let var_ident = &variant.ident;
            let (_, discriminant) = variant
                .discriminant
                .as_ref()
                .expect("Repr enum variants must include a descriminant for each value");

            quote! {
                #discriminant => Self::#var_ident,
            }
        })
        .collect();

    let ident = &input.ident;
    let default = default.unwrap_or_else(
        || quote!(_ => return Err(tdf::DecodeError::Other("Missing fallback enum variant"))),
    );

    quote! {
        impl TdfDeserialize<'_> for #ident {
            fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
                let value = <#repr>::deserialize(r)?;
                Ok(match value {
                    #(#variant_cases)*
                    #default
                })
            }
        }

    }
    .into()
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfTaggedEnumVariantAttr {
    pub key: Option<Expr>,

    #[darling(default)]
    pub tag: Option<Expr>,

    #[darling(default)]
    pub default: bool,

    #[darling(default)]
    pub unset: bool,
}

fn derive_tdf_deserialize_tagged_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    let mut unset_handling = None;
    let mut default_handling = None;

    let field_impls: Vec<_> = data
        .variants
        .iter()
        .map(|variant| {
            let attr: TdfTaggedEnumVariantAttr =
                TdfTaggedEnumVariantAttr::from_attributes(&variant.attrs)
                    .expect("Failed to parse tdf field attrs");

            (variant, attr)
        })
        .filter(|(variant, attr)| {
            if attr.unset || attr.default {
                let var_ident = &variant.ident;
                assert!(
                    matches!(variant.fields, syn::Fields::Unit),
                    "Default/Unset enum values must use the unit structure"
                );

                if attr.unset {
                    unset_handling = quote! {
                        return Ok(Self::#var_ident);
                    }
                    .into();
                } else {
                    default_handling = quote! {
                        _ => Self::#var_ident
                    }
                    .into();
                }

                false
            } else {
                true
            }
        })
        .map(|(variant, attr)| {
            let var_ident = &variant.ident;
            let discriminant = attr
                .key
                .expect("Non default/unset enum variants must have a discriminant key");
            let value_tag = attr.tag;

            // TODO: Ensure no duplicates

            // TODO: Validate value tag matches

            match &variant.fields {
                syn::Fields::Named(fields) => {
                    // Named variants must be group type

                    value_tag.expect("Named tagged enums are groups and need a value tag");

                    let field_idents = fields.named.iter().filter_map(|field| field.ident.as_ref());
                    let field_impls = fields.named.iter().map(|field| {
                        let attr: TdfFieldAttr = TdfFieldAttr::from_attributes(&field.attrs)
                            .expect("Failed to parse tdf field attrs");

                        let field_ident = field.ident.as_ref().unwrap();
                        let field_ty = &field.ty;
                        let tag = attr.tag;

                        quote! {
                            let #field_ident = r.tag::<#field_ty>(#tag)?;
                        }
                    });

                    quote! {
                        #discriminant => {

                            tdf::GroupSlice::deserialize_prefix_two(r)?;

                            #(#field_impls)*

                            tdf::GroupSlice::deserialize_content_skip(r)?;

                            Self::#var_ident {
                                #(#field_idents)*
                            }
                        },
                    }
                }
                // Unnamed may not be group type, use type from value
                syn::Fields::Unnamed(fields) => {
                    let fields = &fields.unnamed;
                    if fields.len() > 1 {
                        panic!("Tagged union cannot have more than one unnamed field");
                    }
                    let field = fields.first().unwrap();
                    let field_ty = &field.ty;

                    quote! {
                        #discriminant => {
                            let value = <#field_ty as tdf::TdfDeserialize<'_>>::deserialize(r)?;
                            Self::#var_ident(value)
                        },
                    }
                }
                syn::Fields::Unit => {
                    quote! {
                        #discriminant => {
                            tag.ty.skip(r)?;
                            Self::#var_ident
                        },
                    }
                }
            }
        })
        .collect();

    let unset_handling = unset_handling.unwrap_or_else(
        || quote!(return Err(tdf::DecodeError::Other("Missing unset enum variant"));),
    );

    let default_handling = default_handling.unwrap_or_else(
        || quote!(_ => return Err(tdf::DecodeError::Other("Missing default enum variant"))),
    );

    let ident = &input.ident;

    quote! {
        impl tdf::TdfDeserialize<'_> for #ident {
            fn deserialize(r: &mut tdf::TdfDeserializer<'_>) -> DecodeResult<Self> {
                let discriminant = <u8 as tdf::TdfDeserialize<'_>>::deserialize(r)?;

                if discriminant == tdf::types::tagged_union::TAGGED_UNSET_KEY {
                    #unset_handling
                }

                let tag = tdf::Tagged::deserialize_owned(r)?;

                Ok(match discriminant {
                    #(#field_impls)*
                    #default_handling
                })
            }
        }
    }
    .into()
}
