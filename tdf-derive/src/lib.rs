use std::f32::consts::E;

use darling::{usage::GenericsExt, FromAttributes, FromDeriveInput};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Attribute, DataEnum, DataStruct, DeriveInput, Expr, Generics, Ident, Type,
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
    let DeriveInput {
        ident,
        generics,
        data,
        attrs,
        ..
    } = parse_macro_input!(input);

    match data {
        syn::Data::Struct(data) => derive_tdf_serialize_struct(ident, generics, data),
        syn::Data::Enum(data) => derive_tdf_serialize_enum(ident, attrs, data),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_tdf_serialize_struct(ident: Ident, generics: Generics, data: DataStruct) -> TokenStream {
    let where_clause = generics.where_clause.as_ref();

    let field_impls = data
        .fields
        .into_iter()
        .map(|field| {
            let attr: TdfFieldAttr = TdfFieldAttr::from_attributes(&field.attrs)
                .expect("Failed to parse tdf field attrs");

            (field, attr)
        })
        .filter(|(_, attr)| !attr.skip)
        .map(|(field, attr)| {
            let field_ident: Option<Ident> = field.ident;
            let field_ty = field.ty;
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

fn derive_tdf_serialize_enum(ident: Ident, attrs: Vec<Attribute>, data: DataEnum) -> TokenStream {
    let enum_attr = TdfEnumAttr::from_attributes(&attrs).unwrap();
    let repr = enum_attr.repr;

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

#[derive(FromDeriveInput)]
#[darling(attributes(repr), forward_attrs(allow, doc, cfg))]
struct DeserializeOpts {
    pub repr: Option<Ident>,
}

#[proc_macro_derive(TdfDeserialize, attributes(tdf))]
pub fn derive_tdf_deserialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let opts = DeserializeOpts::from_derive_input(&input).unwrap();
    let DeriveInput {
        ident,
        generics,
        data,
        attrs,
        ..
    } = input;

    match data {
        syn::Data::Struct(data) => derive_tdf_deserialize_struct(ident, generics, data),
        syn::Data::Enum(data) => derive_tdf_deserialize_enum(ident, attrs, data),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_tdf_deserialize_struct(
    ident: Ident,
    generics: Generics,
    data: DataStruct,
) -> TokenStream {
    let mut lifetimes = generics.lifetimes();
    let lifetime = lifetimes.next();

    if lifetimes.next().is_some() {
        panic!(
            "{} has more than one lifetime, cannot derive TdfDeserialize",
            ident
        );
    }

    let where_clause = generics.where_clause.as_ref();

    let mut field_idents: Vec<Ident> = Vec::new();

    let field_impls = data
        .fields
        .into_iter()
        .map(|field| {
            let attr: TdfFieldAttr = TdfFieldAttr::from_attributes(&field.attrs)
                .expect("Failed to parse tdf field attrs");

            (field, attr)
        })
        .filter(|(_, attr)| !attr.skip)
        .map(|(field, attr)| {
            let field_ident = field.ident.unwrap();
            let field_ty = field.ty;
            let tag = attr.tag;

            // TODO: Validate tags
            field_idents.push(field_ident.clone());

            quote! {
                let #field_ident = r.tag::<#field_ty>(#tag)?;
            }
        });

    if let Some(lifetime) = lifetime {
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
    } else {
        quote! {
            impl #generics TdfDeserialize<'_> for #ident #generics #where_clause {
                fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
                    #(#field_impls)*

                    Ok(Self {
                        #(#field_idents)*
                    })
                }
            }
        }
        .into()
    }
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumAttr {
    #[darling(default)]
    pub tagged: bool,
}

fn derive_tdf_deserialize_enum(
    ident: Ident,
    attrs: Vec<Attribute>,
    data: &DataEnum,
) -> TokenStream {
    let attr: TdfEnumAttr =
        TdfEnumAttr::from_attributes(&attrs).expect("Failed to parse tdf field attrs");

    let is_tagged = data
        .variants
        .iter()
        .any(|variant| !variant.fields.is_empty());

    if attr.tagged {
        derive_tdf_deserialize_tagged_enum(ident, attr, data)
    } else {
        derive_tdf_deserialize_repr_enum(ident, attr, data)
    }
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

fn derive_tdf_deserialize_tagged_enum(
    ident: Ident,
    attr: TdfEnumAttr,
    data: DataEnum,
) -> TokenStream {
    let mut unset_handling = None;
    let mut default_handling = None;

    let field_impls: Vec<_> = data
        .variants
        .into_iter()
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
            let var_ident = variant.ident;
            let discriminant = attr
                .key
                .expect("Non default/unset enum variants must have a discriminant key");
            let value_tag = attr.tag;

            // TODO: Ensure no duplicates

            // TODO: Validate value tag matches

            match variant.fields {
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

    let unset_handling = unset_handling.unwrap_or_else(|| {
        quote! {
            return Err(tdf::DecodeError::Other("Missing unset enum variant"));
        }
    });

    let default_handling = default_handling.unwrap_or_else(|| {
        quote! {
            _ => return Err(tdf::DecodeError::Other("Missing default enum variant"))
        }
    });

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

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumVariantAttr {
    #[darling(default)]
    pub default: bool,
}

fn derive_tdf_deserialize_repr_enum(
    ident: Ident,
    attr: TdfEnumAttr,
    data: DataEnum,
) -> TokenStream {
    let repr = attr
        .repr
        .expect("Non tagged enums must specify the repr type (i.e. u8, u16, u32)");

    let default: Option<Ident> = data
        .variants
        .iter()
        .find(|value| {
            let attr: TdfEnumVariantAttr = TdfEnumVariantAttr::from_attributes(&value.attrs)
                .expect("Failed to parse tdf enum variant attrs");
            attr.default
        })
        .map(|value| value.ident.clone());

    let field_impls: Vec<_> = data
        .variants
        .into_iter()
        .map(|variant| {
            let var_ident = variant.ident;
            let (_, discriminant) = variant.discriminant.unwrap();

            quote! {
                #discriminant => Self::#var_ident,
            }
        })
        .collect();

    if let Some(default) = default {
        quote! {
            impl TdfDeserialize<'_> for #ident {
                fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
                    let value = <#repr>::deserialize(r)?;
                    Ok(match value {
                        #(#field_impls)*
                        _ => Self::#default
                    })
                }
            }

        }
        .into()
    } else {
        quote! {
            impl TdfDeserialize<'_> for #ident {
                fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
                    let value = <#repr>::deserialize(r)?;
                    Ok(match value {
                        #(#field_impls)*
                        _ => return Err(tdf::DecodeError::Other("Missing fallback enum variant"))
                    })
                }
            }
        }
        .into()
    }
}
