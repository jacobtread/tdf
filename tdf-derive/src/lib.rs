use darling::{usage::GenericsExt, FromAttributes};
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

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumAttr {
    pub repr: Ident,
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

#[proc_macro_derive(TdfDeserialize, attributes(tdf))]
pub fn derive_tdf_deserialize(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        generics,
        data,
        attrs,
        ..
    } = parse_macro_input!(input);

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

fn derive_tdf_deserialize_enum(ident: Ident, attrs: Vec<Attribute>, data: DataEnum) -> TokenStream {
    let is_tagged = data
        .variants
        .iter()
        .any(|variant| !variant.fields.is_empty());

    if is_tagged {
        derive_tdf_deserialize_tagged_enum(ident, attrs, data)
    } else {
        derive_tdf_deserialize_repr_enum(ident, attrs, data)
    }
}

fn derive_tdf_deserialize_tagged_enum(
    ident: Ident,
    attrs: Vec<Attribute>,
    data: DataEnum,
) -> TokenStream {
    quote!().into()
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumVariantAttr {
    #[darling(default)]
    pub default: bool,
}

fn derive_tdf_deserialize_repr_enum(
    ident: Ident,
    attrs: Vec<Attribute>,
    data: DataEnum,
) -> TokenStream {
    let enum_attr = TdfEnumAttr::from_attributes(&attrs).unwrap();
    let repr = enum_attr.repr;

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
