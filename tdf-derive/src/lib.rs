use darling::FromAttributes;
use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, Attribute, DataEnum, DataStruct, DeriveInput, Expr, Field, Generics, Ident,
    Lifetime, LifetimeParam,
};

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfFieldAttrs {
    tag: Option<Expr>,
    #[darling(default)]
    skip: bool,
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfStructAttr {
    #[darling(default)]
    group: bool,
    #[darling(default)]
    prefix_two: bool,
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfEnumVariantAttr {
    #[darling(default)]
    default: bool,
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(tdf), forward_attrs(allow, doc, cfg))]
struct TdfTaggedEnumVariantAttr {
    pub key: Option<Expr>,

    #[darling(default)]
    pub tag: Option<Expr>,

    #[darling(default)]
    pub prefix_two: bool,

    #[darling(default)]
    pub default: bool,

    #[darling(default)]
    pub unset: bool,
}

#[proc_macro_derive(TdfSerialize, attributes(tdf))]
pub fn derive_tdf_serialize(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    match &input.data {
        syn::Data::Struct(data) => impl_serialize_struct(&input, data),
        syn::Data::Enum(data) => {
            if is_enum_tagged(data) {
                impl_serialize_tagged_enum(&input, data)
            } else {
                impl_serialize_repr_enum(&input, data)
            }
        }
        syn::Data::Union(_) => panic!("TdfSerialize cannot be implemented on union types"),
    }
}

#[proc_macro_derive(TdfTyped, attributes(tdf))]
pub fn derive_tdf_typed(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    match &input.data {
        syn::Data::Struct(data) => impl_type_struct(&input, data),
        syn::Data::Enum(data) => {
            if is_enum_tagged(data) {
                impl_type_tagged_enum(&input, data)
            } else {
                impl_type_repr_enum(&input, data)
            }
        }
        syn::Data::Union(_) => panic!("TdfTyped cannot be implemented on union types"),
    }
}

#[proc_macro_derive(TdfDeserialize, attributes(tdf))]
pub fn derive_tdf_deserialize(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    match &input.data {
        syn::Data::Struct(data) => impl_deserialize_struct(&input, data),
        syn::Data::Enum(data) => {
            if is_enum_tagged(data) {
                impl_deserialize_tagged_enum(&input, data)
            } else {
                impl_deserialize_repr_enum(&input, data)
            }
        }

        syn::Data::Union(_) => panic!("TdfDeserialize cannot be implemented on union types"),
    }
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

/// Determines whether an enum should be considered to be a Tagged Union
/// rather than a repr enum. Any enum types that have fields cannot be
/// repr types and thus must be Tagged Union's
fn is_enum_tagged(data: &DataEnum) -> bool {
    data.variants
        .iter()
        .any(|variant| !variant.fields.is_empty())
}

fn impl_type_struct(input: &DeriveInput, _data: &DataStruct) -> TokenStream {
    let attr =
        TdfStructAttr::from_attributes(&input.attrs).expect("Failed to parse tdf struct attrs");

    assert!(
        attr.group,
        "Cannot derive TdfTyped on non group struct, type is unknown"
    );

    let ident = &input.ident;
    let generics = &input.generics;
    let where_clause = generics.where_clause.as_ref();

    quote! {
        impl #generics tdf::TdfTyped for #ident #generics #where_clause {
            const TYPE: tdf::TdfType = tdf::TdfType::Group;
        }
    }
    .into()
}

fn impl_type_repr_enum(input: &DeriveInput, _data: &DataEnum) -> TokenStream {
    let ident = &input.ident;
    let repr = get_repr_attribute(&input.attrs)
        .expect("Non-tagged enums require #[repr({ty})] to be specified");

    quote! {
        impl TdfTyped for #ident {
            const TYPE: TdfType = <#repr as TdfTyped>::TYPE;
        }
    }
    .into()
}

fn impl_type_tagged_enum(input: &DeriveInput, _data: &DataEnum) -> TokenStream {
    let ident = &input.ident;

    let generics = &input.generics;
    let where_clause = generics.where_clause.as_ref();

    quote! {
        impl #generics tdf::TdfTyped for #ident #generics #where_clause {
            const TYPE: tdf::TdfType = tdf::TdfType::TaggedUnion;
        }
    }
    .into()
}

fn tag_field_serialize(
    field: &Field,
    tag: Option<Expr>,
    is_struct: bool,
) -> proc_macro2::TokenStream {
    let tag = tag.expect("Fields that arent skipped must specify a tag");
    let ident = &field.ident;
    let ty = &field.ty;

    // TODO: Validate tag

    if is_struct {
        quote!( w.tag_ref::<#ty>(#tag, &self.#ident); )
    } else {
        quote! ( w.tag_ref::<#ty>(#tag, #ident); )
    }
}

fn impl_serialize_struct(input: &DeriveInput, data: &DataStruct) -> TokenStream {
    let attr =
        TdfStructAttr::from_attributes(&input.attrs).expect("Failed to parse tdf struct attrs");
    let ident = &input.ident;
    let generics = &input.generics;
    let where_clause = generics.where_clause.as_ref();

    let serialize_impls = data.fields.iter().filter_map(|field| {
        let attributes =
            TdfFieldAttrs::from_attributes(&field.attrs).expect("Failed to parse tdf field attrs");
        if attributes.skip {
            None
        } else {
            Some(tag_field_serialize(field, attributes.tag, true))
        }
    });

    let mut leading = None;
    let mut trailing = None;

    if attr.group {
        if attr.prefix_two {
            leading = Some(quote! { w.write_byte(2); });
        }

        trailing = Some(quote!( w.tag_group_end();));
    }

    quote! {
        impl #generics TdfSerialize for #ident #generics #where_clause {
            fn serialize<S: TdfSerializer>(&self, w: &mut S) {
                #leading
                #(#serialize_impls)*
                #trailing
            }
        }
    }
    .into()
}

fn impl_serialize_repr_enum(input: &DeriveInput, _data: &DataEnum) -> TokenStream {
    let ident = &input.ident;
    let repr = get_repr_attribute(&input.attrs)
        .expect("Non-tagged enums require #[repr({ty})] to be specified");

    quote! {
        impl TdfSerializeOwned for #ident {
            fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
                <#repr as tdf::TdfSerializeOwned>::serialize_owned(self as #repr, w);
            }
        }
    }
    .into()
}

fn impl_serialize_tagged_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    let ident = &input.ident;

    let field_impls: Vec<_> = data
        .variants
        .iter()
        .map(|variant| {
            let attr: TdfTaggedEnumVariantAttr =
                TdfTaggedEnumVariantAttr::from_attributes(&variant.attrs)
                    .expect("Failed to parse tdf field attrs");

            (variant, attr)
        })
        .map(|(variant, attr)| {
            let var_ident = &variant.ident;
            let value_tag = attr.tag;

            // TODO: Ensure no duplicates

            // TODO: Validate value tag matches

            match &variant.fields {
                syn::Fields::Named(fields) => {
                    assert!(
                        !attr.unset,
                        "Enum variants with fields cannot be used as the unset variant"
                    );

                    let discriminant = attr.key.expect("Missing discriminant key");
                    let value_tag = value_tag.expect("Missing value tag");

                    let mut skip_impl: Option<proc_macro2::TokenStream> = None;
                    let mut field_idents: Vec<&Ident> = Vec::new();
                    let mut serialize_impls: Vec<proc_macro2::TokenStream> = Vec::new();

                    for field in &fields.named {
                        let attributes = TdfFieldAttrs::from_attributes(&field.attrs)
                            .expect("Failed to parse tdf field attrs");
                        if attributes.skip {
                            if skip_impl.is_none() {
                                skip_impl = Some(quote!(, ..))
                            }
                            continue;
                        }

                        let ident = field.ident.as_ref().expect("Field missing ident");

                        field_idents.push(ident);
                        serialize_impls.push(tag_field_serialize(field, attributes.tag, false));
                    }

                    if field_idents.is_empty() && skip_impl.is_some() {
                        skip_impl = Some(quote!(..));
                    }

                    let mut leading = None;

                    if attr.prefix_two {
                        leading = Some(quote!( w.write_byte(2); ))
                    }

                    quote! {
                        Self::#var_ident { #(#field_idents),* #skip_impl } => {

                            w.write_byte(#discriminant);
                            Tagged::serialize_raw(w, #value_tag, TdfType::Group);

                            #leading

                            #(#serialize_impls)*

                            w.tag_group_end();
                        }
                    }
                }
                // Unnamed may not be group type, use type from value
                syn::Fields::Unnamed(fields) => {
                    assert!(
                        !attr.unset,
                        "Enum variants with fields cannot be used as the unset variant"
                    );

                    let discriminant = attr
                        .key
                        .expect("Non default/unset enum variants must have a discriminant key");
                    let value_tag =
                        value_tag.expect("Unnamed tagged enum variants need a value tag");

                    let fields = &fields.unnamed;

                    assert!(
                        fields.len() == 1,
                        "Tagged union cannot have more than one unnamed field"
                    );

                    let field = fields.first().unwrap();
                    let field_ty = &field.ty;

                    quote! {
                        Self::#var_ident(value) => {
                            w.write_byte(#discriminant);
                            Tagged::serialize_raw(w, #value_tag, <#field_ty as TdfTyped>::TYPE);
                            <#field_ty as tdf::TdfSerialize>::serialize(value, w);
                        }
                    }
                }
                syn::Fields::Unit => {
                    assert!(
                        attr.unset || attr.default,
                        "Only unset or default enum variants can have no content"
                    );

                    quote! {
                        Self::#var_ident => {
                            w.write_byte(tdf::types::tagged_union::TAGGED_UNSET_KEY);
                        }
                    }
                }
            }
        })
        .collect();

    quote! {
        impl tdf::TdfSerialize for #ident {
            fn serialize<S: tdf::TdfSerializer>(&self, w: &mut S) {
                match self {
                    #(#field_impls),*
                }
            }
        }
    }
    .into()
}

/// Obtains the lifetime that should be used by [tdf::TdfDeserializer] when
/// deserializing values. If the generic parameters specify a lifetime then
/// that lifetime is used otherwise the default lifetime '_ is used instead
///
/// Will panic if structure uses more than 1 lifetime as its not possible
/// to deserialize with more than one lifetime
fn get_deserialize_lifetime(generics: &Generics) -> LifetimeParam {
    let mut lifetimes = generics.lifetimes();

    let lifetime = lifetimes
        .next()
        .cloned()
        // Use a default '_ lifetime while deserializing when no lifetime is provided
        .unwrap_or_else(|| LifetimeParam::new(Lifetime::new("'_", Span::call_site().into())));

    assert!(
        lifetimes.next().is_none(),
        "Deserializable structs cannot have more than one lifetime"
    );

    lifetime
}

/// Creates a token stream for deserializing the provided `field`
/// loads the tag and whether
fn tag_field_deserialize(field: &Field) -> proc_macro2::TokenStream {
    let attributes =
        TdfFieldAttrs::from_attributes(&field.attrs).expect("Failed to parse tdf field attrs");

    let ident = &field.ident;
    let ty = &field.ty;

    if attributes.skip {
        quote!( let #ident = Default::default(); )
    } else {
        let tag = attributes
            .tag
            .expect("Fields that arent skipped must specify a tag");

        // TODO: Validate tag

        quote!( let #ident = r.tag::<#ty>(#tag)?; )
    }
}

fn impl_deserialize_struct(input: &DeriveInput, data: &DataStruct) -> TokenStream {
    let attributes =
        TdfStructAttr::from_attributes(&input.attrs).expect("Failed to parse tdf struct attrs");

    let ident = &input.ident;

    let generics = &input.generics;
    let lifetime = get_deserialize_lifetime(generics);
    let where_clause = generics.where_clause.as_ref();

    let field_idents = data
        .fields
        .iter()
        .filter_map(|field: &Field| field.ident.as_ref());
    let field_impls = data.fields.iter().map(tag_field_deserialize);

    let mut leading = None;
    let mut trailing = None;

    // Groups need leading deserialization for possible prefixes and trailing
    // deserialization to read any unused tags and to read the group end byte
    if attributes.group {
        leading = Some(quote!( tdf::GroupSlice::deserialize_prefix_two(r)?; ));
        trailing = Some(quote!( tdf::GroupSlice::deserialize_content_skip(r)?; ));
    }

    quote! {
        impl #generics tdf::TdfDeserialize<#lifetime> for #ident #generics #where_clause {
            fn deserialize(r: &mut tdf::TdfDeserializer<#lifetime>) -> tdf::DecodeResult<Self> {
                #leading
                #(#field_impls)*
                #trailing
                Ok(Self {
                    #(#field_idents)*
                })
            }
        }
    }
    .into()
}

fn impl_deserialize_repr_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
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

            assert!(
                default.is_none(),
                "Cannot have more than one default variant"
            );

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

fn impl_deserialize_tagged_enum(input: &DeriveInput, data: &DataEnum) -> TokenStream {
    let mut unset_handling = None;
    let mut default_handling = None;

    let generics = &input.generics;
    let lifetime = get_deserialize_lifetime(generics);
    let where_clause = generics.where_clause.as_ref();

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
                    assert!(
                        !attr.unset,
                        "Enum variants with fields cannot be used as the unset variant"
                    );

                    // Named variants must be group type

                    value_tag.expect("Named tagged enums are groups and need a value tag");

                    let field_idents = fields.named.iter().filter_map(|field| field.ident.as_ref());
                    let field_impls = fields.named.iter().map(tag_field_deserialize);

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
                    assert!(
                        !attr.unset,
                        "Enum variants with fields cannot be used as the unset variant"
                    );

                    let fields = &fields.unnamed;

                    assert!(
                        fields.len() == 1,
                        "Tagged union cannot have more than one unnamed field"
                    );

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
                    assert!(
                        attr.unset || attr.default,
                        "Only unset or default enum variants can have no content"
                    );

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
        impl #generics TdfDeserialize<#lifetime> for #ident #generics #where_clause {
            fn deserialize(r: &mut TdfDeserializer<#lifetime>) -> DecodeResult<Self> {
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
