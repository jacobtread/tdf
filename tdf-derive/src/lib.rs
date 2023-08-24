use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(TdfDeserialize, attributes(tdf))]
pub fn derive_tdf_deserialize(input: TokenStream) -> TokenStream {
    quote! {}.into()
}

#[proc_macro_derive(TdfUnion, attributes(tdf))]
pub fn derive_tdf_union(input: TokenStream) -> TokenStream {
    quote! {}.into()
}

#[proc_macro_derive(TdfEnum, attributes(tdf))]
pub fn derive_tdf_enum(input: TokenStream) -> TokenStream {
    quote! {}.into()
}
