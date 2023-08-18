use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(TaggedUnion, attributes(tdf))]
pub fn derive_tagged_union(input: TokenStream) -> TokenStream {
    quote! {}.into()
}
