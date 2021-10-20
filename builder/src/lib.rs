#![allow(unused)]
use proc_macro::TokenStream;
use quote;
use syn::{parse_macro_input, AttributeArgs, DeriveInput, Item, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;
    let aa = parse_macro_input!(input as DeriveInput);
    println!("{:#?}\naaa", aa);
    TokenStream::new()
    // let a = quote::quote! {
    // //    println!("aaaaaa");
    // };
    // a.into()
    // input
    // unimplemented!()
}
#[proc_macro_attribute]
pub fn proc_attr_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("{:#?}", parse_macro_input!(attr as AttributeArgs));
    println!("{:#?}", parse_macro_input!(item as Item));
    TokenStream::new()
}
