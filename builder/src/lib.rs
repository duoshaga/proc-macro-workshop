// 参考 https://blog.ideawand.com/2021/03/24/rust_procedural_macro/rust_proc_marco_workshop_guide-02/
#![allow(unused)]
use std::fmt::format;

use proc_macro::TokenStream;
use quote;
use syn::{parse_macro_input, spanned::Spanned, AttributeArgs, DeriveInput, Item};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let syn_devive_input = parse_macro_input!(input as DeriveInput);
    // eprintln!(
    //     "{:#?}",
    //     syn_devive_input.attrs.first().unwrap().parse_meta()
    // );
    // eprintln!("{:#?}", syn_devive_input);
    // TokenStream::new()
    match do_expand(&syn_devive_input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
fn do_expand(sdi: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let source_name = sdi.ident.to_string();
    let builder_name = format!("{}Builder", source_name);
    /*
        span
            标记输入在源代码中的位置信息。
            span信息主要用于发生编译错误时，编译器给用户指示出错误的位置。
            编译器报错时，不会展示过程宏生成的代码，而只会展示用户编写的原始文件代码。
            因此，对于我们通过过程宏产生出来的代码，应该指向用户原始代码文件中的某个位置，而不是凭空指向一个不存在的位置，
            否则后续一旦产生编译器报错，将会产生令人难以理解的错误提示。
            由于我们后续要生成的代码都是由用户输入的原始结构体产生的，所以将原始输入结构体的位置信息当做虚构出的标识符的位置信息，
            后续一旦报错，编译器显示的错误提示将指向用户原始的结构体，从而引导用户有效排查问题。
    */
    let builder_ident = syn::Ident::new(&builder_name, sdi.span());
    let source_ident = &sdi.ident;
    let fields = get_fields_from_device_input(sdi)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    let setter_functions = generate_setter_functions(fields)?;
    let build_function = generate_build_function(fields, source_ident)?;
    let ret = quote::quote! {
        pub struct #builder_ident {
            // 下面这行代码是增增的，注意这里的用法：
            // 在当前这个`quote!`宏中，引用了其他`quote!`宏返回的结果
            // 在这里把不同的代码碎片拼接起来，就像搭积木一样
           #builder_struct_fields_def
        }
        impl #source_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_struct_factory_init_clauses),*
                }
            }
        }
        impl #builder_ident {
            #setter_functions
            #build_function
        }
    };
    Ok(ret)
}
type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;
fn get_fields_from_device_input(sdi: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = sdi.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        sdi,
        "Must define on a Struct, not Enum".to_string(),
    ))
}
fn generate_builder_struct_fields_def(
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            let origin_ty = &f.ty;
            if let Some(inner_ty) = get_generic_inner_type(origin_ty, "Option") {
                Ok(quote::quote! {
                    std::option::Option<#inner_ty>
                })
            } else if get_user_specified_ident_for_vec(f)?.is_some() {
                Ok(quote::quote!(#origin_ty))
            } else {
                Ok(quote::quote!(std::option::Option<#origin_ty>))
            }
            // let origin_ty = &f.ty;
            // match get_generic_inner_type(origin_ty, "Option") {
            //     Some(inner_ty) => quote::quote! {
            //         std::option::Option<#inner_ty>
            //     },
            //     None => quote::quote! {
            //         std::option::Option<#origin_ty>
            //     },
            // }
        })
        .collect();
    let types = types?;
    let ret = quote::quote! {
        #(#idents:#types),*
    };
    Ok(ret)
}
fn generate_builder_struct_factory_init_clauses(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if get_user_specified_ident_for_vec(f)?.is_some() {
                Ok(quote::quote! {
                    #ident:std::vec::Vec::new()
                })
            } else {
                Ok(quote::quote! {
                    #ident:std::option::Option::None
                })
            }
        })
        .collect();
    Ok(init_clauses?)
}

fn generate_setter_functions(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    let mut final_tokenstream = proc_macro2::TokenStream::new();

    // for (idx,(ident, type_)) in idents.iter().zip(types.iter()).enumerate() {
    // for ((ident, _type), field) in idents.iter().zip(types.iter()).zip(fields.iter()) {
    for idx in 0..fields.len() {
        let (ident, _type, field) = (idents[idx], types[idx], &fields[idx]);
        let mut tokenstream_piece;
        if let Some(inner_ty) = get_generic_inner_type(_type, "Option") {
            tokenstream_piece = quote::quote! {
                fn #ident(&mut self, #ident:#inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
        } else if let Ok(Some(ref user_specified_ident)) = get_user_specified_ident_for_vec(field) {
            let inner_ty = get_generic_inner_type(_type, "Vec").ok_or(syn::Error::new(
                field.span(),
                "each field must be specified with Vec field",
            ))?;
            tokenstream_piece = quote::quote! {
                fn #user_specified_ident(&mut self,#user_specified_ident:#inner_ty) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };
            // // 如果用户指定的setter名字和原始字段的名字不一样，那么产生另一个setter，这个setter是一次性传入一个列表的
            if user_specified_ident != ident.as_ref().unwrap() {
                tokenstream_piece.extend(quote::quote! {
                    fn #ident(&mut self, #ident:#_type) ->&mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                })
            }
        } else {
            tokenstream_piece = quote::quote! {
                fn #ident(&mut self, #ident:#_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }

        final_tokenstream.extend(tokenstream_piece)
    }
    Ok(final_tokenstream)
}
fn generate_build_function(
    fields: &StructFields,
    origin_struct_ident: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut fill_result_clauses: Vec<_> = Vec::new();
    // fill_result_clauses = idents
    //     .iter()
    //     .map(|f| {
    //         quote::quote! {
    //             #f: self.#f.clone().unwrap()
    //         }
    //     })
    //     .collect();
    //如果使用for ident in idents 会出现 borrow of moved value: `idents`
    for (idx, (ident, ty)) in idents.iter().zip(types.iter()).enumerate() {
        // 第六关修改，只对不是`Option`类型的字段生成校验逻辑
        let tokenstream = if get_user_specified_ident_for_vec(&fields[idx])?.is_some() {
            quote::quote! {
                #ident: self.#ident.clone()
            }
        } else if get_generic_inner_type(ty, "Option").is_none() {
            quote::quote! {
                #ident: self.#ident.clone().unwrap()
            }
        } else {
            quote::quote! {
                #ident: self.#ident.clone()
            }
        };
        fill_result_clauses.push(tokenstream);
    }

    let mut checker_code_pieces = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        // 这里需要区分`Option`类型字段和非`Option`类型字段
        if get_generic_inner_type(types[idx], "Option").is_none()
            && get_user_specified_ident_for_vec(&fields[idx])?.is_none()
        {
            checker_code_pieces.push(quote::quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field missing", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            });
        }
    }
    let token_stream = quote::quote! {
        pub fn build(&mut self) -> std::result::Result<#origin_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#checker_code_pieces)*
            //  ^--注意，由于我们要重复的是一组if判断代码块，它们之间不需要用逗号分隔，所以这里的重复模式是`*`，而不是之前重复结构体字段时用到的`,*`
            let ret = #origin_struct_ident{
                #(#fill_result_clauses),*
            };
            std::result::Result::Ok(ret)
        }
    };
    Ok(token_stream)

    // let ret_tokenstream = quote::quote! {
    //     fn build(&mut self)
    //         -> std::result::Result<#origin_struct_ident,std::boxed::Box<dyn std::error::Error>>{
    //             #(if self.#idents.is_none() {
    //                 let err = format!("{} field missing!",stringify!(#idents));
    //                 return std::result::Result::Err(err.into());
    //             })
    //             *
    //             std::result::Result::Ok(#origin_struct_ident{
    //                 #(#idents:self.#idents.clone().unwrap()
    //             ),*
    //             })
    //         }
    // };
    // Ok(ret_tokenstream)
}
fn get_generic_inner_type<'a>(ty: &'a syn::Type, out_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        // 这里我们取segments的最后一节来判断是不是`T<U>`，这样如果用户写的是`foo:bar::T<U>`我们也能识别出最后的`T<U>`
        if let Some(seg) = path.segments.last() {
            if seg.ident == out_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}
fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        let meta = attr.parse_meta();
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = meta
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            if let Ok(syn::Meta::List(ref list)) = meta {
                                return Err(syn::Error::new_spanned(
                                    list,
                                    r#"expcted `builder(each = "...")`"#,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}
#[proc_macro_attribute]
pub fn proc_attr_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("{:#?}", parse_macro_input!(attr as AttributeArgs));
    println!("{:#?}", parse_macro_input!(item as Item));
    TokenStream::new()
}
