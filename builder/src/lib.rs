use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    token::{Comma, Dyn},
    AngleBracketedGenericArguments, Data, DeriveInput, Field, Fields, GenericArgument, Path,
    PathArguments, PathSegment, Type, TypeParamBound, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let input_ident = input.ident;
    let mut builder = BuilderCreator::new(input_ident);

    match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                builder.update_fields(fields.named);
            }
            _ => panic!("Derive is available only for structs with named fields!"),
        },
        _ => panic!("Derive is available only for structs!"),
    }

    builder.to_token_stream().into()
}

struct BuilderCreator {
    r#struct: Struct,
    source_impl: Implement,
}

impl BuilderCreator {
    fn new(derivable_struct_ident: Ident) -> Self {
        let builder_name = derivable_struct_ident.to_string() + "Builder";
        let builder_ident = Ident::new(&builder_name, Span::call_site());

        let r#struct = Struct::new(builder_ident.clone());

        let func_ident = Ident::new("builder", Span::call_site());
        let return_type = new_type(builder_ident, vec![]);
        let build_func = Function::new(func_ident, return_type, TypeModifier::None);
        let source_impl = Implement::new(derivable_struct_ident, vec![build_func]);

        Self {
            r#struct,
            source_impl,
        }
    }

    fn update_fields(&mut self, fields: Punctuated<Field, Comma>) {
        self.source_impl.functions.first_mut().unwrap().body =
            Some(Group::new(Delimiter::Brace, {
                let ident = &self.r#struct.ident;
                let init_fields = fields
                    .clone()
                    .into_iter()
                    .map(|field| {
                        let ident = field.ident.unwrap();
                        quote!( #ident: None, )
                    })
                    .reduce(|mut lhs, rhs| {
                        lhs.extend(rhs);
                        lhs
                    })
                    .unwrap();
                quote!( #ident { #init_fields } )
            }));

        let modified_fields: Punctuated<Field, Comma> = fields
            .clone()
            .into_iter()
            .map(|mut field| {
                let option = Ident::new("Option", Span::call_site());
                field.ty = new_type(option, vec![field.ty]);
                field
            })
            .collect();

        self.r#struct.fields = modified_fields;
        self.r#struct.create_setters(fields);
        self.r#struct.create_build(self.source_impl.ident.clone());
    }

    fn to_token_stream(&self) -> TokenStream {
        let mut tt = self.r#struct.to_token_stream();
        tt.extend(self.source_impl.to_token_stream());
        tt
    }
}

struct Struct {
    ident: Ident,
    fields: Punctuated<Field, Comma>,
    implement: Implement,
}

impl Struct {
    fn new(builder_ident: Ident) -> Self {
        Struct {
            ident: builder_ident.clone(),
            fields: Punctuated::new(),
            implement: Implement::new_empty(builder_ident),
        }
    }

    fn create_setters(&mut self, fields: Punctuated<Field, Comma>) {
        self.implement.functions = fields
            .into_iter()
            .map(|field| {
                let field_ident = field.ident.unwrap();
                let r#type = Some(field.ty);

                let mut function = Function::new(
                    field_ident.clone(),
                    new_type(Ident::new("Self", Span::call_site()), vec![]),
                    TypeModifier::MutableReference,
                );

                function.params = Punctuated::from_iter(vec![
                    Param {
                        modifier: TypeModifier::MutableReference,
                        name: Ident::new("self", Span::call_site()),
                        r#type: None,
                    },
                    Param {
                        modifier: TypeModifier::None,
                        name: field_ident.clone(),
                        r#type,
                    },
                ]);

                function.body = Some(Group::new(
                    Delimiter::Brace,
                    quote! {
                        self.#field_ident = Some(#field_ident);
                        self
                    },
                ));

                function
            })
            .collect();
    }

    fn create_build(&mut self, derivable_struct_ident: Ident) {
        let name = Ident::new("build", Span::call_site());
        let return_type = new_type(
            Ident::new("Result", Span::call_site()),
            vec![
                new_type(derivable_struct_ident.clone(), vec![]),
                new_type(
                    Ident::new("Box", Span::call_site()),
                    vec![new_dyn_type("std::error::Error")],
                ),
            ],
        );
        let mut function = Function::new(name, return_type, TypeModifier::None);
        function.params = Punctuated::from_iter(vec![Param {
            name: Ident::new("self", Span::call_site()),
            modifier: TypeModifier::None,
            r#type: None,
        }]);

        let struct_body = self
            .fields
            .clone()
            .into_iter()
            .map(|field| {
                let ident = field.ident.unwrap();
                quote!( #ident: self.#ident.unwrap(), )
            })
            .reduce(|mut lhs, rhs| {
                lhs.extend(rhs);
                lhs
            });

        function.body = Some(Group::new(
            Delimiter::Brace,
            quote! {
                Ok(
                    #derivable_struct_ident {
                        #struct_body
                    }
                )
            },
        ));

        self.implement.functions.push(function);
    }

    fn to_token_stream(&self) -> TokenStream {
        let ident = &self.ident;
        let fields = self
            .fields
            .clone()
            .into_iter()
            .map(|field| {
                let ident = field.ident;
                let r#type = field.ty;
                quote! ( #ident: #r#type, )
            })
            .reduce(|mut lhs, rhs| {
                lhs.extend(rhs);
                lhs
            });
        let mut tt = quote! {
            struct #ident {
                #fields
            }
        };
        tt.extend(self.implement.to_token_stream());

        tt
    }
}

struct Implement {
    ident: Ident,
    functions: Vec<Function>,
}

impl Implement {
    fn new(ident: Ident, functions: Vec<Function>) -> Self {
        Self { ident, functions }
    }

    fn new_empty(ident: Ident) -> Self {
        Self {
            ident,
            functions: vec![],
        }
    }

    fn to_token_stream(&self) -> TokenStream {
        let body = self
            .functions
            .iter()
            .map(|function| function.to_token_stream())
            .reduce(|mut lhs, rhs| {
                lhs.extend(rhs);
                lhs
            });
        let ident = &self.ident;
        quote! {
            impl #ident {
                #body
            }
        }
    }
}

struct Function {
    name: Ident,
    params: Punctuated<Param, Comma>,
    modifier: Ident,
    return_type_modifier: TypeModifier,
    return_type: Type,
    body: Option<Group>,
}

impl Function {
    const DEFAULT_MODIFIER: &'static str = "pub";

    fn new(name: Ident, return_type: Type, return_type_modifier: TypeModifier) -> Self {
        Function {
            name,
            params: Punctuated::new(),
            modifier: Ident::new(Self::DEFAULT_MODIFIER, Span::call_site()),
            return_type_modifier,
            return_type,
            body: None,
        }
    }

    fn to_token_stream(&self) -> TokenStream {
        let Function {
            name,
            params,
            modifier,
            return_type_modifier,
            return_type,
            body,
        } = self;
        let params = params
            .iter()
            .map(|param| param.to_token_stream())
            .reduce(|mut lhs, rhs| {
                lhs.extend(rhs);
                lhs
            })
            .unwrap_or(TokenStream::new());
        let body = body.as_ref().unwrap();

        quote! {
            #modifier fn #name(#params) -> #return_type_modifier #return_type
                #body
        }
    }
}

struct Param {
    modifier: TypeModifier,
    name: Ident,
    r#type: Option<Type>,
}

impl Param {
    fn to_token_stream(&self) -> TokenStream {
        let Param {
            modifier,
            name,
            r#type,
        } = self;
        let r#type = r#type
            .as_ref()
            .map(|r#type| quote!(: #r#type))
            .unwrap_or(TokenStream::new());
        quote!( #modifier #name #r#type, )
    }
}

#[allow(dead_code)]
enum TypeModifier {
    MutableReference,
    Reference,
    Mutable,
    None,
}

impl ToTokens for TypeModifier {
    fn to_tokens(&self, tt: &mut TokenStream) {
        tt.extend(match self {
            Self::MutableReference => quote!( &mut ),
            Self::Reference => quote!(&),
            Self::Mutable => quote!(mut),
            Self::None => TokenStream::new(),
        });
    }
}

fn new_type(ident: Ident, generics: Vec<Type>) -> Type {
    let arguments = if generics.is_empty() {
        PathArguments::None
    } else {
        let args = Punctuated::from_iter(generics.into_iter().map(|ty| GenericArgument::Type(ty)));
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            args,
            lt_token: syn::token::Lt(Span::call_site()),
            gt_token: syn::token::Gt(Span::call_site()),
        })
    };
    Type::Path(TypePath {
        qself: None,
        path: Path {
            segments: Punctuated::from_iter(vec![PathSegment { ident, arguments }]),
            leading_colon: None,
        },
    })
}

fn new_dyn_type(path: &str) -> Type {
    let segments = Punctuated::from_iter(path.split("::").map(|part| PathSegment {
        ident: Ident::new(part, Span::call_site()),
        arguments: PathArguments::None,
    }));
    Type::TraitObject(syn::TypeTraitObject {
        dyn_token: Some(Dyn(Span::call_site())),
        bounds: Punctuated::from_iter(vec![TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: Path {
                leading_colon: None,
                segments,
            },
        })]),
    })
}
