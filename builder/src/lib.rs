use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    token::{Comma, Dyn},
    AngleBracketedGenericArguments, Data, DeriveInput, Expr, Field, Fields, GenericArgument, Path,
    PathArguments, PathSegment, Type, TypeParamBound, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
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

        fields
            .into_iter()
            .for_each(|field| self.r#struct.push_field(field));
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
    optional_fields: Vec<usize>,
    fields_with_attr: Vec<usize>,
    fields: Punctuated<Field, Comma>,
    implement: Implement,
}

impl Struct {
    fn new(builder_ident: Ident) -> Self {
        Struct {
            ident: builder_ident.clone(),
            optional_fields: Vec::new(),
            fields_with_attr: Vec::new(),
            fields: Punctuated::new(),
            implement: Implement::new_empty(builder_ident),
        }
    }

    fn push_field(&mut self, mut field: Field) {
        let attr_data = Self::parse_attr_data(&field);

        let maybe_generic_type = unwrap_optional(&field.ty);
        let mut param_type;
        if let Some(generic_type) = maybe_generic_type {
            self.optional_fields.push(self.fields.len());
            param_type = generic_type;
        } else {
            param_type = field.ty.clone();
            let option = Ident::new("Option", Span::call_site());
            field.ty = new_type(option, vec![field.ty]);
        }

        if let Some(_) = &attr_data {
            param_type = unwrap_vector(param_type);
            self.fields_with_attr.push(self.fields.len());
        }

        field.attrs.clear();
        self.fields.push(field.clone());
        self.create_setter(field, param_type, attr_data);
    }

    fn parse_attr_data(field: &Field) -> Option<String> {
        if let Some(attr) = field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("builder"))
        {
            let data: Expr = attr.parse_args().unwrap();
            match data {
                Expr::Assign(assign_expr) => {
                    if let Expr::Path(val) = *assign_expr.left {
                        if !val.path.is_ident("each") {
                            panic!("Currently only knows 'each' param!")
                        }
                    }

                    if let Expr::Lit(literal) = *assign_expr.right {
                        match literal.lit {
                            syn::Lit::Str(literal) => {
                                let data = literal.value();
                                Some(data)
                            }
                            _ => panic!("Expected string literal, but given other types!"),
                        }
                    } else {
                        panic!("Must be a string literal!")
                    }
                }
                _ => panic!("Invalid expression in attribute #[builder(...)]!"),
            }
        } else {
            None
        }
    }

    fn create_setter(&mut self, field: Field, param_type: Type, attr_data: Option<String>) {
        let mut name_ident = field.ident.as_ref().cloned().unwrap();
        if let Some(name) = &attr_data {
            name_ident = Ident::new(name, name_ident.span());
        }

        let mut function = Function::new(
            name_ident.clone(),
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
                name: name_ident.clone(),
                r#type: Some(param_type),
            },
        ]);

        let field_ident = field.ident.unwrap();
        if let Some(_) = attr_data {
            function.body = Some(Group::new(
                Delimiter::Brace,
                quote! {
                    if self.#field_ident.is_some() {
                        unsafe {
                            self.#field_ident.as_mut().unwrap_unchecked().push(#name_ident)
                        }
                    } else {
                        self.#field_ident = Some(vec![#name_ident]);
                    }
                    self
                },
            ));
        } else {
            function.body = Some(Group::new(
                Delimiter::Brace,
                quote! {
                    self.#field_ident = Some(#name_ident);
                    self
                },
            ));
        }

        self.implement.functions.push(function);
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
            modifier: TypeModifier::MutableReference,
            r#type: None,
        }]);

        let struct_body = self
            .fields
            .clone()
            .into_iter()
            .enumerate()
            .map(|(i, field)| {
                let ident = field.ident.unwrap();
                if self.optional_fields.contains(&i) {
                    quote!( #ident: self.#ident.to_owned(), )
                } else if self.fields_with_attr.contains(&i) {
                    quote!( #ident: self.#ident.as_ref().unwrap_or(&vec![]).to_owned(), )
                } else {
                    quote!( #ident: self.#ident.as_ref().unwrap().to_owned(), )
                }
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
        let fields = &self.fields;
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

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Param {
            modifier,
            name,
            r#type,
        } = self;
        let r#type = r#type
            .as_ref()
            .map(|r#type| quote!(: #r#type))
            .unwrap_or(TokenStream::new());
        tokens.extend(quote!( #modifier #name #r#type ));
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

fn unwrap_optional(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            let r#type = &segment.ident;

            if r#type.to_string() == "Option".to_string() {
                match &segment.arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => {
                        if args.len() > 1 {
                            panic!("Option type have more than one generic types!");
                        }

                        match args.first().unwrap() {
                            GenericArgument::Type(ty) => Some(ty.to_owned()),
                            _ => panic!("Option type have other items than Type in generics!"),
                        }
                    }
                    _ => panic!("Option type doesn't have an generic type!"),
                }
            } else {
                None
            }
        }
        _ => panic!("Expected path type, given other type! It's like std::option::Option."),
    }
}

fn unwrap_vector(ty: Type) -> Type {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            let r#type = &segment.ident;

            if r#type.to_string() == "Vec".to_string() {
                match &segment.arguments {
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => {
                        if args.len() > 1 {
                            panic!("Vector type have more than one generic types!");
                        }

                        match args.first().unwrap() {
                            GenericArgument::Type(ty) => ty.to_owned(),
                            _ => panic!("Vector type have other items than Type in generics!"),
                        }
                    }
                    _ => panic!("Vector type doesn't have an generic type!"),
                }

            } else {
                panic!("The helper attribute is not applicable to this attribute, becuase it is not a Vec<T> or Option<Vec<T>>!");
            }
        }
        _ => panic!("Expected path type, given other type! It's like std::option::Option."),
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
