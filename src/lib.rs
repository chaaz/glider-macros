extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::{Gt, Lt};
use syn::{parse_macro_input, FnArg, GenericParam, Generics, ItemFn, Lifetime, LifetimeDef, ReturnType, Type};

#[proc_macro_attribute]
pub fn build_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
  let func = parse_macro_input!(item as ItemFn);
  let attrs = &func.attrs;
  let vis = &func.vis;
  let sig = &func.sig;
  assert!(sig.asyncness.is_some());

  let g0_lifetime = Lifetime::new("'r", Span::call_site());
  let g0 = GenericParam::Lifetime(LifetimeDef {
    attrs: Vec::new(),
    lifetime: g0_lifetime,
    colon_token: None,
    bounds: Punctuated::new()
  });
  let generics = sig.generics.params.iter();
  let mut params = Punctuated::new();
  params.insert(0, g0);
  for g1 in generics {
    params.insert(0, g1.clone());
  }
  let new_generics = Generics {
    lt_token: Some(Lt { spans: [Span::call_site()] }),
    gt_token: Some(Gt { spans: [Span::call_site()] }),
    params,
    where_clause: None
  };

  let name = &sig.ident;
  let mut inputs = sig.inputs.iter();
  let arg1 = inputs.next().unwrap();
  let arg2 = inputs.next().unwrap();
  let arg3 = inputs.next().unwrap();
  let (arg3_i, arg3_t) = match arg3 {
    FnArg::Receiver(_) => panic!("Unexpected receiver argument."),
    FnArg::Typed(t) => (&t.pat, &t.ty)
  };
  let arg3_t = match arg3_t.as_ref() {
    Type::Reference(r) => {
      assert!(r.mutability.is_some());
      assert!(r.lifetime.is_none());
      &r.elem
    }
    _ => panic!("Expected type reference in arg3")
  };
  let output = &sig.output;
  let output = match output {
    ReturnType::Default => panic!("Unexpected default return."),
    ReturnType::Type(_arrow, t) => t
  };
  let block = &func.block;

  let result = quote! {
    #(#attrs)* #vis fn #name #new_generics (#arg1, #arg2, #arg3_i: &'r mut #arg3_t) ->
    std::pin::Pin<std::boxed::Box<dyn std::future::Future<Output = #output> + std::marker::Send + 'r>>
    {
      std::boxed::Box::pin( async move #block )
    }
  };
  result.into()
}

#[proc_macro_attribute]
pub fn run_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
  let func = parse_macro_input!(item as ItemFn);
  let attrs = &func.attrs;
  let vis = &func.vis;
  let sig = &func.sig;
  assert!(sig.asyncness.is_some());

  let generics = sig.generics.params.iter();
  let mut params = Punctuated::new();
  for g1 in generics {
    params.insert(0, g1.clone());
  }
  let new_generics = Generics {
    lt_token: Some(Lt { spans: [Span::call_site()] }),
    gt_token: Some(Gt { spans: [Span::call_site()] }),
    params,
    where_clause: None
  };

  let name = &sig.ident;
  let mut inputs = sig.inputs.iter();
  let arg1 = inputs.next().unwrap();
  let arg2 = inputs.next().unwrap();
  let arg3 = inputs.next().unwrap();
  let arg4 = inputs.next().unwrap();
  let output = &sig.output;
  let output = match output {
    ReturnType::Default => panic!("Unexpected default return."),
    ReturnType::Type(_arrow, t) => t
  };
  let block = &func.block;

  let result = quote! {
    #(#attrs)* #vis fn #name #new_generics(#arg1, #arg2, #arg3, #arg4) ->
    std::pin::Pin<std::boxed::Box<dyn std::future::Future<Output = #output> + std::marker::Send>>
    {
      std::boxed::Box::pin( async move #block )
    }
  };
  result.into()
}
