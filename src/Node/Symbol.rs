// JavaScript symbols. Registered symbols (`Symbol.for`) are interned per key,
// like the engine registry; unregistered ones only carry a description.
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_SYMBOL_ID: AtomicUsize = AtomicUsize::new(1);

thread_local! {
    static REGISTRY: RefCell<HashMap<String, Rc<JsSymbol>>> = RefCell::new(HashMap::new());
}

pub struct JsSymbol {
    id: usize,
    description: String,
}

impl JsSymbol {
    fn new(description: String) -> Rc<JsSymbol> {
        Rc::new(JsSymbol {
            id: NEXT_SYMBOL_ID.fetch_add(1, Ordering::Relaxed),
            description,
        })
    }
}

pub fn purust_symbol_id(symbol: &Rc<JsSymbol>) -> usize {
    symbol.id
}

pub fn purust_symbol_description(symbol: &Rc<JsSymbol>) -> &str {
    &symbol.description
}

pub fn purust_symbol_box(symbol: Rc<JsSymbol>) -> crate::UnknownType {
    crate::Value::Class(Rc::new(symbol))
}

pub fn purust_symbol_unbox(value: &crate::UnknownType) -> Option<Rc<JsSymbol>> {
    if let crate::Value::Class(native) = value.resolve() {
        native.downcast_ref::<Rc<JsSymbol>>().cloned()
    } else {
        None
    }
}

pub fn Node_Symbol_showSymbolImpl() -> crate::UnknownType {
    crate::Value::Func1(purust_core::Func1::Shared(Rc::new(|value| {
        let symbol = purust_symbol_unbox(&value).expect("Node.Symbol.showSymbolImpl: expected a symbol");
        crate::Value::String(format!("Symbol({})", purust_symbol_description(&symbol)))
    })))
}

pub fn Node_Symbol_forImpl() -> crate::UnknownType {
    crate::Value::Func1(purust_core::Func1::Shared(Rc::new(|key| {
        let key = key.unwrap_string();
        let symbol = REGISTRY.with(|registry| {
            let mut registry = registry.borrow_mut();
            registry
                .entry(key.clone())
                .or_insert_with(|| JsSymbol::new(key.clone()))
                .clone()
        });
        purust_symbol_box(symbol)
    })))
}

pub fn Node_Symbol_keyForImpl() -> crate::UnknownType {
    crate::Value::Func1(purust_core::Func1::Shared(Rc::new(|value| {
        let symbol = purust_symbol_unbox(&value);
        let key = REGISTRY.with(|registry| {
            let registry = registry.borrow();
            registry
                .iter()
                .find(|(_, registered)| {
                    symbol
                        .as_ref()
                        .is_some_and(|symbol| purust_symbol_id(symbol) == purust_symbol_id(registered))
                })
                .map(|(key, _)| key.clone())
        });
        let nullable = match key {
            Some(key) => Purs_Data_Nullable::Data_Nullable_notNull(crate::Value::String(key)),
            None => Purs_Data_Nullable::Data_Nullable_null(),
        };
        crate::Value::Class(Rc::new(nullable))
    })))
}
