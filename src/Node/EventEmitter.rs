// Node-compatible event emitters. Listeners are stored in registration order;
// `once`/prepend/remove follow Node's ordering rules and the `newListener` /
// `removeListener` notifications are emitted like Node does.
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

use Purs_Node_Symbol::{purust_symbol_box, purust_symbol_id, purust_symbol_unbox, JsSymbol};

#[derive(Clone)]
enum EventKey {
    Str(String),
    Sym(Rc<JsSymbol>),
}

impl EventKey {
    fn matches(&self, other: &EventKey) -> bool {
        match (self, other) {
            (EventKey::Str(left), EventKey::Str(right)) => left == right,
            (EventKey::Sym(left), EventKey::Sym(right)) => {
                purust_symbol_id(left) == purust_symbol_id(right)
            }
            _ => false,
        }
    }

    fn value(&self) -> crate::UnknownType {
        match self {
            EventKey::Str(name) => crate::Value::String(name.clone()),
            EventKey::Sym(symbol) => purust_symbol_box(symbol.clone()),
        }
    }
}

struct Listener {
    callback: crate::UnknownType,
    once: bool,
}

pub struct EventEmitter {
    listeners: Mutex<Vec<(EventKey, Listener)>>,
    max_listeners: AtomicI64,
    user_data: Mutex<Option<crate::UnknownType>>,
    listen_hook: Mutex<Option<std::sync::Arc<dyn Fn(&str) + Send + Sync>>>,
}

impl EventEmitter {
    fn new() -> EventEmitter {
        EventEmitter {
            listeners: Mutex::new(Vec::new()),
            max_listeners: AtomicI64::new(10),
            user_data: Mutex::new(None),
            listen_hook: Mutex::new(None),
        }
    }

    fn matching(&self, key: &EventKey) -> Vec<usize> {
        self.listeners
            .lock()
            .unwrap()
            .iter()
            .enumerate()
            .filter(|(_, (name, _))| name.matches(key))
            .map(|(index, _)| index)
            .collect()
    }

    fn add(&self, key: EventKey, callback: crate::UnknownType, once: bool, prepend: bool) {
        // Node notifies `newListener` before the listener is registered.
        if !matches!(&key, EventKey::Str(name) if name == "newListener") {
            self.emit(
                EventKey::Str("newListener".to_owned()),
                vec![key.value(), callback.clone()],
            );
        }
        {
            let mut listeners = self.listeners.lock().unwrap();
            if prepend {
                let position = listeners
                    .iter()
                    .position(|(name, _)| !name.matches(&key))
                    .unwrap_or(listeners.len());
                listeners.insert(position, (key.clone(), Listener { callback, once }));
            } else {
                listeners.push((key.clone(), Listener { callback, once }));
            }
        }
        if let EventKey::Str(name) = &key {
            self.run_listen_hook(name);
        }
    }

    fn remove(&self, key: &EventKey, callback: &crate::UnknownType) -> bool {
        let removed = {
            let mut listeners = self.listeners.lock().unwrap();
            let position = listeners.iter().position(|(name, listener)| {
                name.matches(key) && same_callback(&listener.callback, callback)
            });
            match position {
                Some(position) => Some(listeners.remove(position)),
                None => None,
            }
        };
        match removed {
            Some((name, listener)) => {
                if !matches!(&name, EventKey::Str(value) if value == "removeListener") {
                    self.emit(
                        EventKey::Str("removeListener".to_owned()),
                        vec![name.value(), listener.callback],
                    );
                }
                true
            }
            None => false,
        }
    }

    fn emit(&self, key: EventKey, args: Vec<crate::UnknownType>) -> bool {
        let listeners = {
            let mut listeners = self.listeners.lock().unwrap();
            let mut selected = Vec::new();
            let mut index = 0;
            while index < listeners.len() {
                if listeners[index].0.matches(&key) {
                    if listeners[index].1.once {
                        selected.push(listeners.remove(index).1);
                        continue;
                    }
                    selected.push(Listener {
                        callback: listeners[index].1.callback.clone(),
                        once: false,
                    });
                }
                index += 1;
            }
            selected
        };
        let had_listeners = !listeners.is_empty();
        for listener in listeners {
            call_callback(&listener.callback, &args);
        }
        had_listeners
    }
}

fn call_callback(callback: &crate::UnknownType, args: &[crate::UnknownType]) {
    match args.len() {
        0 => {
            callback.unwrap_func1()(crate::Value::Unit);
        }
        1 => {
            callback.unwrap_func1()(args[0].clone());
        }
        2 => {
            callback.unwrap_func2()(args[0].clone(), args[1].clone());
        }
        3 => {
            callback.unwrap_func3()(args[0].clone(), args[1].clone(), args[2].clone());
        }
        4 => {
            callback.unwrap_func4()(
                args[0].clone(),
                args[1].clone(),
                args[2].clone(),
                args[3].clone(),
            );
        }
        _ => panic!("Node.EventEmitter: unsupported emit arity"),
    }
}

/// Identity of a callback value, used by `off` to find the same listener.
fn callback_identity(value: &crate::UnknownType) -> Option<usize> {
    match value.resolve() {
        crate::Value::Func1(crate::Func1::Static(f)) => Some(*f as usize),
        crate::Value::Func1(crate::Func1::Shared(f)) => Some(Rc::as_ptr(f) as *const () as usize),
        crate::Value::Func2(crate::Func2::Static(f)) => Some(*f as usize),
        crate::Value::Func2(crate::Func2::Shared(f)) => Some(Rc::as_ptr(f) as *const () as usize),
        crate::Value::Func3(crate::Func3::Static(f)) => Some(*f as usize),
        crate::Value::Func3(crate::Func3::Shared(f)) => Some(Rc::as_ptr(f) as *const () as usize),
        crate::Value::Func4(crate::Func4::Static(f)) => Some(*f as usize),
        crate::Value::Func4(crate::Func4::Shared(f)) => Some(Rc::as_ptr(f) as *const () as usize),
        crate::Value::Func5(crate::Func5::Static(f)) => Some(*f as usize),
        crate::Value::Func5(crate::Func5::Shared(f)) => Some(Rc::as_ptr(f) as *const () as usize),
        _ => None,
    }
}

fn same_callback(left: &crate::UnknownType, right: &crate::UnknownType) -> bool {
    match (callback_identity(left), callback_identity(right)) {
        (Some(left), Some(right)) => left == right,
        _ => false,
    }
}

fn emitter_box(emitter: Rc<EventEmitter>) -> crate::UnknownType {
    crate::Value::Class(Rc::new(emitter))
}

fn emitter_unbox(value: &crate::UnknownType) -> Rc<EventEmitter> {
    value.unwrap_class::<Rc<EventEmitter>>().clone()
}

fn key_from_value(value: &crate::UnknownType) -> EventKey {
    match purust_symbol_unbox(value) {
        Some(symbol) => EventKey::Sym(symbol),
        None => EventKey::Str(value.unwrap_string()),
    }
}

pub fn Node_EventEmitter_new() -> crate::UnknownType {
    crate::Value::Func1(purust_core::Func1::Static(|_| {
        emitter_box(Rc::new(EventEmitter::new()))
    }))
}

pub fn Node_EventEmitter_eventNamesImpl(emitter: Rc<EventEmitter>) -> crate::UnknownType {
    let mut names: Vec<crate::UnknownType> = Vec::new();
    for (key, _) in emitter.listeners.lock().unwrap().iter() {
        if !names
            .iter()
            .any(|existing| key_matches_value(key, existing))
        {
            names.push(key.value());
        }
    }
    purust_core::mk_array(names)
}

fn key_matches_value(key: &EventKey, value: &crate::UnknownType) -> bool {
    match (key, purust_symbol_unbox(value)) {
        (EventKey::Sym(left), Some(right)) => purust_symbol_id(left) == purust_symbol_id(&right),
        (EventKey::Str(left), None) => matches!(value.resolve(), crate::Value::String(right) if right == left),
        _ => false,
    }
}

pub fn Node_EventEmitter_symbolOrStr() -> crate::UnknownType {
    crate::Value::Func3(purust_core::Func3::Shared(Rc::new(|left, right, value| {
        if purust_symbol_unbox(&value).is_some() {
            left.unwrap_func1()(value)
        } else {
            right.unwrap_func1()(value)
        }
    })))
}

pub fn Node_EventEmitter_getMaxListenersImpl() -> crate::UnknownType {
    crate::Value::Func1(purust_core::Func1::Shared(Rc::new(|value| {
        let emitter = emitter_unbox(&value);
        crate::mk_int(emitter.max_listeners.load(Ordering::Relaxed))
    })))
}

pub fn Node_EventEmitter_listenerCountImpl() -> crate::UnknownType {
    crate::Value::Func2(purust_core::Func2::Shared(Rc::new(|emitter, name| {
        let emitter = emitter_unbox(&emitter);
        let key = EventKey::Str(name.unwrap_string());
        crate::mk_int(emitter.matching(&key).len() as i64)
    })))
}

pub fn Node_EventEmitter_setMaxListenersImpl() -> crate::UnknownType {
    crate::Value::Func2(purust_core::Func2::Shared(Rc::new(|emitter, max| {
        let emitter = emitter_unbox(&emitter);
        emitter.max_listeners.store(max.unwrap_int(), Ordering::Relaxed);
        crate::Value::Unit
    })))
}

pub fn Node_EventEmitter_unsafeEmitFn(emitter: Rc<EventEmitter>) -> crate::UnknownType {
    // Node's `emit` is variadic; the packages this port serves always emit
    // through the native implementations, so the exposed function takes the
    // event name only.
    crate::Value::Func1(purust_core::Func1::Shared(Rc::new(move |name| {
        let key = key_from_value(&name);
        crate::mk_bool(emitter.emit(key, Vec::new()))
    })))
}

pub fn Node_EventEmitter_unsafeOn() -> crate::UnknownType {
    listener_registration(false, false)
}

pub fn Node_EventEmitter_unsafeOnce() -> crate::UnknownType {
    listener_registration(true, false)
}

pub fn Node_EventEmitter_unsafePrependListener() -> crate::UnknownType {
    listener_registration(false, true)
}

pub fn Node_EventEmitter_unsafePrependOnceListener() -> crate::UnknownType {
    listener_registration(true, true)
}

pub fn Node_EventEmitter_unsafeOff() -> crate::UnknownType {
    crate::Value::Func3(purust_core::Func3::Shared(Rc::new(
        |emitter, name, callback| {
            let emitter = emitter_unbox(&emitter);
            let key = EventKey::Str(name.unwrap_string());
            emitter.remove(&key, &callback);
            crate::Value::Unit
        },
    )))
}

fn listener_registration(once: bool, prepend: bool) -> crate::UnknownType {
    crate::Value::Func3(purust_core::Func3::Shared(Rc::new(
        move |emitter, name, callback| {
            let emitter = emitter_unbox(&emitter);
            let key = EventKey::Str(name.unwrap_string());
            emitter.add(key, callback, once, prepend);
            crate::Value::Unit
        },
    )))
}

impl EventEmitter {
    pub fn new_native() -> EventEmitter {
        EventEmitter::new()
    }

    /// Attaches opaque data to an emitter. The stream implementation stores
    /// its state here, because Node streams *are* event emitters and the FFIs
    /// exchange them as such.
    pub fn set_user_data(&self, data: crate::UnknownType) {
        *self.user_data.lock().unwrap() = Some(data);
    }

    pub fn user_data(&self) -> Option<crate::UnknownType> {
        self.user_data.lock().unwrap().clone()
    }

    /// Installs a callback invoked after a listener is registered. Native
    /// integrations use it to deliver state that happened before the listener
    /// existed (child process lifecycle events, for example).
    pub fn set_listen_hook(&self, hook: std::sync::Arc<dyn Fn(&str) + Send + Sync>) {
        *self.listen_hook.lock().unwrap() = Some(hook);
    }

    pub fn run_listen_hook(&self, event: &str) {
        let hook = self.listen_hook.lock().unwrap().clone();
        if let Some(hook) = hook {
            hook(event);
        }
    }

    pub fn listener_count(&self, event: &str) -> usize {
        self.matching(&EventKey::Str(event.to_owned())).len()
    }
}

// Helpers used by the stream implementations, which emit events natively.
pub fn purust_emitter_box(emitter: Rc<EventEmitter>) -> crate::UnknownType {
    emitter_box(emitter)
}

pub fn purust_emitter_unbox(value: &crate::UnknownType) -> Rc<EventEmitter> {
    emitter_unbox(value)
}

pub fn purust_emitter_emit(
    emitter: &Rc<EventEmitter>,
    event: &str,
    args: Vec<crate::UnknownType>,
) -> bool {
    emitter.emit(EventKey::Str(event.to_owned()), args)
}

pub fn purust_emitter_listener_count(emitter: &Rc<EventEmitter>, event: &str) -> usize {
    emitter.listener_count(event)
}

pub fn purust_emitter_set_listen_hook(
    emitter: &Rc<EventEmitter>,
    hook: std::sync::Arc<dyn Fn(&str) + Send + Sync>,
) {
    emitter.set_listen_hook(hook);
}

pub fn purust_emitter_once_native(
    emitter: &Rc<EventEmitter>,
    event: &str,
    callback: crate::UnknownType,
) {
    emitter.add(EventKey::Str(event.to_owned()), callback, true, false);
}

/// Persistent native listener: integrations that consume a whole stream
/// (HTTP request parsing, connection accept loops) must see every event.
pub fn purust_emitter_on_native(
    emitter: &Rc<EventEmitter>,
    event: &str,
    callback: crate::UnknownType,
) {
    emitter.add(EventKey::Str(event.to_owned()), callback, false, false);
}
