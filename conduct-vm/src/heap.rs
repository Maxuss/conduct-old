use std::{
    cell::Cell,
    fmt::Debug,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use conduct_tk::AHashMap;
use internment::Intern;

use crate::{fnc::FunctionDescriptor, vm::DEFAULT_HEAP_CAPACITY};

pub trait Trace: Debug {
    fn trace(&self);
}

#[derive(Debug, Clone, Default)]
pub struct GcHeader {
    pub marked: Cell<bool>,
}

#[derive(Debug)]
pub struct Gc<T: 'static + Sized + Trace>(NonNull<Allocated<T>>);
impl<T: 'static + Trace + Sized> Copy for Gc<T> {}
impl<T: 'static + Sized + Trace> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static + Sized + Trace> Deref for Gc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.get().expect("Dereference Failure").value
    }
}

#[derive(Debug, Clone)]
pub struct Allocated<T: 'static + ?Sized + Trace> {
    gc_header: GcHeader,
    pub value: T,
}

impl<T: 'static + ?Sized + Trace> Allocated<T> {
    pub fn new(v: T) -> Self
    where
        T: Sized,
    {
        Self {
            gc_header: GcHeader::default(),
            value: v,
        }
    }

    pub fn marked(&self) -> bool {
        self.gc_header.marked.get()
    }

    pub fn mark(&self) {
        self.gc_header.marked.set(true)
    }

    pub fn unmark(&self) {
        self.gc_header.marked.set(false)
    }
}

impl<T: 'static + Sized + Trace> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.get_mut_unchecked().value
    }
}

impl<T> Gc<T>
where
    T: 'static + Sized + Trace,
{
    pub fn dangling() -> Self {
        Self(NonNull::dangling())
    }

    pub fn from_ptr(ptr: *mut Allocated<T>) -> Self {
        Self(unsafe { NonNull::new_unchecked(ptr) })
    }

    #[inline]
    pub fn get_unchecked(&self) -> &Allocated<T> {
        unsafe { self.0.as_ref() }
    }

    #[inline]
    pub fn get_mut_unchecked(&mut self) -> &mut Allocated<T> {
        unsafe { self.0.as_mut() }
    }

    #[inline]
    pub fn get(&self) -> Option<&Allocated<T>> {
        if self.0.as_ptr().is_null() {
            None
        } else {
            // SAFETY:
            // - Pointer was checked to be properly aligned and is not null
            // - Pointer is dereferenceable
            // - Pointer points to an initialized value of T on heap
            Some(unsafe { self.0.as_ref() })
        }
    }

    #[inline]
    pub fn get_ptr(&self) -> *mut Allocated<T> {
        self.0.as_ptr()
    }
}

impl<T: 'static + Trace + Sized> Trace for Allocated<T> {
    fn trace(&self) {
        if !self.gc_header.marked.replace(true) {
            self.value.trace();
        }
    }
}

impl<T> Trace for Gc<T>
where
    T: 'static + Sized + Trace,
{
    fn trace(&self) {}
}

impl Trace for FunctionDescriptor {
    fn trace(&self) {}
}

impl Trace for String {
    fn trace(&self) {}
}

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self) {
        for value in self {
            value.trace()
        }
    }
}

impl<T: Trace> Trace for AHashMap<String, T> {
    fn trace(&self) {
        for value in self.values() {
            value.trace()
        }
    }
}

impl Trace for Intern<String> {
    fn trace(&self) {}
}

#[derive(Debug)]
pub struct Heap {
    heap: Vec<Box<Allocated<dyn Trace>>>,
    allocated: usize,
    threshold: usize,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            heap: Vec::with_capacity(DEFAULT_HEAP_CAPACITY),
            allocated: 0,
            threshold: 100,
        }
    }

    pub fn allocate<T: 'static + Trace>(&mut self, data: T) -> Gc<T> {
        let mut boxed = Box::new(Allocated {
            gc_header: GcHeader::default(),
            value: data,
        });
        let ptr = unsafe { NonNull::new_unchecked(&mut *boxed) };

        self.heap.push(boxed);
        self.allocated += std::mem::size_of::<T>();
        Gc::from_ptr(ptr.as_ptr())
    }
}
