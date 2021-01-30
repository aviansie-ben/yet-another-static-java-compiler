use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

fn into_pos(i: usize) -> (usize, u8) {
    (i >> 3, (i & 0x7) as u8)
}

fn from_pos(byte: usize, bit: u8) -> usize {
    (byte << 3) + (bit as usize)
}

pub trait BitVecIndex {
    fn into_index(self) -> usize;
    fn from_index(i: usize) -> Self;
}

impl BitVecIndex for usize {
    fn into_index(self) -> usize {
        self
    }

    fn from_index(i: usize) -> Self {
        i
    }
}

#[derive(PartialEq, Eq)]
struct BitVecInternal {
    bits: Vec<u8>
}

impl BitVecInternal {
    pub fn new() -> Self {
        BitVecInternal { bits: vec![] }
    }

    pub fn clear(&mut self) {
        for b in self.bits.iter_mut() {
            *b = 0;
        };
    }

    pub fn get(&self, i: usize) -> bool {
        let (byte, bit) = into_pos(i);

        ((self.bits.get(byte).copied().unwrap_or(0) >> bit) & 1) != 0
    }

    pub fn set(&mut self, i: usize, val: bool) -> bool {
        let (byte, bit) = into_pos(i);

        if self.bits.len() <= byte {
            self.bits.extend(iter::repeat(0).take(byte + 1 - self.bits.len()));
        };

        let old = self.bits[byte];
        let new = if val {
            old | (1 << bit)
        } else {
            old & !(1 << bit)
        };

        self.bits[byte] = new;
        ((old >> bit) & 1) != 0
    }

    pub fn union(&mut self, other: &Self) -> bool {
        let mut modified = false;

        for (b1, b2) in self.bits.iter_mut().zip(other.bits.iter()) {
            let b = *b1 | *b2;
            modified = modified || b != *b1;
            *b1 = b;
        };

        if other.bits.len() > self.bits.len() {
            self.bits.reserve(other.bits.len() - self.bits.len());
            for b in other.bits[self.bits.len()..].iter() {
                modified = modified || *b != 0;
                self.bits.push(*b);
            };
        };

        modified
    }

    pub fn intersect(&mut self, other: &Self) -> bool {
        let mut modified = false;

        for (b1, b2) in self.bits.iter_mut().zip(other.bits.iter()) {
            let b = *b1 & *b2;
            modified = modified || b != *b1;
            *b1 = b;
        };

        if self.bits.len() > other.bits.len() {
            for b in self.bits[other.bits.len()..].iter_mut() {
                modified = modified || *b != 0;
                *b = 0;
            };
        };

        modified
    }

    pub fn difference(&mut self, other: &Self) -> bool {
        let mut modified = false;

        for (b1, b2) in self.bits.iter_mut().zip(other.bits.iter()) {
            let b = *b1 & !*b2;
            modified = modified || b != *b1;
            *b1 = b;
        };

        modified
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item=usize> + 'a {
        BitVecIter(self, 0)
    }
}

impl Clone for BitVecInternal {
    fn clone(&self) -> Self {
        BitVecInternal { bits: self.bits.clone() }
    }

    fn clone_from(&mut self, other: &BitVecInternal) {
        self.bits.clone_from(&other.bits);
    }
}

struct BitVecIter<'a>(&'a BitVecInternal, usize);

impl <'a> Iterator for BitVecIter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let (mut byte, bit) = into_pos(self.1);

        if byte >= self.0.bits.len() {
            return None;
        };

        let tz = (self.0.bits[byte] >> bit).trailing_zeros();

        if tz != 8 {
            self.1 += (tz + 1) as usize;
            return Some(self.1 - 1);
        };

        for b in self.0.bits[(byte + 1)..].iter().copied() {
            byte += 1;
            if b != 0 {
                let tz = b.trailing_zeros();
                let i = from_pos(byte, tz as u8);

                self.1 = i + 1;
                return Some(i);
            };
        };

        self.1 = !0;
        None
    }
}

#[derive(PartialEq, Eq)]
pub struct BitVec<T: BitVecIndex> {
    bits: BitVecInternal,
    _data: PhantomData<fn (T) -> ()>
}

impl <T: BitVecIndex> BitVec<T> {
    pub fn new() -> Self {
        Self {
            bits: BitVecInternal::new(),
            _data: PhantomData
        }
    }

    pub fn clear(&mut self) {
        self.bits.clear();
    }

    pub fn get(&self, i: T) -> bool {
        self.bits.get(i.into_index())
    }

    pub fn set(&mut self, i: T, val: bool) -> bool {
        self.bits.set(i.into_index(), val)
    }

    pub fn union(&mut self, other: &Self) -> bool {
        self.bits.union(&other.bits)
    }

    pub fn intersect(&mut self, other: &Self) -> bool {
        self.bits.intersect(&other.bits)
    }

    pub fn difference(&mut self, other: &Self) -> bool {
        self.bits.difference(&other.bits)
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item=T> + 'a {
        self.bits.iter().map(BitVecIndex::from_index)
    }
}

impl <T: BitVecIndex> Clone for BitVec<T> {
    fn clone(&self) -> Self {
        BitVec {
            bits: self.bits.clone(),
            _data: PhantomData
        }
    }

    fn clone_from(&mut self, other: &BitVec<T>) {
        self.bits.clone_from(&other.bits);
    }
}

enum LazyImpl<T, F: FnOnce () -> T> {
    Initialized(T),
    Uninitialized(F),
    Poisoned
}

pub struct Lazy<T, F: FnOnce () -> T>(UnsafeCell<LazyImpl<T, F>>);

impl <T, F: FnOnce () -> T> Lazy<T, F> {
    pub fn new(f: F) -> Lazy<T, F> {
        Lazy(UnsafeCell::new(LazyImpl::Uninitialized(f)))
    }

    pub fn unwrap(l: Lazy<T, F>) -> Option<T> {
        match l.0.into_inner() {
            LazyImpl::Initialized(t) => Some(t),
            LazyImpl::Uninitialized(_) => None,
            LazyImpl::Poisoned => None
        }
    }

    pub fn force_init(l: &Lazy<T, F>) {
        unsafe {
            match *l.0.get() {
                LazyImpl::Initialized(_) => {},
                LazyImpl::Uninitialized(_) => match std::mem::replace(&mut *l.0.get(), LazyImpl::Poisoned) {
                    LazyImpl::Uninitialized(f) => {
                        *l.0.get() = LazyImpl::Initialized(f());
                    },
                    _ => unreachable!()
                },
                LazyImpl::Poisoned => panic!("Lazy constructor previously panicked")
            }
        }
    }
}

impl <T, F: FnOnce() -> T> Deref for Lazy<T, F> {
    type Target = T;

    fn deref(&self) -> &T {
        Lazy::force_init(self);
        unsafe {
            match *self.0.get() {
                LazyImpl::Initialized(ref t) => t,
                _ => unreachable!()
            }
        }
    }
}

impl <T, F: FnOnce() -> T> DerefMut for Lazy<T, F> {
    fn deref_mut(&mut self) -> &mut T {
        Lazy::force_init(self);
        unsafe {
            match *self.0.get() {
                LazyImpl::Initialized(ref mut t) => t,
                _ => unreachable!()
            }
        }
    }
}

impl <T, F: FnOnce() -> T> std::panic::UnwindSafe for Lazy<T, F> {}
impl <T, F: FnOnce() -> T> std::panic::RefUnwindSafe for Lazy<T, F> {}

pub struct FuncCache<T: Clone + Eq + Hash, U, F: FnMut (T) -> U> {
    func: F,
    cache: HashMap<T, U>
}

impl <T: Clone + Eq + Hash, U, F: FnMut (T) -> U> FuncCache<T, U, F> {
    pub fn new(func: F) -> Self {
        FuncCache { func, cache: HashMap::new() }
    }

    pub fn clear(&mut self) {
        self.cache.clear()
    }

    pub fn get(&mut self, t: T) -> &U {
        let func = &mut self.func;
        self.cache.entry(t.clone()).or_insert_with(|| {
            func(t)
        })
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::ops::{Deref, DerefMut};
    use itertools::Itertools;
    use super::{BitVec, FuncCache, Lazy};

    #[test]
    fn test_get_set() {
        let mut bv: BitVec<usize> = BitVec::new();

        assert!(!bv.get(0));
        assert!(!bv.get(7));
        assert!(!bv.get(8));

        bv.set(7, true);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(!bv.get(8));

        bv.set(8, false);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(!bv.get(8));

        bv.set(8, true);

        assert!(!bv.get(0));
        assert!(bv.get(7));
        assert!(bv.get(8));

        bv.set(7, false);

        assert!(!bv.get(0));
        assert!(!bv.get(7));
        assert!(bv.get(8));

        bv.set(0, true);
        bv.set(8, false);

        assert!(bv.get(0));
        assert!(!bv.get(7));
        assert!(!bv.get(8));
    }

    #[test]
    fn test_clear() {
        let mut bv: BitVec<usize> = BitVec::new();

        bv.set(0, true);
        bv.set(8, true);
        bv.set(9, true);
        bv.clear();

        assert!(!bv.get(0));
        assert!(!bv.get(1));
        assert!(!bv.get(8));
        assert!(!bv.get(9));
    }

    #[test]
    fn test_union() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv1.set(1, true);
        bv2.set(1, true);
        bv2.set(3, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(1));
        assert!(!bv1.get(2));
        assert!(bv1.get(3));
    }

    #[test]
    fn test_union_bv1_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(8, true);
        bv2.set(0, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(8));
    }

    #[test]
    fn test_union_bv2_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv2.set(8, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(8));
    }

    #[test]
    fn test_intersect() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv1.set(1, true);
        bv2.set(1, true);
        bv2.set(3, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(bv1.get(1));
        assert!(!bv1.get(2));
        assert!(!bv1.get(3));
    }

    #[test]
    fn test_intersect_bv1_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(8, true);
        bv2.set(0, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(!bv1.get(8));
    }

    #[test]
    fn test_intersect_bv2_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv2.set(8, true);

        bv1.intersect(&bv2);

        assert!(!bv1.get(0));
        assert!(!bv1.get(8));
    }

    #[test]
    fn test_difference() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv1.set(1, true);
        bv2.set(1, true);
        bv2.set(3, true);

        bv1.union(&bv2);

        assert!(bv1.get(0));
        assert!(bv1.get(1));
        assert!(!bv1.get(2));
        assert!(bv1.get(3));
    }

    #[test]
    fn test_difference_bv1_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(8, true);
        bv2.set(0, true);

        bv1.difference(&bv2);

        assert!(!bv1.get(0));
        assert!(bv1.get(8));
    }

    #[test]
    fn test_difference_bv2_longer() {
        let mut bv1: BitVec<usize> = BitVec::new();
        let mut bv2: BitVec<usize> = BitVec::new();

        bv1.set(0, true);
        bv2.set(8, true);

        bv1.difference(&bv2);

        assert!(bv1.get(0));
        assert!(!bv1.get(8));
    }

    #[test]
    fn test_iter() {
        let mut bv: BitVec<usize> = BitVec::new();

        bv.set(0, true);
        bv.set(7, true);
        bv.set(8, true);
        bv.set(9, true);
        bv.set(16, true);

        assert_eq!(
            bv.iter().collect_vec(),
            vec![0, 7, 8, 9, 16]
        );
    }

    #[test]
    fn test_lazy_unused_no_side_effects() {
        let mut run = false;
        let _ = Lazy::new(|| { run = true; () });

        assert!(!run);
    }

    #[test]
    fn test_lazy_deref() {
        let lazy = Lazy::new(|| 100i32);

        assert_eq!(100, *Deref::deref(&lazy));
        assert_eq!(100, *Deref::deref(&lazy));
    }

    #[test]
    fn test_lazy_deref_mut() {
        let mut lazy = Lazy::new(|| 100i32);

        assert_eq!(100, *DerefMut::deref_mut(&mut lazy));
        assert_eq!(100, *DerefMut::deref_mut(&mut lazy));

        *lazy = 200i32;

        assert_eq!(200, *Deref::deref(&lazy));
        assert_eq!(200, *DerefMut::deref_mut(&mut lazy));
    }

    fn poisoned_lazy() -> Lazy<i32, impl FnOnce() -> i32> {
        let lazy = Lazy::new(|| panic!("Fake panic for testing"));

        let _ = std::panic::catch_unwind(|| Lazy::force_init(&lazy));
        lazy
    }

    #[test]
    #[should_panic(expected = "Lazy constructor previously panicked")]
    fn test_lazy_poison_deref() {
        let _ = Deref::deref(&poisoned_lazy());
    }

    #[test]
    #[should_panic(expected = "Lazy constructor previously panicked")]
    fn test_lazy_poison_deref_mut() {
        let _ = DerefMut::deref_mut(&mut poisoned_lazy());
    }

    #[test]
    fn test_lazy_unwrap() {
        assert_eq!(None, Lazy::unwrap(Lazy::new(|| 100i32)));
        assert_eq!(Some(100), Lazy::unwrap({ let lazy = Lazy::new(|| 100i32); Lazy::force_init(&lazy); lazy }));
        assert_eq!(None, Lazy::unwrap(poisoned_lazy()));
    }

    #[test]
    fn test_cache_basic() {
        let mut cache = FuncCache::new(|x| x);

        assert_eq!(*cache.get(1), 1);
        assert_eq!(*cache.get(2), 2);
        assert_eq!(*cache.get(3), 3);
    }

    #[test]
    fn test_cache_multicall() {
        let num_calls = Cell::new(0);
        let mut cache = FuncCache::new(|_| {
            num_calls.set(num_calls.get() + 1);
            num_calls.get() - 1
        });

        assert_eq!(*cache.get(1), 0);
        assert_eq!(*cache.get(1), 0);
        assert_eq!(num_calls.get(), 1);

        assert_eq!(*cache.get(0), 1);
        assert_eq!(*cache.get(0), 1);
        assert_eq!(num_calls.get(), 2);

        assert_eq!(*cache.get(1), 0);
        assert_eq!(num_calls.get(), 2);
    }

    #[test]
    fn test_cache_clear() {
        let num_calls = Cell::new(0);
        let mut cache = FuncCache::new(|_| {
            num_calls.set(num_calls.get() + 1);
            num_calls.get() - 1
        });

        assert_eq!(*cache.get(1), 0);
        assert_eq!(num_calls.get(), 1);

        cache.clear();

        assert_eq!(*cache.get(1), 1);
        assert_eq!(num_calls.get(), 2);
    }
}
