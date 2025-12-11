use std::collections::HashSet;
use std::hash::Hash;

pub trait Accumulate<T> {
    fn accumulate(&mut self, item: T);
}

impl<T> Accumulate<T> for Vec<T> {
    #[inline(always)]
    fn accumulate(&mut self, item: T) {
        self.push(item);
    }
}

impl<T: Eq + Hash> Accumulate<T> for HashSet<T> {
    #[inline(always)]
    fn accumulate(&mut self, item: T) {
        self.insert(item);
    }
}

impl<T> Accumulate<T> for () {
    #[inline(always)]
    fn accumulate(&mut self, _item: T) {}
}
