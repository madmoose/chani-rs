use std::collections::{HashSet, VecDeque};

pub struct WorkQueue<T> {
    queue: VecDeque<T>,
    seen: HashSet<T>,
}

impl<T: Eq + std::hash::Hash + Clone> WorkQueue<T> {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            seen: HashSet::new(),
        }
    }

    pub fn push(&mut self, item: T) {
        if self.seen.insert(item.clone()) {
            self.queue.push_back(item);
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.queue.pop_front()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}
