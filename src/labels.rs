pub struct LabelAllocator {
    next_label: u32,
}

impl Default for LabelAllocator {
    fn default() -> Self {
        Self { next_label: 1 }
    }
}

impl LabelAllocator {
    pub fn allocate_label(&mut self) -> String {
        let label = format!("label_{:<02}", self.next_label);

        self.next_label += 1;

        label
    }
}