use std::collections::HashMap;

pub type Label = String;

pub struct LabelAllocator {
    next_label: u32
}

pub struct Intern {
    map: HashMap<String, Label>,
    label_alloc: LabelAllocator,
}

impl Default for LabelAllocator {
    fn default() -> Self {
        Self { next_label: 1 }
    }
}

impl LabelAllocator {
    fn allocate_label(&mut self) -> Label {
        let label = format!("intern_{:<02}", self.next_label);

        self.next_label += 1;

        label
    }
}

impl Intern {
    pub fn new() -> Intern {
        Intern {
            map: HashMap::new(),
            label_alloc: LabelAllocator::default()
        }
    }

    pub fn add(&mut self, data: &str) -> Label {
        use std::collections::hash_map::Entry;
        match self.map.entry(data.to_owned()) {
            Entry::Occupied(existing) => existing.get().clone(),
            Entry::Vacant(empty) => empty.insert(self.label_alloc.allocate_label()).clone(),
        }
    }


    pub fn get_labels(&self) -> Vec<(Label, String)> {
        self.map.iter().map(|(data, label)| {
            (label.clone(), data.clone())
        }).collect()
    }
}