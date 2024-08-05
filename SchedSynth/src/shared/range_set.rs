#[derive(Clone,Eq)]
pub struct IntegerRangeSet {
    ranges: Vec<RangeType<i32>>
}

impl PartialEq for IntegerRangeSet {
    fn eq(&self, other: &IntegerRangeSet) -> bool {
        self.ranges == other.ranges
    }
}

#[derive(Clone,Eq)]
pub enum RangeType<Item> {
    Between(Item, Item),
    Set(Vec<Item>)
}

impl<T: PartialEq> PartialEq for RangeType<T> {
    fn eq(&self, other: &RangeType<T>) -> bool {
        match (self, other) {
            (RangeType::Between(a, b), RangeType::Between(c, d)) => a == c && b == d,
            (RangeType::Set(a), RangeType::Set(b)) => a == b,
            _ => false
        }
    }
}

impl std::fmt::Display for IntegerRangeSet {
 fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        for range in &self.ranges {
            match range {
                RangeType::Between(start, end) => {
                    result.push_str(&format!("{}-{},", start, end));
                },
                RangeType::Set(items) => {
                    for item in items {
                        result.push_str(&format!("{},", item));
                    }
                }
            }
        }
        write!(f, "{}", result)
    }
}

pub trait Range<Item>: Clone {
    fn contains(&self, i: Item) -> bool;
    fn to_string(&self) -> String;
}

pub trait TotalOrderRange<Item> {
    fn min_elt(&self) -> Item;
    fn max_elt(&self) -> Item;
}

impl TotalOrderRange<i32> for IntegerRangeSet {
    fn min_elt(&self) -> i32 {
        let mut min = std::i32::MAX;
        for range in &self.ranges {
            match range {
                RangeType::Between(start, _) => {
                    if *start < min {
                        min = *start;
                    }
                },
                RangeType::Set(items) => {
                    for item in items {
                        if *item < min {
                            min = *item;
                        }
                    }
                }
            }
        }
        min
    }

    fn max_elt(&self) -> i32 {
        let mut max = std::i32::MIN;
        for range in &self.ranges {
            match range {
                RangeType::Between(_, end) => {
                    if *end > max {
                        max = *end;
                    }
                },
                RangeType::Set(items) => {
                    for item in items {
                        if *item > max {
                            max = *item;
                        }
                    }
                }
            }
        }
        max
    }
}

impl ToString for RangeType<i32> {
    fn to_string(&self) -> String {
        match self {
            RangeType::Between(f, t) => f.to_string() + " -- " + &t.to_string(),
            RangeType::Set(items) => {
                let mut vec_items = String::new();
                for item in items {
                    vec_items.push_str(&(item.to_string() + ", "))
                }
                "{".to_owned() + &vec_items + "}"
            }
        }
    }
}

pub fn AnyIntegerSet() -> IntegerRangeSet {
    // return IntegerRangeSet for all numbers (approximate 
    // as range -2048 to 2048 for now)
    IntegerRangeSet {
        ranges: vec![RangeType::Between(-2048, 2048)]
    }
}

pub fn set_from_name(name: String) -> IntegerRangeSet {
    match name.as_str() {
        "Positives" => PositiveIntegerSet(),
        "SmallPositives" => SmallPositiveInegerSet(),
        _ => panic!("Unknown set name {}", name)
    }
}

pub fn PositiveIntegerSet() -> IntegerRangeSet {
    IntegerRangeSet {
        ranges: vec![RangeType::Between(1, 2048)]
    }
}

pub fn SmallPositiveInegerSet() -> IntegerRangeSet {
    IntegerRangeSet {
        ranges: vec![RangeType::Between(1, 64)]
    }
}
