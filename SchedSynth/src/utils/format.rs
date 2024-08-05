use std::collections::HashSet;
// pretty print hashset into a string assuming that the elements are formatable.

pub fn hashset_to_string<T: std::fmt::Display>(set: &HashSet<T>) -> String {
    let mut result = String::new();
    result.push_str("{");
    for item in set {
        result.push_str(&format!("{}, ", item));
    }
    result.push_str("}");
    result
}
