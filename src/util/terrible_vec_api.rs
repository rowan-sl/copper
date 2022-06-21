/// self contains [at, len), returned contains [0, at)
#[inline(always)]
#[must_use]
#[allow(dead_code)]
pub fn take_n<T>(input: &mut Vec<T>, n: usize) -> Option<Vec<T>> {
    if n > input.len() {
        return None;
    }
    let mut new_input = input.split_off(n);
    std::mem::swap(&mut new_input, input);
    Some(new_input)
}

#[inline(always)]
#[must_use]
#[allow(dead_code)]
pub fn borrow_n<T>(input: &[T], n: usize) -> Option<&[T]> {
    if n > input.len() {
        return None;
    }
    Some(&input[..n])
}
