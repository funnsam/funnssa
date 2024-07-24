pub fn parallel_move<T: Clone + Copy + Eq, F: FnMut(&T, &T) -> T>(
    pcopy: &mut Vec<(T, T)>,
    alloc: &mut F,
) -> Vec<(T, T)> {
    let mut seq = Vec::with_capacity(pcopy.len());
    while pcopy.iter().any(|(b, a)| a != b) {
        if let Some((i, (b, a))) = pcopy
            .iter()
            .enumerate()
            .find(|(_, (b, _))| !pcopy.iter().any(|(_, b2)| b2 == b))
        {
            seq.push((*b, *a));
            pcopy.remove(i);
        } else {
            let (i, (b, a)) = pcopy.iter().enumerate().find(|(_, (b, a))| a != b).unwrap();
            let ap = alloc(b, a);
            seq.push((ap, *a));
            pcopy[i] = (*b, ap);
        }
    }

    seq
}
