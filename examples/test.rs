use funnssa::*;

fn main() {
    let mut builder = builder::Builder::new();
    let f = builder.create_function();
    builder.position_at_function(f);

    // https://users-cs.au.dk/gerth/advising/thesis/henrik-knakkegaard-christensen.pdf page 19
    // TODO:

    builder.done();
}
