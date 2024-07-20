use funnssa::*;

fn main() {
    let mut builder = builder::Builder::new();
    let f = builder.create_function();
    builder.position_at_function(f);

    // https://users-cs.au.dk/gerth/advising/thesis/henrik-knakkegaard-christensen.pdf page 19
    let entry = builder.push_block();
    let a = builder.push_block();
    let b = builder.push_block();
    let c = builder.push_block();
    let d = builder.push_block();
    let e = builder.push_block();

    builder.position_at_bb(entry);
    builder.set_term(Terminator::CondBranch(Value::Integer(0), d, b));

    builder.position_at_bb(a);
    builder.set_term(Terminator::Return(None));

    builder.position_at_bb(b);
    builder.set_term(Terminator::CondBranch(Value::Integer(0), e, a));

    builder.position_at_bb(c);
    builder.set_term(Terminator::UncondBranch(a));

    builder.position_at_bb(d);
    builder.set_term(Terminator::CondBranch(Value::Integer(0), c, e));

    builder.position_at_bb(e);
    builder.set_term(Terminator::UncondBranch(e));

    builder.done();
}
