use funnssa::*;

fn main() {
    let mut builder = builder::Builder::new();
    let f = builder.create_function();
    builder.position_at_function(f);

    let a = builder.push_block();
    let b = builder.push_block();
    let c = builder.push_block();
    let e = builder.push_block();
    let f = builder.push_block();

    builder.position_at_bb(a);
    builder.set_term(Terminator::UncondBranch(b));

    builder.position_at_bb(b);
    builder.set_term(Terminator::CondBranch(Value::Integer(0), c, f));

    builder.position_at_bb(c);
    builder.set_term(Terminator::UncondBranch(e));

    builder.position_at_bb(e);
    builder.set_term(Terminator::UncondBranch(b));

    builder.position_at_bb(f);
    builder.set_term(Terminator::Return(None));

    builder.done();
}
