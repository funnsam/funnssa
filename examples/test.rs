use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();
    let fig3_1 = builder.create_function("fig3_1");
    builder.position_at_function(fig3_1);
    let typ = ValueType::Int(32);

    let entry = builder.push_block();

    builder.position_at_bb(entry);
    let x = builder.push_alloc(typ.into());
    let zero = builder.push_int_const(32, 0);
    builder.push_store(x, zero.into());
    let a = builder.push_load(x, typ);
    let b = builder.push_int_op(IntOp::Add, a.try_into().unwrap(), zero);
    builder.push_store(x, b.into());
    let h = builder.push_load(x, typ);
    builder.set_ret(Some(h));

    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");
}
