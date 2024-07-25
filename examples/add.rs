use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();

    let typ = ValueType::Int(32);
    let (start, _) = builder.create_function(Linkage::Public, "_start", vec![], Some(typ));

    builder.position_at_function(start);
    let l = builder.push_block();
    builder.position_at_bb(l);
    let one = builder.push_int_const(32, 1);
    let two = builder.push_int_const(32, 2);
    let res = builder.push_int_op(IntOp::Add, one, two);
    builder.set_ret(Some(res.into()));

    println!("{builder}");
    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");

    let sel = arch::x86_64::X64Selector;
    let vc = arch::VCode::generate::<_, regalloc::linear::LinearAlloc<_>>(prog, sel);

    vc.emit_assembly(&mut std::io::stdout()).unwrap();
}
