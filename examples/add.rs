use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();

    let typ = ValueType::Int(32);
    let (start, _) = builder.create_function(Linkage::Public, "main", vec![], Some(typ));

    builder.position_at_function(start);
    let l = builder.push_block();
    builder.position_at_bb(l);
    let one = builder.build_int_const(32, 1);
    let two = builder.build_int_const(32, 2);
    let res = builder.build_int_op(IntOp::Add, one, two);
    builder.set_ret(Some(res.into()));

    println!("{builder}");
    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");

    let sel = arch::urcl::UrclSelector::new();
    let vc = arch::VCode::generate::<_, regalloc::graph::GraphAlloc<_>>(&prog, sel);

    vc.emit_assembly(&mut std::io::stdout()).unwrap();
}
