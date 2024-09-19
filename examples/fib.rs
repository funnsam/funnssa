use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();

    let typ = ValueType::Int(32);
    let (fib, _) = builder.create_function(Linkage::Public, "_start", vec![], Some(typ));
    let (print, _) = builder.create_function(Linkage::External, "print_num", vec![typ], None);

    {
        builder.position_at_function(fib);

        let entry = builder.push_block();
        let iter = builder.push_block();
        let exit = builder.push_block();

        builder.position_at_bb(entry);
        let x = builder.build_alloc(typ.into());
        let y = builder.build_alloc(typ.into());
        let one = builder.build_int_const(32, 1);
        let zero = builder.build_int_const(32, 0);
        let xmax = builder.build_int_const(32, 1000);
        builder.build_store(x, one);
        builder.build_store(y, zero);
        builder.set_uncond_br(iter);

        builder.position_at_bb(iter);
        let xv = builder.build_load(x, typ).try_into().unwrap();
        let yv = builder.build_load(y, typ).try_into().unwrap();
        let nx = builder.build_int_op(IntOp::Add, xv, yv);
        let ny = xv;
        builder.build_call(print, vec![ny.into()]);
        builder.build_store(x, nx);
        builder.build_store(y, ny);

        let c = builder.build_int_op(IntOp::ULt, ny, xmax);
        builder.set_cond_br(c, iter, exit);

        builder.position_at_bb(exit);
        let zero = builder.build_int_const(32, 0);
        builder.set_ret(Some(zero.into()));
    }

    eprintln!("{builder}");
    let prog = builder.done();
    eprintln!("{prog}");

    let sel = arch::urcl::UrclSelector::new();
    let vc = arch::VCode::generate::<_, regalloc::graph::GraphAlloc<_>>(&prog, sel);

    vc.emit_assembly(&mut std::io::stdout()).unwrap();
}
