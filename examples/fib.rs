use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();

    let typ = ValueType::Int(32);
    let (fib, _) = builder.create_function(Linkage::Public, "main", vec![], Some(typ));
    let (print, _) = builder.create_function(Linkage::External, "print", vec![typ], None);

    {
        builder.position_at_function(fib);

        let entry = builder.push_block();
        let iter = builder.push_block();
        let exit = builder.push_block();

        builder.position_at_bb(entry);
        let x = builder.push_alloc(typ.into());
        let y = builder.push_alloc(typ.into());
        let one = builder.push_int_const(32, 1);
        let zero = builder.push_int_const(32, 0);
        let xmax = builder.push_int_const(32, 1000);
        builder.push_store(x, one);
        builder.push_store(y, zero);
        builder.set_uncond_br(iter);

        builder.position_at_bb(iter);
        let xv = builder.push_load(x, typ).try_into().unwrap();
        let yv = builder.push_load(y, typ).try_into().unwrap();
        let nx = builder.push_int_op(IntOp::Add, xv, yv);
        let ny = xv;
        // builder.push_call(print, vec![ny.into()]);
        builder.push_store(x, nx);
        builder.push_store(y, ny);

        let c = builder.push_int_op(IntOp::ULt, ny, xmax);
        builder.set_cond_br(c, iter, exit);

        builder.position_at_bb(exit);
        let zero = builder.push_int_const(32, 0);
        builder.set_ret(Some(zero.into()));
    }

    println!("{builder}");
    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");

    let sel = arch::x86_64::X64Selector::new();
    let vc = arch::VCode::generate::<_, regalloc::graph::GraphAlloc<_>>(&prog, sel);

    vc.emit_assembly(&mut std::io::stdout()).unwrap();
}
