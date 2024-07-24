use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();
    let int = ValueType::Int(32);
    let (start, _) = builder.create_function(Linkage::Public, "_start", vec![], None);
    let (fib, args) = builder.create_function(Linkage::Public, "rfib", vec![int], Some(int));

    {
        builder.position_at_function(start);
        let l = builder.push_block();

        builder.position_at_bb(l);
        let v = builder.push_int_const(32, 10);
        let _ = builder.push_call(fib, vec![v.into()]);
        builder.set_ret(None);
    }

    {
        builder.position_at_function(fib);
        let e = builder.push_block();
        let a = builder.push_block();
        let b = builder.push_block();
        let n = args[0].try_into().unwrap();

        builder.position_at_bb(e);
        let one = builder.push_int_const(32, 1);
        let c = builder.push_int_op(IntOp::ULe, n, one);
        builder.set_cond_br(c, a, b);

        builder.position_at_bb(a);
        builder.set_ret(Some(n.into()));

        builder.position_at_bb(b);
        let nm1 = builder.push_int_op(IntOp::Sub, n, one);
        let nm2 = builder.push_int_op(IntOp::Sub, nm1, one);
        let l = builder.push_call(fib, vec![nm1.into()]).unwrap().try_into().unwrap();
        let r = builder.push_call(fib, vec![nm2.into()]).unwrap().try_into().unwrap();
        let ret = builder.push_int_op(IntOp::Add, l, r);
        builder.set_ret(Some(ret.into()));
    }

    println!("{builder}");
    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");

    let sel = arch::urcl::UrclSelector::default();
    let vc = arch::VCode::generate::<_, regalloc::linear::LinearAlloc<_>>(prog, sel);

    vc.emit_assembly(&mut std::io::stdout()).unwrap();
}
