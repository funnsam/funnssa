use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();
    let fig3_1 = builder.create_function("fig3_1");
    builder.position_at_function(fig3_1);
    let typ = ValueType::Int(32);

    let entry = builder.push_block();
    let a = builder.push_block();
    let b = builder.push_block();
    let c = builder.push_block();
    let d = builder.push_block();
    let e = builder.push_block();

    builder.position_at_bb(entry);
    let x = builder.push_alloc(typ.into());
    let y = builder.push_alloc(typ.into());
    builder.set_term(Terminator::UncondBranch(a));

    {
        builder.position_at_bb(a);
        let xv = builder.push_load(x, typ).try_into().unwrap();
        builder.set_term(Terminator::CondBranch(xv, b, c));
    }

    {
        builder.position_at_bb(b);
        let zero = builder.int_const(32, 0);
        builder.push_store(x, zero.into());
        builder.push_store(y, zero.into());
        builder.set_term(Terminator::UncondBranch(d));
    }

    {
        builder.position_at_bb(c);
        let tv = builder.push_load(x, typ);
        let yv = builder.push_load(y, typ);
        builder.push_store(x, yv);
        builder.push_store(y, tv);
        builder.set_term(Terminator::CondBranch(tv.try_into().unwrap(), d, e));
    }

    {
        builder.position_at_bb(d);
        let xv = builder.push_load(x, typ).try_into().unwrap();
        let yv = builder.push_load(y, typ).try_into().unwrap();
        let rv = builder.push_int_op(IntOp::Add, xv, yv);
        builder.push_store(x, rv.into());
        builder.set_term(Terminator::CondBranch(xv, a, e));
    }

    {
        builder.position_at_bb(e);
        let xv = builder.push_load(x, typ);
        builder.set_term(Terminator::Return(Some(xv)));
    }

    let prog = builder.done();
    println!("{prog}");
    prog.print_cfg();
}
