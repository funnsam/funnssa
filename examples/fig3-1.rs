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
    let zero = builder.push_int_const(32, 0);
    builder.set_uncond_br(a);

    {
        builder.position_at_bb(a);
        builder.set_cond_br(zero, b, c);
    }

    {
        builder.position_at_bb(b);
        builder.push_store(x, zero);
        builder.push_store(y, zero);
        builder.set_uncond_br(d);
    }

    {
        builder.position_at_bb(c);
        let tv = builder.push_load(x, typ);
        let yv = builder.push_load(y, typ);
        builder.push_store(x, yv);
        builder.push_store(y, tv);
        builder.set_cond_br(zero, d, e);
    }

    {
        builder.position_at_bb(d);
        let xv = builder.push_load(x, typ).try_into().unwrap();
        let yv = builder.push_load(y, typ).try_into().unwrap();
        let rv = builder.push_int_op(IntOp::Add, xv, yv);
        builder.push_store(x, rv);
        builder.set_cond_br(zero, a, e);
    }

    {
        builder.position_at_bb(e);
        let xv = builder.push_load(x, typ);
        builder.set_ret(Some(xv));
    }

    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");
}
