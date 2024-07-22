use funnssa::{*, types::*};

fn main() {
    let mut builder = builder::Builder::new();
    let crit = builder.create_function("crit");
    builder.position_at_function(crit);
    let typ = ValueType::Int(32);

    let entry = builder.push_block();
    let a = builder.push_block();
    let b = builder.push_block();

    builder.position_at_bb(entry);
    let x = builder.push_alloc(typ.into());
    let zero = builder.push_int_const(32, 0);
    builder.push_store(x, zero);
    builder.set_cond_br(zero, a, b);

    {
        builder.position_at_bb(a);
        let one = builder.push_int_const(32, 1);
        builder.push_store(x, one);
        builder.set_uncond_br(b);
    }

    {
        builder.position_at_bb(b);
        let xv = builder.push_load(x, typ);
        builder.set_ret(Some(xv));
    }

    let prog = builder.done();
    prog.print_cfg();
    println!("{prog}");

    let vc = arch::VCode::generate::<arch::urcl::UrclSelector>(prog);
    println!("{vc}");
}
