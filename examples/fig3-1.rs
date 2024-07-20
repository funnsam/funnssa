use funnssa::*;

fn main() {
    let mut builder = builder::Builder::new();
    let fig3_1 = builder.create_function();
    builder.position_at_function(fig3_1);

    let entry = builder.push_block();
    let a = builder.push_block();
    let b = builder.push_block();
    let c = builder.push_block();
    let d = builder.push_block();
    let e = builder.push_block();
    let x = builder.alloc_ptr();
    let y = builder.alloc_ptr();

    {
        builder.position_at_bb(entry);
        builder.set_term(Terminator::UncondBranch(a));
    }

    {
        builder.position_at_bb(a);
        let xv = builder.alloc_var();
        builder.push_inst(Instruction::LoadPtr(xv, x));
        builder.set_term(Terminator::CondBranch(Value::Variable(xv), b, c));
    }

    {
        builder.position_at_bb(b);
        builder.push_inst(Instruction::StorePtr(y, Value::Integer(0)));
        builder.push_inst(Instruction::StorePtr(x, Value::Integer(0)));
        builder.set_term(Terminator::UncondBranch(d));
    }

    {
        builder.position_at_bb(c);
        let tv = builder.alloc_var();
        let yv = builder.alloc_var();
        builder.push_inst(Instruction::LoadPtr(tv, x));
        builder.push_inst(Instruction::LoadPtr(yv, y));
        builder.push_inst(Instruction::StorePtr(x, Value::Variable(yv)));
        builder.push_inst(Instruction::StorePtr(y, Value::Variable(tv)));
        builder.set_term(Terminator::CondBranch(Value::Variable(tv), d, e));
    }

    {
        builder.position_at_bb(d);
        let xv = builder.alloc_var();
        let yv = builder.alloc_var();
        let rv = builder.alloc_var();
        builder.push_inst(Instruction::LoadPtr(xv, x));
        builder.push_inst(Instruction::LoadPtr(yv, y));
        builder.push_inst(Instruction::Add(rv, Value::Variable(xv), Value::Variable(yv)));
        builder.push_inst(Instruction::StorePtr(x, Value::Variable(rv)));
        builder.set_term(Terminator::CondBranch(Value::Variable(xv), a, e));
    }

    {
        builder.position_at_bb(e);
        let xv = builder.alloc_var();
        builder.push_inst(Instruction::LoadPtr(xv, x));
        builder.set_term(Terminator::Return(Some(Value::Variable(xv))));
    }

    let prog = builder.done();
    println!("{prog}");
}
