use std::{cell::Cell, fmt::Write};

use crate::proc_macro::{Ident, Literal};

use crate::{error::Error, instruction::Instruction};

pub fn execute(instructions: &[Instruction]) -> Result<(), Error> {
    let mut next_instruction = 0;

    while let Some(instruction) = instructions.get(next_instruction) {
        next_instruction += 1;

        match *instruction {
            Instruction::BoolAnd { lhs, rhs, dst } => {
                dst.set(lhs.get() & rhs.get());
            }
            Instruction::BoolCopy { val, dst } => {
                dst.set(val.get());
            }
            Instruction::BoolDisplay { val, dst } => {
                update_cell(dst, |dst| {
                    write!(dst, "{}", val.get()).expect("displaying `bool` should not fail");
                });
            }
            Instruction::BoolEmit { val, dst, span } => {
                update_cell(dst, |dst| {
                    dst.extend([Ident::new(if val.get() { "true" } else { "false" }, span)])
                });
            }
            Instruction::BoolNot { val, dst } => {
                dst.set(!val.get());
            }
            Instruction::BoolOr { lhs, rhs, dst } => {
                dst.set(lhs.get() | rhs.get());
            }
            Instruction::BoolXor { lhs, rhs, dst } => {
                dst.set(lhs.get() ^ rhs.get());
            }
            Instruction::IntAdd {
                lhs,
                rhs,
                dst,
                span,
            } => {
                if let Some(result) = lhs.get().checked_add(rhs.get()) {
                    dst.set(result);
                } else {
                    return Err(Error::new(span, "attempt to add with overflow"));
                }
            }
            Instruction::IntCopy { val, dst } => {
                dst.set(val.get());
            }
            Instruction::IntDisplay { val, dst } => {
                update_cell(dst, |dst| {
                    write!(dst, "{}", val.get()).expect("displaying `i32` should not fail");
                });
            }
            Instruction::IntDiv {
                lhs,
                rhs,
                dst,
                span,
            } => {
                if let Some(result) = lhs.get().checked_div(rhs.get()) {
                    dst.set(result);
                } else {
                    return Err(Error::new(
                        span,
                        if rhs.get() == 0 {
                            "attempt to divide by zero"
                        } else {
                            "attempt to divide with overflow"
                        },
                    ));
                }
            }
            Instruction::IntEmit { val, dst, span } => {
                let mut literal = Literal::i32_unsuffixed(val.get());
                literal.set_span(span);
                update_cell(dst, |dst| dst.extend([literal]));
            }
            Instruction::IntMul {
                lhs,
                rhs,
                dst,
                span,
            } => {
                if let Some(result) = lhs.get().checked_mul(rhs.get()) {
                    dst.set(result);
                } else {
                    return Err(Error::new(span, "attempt to multiply with overflow"));
                }
            }
            Instruction::IntNeg { val, dst, span } => {
                if let Some(result) = val.get().checked_neg() {
                    dst.set(result);
                } else {
                    return Err(Error::new(span, "attempt to negate with overflow"));
                }
            }
            Instruction::IntRem {
                lhs,
                rhs,
                dst,
                span,
            } => {
                if let Some(result) = lhs.get().checked_rem(rhs.get()) {
                    dst.set(result);
                } else {
                    return Err(Error::new(
                        span,
                        if rhs.get() == 0 {
                            "attempt to calculate the remainder with a divisor of zero"
                        } else {
                            "attempt to calculate the remainder with overflow"
                        },
                    ));
                }
            }
            Instruction::IntSub {
                lhs,
                rhs,
                dst,
                span,
            } => {
                if let Some(result) = lhs.get().checked_sub(rhs.get()) {
                    dst.set(result);
                } else {
                    return Err(Error::new(span, "attempt to subtract with overflow"));
                }
            }
            Instruction::StrCopy { val, dst } => {
                update_2_cells(val, dst, |val, dst| val.clone_into(dst));
            }
            Instruction::StrDisplay { val, dst } => {
                update_2_cells(val, dst, |val, dst| *dst += val);
            }
            Instruction::StrEmit { val, dst, span } => {
                update_2_cells(val, dst, |val, dst| {
                    let is_valid_ident = val
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                        && val.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');

                    if is_valid_ident {
                        dst.extend([Ident::new(val, span)]);
                        Ok(())
                    } else {
                        Err(Error::new(
                            span,
                            format!("attempt to emit invalid identifier `{val}`"),
                        ))
                    }
                })?;
            }
            Instruction::StrEmitStr { val, dst, span } => {
                update_2_cells(val, dst, |val, dst| {
                    let mut literal = Literal::string(val);
                    literal.set_span(span);
                    dst.extend([literal]);
                });
            }
        }
    }

    Ok(())
}

fn update_cell<T: Default, O>(cell: &Cell<T>, f: impl FnOnce(&mut T) -> O) -> O {
    let mut take = cell.take();
    let output = f(&mut take);
    cell.set(take);
    output
}

fn update_2_cells<T0: Default, T1: Default, O>(
    cell_0: &Cell<T0>,
    cell_1: &Cell<T1>,
    f: impl FnOnce(&mut T0, &mut T1) -> O,
) -> O {
    let mut take_0 = cell_0.take();
    let mut take_1 = cell_1.take();
    let output = f(&mut take_0, &mut take_1);
    cell_0.set(take_0);
    cell_1.set(take_1);
    output
}
