use super::*;

pub trait Optimize {
    fn optimize(&mut self, ctx: &mut Context);
}
