use anyhow::Result;

use crate::parsing::parser::ast::Expr;

pub trait Visitor {
    fn visit_expr(&mut self, expr: Expr) -> Result<Option<Expr>>;
}

pub trait VisitingIteratorExt: Iterator<Item = Result<Expr>> {
    fn visitor<'a, V: 'a>(self, vis: &'a mut V) -> Visiting<'a, V, Self>
    where
        Self: Sized,
        V: Visitor;
}

impl<T: Iterator<Item = Result<Expr>>> VisitingIteratorExt for T {
    fn visitor<'a, V: 'a>(self, vis: &'a mut V) -> Visiting<'a, V, T>
    where
        Self: Sized,
        V: Visitor,
    {
        Visiting {
            incoming: self,
            visitor: vis,
        }
    }
}

pub struct Visiting<'a, V: 'a, I> {
    incoming: I,
    visitor: &'a mut V,
}

impl<'a, V: 'a, I> Iterator for Visiting<'a, V, I>
where
    V: Visitor,
    I: Iterator<Item = Result<Expr>>,
{
    type Item = Result<Expr>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.incoming.next()?;
            match {
                match next {
                    Ok(expr) => match self.visitor.visit_expr(expr) {
                        Ok(Some(ret)) => Some(Ok(ret)),
                        Ok(None) => None,
                        Err(e) => Some(Err(e)),
                    },
                    Err(e) => Some(Err(e)),
                }
            } {
                Some(r) => break Some(r),
                None => continue,
            }
        }
    }
}
