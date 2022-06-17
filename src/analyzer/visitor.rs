use anyhow::Result;

use crate::parse2::AstNode;

pub trait Visitor {
    fn visit_node(&mut self, node: AstNode) -> Result<Option<AstNode>>;
}

pub trait VisitingIteratorExt: Iterator<Item = Result<AstNode>> {
    fn visitor<'a, V: 'a>(self, vis: &'a mut V) -> Visiting<'a, V, Self>
    where
        Self: Sized,
        V: Visitor;
}

impl<T: Iterator<Item = Result<AstNode>>> VisitingIteratorExt for T {
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
    I: Iterator<Item = Result<AstNode>>,
{
    type Item = Result<AstNode>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.incoming.next()?;
            match {
                match next {
                    Ok(node) => match self.visitor.visit_node(node) {
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
