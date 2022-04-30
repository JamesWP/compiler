use crate::ast;

type Result<T> = std::io::Result<T>;
pub fn evaluate_condition_expression(expr: &ast::Expression) -> Result<bool> {
    let value = evaluate_expression(expr)?;

    // if the expression evaluates to 0 then it is false otherwise it is true
    Ok(value != 0)
}

fn evaluate_expression(expr: &ast::Expression) -> Result<i32> {
    match &expr.node {
        ast::ExpressionNode::Binary(_, _, _) => todo!(),
        ast::ExpressionNode::Unary(_, _) => todo!(),
        ast::ExpressionNode::Conditional(_, _, _) => todo!(),
        ast::ExpressionNode::Value(v) => match v {
            ast::Value::Literal(value) => match value {
                ast::LiteralValue::Int32(value) => Ok(*value),
                ast::LiteralValue::StringLiteral(_) => todo!(),
                ast::LiteralValue::CharLiteral(_) => todo!(),
            },
            ast::Value::Identifier(_, _) => todo!(),
        },
        ast::ExpressionNode::Call(_, _) => todo!(),
    }
}
