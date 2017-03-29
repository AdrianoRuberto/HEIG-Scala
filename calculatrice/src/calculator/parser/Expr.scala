package calculator.parser

sealed trait Expr

object Expr {
	case class Literal(value: Double) extends Expr
	case class Reference(name: String) extends Expr
	case class Assign(name: String, value: Expr) extends Expr
	case class Binary(operator: String, lhs: Expr, rhs: Expr) extends Expr
	case class Unary(operator: String, operand: Expr) extends Expr
	case class Call(fun: String, args: Seq[Expr]) extends Expr
}

