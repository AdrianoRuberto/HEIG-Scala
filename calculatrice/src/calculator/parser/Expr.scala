package calculator.parser

/** Base trait for expression AST */
sealed trait Expr

object Expr {
	/** A number literal */
	case class Literal(value: Double) extends Expr

	/** A variable reference */
	case class Reference(name: String) extends Expr

	/** An assign operation */
	case class Assign(name: String, value: Expr) extends Expr

	/** A binary operation */
	case class Binary(operator: String, lhs: Expr, rhs: Expr) extends Expr

	/** An unary operation */
	case class Unary(operator: String, operand: Expr) extends Expr

	/** A function call */
	case class Call(fun: String, args: List[Expr]) extends Expr
}

