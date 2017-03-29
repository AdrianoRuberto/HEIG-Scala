package calculator.parser

import calculator.lexer.Token.SourceToken

private[parser] sealed trait StepResult[+T] {
	def map[U](f: T => U): StepResult[U]
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U]
}

private[parser] case class Success[+T](value: T, rest: List[SourceToken]) extends StepResult[T] {
	def map[U](f: (T) => U): StepResult[U] = Success(f(value), rest)
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U] = this
}

private[parser] case class Failure(token: SourceToken) extends StepResult[Nothing] {
	def map[U](f: (Nothing) => U): StepResult[U] = this
	def orElse[U >: Nothing](fallback: => StepResult[U]): StepResult[U] = fallback
}
