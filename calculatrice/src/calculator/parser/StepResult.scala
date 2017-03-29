package calculator.parser

import calculator.lexer.Token.SourceToken

private[parser] sealed trait StepResult[+T] {
	def map[U](f: T => U): StepResult[U]
	def flatMap[U](f: (T, List[SourceToken]) => StepResult[U]): StepResult[U]
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U]
	def asDefinitive: StepResult[T]
}

private[parser] case class Success[+T](value: T, rest: List[SourceToken]) extends StepResult[T] {
	def map[U](f: (T) => U): StepResult[U] = Success(f(value), rest)
	def flatMap[U](f: (T, List[SourceToken]) => StepResult[U]): StepResult[U] = f(value, rest)
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U] = this
	def asDefinitive: StepResult[T] = this
}

private[parser] case class Failure(token: SourceToken, definitive: Boolean = false) extends StepResult[Nothing] {
	def map[U](f: (Nothing) => U): StepResult[U] = this
	def flatMap[U](f: (Nothing, List[SourceToken]) => StepResult[U]): StepResult[U] = this
	def orElse[U >: Nothing](fallback: => StepResult[U]): StepResult[U] = if (definitive) this else fallback
	def asDefinitive: StepResult[Nothing] = if (definitive) this else copy(definitive = true)
}
