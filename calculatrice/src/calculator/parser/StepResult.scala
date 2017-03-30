package calculator.parser

import calculator.lexer.Token.SourceToken

/** The result of a parser step */
private[parser] sealed trait StepResult[+T] {
	/** Transforms the value of this result, if successful */
	def map[U](f: T => U): StepResult[U]

	/** Transforms the value of this result by using another result, if successful */
	def flatMap[U](f: (T, List[SourceToken]) => StepResult[U]): StepResult[U]

	/** If this step was a failure, the other result is used instead */
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U]

	/** If this step was a failure, mark it as definitive to prevent backtracking */
	def asDefinitive: StepResult[T]
}

/** Successful parsing */
private[parser] case class Success[+T](value: T, rest: List[SourceToken]) extends StepResult[T] {
	def map[U](f: (T) => U): StepResult[U] = Success(f(value), rest)
	def flatMap[U](f: (T, List[SourceToken]) => StepResult[U]): StepResult[U] = f(value, rest)
	def orElse[U >: T](fallback: => StepResult[U]): StepResult[U] = this
	def asDefinitive: StepResult[T] = this
}

/**
  * Failed parsing
  *
  * @param token      the unexpected token
  * @param definitive if definitive, this failure won't be recoverable by backtracking
  */
private[parser] case class Failure(token: SourceToken, definitive: Boolean = false) extends StepResult[Nothing] {
	def map[U](f: (Nothing) => U): StepResult[U] = this
	def flatMap[U](f: (Nothing, List[SourceToken]) => StepResult[U]): StepResult[U] = this
	def orElse[U >: Nothing](fallback: => StepResult[U]): StepResult[U] = if (definitive) this else fallback
	def asDefinitive: StepResult[Nothing] = if (definitive) this else copy(definitive = true)
}
