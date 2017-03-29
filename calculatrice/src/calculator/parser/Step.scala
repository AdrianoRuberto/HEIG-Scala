package calculator.parser
import calculator.lexer.Token
import calculator.lexer.Token.SourceToken
import calculator.parser.Step.Tokens

import scala.annotation.tailrec

private[parser] trait Step[T] {
	def apply(tokens: Tokens): StepResult[T]

	def ~[B](next: => Step[B]): Step[T ~ B] = Step.and(this, next)
	def |[B >: T](next: => Step[B]): Step[B] = Step.or(this, next)

	def ? : Step[Option[T]] = Step.maybe(this)
	def * : Step[List[T]] = Step.repeat(this)

	def map[B](f: T => B): Step[B] = Step.simple(apply(_).map(f))
}

private[parser] object Step {
	private type Tokens = List[SourceToken]

	def simple[T](f: Tokens => StepResult[T]): Step[T] = (tokens: Tokens) => f(tokens)

	def partial[T](f: PartialFunction[Tokens, StepResult[T]]): Step[T] = Step.simple { tokens: Tokens =>
		f.applyOrElse[Tokens, StepResult[T]](tokens, _ => Failure(tokens.head))
	}

	def single[T](f: PartialFunction[Token, T]): Step[T] = Step.partial {
		case tok :: next if f.isDefinedAt(tok.token) => Success(f(tok.token), next)
	}

	def and[T, U](first: Step[T], second: => Step[U]): Step[T ~ U] = {
		(tokens: Tokens) => {
			first(tokens) match {
				case Success(a, rest) =>
					second(rest) match {
						case Success(b, next) => Success(new ~(a, b), next)
						case fail: Failure => fail
					}
				case fail: Failure => fail
			}
		}
	}

	def maybe[T](step: Step[T]): Step[Option[T]] = {
		(tokens: Tokens) => step(tokens).map(Some.apply) orElse Success(None, tokens)
	}

	def repeat[T](step: Step[T]): Step[List[T]] = {
		@tailrec
		def loop(tokens: Tokens, acc: List[T] = Nil): StepResult[List[T]] = {
			step(tokens) match {
				case Success(item, next) => loop(next, item :: acc)
				case fail: Failure => Success(acc.reverse, tokens)
			}
		}
		(tokens: Tokens) => loop(tokens)
	}

	def or[T, U >: T](first: Step[T], second: => Step[U]): Step[U] = {
		(tokens: Tokens) => first(tokens) orElse second(tokens)
	}
}
