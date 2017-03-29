package calculator.parser
import calculator.lexer.Token
import calculator.lexer.Token.SourceToken
import calculator.parser.Step.Tokens

import scala.annotation.tailrec

private[parser] trait Step[T] {
	/** Apply this parser step to the given input tokens */
	def apply(tokens: Tokens): StepResult[T]

	/**
	  * Constructs a new parser step by combining this step with another one.
	  * The resulting step will return the results of both step only if both steps succeed.
	  */
	def ~[B](next: => Step[B]): Step[T ~ B] = {
		(tokens: Tokens) => apply(tokens).flatMap { case (a, rest) => next(rest).map(b => new ~(a, b)) }
	}

	/**
	  * Constructs a new parser step by combining this step with another one.
	  * The resulting step will return the result of the first step if it succeed, otherwise
	  * the result of the other step will be returned.
	  */
	def |[B >: T](next: => Step[B]): Step[B] = (tokens: Tokens) => apply(tokens) orElse next(tokens)

	/** Constructs an optional parser step from this step */
	def ? : Step[Option[T]] = (tokens: Tokens) => apply(tokens).map(Some.apply) orElse Success(None, tokens)

	/** Constructs a new parser step that parses a repetition of this step */
	def * : Step[List[T]] = {
		@tailrec
		def loop(tokens: Tokens, acc: List[T] = Nil): StepResult[List[T]] = {
			apply(tokens) match {
				case Success(item, next) => loop(next, item :: acc)
				case fail: Failure => Success(acc.reverse, tokens)
			}
		}
		(tokens: Tokens) => loop(tokens)
	}

	/** Sets failure of this parse step as definitive to prevent backtracking */
	def ! : Step[T] = (tokens: Tokens) => apply(tokens).asDefinitive

	/** Constructs a new parser step that parses the same input as this step but mapping its result */
	def map[B](f: T => B): Step[B] = (tokens: Tokens) => apply(tokens).map(f)
}

private[parser] object Step {
	/** Input type of parser Step functions */
	private type Tokens = List[SourceToken]

	/** Creates a parser Step that matches a single token */
	def single[T](f: PartialFunction[Token, T]): Step[T] = (tokens: Tokens) => tokens match {
		case tok :: next => f.lift(tok.token).map(Success(_, next)).getOrElse(Failure(tok))
		case Nil => Failure(SourceToken(Token.Unknown('?'), 0))
	}
}
