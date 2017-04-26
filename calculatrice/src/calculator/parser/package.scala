package calculator

package object parser {
	/**
	  * Combined results of two parser steps.
	  * This is the type returned by a parser step built with `a ~ b`.
	  */
	private[parser] case class ~[+A, +B](a: A, b: B)
}
