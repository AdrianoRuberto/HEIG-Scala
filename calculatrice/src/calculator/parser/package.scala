package calculator

package object parser {
	private[parser] case class ~[+A, +B](a: A, b: B)
}
