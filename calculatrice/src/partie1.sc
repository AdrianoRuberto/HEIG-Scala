
def operation(op: Char, a: Double, b: Double): Double = op match {
	case '+' => a + b
	case '-' => a - b
	case '/' => a / b
	case '*' => a * b
	case '^' => Math.pow(a, b)
}

def operation(op: Char, a: Double): Double = op match {
	case '!' => factorial(a.toLong)
}

def factorial(n: Long): Long = {
	require(n >= 0)
	def loop(n: Long, acc: Long): Long = {
		if (n <= 1) acc
		else loop(n - 1, acc * n)
	}
	loop(n, 1)
}

def gcd(a: Long, b: Long): Long = {
	if (b == 0) a else gcd(b, a % b)
}

def solve(a: Double, b: Double, c: Double): String = {
	require(a != 0)
	val delta = b * b - 4 * a * c

	if (delta > 0) {
		val first = (-b + Math.sqrt(delta)) / (2 * a)
		val second = (-b - Math.sqrt(delta)) / (2 * a)
		s"$first, $second"
	} else {
		val real = -b / (2 * a)
		if (delta == 0) {
			real.toString
		} else {
			val imaginary = Math.sqrt(-delta) / (2 * a)
			s"$real ± ${imaginary}i"
		}
	}
}

def sqrt(n: Double): Double = {
	require(n >= 0)
	val epsilon = 0.0001
	def loop(x: Double): Double = {
		if (Math.abs(x * x - n) / n < epsilon) x
		else loop((x + n / x) / 2)
	}
	loop(1)
}

def prime(n: Long): Boolean = {
	require(n.isWhole && n > 0)
	def loop(x: Long): Boolean = {
		if (x == 1) true
		else if (n % x == 0) false
		else loop(x - 1)
	}
	loop(Math.sqrt(n).toLong)
}

def egcd(u: Long, v: Long): (Long, Long, Long) = ???

def modInvert(u: Long, v: Long): Long = {
	val (x, _, z) = egcd(u, v)
	assert(z == 1)
	x
}
