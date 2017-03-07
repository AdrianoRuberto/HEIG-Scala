
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
  def loop(n: Long, acc: Long): Long =
    if (n == 0) acc
    else loop(n - 1, acc * n)

  loop(n, 1)
}

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

def solve(a: Double, b: Double, c: Double): String = {
  if (a == 0) throw new UnsupportedOperationException

  val delta = b * b - 4 * a * c

  if (delta > 0) {
    (
      (-b + Math.sqrt(delta)) / (2 * a),
      (-b - Math.sqrt(delta)) / (2 * a)
    ).toString()
  } else if (delta == 0) {
    (-b / (2 * a)).toString
  } else {
    ((-b / (2 * a)).toString + " + " + ((-b + Math.sqrt(-delta)) / (2 * a)) + "i",
      (-b / (2 * a)).toString + " + " + ((+b + Math.sqrt(-delta)) / (2 * a)) + "i").toString()
  }
}

solve(1, 1, 1)



