package calculator

import scala.io.StdIn.readLine

object Main {
	val engine = new Calculator

	def main(args: Array[String]): Unit = {
		println("Welcome to the Scala calculator !")
		console()
	}

	def console(): Unit = {
		print("> ")
		readLine match {
			case "quit" =>
				println("Bye !")
			case "usage" =>
				usage()
				console()
			case line =>
				engine.execute(line) match {
					case CalculatorValue(value) => println(s"ans = $value")
					case CalculatorMessage(msg) => println(msg)
				}
				console()
		}
	}

	def usage() = ???
}
