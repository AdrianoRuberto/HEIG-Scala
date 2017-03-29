package calculator

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
	val engine = new Calculator

	def main(args: Array[String]): Unit = {
		println("Welcome to the Scala calculator !")
		console()
	}

	@tailrec
	def console(): Unit = {
		print("> ")
		readLine match {
			case "quit" => println("Bye !"); return
			case "usage" => usage()
			case line if line.trim.nonEmpty =>
				engine.execute(line) match {
					case CalculatorValue(value) => println(s"ans = $value")
					case CalculatorMessage(msg) => println(msg)
				}
			case blank => // ignore
		}
		console()
	}

	def usage() = ???
}
