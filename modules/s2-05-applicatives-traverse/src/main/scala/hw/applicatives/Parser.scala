package hw.applicatives

object Parser:
  opaque type P[A] = String => Either[String, (String, A)]

  extension [A](parser: P[A]) def parse(input: String): Either[String, (String, A)] = parser(input)

  def defer[A](p: => P[A]): P[A] = p(_)

  def charWhere(f: Char => Boolean): P[Char] = str =>
    str.headOption.toRight("expected at least one more char symbol").flatMap {
      case x if f(x) => Right((str.tail, x))
      case x         => Left(s"unexpected symbol \"$x\"")
    }

  def number: P[Double] = { input =>
    val numRegex = """^-?\d+(\.\d+)?""".r
    numRegex.findPrefixOf(input) match {
      case Some(matched) =>
        val rest = input.drop(matched.length)
        Right((rest, matched.toDouble))
      case None =>
        Left("expected a number")
    }
  }

  def term: P[Double] = input => {
    def nextOp(left: Double, remaining: String): Either[String, (String, Double)] = {
      if (remaining.isEmpty) Right(("", left))
      else {
        remaining.headOption match {
          case Some('*') | Some('/') if remaining.length > 1 && (remaining.charAt(1).isDigit || remaining.charAt(1) == '.') =>
            number.parse(remaining.tail).flatMap {
              case (restNum, num) =>
                val newResult = if (remaining.head == '*') left * num else left / num
                nextOp(newResult, restNum)
            }
          case _ => Right((remaining, left))
        }
      }
    }

    number.parse(input).flatMap {
      case (rest, num) => nextOp(num, rest)
    }
  }

  def expression: P[Double] = input => {
    def addSubtract(left: Double, remaining: String): Either[String, (String, Double)] = {
      if (remaining.isEmpty) Right(("", left))
      else remaining.headOption match {
        case Some('+') | Some('-') => term.parse(remaining.tail).flatMap {
          case (restTerm, num) =>
            val op = if (remaining.head == '+') left + num else left - num
            addSubtract(op, restTerm)
        }
        case _ => Right((remaining, left))
      }
    }

    term.parse(input).flatMap {
      case (rest, num) => addSubtract(num, rest)
    }
  }

type Parser[A] = Parser.P[A]

import Parser.*

object DoubleCalculator:

  def calculator: Parser[Double] = expression
