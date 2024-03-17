// Файл для реализации макросов использующихся в Task.scala
package hw

import scala.quoted.{Expr, Quotes, ToExpr, quotes}
import scala.util.Try
import scala.util.matching.Regex

// Макрос для задания II.1
object SafeRegex:
  inline def apply(inline str: String): Regex =
    ${ SafeRegexMacro.parse('str) }

object SafeRegexMacro:
  def parse(str: Expr[String])(using Quotes): Expr[Regex] =
    import quotes.reflect.report
    str.value match {
      case Some(value) =>
        Try {
          new scala.util.matching.Regex(value)
        }.fold(
          _ => report.errorAndAbort(s"Wrong regex: $value", str),
          x => Expr(x)
        )
      case None => '{ new scala.util.matching.Regex($str) }
    }

  given ToExpr[Regex] with
    def apply(x: Regex)(using Quotes): Expr[Regex] =
      '{ new scala.util.matching.Regex(${ Expr(x.pattern.pattern()) }) }

//-----------------------------------------------------------------------------
// Макрос для задания II.2

case class SourceFilePosition(file: String, line: Int)

object SourceFilePosition:
  inline def get: SourceFilePosition = ${ SourceFileMacro.getSourceFilePosition }

object SourceFileMacro:
  def getSourceFilePosition(using qctx: Quotes): Expr[SourceFilePosition] = {
    import quotes.reflect._
    '{ SourceFilePosition(${ Expr(Position.ofMacroExpansion.sourceFile.name) }, ${ Expr(Position.ofMacroExpansion.endLine) }) }
  }
