package hw.contravariants

import cats.Contravariant
import cats.implicits.toContravariantOps

final case class DegreesFahrenheit(value: Int)

trait Encoder[-T]:
  def apply(value: T): String

object Encoder:
  def encode[T](value: T)(using encoder: Encoder[T]): String =
    encoder(value)

  /** Реализуйте инстанс Contravariant для Encoder
    */
  given Contravariant[Encoder] = new Contravariant[Encoder] {
    def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] =
      value => fa(f(value))
  }

object EncoderInstances:

  /** Реализуйте Encoder для Option и произвольного типа, для которого есть Encoder в скоупе. None должен
    * преобразовываться в значение `<none>`
    */
  given [T](using e: Encoder[T]): Encoder[Option[T]] = {
    case None    => "<none>"
    case Some(a) => summon[Encoder[T]].apply(a)
  }

  /** Реализуйте Encoder для List и произвольного типа, для которого есть Encoder в скоупе. Элементы листа в
    * результирующей строке должны быть разделены запятой.
    */
  given [T: Encoder]: Encoder[List[T]] = list => list.map(summon[Encoder[T]].apply).mkString(",")

  /** Реализуйте encoder для строки
    */
  given Encoder[String] = str => str

  /** Реализуйте encoder числа в строку
    */
  given Encoder[Int] = num => num.toString

  /** Реализуйте encoder булева значения в строку
    */
  given Encoder[Boolean] = bool => bool.toString

  /** Реализуйте encoder для DegreesFahrenheit через использование существующего encoder и Contravariant
    */
  given Encoder[DegreesFahrenheit] = summon[Encoder[Int]].contramap(_.value)
