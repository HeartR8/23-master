package hw.functors

import cats.implicits.toBifunctorOps
import cats.syntax.traverse.toTraverseOps
import hw.functors.Decoder.*
//import cats.Bifunctor

object BDecoderInstances:

  /** Реализуйте декодер для Option и произвольного типа, для которого есть Decoder в скоупе. Если исходная строка -
    * пустая, или имеет значение <none> или null, то в результате должен быть None
    */
  given [E, T](using decoder: Decoder[E, T]): Decoder[E, Option[T]] = {
    case "<none>"           => Right(None)
    case null               => Right(None)
    case str if str.isEmpty => Right(None)
    case str                => decoder.apply(str).map(Some(_))
  }

  /** Реализуйте декодер для List и произвольного типа, для которого есть Decoder в скоупе. Элементы листа в исходной
    * строке разделены запятой.
    */
  given [E, T](using decoder: Decoder[E, T]): Decoder[E, List[T]] = { raw =>
    val elements = raw.split(", ")
    elements match {
      case Array("") =>
        Right(List.empty)
      case _ =>
        val decodedElements = elements.map(decoder.apply)
        val resultList      = decodedElements.toList.sequence
        resultList.map(_.toList)
    }
  }

  /** Реализуйте декодер из строки в строку
    */
  given Decoder[DecoderError, String] = (raw: String) => Right(raw)

  /** Реализуйте декодер из строки в число с заданным типом ошибки, используя Decoder.attempt() и Bifunctor
    */
  given Decoder[NumberFormatDecoderError.type, Int] = Decoder.attempt(_.toInt).leftMap(_ => NumberFormatDecoderError)

  /** Реализуйте декодер из строки в булево значение, используя Decoder.attempt() и Bifunctor
    */
  given Decoder[IllegalArgumentDecoderError.type, Boolean] =
    Decoder.attempt(_.toBoolean).leftMap(_ => IllegalArgumentDecoderError)

  /** Реализуйте декодер для DegreesFahrenheit через использование существующего декодера и Bifunctor
    */
  given Decoder[InvalidDegreesFahrenheitValue.type, DegreesFahrenheit] = (raw: String) =>
    Decoder.attempt(s => DegreesFahrenheit(s.toInt))(raw).leftMap(_ => InvalidDegreesFahrenheitValue)
