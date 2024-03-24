package hw.functors

import hw.functors.FDecoder.FDecoder
import cats.syntax.traverse.toTraverseOps
import cats.implicits.toBifunctorOps

object FDecoderInstances:
  /** Реализуйте декодер для Option и произвольного типа, для которого есть Decoder в скоупе. Если исходная строка -
    * пустая, или имеет значение `<none>` или null, то в результате должен быть None
    */
  given [T](using decoder: FDecoder[T]): FDecoder[Option[T]] = {
    case "<none>"           => Right(None)
    case null               => Right(None)
    case str if str.isEmpty => Right(None)
    case str                => decoder.apply(str).map(Some(_))
  }

  /** Реализуйте декодер для List и произвольного типа, для которого есть Decoder в скоупе. Элементы листа в исходной
    * строке разделены запятой.
    */
  given [T: FDecoder]: FDecoder[List[T]] = { raw =>
    val elements = raw.split(", ")
    elements match {
      case Array("") =>
        Right(List.empty)
      case _ =>
        val decodedElements = elements.map(FDecoder.decode)
        val resultList      = decodedElements.toList.sequence
        resultList.map(_.toList)
    }
  }

  /** Реализуйте декодер из строки в строку
    */
  given strDecoder: FDecoder[String] = (raw: String) => Right(raw)

  /** Реализуйте декодер из строки в число, используя `NumberFormatDecoderError` в результате в случае, если строка - не
    * число
    */
  given intDecoder: FDecoder[Int] = Decoder.attempt(_.toInt).leftMap(_ => NumberFormatDecoderError)

  /** Реализуйте декодер из строки в булево значение, используя ошибку `IllegalArgumentDecoderError` в случае,если
    * строка не парсится в boolean
    */
  given boolDecoder: FDecoder[Boolean] = Decoder.attempt(_.toBoolean).leftMap(_ => IllegalArgumentDecoderError)

  /** Реализуйте декодер для DegreesFahrenheit через использование существующего декодера и инстанса Functor. В случае
    * ошибки данный декодер должен возвращать InvalidDegreesFahrenheitValue
    */
  given FDecoder[DegreesFahrenheit] = ???
