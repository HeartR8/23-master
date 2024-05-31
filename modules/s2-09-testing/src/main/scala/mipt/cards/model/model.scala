package mipt.cards.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import scala.util.matching.Regex
import scala.compiletime.{codeOf, error}

import mipt.common.error.{InvalidId, InvalidValue}

object CardUcid:

  opaque type Id <: String = String
  object Id:
    inline def apply(id: String): Id =
      inline if id.isEmpty
      then error(codeOf(id) + " is invalid.")
      else id

    def either(id: String): Either[InvalidId, Id] =
      if id.isBlank | id.trim.length < id.length
      then Left(InvalidId(s"Id invalid: $id"))
      else Right(id)


  def apply(id: String): Id = id

given Encoder[CardUcid] = Encoder[String].contramap(identity)

given Decoder[CardUcid] = Decoder[String].map(CardUcid(_))

type CardUcid = CardUcid.Id

object CardNumber:

  opaque type Value <: String = String
  object Value:
    inline def apply(value: String): Value =
      inline if value.isEmpty
      then error(codeOf(value) + " is invalid.")
      else value

    def either(value: String): Either[InvalidValue, Value] =
      cardNumberPattern.findFirstMatchIn(value) match
        case Some(_) => Right(value)
        case None => Left(InvalidValue(s"Value invalid: ${value}"))

    private val cardNumberPattern: Regex = "^(?:[0-9]{4}-){3}[0-9]{4}$".r

  def apply(value: String): Value = value

given Encoder[CardNumber] = Encoder[String].contramap(identity)

given Decoder[CardNumber] = Decoder[String].map(CardNumber(_))

type CardNumber = CardNumber.Value

case class Card(ucid: CardUcid, number: CardNumber, amount: Double)

object Card:
  given Encoder[Card] = deriveEncoder[Card]

  given Decoder[Card] = deriveDecoder[Card]
