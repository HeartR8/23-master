package mipt.common.error

import scala.util.control.NoStackTrace

final case class InvalidId(message: String) extends RuntimeException(message) with NoStackTrace

final case class InvalidValue(message: String) extends RuntimeException(message) with NoStackTrace