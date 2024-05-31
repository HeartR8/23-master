package mipt.service

import cats.effect.IO
import mipt.cards.cache.CardsCache
import mipt.cards.external.CardsExternalService
import mipt.cards.model.Card
import mipt.common.model.UserId
import mipt.cards.service.CardService
import mipt.testdata.CardsTestData
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import eu.monniot.scala3mock.ScalaMocks
import scala.concurrent.ExecutionContext
import java.util.UUID

class CardServiceAdditionalTests extends AnyFunSuite with BeforeAndAfter with ScalaMocks {

  given ec: ExecutionContext = ExecutionContext.global
  given ioRuntime: IORuntime = IORuntime.global

  test("handle an empty list of cards from external service") {
    withExpectations() {
      val userId = UserId(UUID.randomUUID().toString)
      val externalService = mock[CardsExternalService[IO]]
      val cache = mock[CardsCache[IO]]

      val service = CardService.apply[IO](externalService, cache)

      when(externalService.getUserCards).expects(*).returns(IO.pure(List.empty[Card]))
      when(cache.putUserCards).expects(*, *).returning(IO.unit)

      val result = service.getUserCards(userId).attempt.unsafeRunSync()
      assert(result == Right(List.empty))
    }
  }
}