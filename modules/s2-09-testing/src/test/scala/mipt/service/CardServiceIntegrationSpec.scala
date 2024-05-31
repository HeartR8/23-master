package mipt.service

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import com.dimafeng.testcontainers.lifecycle.and
import com.dimafeng.testcontainers.{MockServerContainer, RedisContainer}
import com.dimafeng.testcontainers.scalatest.TestContainersForAll
import dev.profunktor.redis4cats.RedisCommands
import io.circe.syntax._
import mipt.testdata.CardsTestData
import mipt.utils.MockServerClientWrapper
import mipt.cards.service.CardService
import mipt.wirings.ProgramWiring
import mipt.cards.wirings.CardsWiring
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class CardServiceIntegrationSpec extends AsyncFlatSpec with Matchers with TestContainersForAll with AsyncIOSpec {

  override type Containers = MockServerContainer and RedisContainer

  val testData = new CardsTestData {}
  import testData._

  "getUserCards" should "return cards from external service and put them to cache for fallback" in
    withEnvironment { (mockServer, redis, service) =>
      for {
        _ <- MockServerClientWrapper.mockGetCards(
              mockServer,
              userId,
              cards.asJson.toString
            )
        _ <- service.getUserCards(userId).map(_ shouldBe cards)
        _ <- redis.get(userId).map(_ shouldBe Some(cards.asJson.toString))
      } yield ()
    }

  it should "return cards from external service and update cache with them for fallback" in
    withEnvironment { (mockServer, redis, service) =>
      for {
        _ <- redis.set(anotherUserId, cards.asJson.toString)
        _ <- MockServerClientWrapper.mockGetCards(mockServer, anotherUserId, anotherCards.asJson.toString)
        _ <- service.getUserCards(anotherUserId).map(_ shouldBe anotherCards)
        _ <- redis.get(anotherUserId).map(_ shouldBe Some(anotherCards.asJson.toString))
      } yield ()
    }

  override def startContainers(): Containers = {
    val mockServer = MockServerContainer.Def().start()
    val redis = RedisContainer.Def().start()

    mockServer and redis
  }

  def withEnvironment(f: (MockServerContainer, RedisCommands[IO, String, String], CardService[IO]) => IO[Unit]): IO[Unit] =
    withContainers { case mockServer and redis =>
      val httpClientResource = ProgramWiring.httpClient[IO]
      val redisResource = ProgramWiring.redis[IO](redis.redisUri)
      val externalServiceUri = mockServer.endpoint

      val serviceResource = CardsWiring.wire[IO](redisResource, httpClientResource, externalServiceUri)

      (httpClientResource, redisResource, serviceResource).tupled.use {
        case (_, redisCommands, service) =>
          f(mockServer, redisCommands, service)
      }
    }
}
