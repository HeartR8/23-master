package mipt.cards.service

import cats.MonadThrow
import cats.syntax.all.*
import mipt.cards.cache.CardsCache
import mipt.cards.external.CardsExternalService
import mipt.cards.model.Card
import mipt.common.model.UserId

trait CardService[F[_]]:
  def getUserCards(userId: UserId): F[List[Card]]

object CardService:

  def apply[F[_] : MonadThrow](externalService: CardsExternalService[F],
                               cache: CardsCache[F]): CardService[F] =
    new Impl[F](externalService, cache)
  
  private class Impl[F[_]: MonadThrow](externalService: CardsExternalService[F],
                                       cache: CardsCache[F]) extends CardService[F]:

    override def getUserCards(userId: UserId): F[List[Card]] = {
      for {
        cards <- externalService.getUserCards(userId)
        _     <- cache.putUserCards(userId, cards).handleError(_ => ())
      } yield cards
    }.handleErrorWith(_ => cache.getUserCards(userId))
