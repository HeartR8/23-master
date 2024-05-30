package hw.monads

import cats.MonadThrow
import cats.syntax.all._

opaque type UserId <: Int = Int

object UserId:
  def apply(i: Int): UserId = i

opaque type UserName <: String = String

object UserName:
  def apply(s: String): UserName = s

opaque type Age <: Byte = Byte

object Age:
  val Adult: Age = 18

  def apply(v: Byte): Age = v

final case class User(id: UserId, name: UserName, age: Age, friends: Set[UserId]):
  def isAdult: Boolean = age >= Age.Adult

trait UserRepository[F[_]]:
  def findAll: F[List[User]]

  def create(name: UserName, age: Age, friends: Set[UserId] = Set.empty): F[User]

  def delete(userId: UserId): F[Unit]

  def update(user: User): F[Unit]

object UserRepository:
  case class UserNotFoundError(id: UserId) extends Throwable

  type Op[F[_], T] = UserRepository[F] => F[T]

  /** Имплементируейте MonadThrow для работы с репозиторием. MonadThrow - это частный случай MonadError, который в свою
   * очередь является монадой с добавленным эффектом ошибки. Для MonadError это произвольный тип, для MonadThrow это
   * тип Throwable.
   *
   * Для имплементации MonadThrow нужно вывести методы монады pure, flatMap и tailRecM и методы raiseError и
   * handleErrorWith от MonadError, используя требование наличия MonadThrow для F
   */
  given[F[_] : MonadThrow]: MonadThrow[Op[F, *]] =
  new MonadThrow[Op[F, *]] {
    def pure[A](x: A): Op[F, A] = _ => MonadThrow[F].pure(x)

    def flatMap[A, B](fa: Op[F, A])(f: A => Op[F, B]): Op[F, B] = userRepository =>
      fa(userRepository).flatMap(a => f(a)(userRepository))

    def handleErrorWith[A](fa: Op[F, A])(f: Throwable => Op[F, A]): Op[F, A] = userRepository =>
      fa(userRepository).handleErrorWith(err => f(err)(userRepository))

    def raiseError[A](e: Throwable): Op[F, A] = userRepository => MonadThrow[F].raiseError(e)

    def tailRecM[A, B](a: A)(f: A => Op[F, Either[A, B]]): Op[F, B] = userRepository =>
      MonadThrow[F].tailRecM(a)(a => f(a)(userRepository))
  }

  object Operations:
    def findAll[F[_]]: Op[F, List[User]] =
      _.findAll

    def create[F[_]](name: UserName, age: Age, friends: Set[UserId] = Set.empty): Op[F, User] =
      _.create(name, age, friends)

    def delete[F[_]](userId: UserId): Op[F, Unit] = _.delete(userId)

    def update[F[_]](user: User): Op[F, Unit] = _.update(user)

    /** реализуйте композитные методы, используя базовые выше
     *
     * для работы с ошибками можно использовать синтаксис из cats.syntax.applicativeError val err: Op[User] =
     * UserNotFoundError(UserId(1)).raiseError[Op, User]
     */

    /** Метод опционального поиска пользователя */
    def findMaybe[F[_]](userId: UserId)(using me: MonadThrow[F]): Op[F, Option[User]] =
      userRepository =>
        findAll(userRepository).map(_.find(_.id == userId))

    /** Метод поиска пользователя. Если пользователь не найден, должна генерироваться ошибка UserNotFound */
    def find[F[_]](userId: UserId)(using me: MonadThrow[F]): Op[F, User] =
      userRepository =>
        findAll(userRepository).flatMap { users =>
          users.find(_.id == userId).liftTo[F](UserNotFoundError(userId))
        }

    /** Метод добавления друга к пользователю. */
    def addFriend[F[_]](currentUserId: UserId, friendId: UserId)(using me: MonadThrow[F]): Op[F, User] =
      for {
        currentUser <- find[F](currentUserId)
        friend <- find[F](friendId)
        updatedUser = currentUser.copy(friends = currentUser.friends + friendId)
        _ <- update[F](updatedUser)
      } yield updatedUser


    /** Метод удаления друга у пользователя */
    def deleteFriend[F[_]](currentUserId: UserId, friendId: UserId)(using me: MonadThrow[F]): Op[F, User] =
      for {
        currentUser <- find[F](currentUserId).adaptErr(_ => UserNotFoundError(currentUserId))
        friend <- find[F](friendId).adaptErr(_ => UserNotFoundError(friendId))
        updatedFriends = currentUser.friends - friendId
        updatedUser = currentUser.copy(friends = updatedFriends)
        _ <- update[F](updatedUser)
      } yield updatedUser

    /** Метод получения всех друзей пользователя */
    def getUserFriends[F[_]](userId: UserId)(using me: MonadThrow[F]): Op[F, List[User]] =
      for {
        currentUser <- find[F](userId).adaptErr(_ => UserNotFoundError(userId))
        friendIds = currentUser.friends.toList
        friends <- friendIds.traverse(friendId => find[F](friendId).adaptErr(_ => UserNotFoundError(friendId)))
      } yield friends

    /** Метод получения пользователей, у которых в друзьях только взрослые пользователи */
    def getUsersWithAdultOnlyFriends[F[_]]()(using me: MonadThrow[F]): Op[F, List[User]] =
      for {
        allUsers <- findAll[F]
        adultUsers = allUsers.filter(_.isAdult)
        usersWithAdultOnlyFriends = allUsers.filter(user =>
          user.friends.nonEmpty && user.friends.forall(friendId => adultUsers.exists(_.id == friendId))
        )
      } yield usersWithAdultOnlyFriends

    /** Метод удаления всех молодых пользователей */
    def deleteAllJuniorUsers[F[_]]()(using me: MonadThrow[F]): Op[F, Unit] =
      for {
        allUsers <- findAll[F]
        juniorUsers = allUsers.filter(user => !user.isAdult)
        _ <- juniorUsers.traverse(user => delete[F](user.id)).void
      } yield ()

    /** Метод создания сообщества, где все являются друзьями друг для друга. На вход подается список атрибутов
     * пользователей из сообщества
     */
    def createCommunity[F[_]](community: List[(UserName, Age)])(using me: MonadThrow[F]): Op[F, List[User]] =
      for {
        users <- community.traverse { case (name, age) =>
          create[F](name, age)
        }

        communityUsers = users.toSet
        _ <- communityUsers.toList.traverse { user =>
          val friends = communityUsers - user
          val updatedUser = user.copy(friends = friends.map(_.id))
          update[F](updatedUser)
        }
        allUsers <- findAll[F]
      } yield allUsers