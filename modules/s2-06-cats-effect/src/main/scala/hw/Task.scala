package hw

import java.util.UUID
import cats.effect.kernel.Async
import cats.effect.implicits.monadCancelOps_
import cats.effect.std.{Queue, Semaphore}
import cats.syntax.all._


/**
 * I. Пул воркеров с балансировкой нагрузки
 *
 * Есть ограниченное число воркеров, каждый из которых может иметь свое состояние.
 * (В тестах состояние - подсчет числа раз, которое запустился воркер, на практике это может быть, например, коннекшн к базе).
 *
 * Воркер может исполнять одну задачу (ф-ция In => F[Out]) в один момент времени.
 *
 * Необходимо реализовать метод `WorkerPool.of`, который создает пул воркеров `WorkerPool` для запуска задач.
 * При этом:
 *   - (при нескольких доступных воркерах) задачи должны исполняться параллельно
 *   - если все воркеры заняты, то вызов run должен ожидать того, пока какой-нибудь из воркеров не освободится
 *   - должна происходить балансировка задач - выбираться должен ПЕРВЫЙ свободный воркер для задачи, а не конкретный
 *   - воркер должен становиться доступным после выполнения задачи, независимо от того, была ли выполнена задача/отменена/завершена с ошибкой (исключением)
 *
 * Метод `run` в `WorkerPool` для запуска задач, метод `add` для добавления воркеров, метод `removeAll` для удаления всех воркеров.
 * Если все воркеры были удалены, то метож `run` должен ожидать пока воркеры не добавятся.
 *
 * Примеры можно посмотреть в тестах.
 */
trait WorkerPool[F[_], In, Out]:
  def run(in: In): F[Out]

  def add(worker: Worker[F, In, Out]): F[Unit]

  def removeAll: F[Unit]

type WorkerId = WorkerId.T

object WorkerId:
  opaque type T <: UUID = UUID

  def apply(id: UUID): T = id

case class Worker[F[_], In, Out](id: WorkerId, run: In => F[Out]):
  def apply(in: In): F[Out] = run(in)

object WorkerPool {
  def of[F[_] : Async, In, Out](fs: List[Worker[F, In, Out]]): F[WorkerPool[F, In, Out]] =
    for
      queue <- Queue.bounded[F, Worker[F, In, Out]](fs.size)
      semaphore <- Semaphore[F](fs.size)
      _ <- fs.traverse(queue.offer)
    yield new:
      def run(in: In): F[Out] =
        semaphore.acquire >>
          queue.take.flatMap { worker =>
            worker.run(in).guarantee(
              queue.offer(worker) >> semaphore.release
            )
          }

      def add(worker: Worker[F, In, Out]): F[Unit] =
        semaphore.release >> queue.offer(worker)

      def removeAll: F[Unit] =
        semaphore.acquireN(fs.size) >>
          queue.take.replicateA(fs.size).void
}
