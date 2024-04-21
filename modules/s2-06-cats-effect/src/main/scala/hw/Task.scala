package hw

import java.util.UUID
import cats.effect.kernel.Ref
import cats.effect.Sync
import cats.implicits._

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
  def of[F[_] : Sync, In, Out](fs: List[Worker[F, In, Out]]): F[WorkerPool[F, In, Out]] = {
    for {
      workerRefs <- fs.traverse(worker => Ref.of[F, Worker[F, In, Out]](worker))
      availableWorkers <- Ref.of[F, List[Ref[F, Worker[F, In, Out]]]](workerRefs)
    } yield new WorkerPool[F, In, Out] {
      override def run(in: In): F[Out] = {
        availableWorkers.get.flatMap {
          case Nil => Sync[F].defer(run(in))
          case workerRef :: tail =>
            workerRef.get.flatMap { worker =>
              worker.run(in)
            }
        }
      }

      override def add(worker: Worker[F, In, Out]): F[Unit] = {
        Ref.of[F, Worker[F, In, Out]](worker).flatMap { workerRef =>
          availableWorkers.update(workerRef :: _)
        }
      }

      override def removeAll: F[Unit] = availableWorkers.set(List.empty[Ref[F, Worker[F, In, Out]]])
    }
  }
}
