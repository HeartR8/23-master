package hw.traverse

import cats.{Applicative, Eval, Traverse}
import cats.syntax.traverse._
import cats.syntax.apply._

import java.time.ZonedDateTime

object Template:
  opaque type S = String

  def apply(s: String): S = s

type Template = Template.S

object Rendered:
  opaque type S = String

  def apply(s: String): S = s

type Rendered = Rendered.S

trait Localization[F[_]]:
  def localize: Template => F[Rendered]

case class Form[A](
                    title: A,
                    tags: List[A],
                    subForms: List[Form[A]],
                    date: ZonedDateTime
                  )

object FormTraverse:
  given Traverse[Form] with
    override def traverse[G[_]: Applicative, A, B](fa: Form[A])(f: A => G[B]): G[Form[B]] =
      val titleF = f(fa.title)
      val tagsF = fa.tags.traverse(f)
      val subFormsF = fa.subForms.traverse(traverse(_)(f))
      (titleF, tagsF, subFormsF).mapN((title, tags, subForms) => Form(title, tags, subForms, fa.date))

    override def foldLeft[A, B](fa: Form[A], b: B)(f: (B, A) => B): B =
      val b1 = f(b, fa.title)
      val b2 = fa.tags.foldLeft(b1)(f)
      fa.subForms.foldLeft(b2)((acc, form) => foldLeft(form, acc)(f))

    override def foldRight[A, B](fa: Form[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      val lb1 = f(fa.title, lb)
      val lb2 = fa.tags.foldRight(lb1)((tag, acc) => f(tag, acc))
      fa.subForms.foldRight(lb2)((form, acc) => foldRight(form, acc)(f))

object FormLocalization:
  import FormTraverse.given
  def localize[F[_]: Applicative](form: Form[Template])(using loc: Localization[F]): F[Form[Rendered]] =
    form.traverse(template => loc.localize(template))
