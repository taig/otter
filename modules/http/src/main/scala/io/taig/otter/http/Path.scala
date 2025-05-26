package io.taig.otter.http

import cats.Invariant
import cats.Show
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Enrichment

type Path[A] = Enrichment[Path.Value, A]

object Path:
  sealed abstract class Value[A] extends Product with Serializable:
    def toSegments: Chain[String | Parameter[?]]

    final def imap[B](f: A => B)(g: B => A): Path.Value[B] = Value.Modify(self = this, f, g)

    final def zip[B](path: Path.Value[B]): Path.Value[(A, B)] = Value.Zip(left = this, right = path)

    final def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Path.Value[merge.Out] = ???
    // zip(parameter.toPath).imap(merge.apply)(merge.unapply)

    final def /[B](name: String): Path.Value[A] = ???

    // final def toUrl: Url[A] = Url.Root(path = this, queries = Queries.Empty).imap((a, _) => a)((_, ()))

  object Value:
    private[otter] case object Empty extends Value[Unit]:
      override def toSegments: Chain[String | Parameter[?]] = Chain.empty

    final private[otter] case class Modify[A, B](self: Value[A], f: A => B, g: B => A) extends Value[B]:
      export self.toSegments

    final private[otter] case class Root[A](parameter: Parameter[A]) extends Value[A]:
      override def toSegments: Chain[String | Parameter[?]] = Chain.one(parameter)

    final private[otter] case class Static(name: String) extends Value[Unit]:
      override def toSegments: Chain[String | Parameter[?]] = Chain.one(name)

    final private[otter] case class Zip[A, B](left: Value[A], right: Value[B]) extends Value[(A, B)]:
      override def toSegments: Chain[String | Parameter[?]] = left.toSegments ++ right.toSegments

  type Data = Chain[String]

  val Empty: Path[Unit] = Enrichment(Value.Empty)

  // given Invariant[Path] with
  //   override def imap[A, B](fa: Value[A])(f: A => B)(g: B => A): Value[B] = fa.imap(f)(g)

  given Show[Value[?]] = _.toSegments
    .map {
      case segment: String         => segment
      case parameter: Parameter[?] => parameter.show
    }
    .mkString_("/", "/", "")
