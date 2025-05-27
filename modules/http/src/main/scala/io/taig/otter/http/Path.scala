package io.taig.otter.http

import cats.Invariant
import cats.Show
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.Merge
import io.taig.otter.Enrichment
import io.taig.otter.Metadata

type Path[A] = Enrichment[Path.Value, A]

object Path:
  sealed abstract class Value[A] extends Product with Serializable:
    def toSegments: Chain[String | Parameter[?]]

    final def imap[B](f: A => B)(g: B => A): Path.Value[B] = Value.Modify(self = this, f, g)

    final def zip[B](path: Path.Value[B]): Path.Value[(A, B)] = Value.Zip(left = this, right = path)

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

    given [A]: Show[Path.Value[A]] = _.toSegments
      .map {
        case segment: String         => segment
        case parameter: Parameter[?] => parameter.show
      }
      .mkString_("/", "/", "")

  type Data = Chain[String]

  val Empty: Path[Unit] = Enrichment(Value.Empty)

  extension [A](self: Path[A])
    def toSegments: Chain[String | Parameter[?]] = self.self.toSegments

    def zip[B](path: Path[B]): Path[(A, B)] = Enrichment(self.self.zip(path.self))

    def merge[B](path: Path[B])(using merge: Merge[A, B]): Path[merge.Out] = self.zip(path).merge

    def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Path[merge.Out] =
      self.zip(parameter.toPath).merge

    def /(name: String): Path[A] = Enrichment(self.self.zip(Value.Static(name)).imap((a, _) => a)((_, ())))

    def toUrl: Url[A] = Enrichment(Url.Value.Root(path = self, queries = Queries.Empty).imap((a, _) => a)((_, ())))

  given EnrichedSchemaInvariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.mapF(_.imap(f)(g))

    extension [A](self: Path[A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Path[A] = self.modifyMetadata(f)

  given [A]: Show[Path[A]] = _.self.show
