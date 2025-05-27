package io.taig.otter.http

import cats.Invariant
import cats.Show
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.Merge
import io.taig.otter.Enrichment
import io.taig.otter.Metadata

final case class Path[A](self: Enrichment[Path.Value[A]]) extends AnyVal:
  inline def value: Path.Value[A] = self.self

  def toSegments: Chain[String | Parameter[?]] = value.toSegments

  def zip[B](path: Path[B]): Path[(A, B)] = Path(Enrichment(value.zip(path.value)))

  def *[B](path: Path[B])(using merge: Merge[A, B]): Path[merge.Out] = zip(path).merge

  def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Path[merge.Out] = this * parameter.toPath

  def /(name: String): Path[A] = Path(Enrichment(value.zip(Path.Value.Static(name)).imap((a, _) => a)((_, ()))))

  def toUrl: Url[A] = Url(Enrichment(Url.Value.Root(path = this, queries = Queries.Empty).imap((a, _) => a)((_, ()))))

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

  val Empty: Path[Unit] = Path(Enrichment(Value.Empty))

  given EnrichedSchemaInvariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Path[A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Path[A] =
        self.copy(self = self.self.modifyMetadata(f))

  given [A]: Show[Path[A]] = _.value.show
