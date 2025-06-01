package io.taig.otter.http

import cats.Show
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Metadata
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant

final case class Path[A](value: Path.Value[A], metadata: Metadata):
  def toSegments: Chain[String | Parameter[?]] = value.toSegments

  def zip[B](path: Path[B]): Path[(A, B)] = Path(value = value.zip(path.value), metadata = Metadata.Empty)

  def *[B](path: Path[B])(using merge: Merge[A, B]): Path[merge.Out] = zip(path).merge

  def /[B](parameter: Parameter[B])(using merge: Merge[A, B]): Path[merge.Out] = this * parameter.toPath

  def /(name: String): Path[A] = Path(
    value = value.zip(Path.Value.Static(name)).imap((a, _) => a)((_, ())),
    metadata = Metadata.Empty
  )

  def toUrl: Url[A] = Url(
    value = Url.Value.Root(path = this, queries = Queries.Empty).imap((a, _) => a)((_, ())),
    metadata = Metadata.Empty
  )

object Path:
  sealed abstract class Value[A] extends Product, Serializable:
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

  val Empty: Path[Unit] = Path(value = Value.Empty, metadata = Metadata.Empty)

  given SchemaInvariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Path[A]] = new Enriched[Path[A]]:
      override def metadata(a: Path[A]): Metadata = a.metadata
      override def modifyMetadata(a: Path[A])(f: Metadata => Metadata): Path[A] =
        a.copy(metadata = f(a.metadata))

  given [A]: Show[Path[A]] = _.value.show
