package io.taig.otter.schema

import cats.~>
import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.Metadata
import io.taig.otter.Shape

// TODO support for optional
sealed abstract class Tuple[+S[_], A] extends Schema[S, A]:
  def schemas: Chain[Reference[S, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A]

  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A]

  final override def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

  final def zip[S1[a] >: S[a], B](schema: Tuple[S1, B]): Tuple[S1, (A, B)] =
    Tuple.Zip(left = this, right = schema, metadata = Metadata.Empty)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def mapK[S1[a] >: Nothing, T[_]](fK: S1 ~> T): Tuple[T, Unit] = this
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.{metadata, schemas}
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A](schema: Reference[S, A], metadata: Metadata) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, A]] = Chain(schema)
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A] = copy(schema = schema.mapK(fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Tuple[S, A],
      right: Tuple[S, B],
      metadata: Metadata
  ) extends Tuple[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  trait Component[+Self[_], -Value[_]](using self: Shape.Tuple[Self, Value]):
    final def TNil: Self[Unit] = self.empty

    extension [A](value: Value[A]) final def toTuple: Self[A] = self.one(value)

  given [Value[_]]: Shape.Tuple[Tuple[Value, *], Value] = new Shape.Tuple[Tuple[Value, *], Value]:
    override def empty: Tuple[Value, Unit] = Tuple.Empty(Metadata.Empty)
    override def one[A](schema: => Value[A]): Tuple[Value, A] = Tuple.Root(
      schema = Reference.later(schema),
      metadata = Metadata.Empty
    )
    extension [A](self: Tuple[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Tuple[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Tuple[Value, B] = self.imap(f)(g)
      override def zip[B](schema: Tuple[Value, B]): Tuple[Value, (A, B)] = self.zip(schema)
