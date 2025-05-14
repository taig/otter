package io.taig.otter.schema

import cats.arrow.FunctionK
import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Invariant
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.schema.Primitive.Boolean.Component as PrimitiveBooleanComponent
import io.taig.otter.schema.Primitive.Number.Component as PrimitiveNumberComponent
import io.taig.otter.schema.Primitive.String.Component as PrimitiveStringComponent
import io.taig.otter.schema.Primitive.Component as PrimitiveComponent

sealed abstract class Field[+S[_], +T[_], A] extends Schema[T, A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  override def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]

  final override def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Field[S, U, A]

  final def optional: Field[S, T, Option[A]] = Field.Optional(self = this)

object Field:
  final private[otter] case class Modify[+S[_], +T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[+S[_], +T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Field[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, B] = copy(value = value.mapK(fK))

  final private[otter] case class Optional[+S[_], +T[_], A](self: Field[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: FunctionK[T1, U]): Field[S, U, Option[A]] = copy(self = self.mapK(fK))

  trait Shape[Self[_], -Key[_], -Value[_], +Record[_]] extends Schema.Shape[Self]:
    def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

    extension [A](self: Self[A])
      def optional: Self[Option[A]]
      def toRecord: Record[A]

  object Shape:
    def apply[Self[_], Key[_], Value[_], Record[_]: Invariant](
        lift: [A] => (self: Field[Key, Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Field[Key, Value, A]
    ): Field.Shape[Self, Key, Value, Record] = new Shape[Self, Key, Value, Record]:
      override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = lift:
        Root(
          key = Reference.Constant(self = Reference.later(key), value = name),
          value = Reference.later(value),
          metadata = Metadata.Empty
        )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def optional: Self[Option[A]] = lift(extract(self).optional)
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        // override def zip[B](codec: Self[B]): Record[(A, B)] = toRecord.zip(codec.toRecord)
        override def toRecord: Record[A] = ??? // codec.record(self)

  trait Component[+Self[_], -Key[_], -Value[_], Record[_]](using shape: Field.Shape[Self, Key, Value, Record]):
    final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
      shape.field(name, key, value)

  object Component:
    trait Primitive[+Self[_], Key[_], -Value[_], Record[_]]
        extends Component.Primitive.Boolean[Self, Key, Value, Record],
          Component.Primitive.Number[Self, Key, Value, Record],
          Component.Primitive.String[Self, Key, Value, Record]:
      override def key: PrimitiveComponent[Key]

    object Primitive:
      trait Boolean[+Self[_], Key[_], -Value[_], Record[_]] extends Component[Self, Key, Value, Record]:
        def key: PrimitiveBooleanComponent[Key]

        final def field[A](name: SBoolean, codec: => Value[A]): Self[A] =
          field(name, key = key.boolean, value = codec)

      trait Number[+Self[_], Key[_], -Value[_], Record[_]] extends Component[Self, Key, Value, Record]:
        def key: PrimitiveNumberComponent[Key]

        final def field[A](name: BigDecimal, codec: => Value[A]): Self[A] =
          field(name, key = key.bigDecimal, value = codec)
        final def field[A](name: BigInt, codec: => Value[A]): Self[A] = field(name, key = key.bigInteger, value = codec)
        final def field[A](name: JBigDecimal, codec: => Value[A]): Self[A] =
          field(name, key = key.jBigDecimal, value = codec)
        final def field[A](name: JBigInteger, codec: => Value[A]): Self[A] =
          field(name, key = key.jBigInteger, value = codec)
        final def field[A](name: SDouble, codec: => Value[A]): Self[A] = field(name, key = key.double, value = codec)
        final def field[A](name: SFloat, codec: => Value[A]): Self[A] = field(name, key = key.float, value = codec)
        final def field[A](name: SInt, codec: => Value[A]): Self[A] = field(name, key = key.int, value = codec)
        final def field[A](name: SLong, codec: => Value[A]): Self[A] = field(name, key = key.long, value = codec)

      trait String[+Self[_], Key[_], -Value[_], Record[_]] extends Component[Self, Key, Value, Record]:
        def key: PrimitiveStringComponent[Key]

        final def field[A](name: JString, codec: => Value[A]): Self[A] =
          field(name, key = key.string, value = codec)
