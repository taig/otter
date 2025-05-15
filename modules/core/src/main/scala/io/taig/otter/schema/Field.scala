package io.taig.otter.schema
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Shape
import io.taig.otter.schema.Primitive.Component as PrimitiveComponent

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Field[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Field[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Field[S, T, B] = Field.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, A]

  final def optional: Field[S, T, Option[A]] = Field.Optional(self = this)

object Field:
  final private[otter] case class Modify[+S[_], +T[_], A, B](self: Field[S, T, A], f: A => B, g: B => A)
      extends Field[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] = copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[+S[_], +T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Field[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, B] =
      copy(value = value.mapK[T1, U](fK))

  final private[otter] case class Optional[+S[_], +T[_], A](self: Field[S, T, A]) extends Field[S, T, Option[A]]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Field[S, T, Option[A]] =
      copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Field[S, U, Option[A]] =
      copy(self = self.mapK[T1, U](fK))

  trait Component[Self[_], -Key[_], -Value[_], Record[_]](using
      shape: Shape.Field[Self, Key, Value],
      record: Shape.Record[Record, Self]
  ):
    final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
      shape.field(name, key, value)

    extension [A](self: Self[A]) def toRecord: Record[A] = record.record(self)

  object Component:
    trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
        extends Field.Component.Primitive.Boolean[Self, Key, Value, Record],
          Field.Component.Primitive.Number[Self, Key, Value, Record],
          Field.Component.Primitive.String[Self, Key, Value, Record]:
      override def key: PrimitiveComponent[Key]

    object Primitive:
      trait Boolean[Self[_], Key[_], -Value[_], Record[_]] extends Field.Component[Self, Key, Value, Record]:
        def key: PrimitiveComponent.Boolean[Key]

        final def field[A](name: SBoolean, schema: => Value[A]): Self[A] =
          field(name, key = key.boolean, value = schema)

      trait Number[Self[_], Key[_], -Value[_], Record[_]] extends Field.Component[Self, Key, Value, Record]:
        def key: PrimitiveComponent.Number[Key]

        final def field[A](name: BigDecimal, schema: => Value[A]): Self[A] =
          field(name, key = key.bigDecimal, value = schema)
        final def field[A](name: BigInt, schema: => Value[A]): Self[A] =
          field(name, key = key.bigInteger, value = schema)
        final def field[A](name: JBigDecimal, schema: => Value[A]): Self[A] =
          field(name, key = key.jBigDecimal, value = schema)
        final def field[A](name: JBigInteger, schema: => Value[A]): Self[A] =
          field(name, key = key.jBigInteger, value = schema)
        final def field[A](name: SDouble, schema: => Value[A]): Self[A] = field(name, key = key.double, value = schema)
        final def field[A](name: SFloat, schema: => Value[A]): Self[A] = field(name, key = key.float, value = schema)
        final def field[A](name: SInt, schema: => Value[A]): Self[A] = field(name, key = key.int, value = schema)
        final def field[A](name: SLong, schema: => Value[A]): Self[A] = field(name, key = key.long, value = schema)

      trait String[Self[_], Key[_], -Value[_], Record[_]] extends Field.Component[Self, Key, Value, Record]:
        def key: PrimitiveComponent.String[Key]

        final def field[A](name: JString, schema: => Value[A]): Self[A] =
          field(name, key = key.string, value = schema)

  given [Key[_], Value[_]]: Shape.Field[Field[Key, Value, *], Key, Value] =
    new Shape.Field[Field[Key, Value, *], Key, Value]:

      override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] =
        Root(
          key = Reference.Constant(self = Reference.later(key), value = name),
          value = Reference.later(value),
          metadata = Metadata.Empty
        )

      extension [A](fa: Field[Key, Value, A])
        override def imap[B](f: A => B)(g: B => A): Field[Key, Value, B] = fa.imap(f)(g)
        override def optional: Field[Key, Value, Option[A]] = fa.optional
        override def metadata: Metadata = fa.metadata
        override def modifyMetadata(f: Metadata => Metadata): Field[Key, Value, A] = fa.modifyMetadata(f)
