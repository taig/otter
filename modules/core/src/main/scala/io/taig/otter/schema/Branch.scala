package io.taig.otter.schema

import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Invariant
import io.taig.otter.schema.Primitive.Component as PrimitiveComponent
import io.taig.otter.Shape
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Branch[+S[_], +T[_], A] extends Schema[T, A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, A]

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] = copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] = copy(value = value.mapK[T1, U](fK))

  trait Component[Self[_], -Key[_], -Value[_], Sum[_]](using shape: Shape.Branch[Self, Key, Value]):
    final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = shape.branch(name, key, value)

    extension [A](self: Self[A]) def toSum: Sum[A] = ???

  object Component:
    trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
        extends Branch.Component.Primitive.Boolean[Self, Key, Value, Record],
          Branch.Component.Primitive.Number[Self, Key, Value, Record],
          Branch.Component.Primitive.String[Self, Key, Value, Record]:
      override def key: PrimitiveComponent[Key]

    object Primitive:
      trait Boolean[Self[_], Key[_], -Value[_], Sum[_]] extends Branch.Component[Self, Key, Value, Sum]:
        def key: PrimitiveComponent.Boolean[Key]

        final def branch[A](name: SBoolean, schema: => Value[A]): Self[A] =
          branch(name, key = key.boolean, value = schema)

      trait Number[Self[_], Key[_], -Value[_], Sum[_]] extends Branch.Component[Self, Key, Value, Sum]:
        def key: PrimitiveComponent.Number[Key]

        final def branch[A](name: BigDecimal, schema: => Value[A]): Self[A] =
          branch(name, key = key.bigDecimal, value = schema)
        final def branch[A](name: BigInt, schema: => Value[A]): Self[A] =
          branch(name, key = key.bigInteger, value = schema)
        final def branch[A](name: JBigDecimal, schema: => Value[A]): Self[A] =
          branch(name, key = key.jBigDecimal, value = schema)
        final def branch[A](name: JBigInteger, schema: => Value[A]): Self[A] =
          branch(name, key = key.jBigInteger, value = schema)
        final def branch[A](name: SDouble, schema: => Value[A]): Self[A] =
          branch(name, key = key.double, value = schema)
        final def branch[A](name: SFloat, schema: => Value[A]): Self[A] = branch(name, key = key.float, value = schema)
        final def branch[A](name: SInt, schema: => Value[A]): Self[A] = branch(name, key = key.int, value = schema)
        final def branch[A](name: SLong, schema: => Value[A]): Self[A] = branch(name, key = key.long, value = schema)

      trait String[Self[_], Key[_], -Value[_], Sum[_]] extends Branch.Component[Self, Key, Value, Sum]:
        def key: PrimitiveComponent.String[Key]

        final def branch[A](name: JString, schema: => Value[A]): Self[A] =
          branch(name, key = key.string, value = schema)

  given [Key[_], Value[_]]: Shape.Branch[Branch[Key, Value, *], Key, Value] with
    override def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Branch[Key, Value, B] =
      Root(
        key = Reference.Constant(self = Reference.later(key), value = name),
        value = Reference.later(value),
        metadata = Metadata.Empty
      )

    extension [A](self: Branch[Key, Value, A])
      override def key: Reference.Constant[Key, ?] = self.key
      override def value: Reference[Value, ?] = self.value
      override def imap[B](f: A => B)(g: B => A): Branch[Key, Value, B] = self.imap(f)(g)
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[Key, Value, A] = self.modifyMetadata(f)
