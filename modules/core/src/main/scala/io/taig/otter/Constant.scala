package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.Primitive.Component as PrimitiveComponent

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Constant[+S[_], A] extends Product with Serializable:
  def metadata: Metadata
  def schema: Reference.Constant[S, ?]
  def modifyMetadata(f: Metadata => Metadata): Constant[S, A]
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, A]
  final def imap[B](f: A => B)(g: B => A): Constant[S, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.{metadata, schema}
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](
      schema: Reference.Constant[S, A],
      eq: Eq[A],
      metadata: Metadata
  ) extends Constant[S, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, Unit] =
      copy(schema = schema.mapK[S1, T](fK))

  trait Component[+Self[_], -Value[_]](using self: Schema.Constant[Self, Value]):
    final def constant[A: Eq](schema: => Value[A], value: A): Self[Unit] = self.constant(schema, value)

  object Component:
    trait Primitive[+Self[_], -Value[_]]
        extends Constant.Component.Primitive.Boolean[Self, Value],
          Constant.Component.Primitive.Number[Self, Value],
          Constant.Component.Primitive.String[Self, Value]:
      this: PrimitiveComponent[Value] =>

    object Primitive:
      trait Boolean[+Self[_], -Value[_]] extends Constant.Component[Self, Value]:
        this: PrimitiveComponent.Boolean[Value] =>

        final def constant(value: SBoolean): Self[Unit] = constant(schema = boolean, value)

      trait Number[+Self[_], -Value[_]] extends Constant.Component[Self, Value]:
        this: PrimitiveComponent.Number[Value] =>
        final def constant(value: JBigDecimal): Self[Unit] =
          constant(schema = jBigDecimal, value)(using Eq.fromUniversalEquals)
        final def constant(value: BigDecimal): Self[Unit] = constant(schema = bigDecimal, value)
        final def constant(value: JBigInteger): Self[Unit] =
          constant(schema = jBigInteger, value)(using Eq.fromUniversalEquals)
        final def constant(value: BigInt): Self[Unit] = constant(schema = bigInteger, value)
        final def constant(value: SLong): Self[Unit] = constant(schema = long, value)
        final def constant(value: SDouble): Self[Unit] = constant(schema = double, value)
        final def constant(value: SFloat): Self[Unit] = constant(schema = float, value)
        final def constant(value: SInt): Self[Unit] = constant(schema = int, value)

      trait String[+Self[_], -Value[_]] extends Constant.Component[Self, Value]:
        this: PrimitiveComponent.String[Value] =>
        final def constant(value: JString): Self[Unit] = constant(schema = string, value)
        final def constant(value: UUID): Self[Unit] = constant(schema = uuid, value)

  given [Value[_]]: Schema.Constant[Constant[Value, *], Value] with
    override def constant[A](schema: => Value[A], value: A)(using eq: Eq[A]): Constant[Value, Unit] = Root(
      schema = Reference.Constant(self = Reference.later(schema), value),
      eq,
      metadata = Metadata.Empty
    )

    extension [A](fa: Constant[Value, A])
      override def imap[B](f: A => B)(g: B => A): Constant[Value, B] = fa.imap(f)(g)
      override def modifyMetadata(f: Metadata => Metadata): Constant[Value, A] = fa.modifyMetadata(f)
      override def metadata: Metadata = fa.metadata
