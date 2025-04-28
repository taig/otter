package io.taig.otter

import cats.Eq

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait ConstantDsl[+Self[_], -Value[_]](using codec: Codec.Constant[Self, Value]):
  self =>

  final def constant[A: Eq](codec: => Value[A], value: A): Self[Unit] = self.codec.constant(codec, value)

object ConstantDsl:
  trait Primitive[+Self[_], -Value[_]]
      extends ConstantDsl.Primitive.Boolean[Self, Value],
        ConstantDsl.Primitive.Number[Self, Value],
        ConstantDsl.Primitive.String[Self, Value]:
    this: PrimitiveDsl[Value] =>

  object Primitive:
    trait Boolean[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.Boolean[Value] =>

      final def constant(value: SBoolean): Self[Unit] = constant(codec = boolean, value)

    trait Number[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.Number[Value] =>
      final def constant(value: JBigDecimal): Self[Unit] =
        constant(codec = jBigDecimal, value)(using Eq.fromUniversalEquals)
      final def constant(value: BigDecimal): Self[Unit] = constant(codec = bigDecimal, value)
      final def constant(value: JBigInteger): Self[Unit] =
        constant(codec = jBigInteger, value)(using Eq.fromUniversalEquals)
      final def constant(value: BigInt): Self[Unit] = constant(codec = bigInteger, value)
      final def constant(value: SLong): Self[Unit] = constant(codec = long, value)
      final def constant(value: SDouble): Self[Unit] = constant(codec = double, value)
      final def constant(value: SFloat): Self[Unit] = constant(codec = float, value)
      final def constant(value: SInt): Self[Unit] = constant(codec = int, value)

    trait String[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.String[Value] =>
      final def constant(value: JString): Self[Unit] = constant(codec = string, value)
      final def constant(value: UUID): Self[Unit] = constant(codec = uuid, value)
