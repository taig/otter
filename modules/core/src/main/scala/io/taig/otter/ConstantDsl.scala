package io.taig.otter

import cats.Eq
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import java.util.UUID

trait ConstantDsl[+Self[_], -Value[_]](using codec: Codec.Constant[Self, Value]):
  self =>

  final def constant[A: Eq](codec: => Value[A], value: A): Self[A] = self.codec.constant(codec, value)

object ConstantDsl:
  trait Primitive[+Self[_], -Value[_]]
      extends ConstantDsl.Primitive.Boolean[Self, Value],
        ConstantDsl.Primitive.Number[Self, Value],
        ConstantDsl.Primitive.String[Self, Value]:
    this: PrimitiveDsl[Value] =>

  object Primitive:
    trait Boolean[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.Boolean[Value] =>

      final def constant(value: SBoolean): Self[SBoolean] = constant(codec = boolean, value)

    trait Number[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.Number[Value] =>

      final def constant(value: JBigDecimal): Self[JBigDecimal] =
        constant(codec = jBigDecimal, value)(using Eq.fromUniversalEquals)
      final def constant(value: BigDecimal): Self[BigDecimal] = constant(codec = bigDecimal, value)
      final def constant(value: JBigInteger): Self[JBigInteger] =
        constant(codec = jBigInteger, value)(using Eq.fromUniversalEquals)
      final def constant(value: BigInt): Self[BigInt] = constant(codec = bigInteger, value)
      final def constant(value: SLong): Self[SLong] = constant(codec = long, value)
      final def constant(value: SDouble): Self[SDouble] = constant(codec = double, value)
      final def constant(value: SFloat): Self[SFloat] = constant(codec = float, value)
      final def constant(value: SInt): Self[SInt] = constant(codec = int, value)

    trait String[+Self[_], -Value[_]] extends ConstantDsl[Self, Value]:
      this: PrimitiveDsl.String[Value] =>

      final def constant(value: JString): Self[JString] = constant(codec = string, value)
      final def constant(value: UUID): Self[UUID] = constant(codec = uuid, value)
