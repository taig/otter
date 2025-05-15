package io.taig.otter.component

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.Argument
import cats.syntax.all.*
import io.taig.otter.Comparison
import cats.Eq
import io.taig.otter.schema.ConstantSchema

trait ConstantComponent[+Self[_], -Value[_]](using self: ConstantSchema[Self, Value]):
  final def constant[A: Eq](schema: => Value[A], value: A): Self[Unit] = self(schema, value)

object ConstantComponent:
  trait Primitive[+Self[_], -Value[_]]
      extends ConstantComponent.Primitive.Boolean[Self, Value],
        ConstantComponent.Primitive.Number[Self, Value],
        ConstantComponent.Primitive.String[Self, Value]:
    this: PrimitiveComponent[Value] =>

  object Primitive:
    trait Boolean[+Self[_], -Value[_]] extends ConstantComponent[Self, Value]:
      this: PrimitiveComponent.Boolean[Value] =>

      final def constant(value: SBoolean): Self[Unit] = constant(schema = boolean, value)

    trait Number[+Self[_], -Value[_]] extends ConstantComponent[Self, Value]:
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

    trait String[+Self[_], -Value[_]] extends ConstantComponent[Self, Value]:
      this: PrimitiveComponent.String[Value] =>
      final def constant(value: JString): Self[Unit] = constant(schema = string, value)
      final def constant(value: UUID): Self[Unit] = constant(schema = uuid, value)
