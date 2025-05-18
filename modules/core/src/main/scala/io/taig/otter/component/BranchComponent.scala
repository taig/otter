package io.taig.otter.component

import io.taig.otter.schema.BranchSchema
import io.taig.otter.schema.SumSchema

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait BranchComponent[Self[_], Key[_], -Value[_]](using
    self: BranchSchema[Self, Key, Value]
):
  final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
    self(name, key, value)

object BranchComponent:
  trait Primitive[Self[_], Key[_], -Value[_]]
      extends BranchComponent.Primitive.Boolean[Self, Key, Value],
        BranchComponent.Primitive.Number[Self, Key, Value],
        BranchComponent.Primitive.String[Self, Key, Value]:
    override def key: PrimitiveComponent[Key]

  object Primitive:
    trait Boolean[Self[_], Key[_], -Value[_]] extends BranchComponent[Self, Key, Value]:
      def key: PrimitiveComponent.Boolean[Key]

      final def branch[A](name: SBoolean, schema: => Value[A]): Self[A] =
        branch(name, key = key.boolean, value = schema)

    trait Number[Self[_], Key[_], -Value[_]] extends BranchComponent[Self, Key, Value]:
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
      final def branch[A](name: SFloat, schema: => Value[A]): Self[A] =
        branch(name, key = key.float, value = schema)
      final def branch[A](name: SInt, schema: => Value[A]): Self[A] =
        branch(name, key = key.int, value = schema)
      final def branch[A](name: SLong, schema: => Value[A]): Self[A] =
        branch(name, key = key.long, value = schema)

    trait String[Self[_], Key[_], -Value[_]] extends BranchComponent[Self, Key, Value]:
      def key: PrimitiveComponent.String[Key]

      final def branch[A](name: JString, schema: => Value[A]): Self[A] =
        branch(name, key = key.string, value = schema)
