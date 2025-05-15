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

trait BranchComponent[Self[_], -Key[_], -Value[_], Sum[_]]:
  final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = ??? // self.branch(name, key, value)

  extension [A](self: Self[A]) def toSum: Sum[A] = ???

object BranchComponent:
  trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
      extends BranchComponent.Primitive.Boolean[Self, Key, Value, Record],
        BranchComponent.Primitive.Number[Self, Key, Value, Record],
        BranchComponent.Primitive.String[Self, Key, Value, Record]:
    override def key: PrimitiveComponent[Key]

  object Primitive:
    trait Boolean[Self[_], Key[_], -Value[_], Sum[_]] extends BranchComponent[Self, Key, Value, Sum]:
      def key: PrimitiveComponent.Boolean[Key]

      final def branch[A](name: SBoolean, schema: => Value[A]): Self[A] =
        branch(name, key = key.boolean, value = schema)

    trait Number[Self[_], Key[_], -Value[_], Sum[_]] extends BranchComponent[Self, Key, Value, Sum]:
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

    trait String[Self[_], Key[_], -Value[_], Sum[_]] extends BranchComponent[Self, Key, Value, Sum]:
      def key: PrimitiveComponent.String[Key]

      final def branch[A](name: JString, schema: => Value[A]): Self[A] =
        branch(name, key = key.string, value = schema)
