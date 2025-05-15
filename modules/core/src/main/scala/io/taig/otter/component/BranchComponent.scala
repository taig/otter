package io.taig.otter.component

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.Branch
import io.taig.otter.Reference
import io.taig.otter.Metadata
import io.taig.otter.schema.SumSchema

trait BranchComponent[Key[_], Value[_], Sum[_]](using sum: SumSchema[Sum, Key, Value]):
  final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Branch[Key, Value, B] = Branch.Root(
    key = Reference.Constant(self = Reference.later(key), name),
    value = Reference.later(value),
    metadata = Metadata.Empty
  )

  extension [A](self: Branch[Key, Value, A]) def toSum: Sum[A] = sum.sum(self)

object BranchComponent:
  trait Primitive[Key[_], Value[_], Record[_]]
      extends BranchComponent.Primitive.Boolean[Key, Value, Record],
        BranchComponent.Primitive.Number[Key, Value, Record],
        BranchComponent.Primitive.String[Key, Value, Record]:
    override def key: PrimitiveComponent[Key]

  object Primitive:
    trait Boolean[Key[_], Value[_], Sum[_]] extends BranchComponent[Key, Value, Sum]:
      def key: PrimitiveComponent.Boolean[Key]

      final def branch[A](name: SBoolean, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.boolean, value = schema)

    trait Number[Key[_], Value[_], Sum[_]] extends BranchComponent[Key, Value, Sum]:
      def key: PrimitiveComponent.Number[Key]

      final def branch[A](name: BigDecimal, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.bigDecimal, value = schema)
      final def branch[A](name: BigInt, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.bigInteger, value = schema)
      final def branch[A](name: JBigDecimal, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.jBigDecimal, value = schema)
      final def branch[A](name: JBigInteger, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.jBigInteger, value = schema)
      final def branch[A](name: SDouble, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.double, value = schema)
      final def branch[A](name: SFloat, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.float, value = schema)
      final def branch[A](name: SInt, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.int, value = schema)
      final def branch[A](name: SLong, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.long, value = schema)

    trait String[Key[_], Value[_], Sum[_]] extends BranchComponent[Key, Value, Sum]:
      def key: PrimitiveComponent.String[Key]

      final def branch[A](name: JString, schema: => Value[A]): Branch[Key, Value, A] =
        branch(name, key = key.string, value = schema)
