package io.taig.otter.component

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.Schema
import io.taig.otter.Field
import io.taig.otter.Reference

trait FieldComponent[Self[_], -Key[_], -Value[_], Record[_]](using
    // self: Schema.Field[Self, Key, Value],
    record: Schema.Record[Record, Key, Value]
):
  final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = ???
  // Field.Root(key = Reference.Constant(self = Reference.later(key), value = name), value = Reference.later(value))

  extension [A](self: Self[A]) def toRecord: Record[A] = ??? // record.record(self)

object FieldComponent:
  trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
      extends FieldComponent.Primitive.Boolean[Self, Key, Value, Record],
        FieldComponent.Primitive.Number[Self, Key, Value, Record],
        FieldComponent.Primitive.String[Self, Key, Value, Record]:
    override def key: PrimitiveComponent[Key]

  object Primitive:
    trait Boolean[Self[_], Key[_], -Value[_], Record[_]] extends FieldComponent[Self, Key, Value, Record]:
      def key: PrimitiveComponent.Boolean[Key]

      final def field[A](name: SBoolean, schema: => Value[A]): Self[A] =
        field(name, key = key.boolean, value = schema)

    trait Number[Self[_], Key[_], -Value[_], Record[_]] extends FieldComponent[Self, Key, Value, Record]:
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

    trait String[Self[_], Key[_], -Value[_], Record[_]] extends FieldComponent[Self, Key, Value, Record]:
      def key: PrimitiveComponent.String[Key]

      final def field[A](name: JString, schema: => Value[A]): Self[A] =
        field(name, key = key.string, value = schema)
