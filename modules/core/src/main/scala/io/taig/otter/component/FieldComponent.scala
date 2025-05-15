package io.taig.otter.component

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.Field
import io.taig.otter.Reference
import io.taig.otter.schema.RecordSchema
import io.taig.otter.Metadata
import io.taig.otter.Merge

trait FieldComponent[Key[_], Value[_], Record[_]](using record: RecordSchema[Record, Key, Value]):
  final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Field[Key, Value, B] = Field.Root(
    key = Reference.Constant(self = Reference.later(key), value = name),
    value = Reference.later(value),
    metadata = Metadata.Empty
  )

  extension [A](self: Field[Key, Value, A])
    def :*[B](field: Field[Key, Value, B])(using merge: Merge[A, B]): Record[merge.Out] =
      self.toRecord.merge(field.toRecord)
    def *:[B](field: Field[Key, Value, B])(using merge: Merge[A, B]): Record[merge.Out] =
      self.toRecord.merge(field.toRecord)

    def toRecord: Record[A] = record.record(self)

  extension [A](self: Record[A])
    def *:[B](field: Field[Key, Value, B])(using merge: Merge[A, B]): Record[merge.Out] =
      self.merge(field.toRecord)

object FieldComponent:
  trait Primitive[Key[_], Value[_], Record[_]]
      extends FieldComponent.Primitive.Boolean[Key, Value, Record],
        FieldComponent.Primitive.Number[Key, Value, Record],
        FieldComponent.Primitive.String[Key, Value, Record]:
    override def key: PrimitiveComponent[Key]

  object Primitive:
    trait Boolean[Key[_], Value[_], Record[_]] extends FieldComponent[Key, Value, Record]:
      def key: PrimitiveComponent.Boolean[Key]

      final def field[A](name: SBoolean, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.boolean, value = schema)

    trait Number[Key[_], Value[_], Record[_]] extends FieldComponent[Key, Value, Record]:
      def key: PrimitiveComponent.Number[Key]

      final def field[A](name: BigDecimal, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.bigDecimal, value = schema)
      final def field[A](name: BigInt, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.bigInteger, value = schema)
      final def field[A](name: JBigDecimal, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.jBigDecimal, value = schema)
      final def field[A](name: JBigInteger, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.jBigInteger, value = schema)
      final def field[A](name: SDouble, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.double, value = schema)
      final def field[A](name: SFloat, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.float, value = schema)
      final def field[A](name: SInt, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.int, value = schema)
      final def field[A](name: SLong, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.long, value = schema)

    trait String[Key[_], Value[_], Record[_]] extends FieldComponent[Key, Value, Record]:
      def key: PrimitiveComponent.String[Key]

      final def field[A](name: JString, schema: => Value[A]): Field[Key, Value, A] =
        field(name, key = key.string, value = schema)
