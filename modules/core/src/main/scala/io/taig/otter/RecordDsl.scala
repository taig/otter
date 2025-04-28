package io.taig.otter

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait RecordDsl[+Self[_], -Key[_], -Value[_]](using codec: Codec.Record[Self, Key, Value]):
  final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
    codec.field(name, key, value)

object RecordDsl:
  trait Primitive[+Self[_], Key[_], -Value[_]]
      extends RecordDsl.Primitive.Boolean[Self, Key, Value],
        RecordDsl.Primitive.Number[Self, Key, Value],
        RecordDsl.Primitive.String[Self, Key, Value]:
    override protected def key: PrimitiveDsl[Key]

  object Primitive:
    trait Boolean[+Self[_], Key[_], -Value[_]] extends RecordDsl[Self, Key, Value]:
      protected def key: PrimitiveDsl.Boolean[Key]

      final def field[A](name: SBoolean, codec: => Value[A]): Self[A] =
        field(name, key = key.boolean, value = codec)

    trait Number[+Self[_], Key[_], -Value[_]] extends RecordDsl[Self, Key, Value]:
      protected def key: PrimitiveDsl.Number[Key]

      final def field[A](name: BigDecimal, codec: => Value[A]): Self[A] =
        field(name, key = key.bigDecimal, value = codec)
      final def field[A](name: BigInt, codec: => Value[A]): Self[A] = field(name, key = key.bigInteger, value = codec)
      final def field[A](name: JBigDecimal, codec: => Value[A]): Self[A] =
        field(name, key = key.jBigDecimal, value = codec)
      final def field[A](name: JBigInteger, codec: => Value[A]): Self[A] =
        field(name, key = key.jBigInteger, value = codec)
      final def field[A](name: SDouble, codec: => Value[A]): Self[A] = field(name, key = key.double, value = codec)
      final def field[A](name: SFloat, codec: => Value[A]): Self[A] = field(name, key = key.float, value = codec)
      final def field[A](name: SInt, codec: => Value[A]): Self[A] = field(name, key = key.int, value = codec)
      final def field[A](name: SLong, codec: => Value[A]): Self[A] = field(name, key = key.long, value = codec)

    trait String[+Self[_], Key[_], -Value[_]] extends RecordDsl[Self, Key, Value]:
      protected def key: PrimitiveDsl.String[Key]

      final def field[A](name: JString, codec: => Value[A]): Self[A] =
        field(name, key = key.string, value = codec)
