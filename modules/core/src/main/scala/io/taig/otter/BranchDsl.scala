package io.taig.otter

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait BranchDsl[+Self[_], -Key[_], -Value[_], Sum[_]](using codec: Codec.Branch[Self, Key, Value, Sum]):
  final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = codec.branch(name, key, value)

object BranchDsl:
  trait Primitive[+Self[_], Key[_], -Value[_], Record[_]]
      extends BranchDsl.Primitive.Boolean[Self, Key, Value, Record],
        BranchDsl.Primitive.Number[Self, Key, Value, Record],
        BranchDsl.Primitive.String[Self, Key, Value, Record]:
    override def key: PrimitiveDsl[Key]

  object Primitive:
    trait Boolean[+Self[_], Key[_], -Value[_], Sum[_]] extends BranchDsl[Self, Key, Value, Sum]:
      def key: PrimitiveDsl.Boolean[Key]

      final def branch[A](name: SBoolean, codec: => Value[A]): Self[A] =
        branch(name, key = key.boolean, value = codec)

    trait Number[+Self[_], Key[_], -Value[_], Sum[_]] extends BranchDsl[Self, Key, Value, Sum]:
      def key: PrimitiveDsl.Number[Key]

      final def branch[A](name: BigDecimal, codec: => Value[A]): Self[A] =
        branch(name, key = key.bigDecimal, value = codec)
      final def branch[A](name: BigInt, codec: => Value[A]): Self[A] = branch(name, key = key.bigInteger, value = codec)
      final def branch[A](name: JBigDecimal, codec: => Value[A]): Self[A] =
        branch(name, key = key.jBigDecimal, value = codec)
      final def branch[A](name: JBigInteger, codec: => Value[A]): Self[A] =
        branch(name, key = key.jBigInteger, value = codec)
      final def branch[A](name: SDouble, codec: => Value[A]): Self[A] = branch(name, key = key.double, value = codec)
      final def branch[A](name: SFloat, codec: => Value[A]): Self[A] = branch(name, key = key.float, value = codec)
      final def branch[A](name: SInt, codec: => Value[A]): Self[A] = branch(name, key = key.int, value = codec)
      final def branch[A](name: SLong, codec: => Value[A]): Self[A] = branch(name, key = key.long, value = codec)

    trait String[+Self[_], Key[_], -Value[_], Sum[_]] extends BranchDsl[Self, Key, Value, Sum]:
      def key: PrimitiveDsl.String[Key]

      final def branch[A](name: JString, codec: => Value[A]): Self[A] =
        branch(name, key = key.string, value = codec)
