package io.taig.otter.schema

import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Invariant
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Branch[+S[_], +T[_], A] extends Schema[T, A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, A]

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(value = value.mapK(fK))

  trait Shape[Self[_], -Key[_], -Value[_], Sum[_]] extends Schema.Shape[Self], Invariant.Coproduct.Lift[Self, Sum]:
    def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

    extension [A](self: Self[A]) def toSum: Sum[A]

  // trait Component[+Self[_], -Key[_], -Value[_], Sum[_]](using codec: Codec.Branch[Self, Key, Value, Sum]):
  //   final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = codec.branch(name, key, value)

  // object Component:
  //   trait Primitive[+Self[_], Key[_], -Value[_], Record[_]]
  //       extends Component.Primitive.Boolean[Self, Key, Value, Record],
  //         Component.Primitive.Number[Self, Key, Value, Record],
  //         Component.Primitive.String[Self, Key, Value, Record]:
  //     override def key: PrimitiveDsl[Key]

  //   object Primitive:
  //     trait Boolean[+Self[_], Key[_], -Value[_], Sum[_]] extends Component[Self, Key, Value, Sum]:
  //       def key: PrimitiveDsl.Boolean[Key]

  //       final def branch[A](name: SBoolean, codec: => Value[A]): Self[A] =
  //         branch(name, key = key.boolean, value = codec)

  //     trait Number[+Self[_], Key[_], -Value[_], Sum[_]] extends Component[Self, Key, Value, Sum]:
  //       def key: PrimitiveDsl.Number[Key]

  //       final def branch[A](name: BigDecimal, codec: => Value[A]): Self[A] =
  //         branch(name, key = key.bigDecimal, value = codec)
  //       final def branch[A](name: BigInt, codec: => Value[A]): Self[A] = branch(name, key = key.bigInteger, value = codec)
  //       final def branch[A](name: JBigDecimal, codec: => Value[A]): Self[A] =
  //         branch(name, key = key.jBigDecimal, value = codec)
  //       final def branch[A](name: JBigInteger, codec: => Value[A]): Self[A] =
  //         branch(name, key = key.jBigInteger, value = codec)
  //       final def branch[A](name: SDouble, codec: => Value[A]): Self[A] = branch(name, key = key.double, value = codec)
  //       final def branch[A](name: SFloat, codec: => Value[A]): Self[A] = branch(name, key = key.float, value = codec)
  //       final def branch[A](name: SInt, codec: => Value[A]): Self[A] = branch(name, key = key.int, value = codec)
  //       final def branch[A](name: SLong, codec: => Value[A]): Self[A] = branch(name, key = key.long, value = codec)

  //     trait String[+Self[_], Key[_], -Value[_], Sum[_]] extends Component[Self, Key, Value, Sum]:
  //       def key: PrimitiveDsl.String[Key]

  //       final def branch[A](name: JString, codec: => Value[A]): Self[A] =
  //         branch(name, key = key.string, value = codec)
