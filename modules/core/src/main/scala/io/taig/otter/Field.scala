package io.taig.otter

import cats.Eq
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violations

sealed abstract class Field[A](val nulls: Option[Null]):
  self =>
  def key: Value.Required[?]
  def codec: Codec[?]
  def name: String

  final def isOptional: Boolean = codec.isOptional

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = toRecord.to
  final def toRecord: Record[A] = Record(this)
  def toProduct: Product[A]

  final def nulls(f: Option[Null] => Option[Null]): Field[A] = new Field[A](f(nulls)) { export self.* }
  final def nulls(value: Option[Null]): Field[A] = nulls(_ => value)
  final def nulls(value: Null): Field[A] = nulls(Some(value))

  def decodeWithRemainders(data: Chain[(String, Data)]): Validated[Violations, (Chain[(String, Data)], A)]
  final def encode(a: A, parent: Null): Chain[(String, Data)] = (parent, nulls) match
    case (_, Some(nulls)) => encodeWithNull(a, nulls)
    case (parent, _)      => encodeWithNull(a, parent)
  protected def encodeWithNull(a: A, nulls: Null): Chain[(String, Data)]

object Field extends ToFieldOps:
  def apply[A: Eq, B](a: A, ofKey: => Value.Required[A], ofCodec: => Codec[B]): Field[B] = new Field[B](None):
    override def key: Value.Required[?] = ofKey
    override def codec: Codec[?] = ofCodec
    override def name: String = ofKey.print(a)

    override def toProduct: Product[B] = Product(ofCodec)

    // TODO we gotta do some parsing here!
    override def decodeWithRemainders(data: Chain[(String, Data)]): Validated[Violations, (Chain[(String, Data)], B)] =
      data.firstWithRemainders(name) match
        case Some((head, tail)) => ofCodec.decode(head).tupleLeft(tail)
        case None               => ofCodec.decode(None).tupleLeft(data)

    override def encodeWithNull(b: B, nulls: Null): Chain[(String, Data)] = ofCodec.encode(b) match
      case Data.Null if nulls === Null.Hide => Chain.empty
      case data                             => Chain.one(this.name, data)
