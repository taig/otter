package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violations

// TODO use Eq and key codec to do stuff
final case class Field[A](name: String, codec: Codec[A], nulls: Option[Null]):
  def isOptional: Boolean = codec.isOptional

  def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = toRecord.to
  def toRecord: Record[A] = Record(this)

  def nulls(f: Option[Null] => Option[Null]): Field[A] = copy(nulls = f(nulls))
  def nulls(value: Option[Null]): Field[A] = nulls(_ => value)
  def nulls(value: Null): Field[A] = nulls(Some(value))

  def decodeWithRemainders(data: Chain[(String, Data)]): Validated[Violations, (Chain[(String, Data)], A)] =
    data.firstWithRemainders(name) match
      case Some((head, tail)) => codec.decode(head).tupleLeft(tail)
      case None               => codec.decode(None).tupleLeft(data)

  def encode(a: A, parent: Null): Chain[(String, Data)] =
    val nulls = (parent, this.nulls) match
      case (_, Some(nulls)) => nulls
      case (nulls, None)    => nulls

    codec.encode(a) match
      case Data.Null if nulls === Null.Hide => Chain.empty
      case data                             => Chain.one(this.name, data)

object Field extends ToFieldOps:
  def apply[A, B](name: A, key: Value.Required[A], codec: Codec[B]): Field[B] =
    Field(key.print(name), codec, None)
