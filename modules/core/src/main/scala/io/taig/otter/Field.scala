package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violations

final case class Field[A](name: String, schema: Schema[A], nulls: Option[Null]):
  def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = toRecord.to
  def toRecord: Record[A] = Record(this)

  def nulls(f: Option[Null] => Option[Null]): Field[A] = copy(nulls = f(nulls))
  def nulls(value: Option[Null]): Field[A] = nulls(_ => value)
  def nulls(value: Null): Field[A] = nulls(Some(value))

  def decodeWithRemainders(data: Chain[(String, Data)]): Validated[Violations, (Chain[(String, Data)], A)] =
    data.firstWithRemainders(name) match
      case Some((head, tail)) => schema.decode(head).tupleLeft(tail)
      case None               => schema.decode(None).tupleLeft(data)

  def encode(a: A, parent: Null): Chain[(String, Data)] =
    val nulls = (parent, this.nulls) match
      case (_, Some(nulls)) => nulls
      case (nulls, None)    => nulls

    schema.encode(a) match
      case Data.Null if nulls === Null.Hide => Chain.empty
      case data                             => Chain.one(this.name, data)

object Field extends ToFieldOps:
  def apply[A, B](name: A, key: Value[A], schema: Schema[B]): Field[B] =
    Field(key.print(name).orEmpty, schema, None)
