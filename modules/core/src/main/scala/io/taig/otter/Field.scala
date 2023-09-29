package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.syntax.*

sealed abstract class Field[A](val name: String, val nulls: Option[Null]):
  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = toRecord.to
  final def toRecord: Record[A] = Record(this)

  final def nulls(f: Option[Null] => Option[Null]): Field[A] = Field(this, name, f(nulls))
  final def nulls(value: Option[Null]): Field[A] = nulls(_ => value)
  final def nulls(value: Null): Field[A] = nulls(Some(value))

  final def decodeWithRemainders(data: Chain[(String, Data)]): Validated[Violations, (Chain[(String, Data)], A)] =
    decodeWithRemainders(name, data)

  protected def decodeWithRemainders(
      name: String,
      data: Chain[(String, Data)]
  ): Validated[Violations, (Chain[(String, Data)], A)]

  final def encode(a: A, parent: Null): Chain[(String, Data)] =
    val nulls = (parent, this.nulls) match
      case (_, Some(nulls)) => nulls
      case (nulls, None)    => nulls
    encode(name, a, nulls)
  protected def encode(name: String, a: A, nulls: Null): Chain[(String, Data)]

object Field extends ToFieldOps:
  def apply[A](field: Field[A], name: String, nulls: Option[Null]): Field[A] =
    new Field[A](name, nulls) { export field.* }

  def apply[A, B](name: A, key: Schema.Value[A], schema: Schema[B]): Field[B] =
    new Field[B](key.print(name).orEmpty, None):
      override def decodeWithRemainders(
          name: String,
          data: Chain[(String, Data)]
      ): Validated[Violations, (Chain[(String, Data)], B)] =
        data.firstWithRemainders(name) match
          case Some((head, tail)) => schema.decode(head).tupleLeft(tail)
          case None               => schema.decode(None).tupleLeft(data)

      override def encode(name: String, b: B, nulls: Null): Chain[(String, Data)] = schema.encode(b) match
        case Data.Null if nulls === Null.Hide => Chain.empty
        case data                             => Chain.one(this.name, data)
