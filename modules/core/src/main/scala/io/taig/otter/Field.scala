package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations

sealed abstract class Field[A](val name: String, val nulls: Option[Null]):
  final transparent inline def :*[B](field: Field[B]): Record[?] = toRecord :* field
  final transparent inline def *:[B](field: Field[B]): Record[?] = field *: toRecord

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = toRecord.to
  final def toRecord: Record[A] = Record(this)

  final def decodeWithRemainders(data: Data.Object): Validated[Violations, (Data.Object, A)] =
    decodeWithRemainders(name, data)
  protected def decodeWithRemainders(name: String, data: Data.Object): Validated[Violations, (Data.Object, A)]

  final def encode(a: A, parent: Null): Data.Object =
    val nulls = (parent, this.nulls) match
      case (_, Some(nulls)) => nulls
      case (nulls, None)    => nulls
    encode(name, a, nulls)
  protected def encode(name: String, a: A, nulls: Null): Data.Object

object Field:
  def apply[A, B](name: A, key: Value[A], schema: Schema[B]): Field[B] = new Field[B](key.print(name).orEmpty, None):
    override def decodeWithRemainders(name: String, data: Data.Object): Validated[Violations, (Data.Object, B)] =
      data.firstWithRemainders(name) match
        case Some((head, tail)) => schema.decode(head).tupleLeft(tail)
        case None               => schema.decode(Data.Null).tupleLeft(data)

    override def encode(name: String, b: B, nulls: Null): Data.Object = schema.encode(b) match
      case Data.Null if nulls === Null.Hide => Data.Object.Empty
      case data                             => Data.Object.one(this.name, data)
