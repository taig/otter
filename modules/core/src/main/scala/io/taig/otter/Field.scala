package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations

sealed abstract class Field[A](val name: String, val nulls: Option[Null]):
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
  def apply[A, B](name: A, key: Value[A], value: Schema[B]): Field[B] = new Field[B](key.print(name).orEmpty, None):
    override def decodeWithRemainders(name: String, data: Data.Object): Validated[Violations, (Data.Object, B)] =
      data.firstWithRemainders(name) match
        case Some((head, tail)) => value.decode(head).tupleLeft(tail)
        case None               => value.decode(Data.Null).tupleLeft(data)

    override def encode(name: String, b: B, nulls: Null): Data.Object = value.encode(b) match
      case Data.Null if nulls === Null.Hide => Data.Object.Empty
      case data                             => Data.Object.one(this.name, data)
