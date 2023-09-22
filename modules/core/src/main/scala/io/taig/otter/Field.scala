package io.taig.otter

import cats.syntax.all.*

final case class Field[A, B](name: A, key: Schema.Value[A], value: Schema[B], nulls: Option[Null]):
  def isOptional: Boolean = value.isOptional

  def :*[C, D](field: Field[C, D]): Schema.Record[(B, D)] = ???
  def *:[C, D](field: Field[C, D]): Schema.Record[(D, B)] = ???

  def toRecord: Schema.Record[B] = Schema.Record(this)
  def to[C](using Evidence.Product.Aux[C, B]): Schema.Record[C] = toRecord.to

object Field:
  def apply[A, B](name: A, key: Schema.Value[A], value: Schema[B]): Field[A, B] =
    Field(name, key, value, None)
