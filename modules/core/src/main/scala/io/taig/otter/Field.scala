package io.taig.otter

import cats.syntax.all.*

final case class Field[A, B](name: A, key: Schema.Value[A], value: Schema[B], nulls: Option[Null]):
  def isOptional: Boolean = value.isOptional

  def :*[C, D](field: Field[C, D]): Record[(B, D)] = ???
  def *:[C, D](field: Field[C, D]): Record[(D, B)] = ???

  def toRecord: Record[B] = Record(this)
  def to[C](using Evidence.Product.Aux[C, B]): Record[C] = toRecord.to

object Field:
  def apply[A, B](name: A, key: Schema.Value[A], value: Schema[B]): Field[A, B] =
    Field(name, key, value, None)
