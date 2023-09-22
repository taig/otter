package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

import scala.collection.immutable.VectorMap

final case class Field[A, B](name: A, key: Schema.Value[A], value: Schema.Value[B], nulls: Option[Null]):
  def isOptional: Boolean = value.isOptional
  def to[C](using Evidence.Product.Aux[C, B]) = ??? // : Record[C] = toRecord.to[B]
