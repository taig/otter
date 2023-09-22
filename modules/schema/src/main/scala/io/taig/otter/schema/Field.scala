package io.taig.otter.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import scala.collection.immutable.VectorMap

final case class Field[A, B](name: A, key: Schema.Value[A], value: Schema.Value[B], nulls: Option[Null]):
  def isOptional: Boolean = value.isOptional
  def to[C](using Evidence.Product.Aux[C, B]) = ??? // : Record[C] = toRecord.to[B]
