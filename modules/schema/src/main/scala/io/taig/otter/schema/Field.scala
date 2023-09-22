package io.taig.otter.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import scala.collection.immutable.VectorMap

final case class Field[A, B](key: Schema.Value[A], nulls: Option[Null], value: Schema.Value[B]):
  def isOptional: Boolean = value.isOptional
  def to[C](using Evidence.Product.Aux[C, B]) = ??? // : Record[C] = toRecord.to[B]
