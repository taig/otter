//package io.taig.otter
//
//import cats.syntax.all.*
//
//final case class Field[A, B](name: A, key: Schema.Value[A], value: Schema[B], nulls: Option[Null]):
//  def isOptional: Boolean = value.isOptional
//
//  def toRecord: Schema.Record[B] = Schema.Record(this)
//  def to[C](using Evidence.Product.Aux[C, B]): Schema.Record[C] = toRecord.to
//
//object Field extends ToFieldOps:
//  def apply[A, B](name: A, key: Schema.Value[A], value: Schema[B]): Field[A, B] =
//    Field(name, key, value, None)
