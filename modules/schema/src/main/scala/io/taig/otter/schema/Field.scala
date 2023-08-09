package io.taig.otter.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*

import scala.collection.immutable.VectorMap

abstract class Field[A, B]:
  self =>
  def key: Schema.Value[?]
  def name: A
  def schema: Schema[?]

  def properties: Field.Properties[B]
  final def copy(update: Field.Properties[B]): Field[A, B] = new Field[A, B]:
    export self.{_encode, decodeWithRemainders, key, name, schema}
    override def properties: Field.Properties[B] = update

  final class Nulls:
    def value: Option[Null] = properties.nulls
    def modify(f: Option[Null] => Option[Null]): Field[A, B] = copy(properties.copy(nulls = f(properties.nulls)))
    def apply(value: Option[Null]): Field[A, B] = modify(_ => value)
    def inherit: Field[A, B] = apply(None)
    def hide: Field[A, B] = apply(Some(Null.Hide))
    def show: Field[A, B] = apply(Some(Null.Show))

  def nulls: Nulls = new Nulls

  def decodeWithRemainders(openapi: VectorMap[String, OpenApi]): Validated[Violations, (VectorMap[String, OpenApi], B)]
  final def encode(b: B, nulls: Null): OpenApi.Object = _encode(b, properties.nulls.getOrElse(nulls))
  protected def _encode(b: B, nulls: Null): OpenApi.Object

//  def toRecord: Record[B] = Schema.Record(this)
//  def to[C](using Evidence.Product.Aux[C, B]): Record[C] = toRecord.to[C]

object Field extends ToFieldOps:
  final case class Properties[+A](default: Option[A], nulls: Option[Null])

  object Properties:
    val Default: Field.Properties[Nothing] = Properties(None, None)

  def apply[A, B](_name: A, _key: => Schema.Value[A], _schema: => Schema[B]): Field[A, B] = new Field[A, B]:
    override def key: Schema.Value[A] = _key
    override def name: A = _name
    def printKey: String = key.print(name).getOrElse("")
    override def schema: Schema[B] = _schema
    override def properties: Properties[B] = Properties.Default
    override def decodeWithRemainders(
        openapi: VectorMap[String, OpenApi]
    ): Validated[Violations, (VectorMap[String, OpenApi], B)] =
      val key = printKey
      schema.decode(openapi.get(key).flatMap(_.asValue)).tupleLeft(openapi.removed(key))
    override def _encode(b: B, nulls: Null): OpenApi.Object = (schema.encode(b), nulls) match
      case (Some(value), _)  => OpenApi.obj(printKey := value)
      case (None, Null.Show) => OpenApi.obj(printKey := OpenApi.Null)
      case (None, Null.Hide) => OpenApi.Object.Empty
