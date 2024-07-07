package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.Plain.*
import io.taig.otter.Decoder
import io.taig.otter as Base
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import cats.data.Chain

object JsonDecoder extends Decoder[Schema.Reader, Json]:
  override def apply[A](schema: Schema.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case schema: Collection.Reader[A] =>
      if json.isNull then CollectionJsonDecoder(schema, none)
      else
        json.asArray match
          case Some(array) => CollectionJsonDecoder(schema, array.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = typeOf(json).asJson)).invalid
    case schema: Dictionary.Reader[A] =>
      if json.isNull then DictionaryJsonDecoder(schema, none)
      else
        json.asObject match
          case Some(obj) => DictionaryJsonDecoder(schema, obj.toList.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "object"), actual = typeOf(json).asJson)).invalid
    case schema: Enumeration.Reader[A] => EnumerationJsonDecoder(schema, json)
    case schema: Sum.Reader[A]         => ???
    case schema: Primitive.Reader[A]   => PrimitiveJsonDecoder(schema, json)
    case schema: Product.Reader[A] =>
      if json.isNull then ProductJsonDecoder(schema, none)
      else
        json.asArray match
          case Some(array) => ProductJsonDecoder(schema, array.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = typeOf(json).asJson)).invalid
    case schema: Record.Reader[A] =>
      if json.isNull then RecordJsonDecoder(schema, none)
      else
        json.asObject match
          case Some(obj) => RecordJsonDecoder(schema, Chain.fromIterableOnce(obj.toIterable).some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "object"), actual = typeOf(json).asJson)).invalid
    case schema: Union.Reader[A] => UnionJsonDecoder(schema, json)
