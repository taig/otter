package io.taig.otter.json

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.*
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import cats.data.Chain

object JsonDecoder extends Decoder[Schema.Reader.Via[Json, *], Json]:
  override def apply[A](schema: Schema.Reader.Via[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case schema: Collection.Reader.Via[Json, A] =>
      if json.isNull then CollectionJsonDecoder(schema, none)
      else
        json.asArray match
          case Some(array) => CollectionJsonDecoder(schema, array.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = typeOf(json).asJson)).invalid
    case schema: Dictionary.Reader.Via[Json, A] =>
      if json.isNull then DictionaryJsonDecoder(schema, none)
      else
        json.asObject match
          case Some(obj) => DictionaryJsonDecoder(schema, obj.toList.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "object"), actual = typeOf(json).asJson)).invalid
    case schema: Dynamic.Reader[Json, A]         => DynamicJsonDecoder(schema, json)
    case schema: Enumeration.Reader.Via[Json, A] => EnumerationJsonDecoder(schema, json)
    case schema: Sum.Reader.Via[Json, A] =>
      if json.isNull then SumJsonDecoder(schema, none)
      else
        json.asObject match
          case Some(obj) => SumJsonDecoder(schema, obj.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "object"), actual = typeOf(json).asJson)).invalid
    case schema: Primitive.Reader[A] => PrimitiveJsonDecoder(schema, json)
    case schema: Product.Reader.Via[Json, A] =>
      if json.isNull then ProductJsonDecoder(schema, none)
      else
        json.asArray match
          case Some(array) => ProductJsonDecoder(schema, array.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = typeOf(json).asJson)).invalid
    case schema: Record.Reader.Via[Json, A] =>
      if json.isNull then RecordJsonDecoder(schema, none)
      else
        json.asObject match
          case Some(obj) => RecordJsonDecoder(schema, Chain.fromIterableOnce(obj.toIterable).some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "object"), actual = typeOf(json).asJson)).invalid
    case schema: Union.Reader.Via[Json, A] => UnionJsonDecoder(schema, json)
