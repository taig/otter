package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.Plain.*
import io.taig.otter.Decoder
import io.taig.otter as Base
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonDecoder extends Decoder[Schema.Reader, Json]:
  override def apply[A](schema: Schema.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case schema: Collection.Reader[A] =>
      if json.isNull then JsonCollectionDecoder(schema, none)
      else
        json.asArray match
          case Some(array) => JsonCollectionDecoder(schema, array.some)
          case None =>
            Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = typeOf(json).asJson)).invalid
    case schema: Primitive.Reader[A] => JsonPrimitiveDecoder(schema, json)
    case schema: Union.Reader[A]     => ???
