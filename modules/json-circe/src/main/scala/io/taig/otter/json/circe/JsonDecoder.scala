package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.Plain.*
import io.taig.otter.Decoder
import io.taig.otter as Base
import io.taig.otter.Isomorphic
import io.taig.otter.Reader
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonDecoder extends Decoder[Schema.Reader, Json]:
  override def apply[A](schema: Schema.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
    case Isomorphic.Root(schema) => apply(schema, json)
    case Reader.Root(schema)     => apply(schema, json)

  def apply[A](
      schema: Base.Optional[Base.Schema[Parent.Reader.Any, *], A],
      json: Json
  ): Validated[Violations[Json, Json], A] = schema match
    case Base.Optional.Root(self) if json.isNull => none.valid[Violations[Json, Json]]
    case Base.Optional.Root(self)                => apply(self, json).map(_.some)
    case Base.Required(data)                     => apply(data, json)

  def apply[A](data: Base.Schema[Parent.Reader.Any, A], json: Json): Validated[Violations[Json, Json], A] = data match
    case schema: Base.Collection[Parent.Reader.Any, A] =>
      json.asArray
        .toValid(Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)))
        .andThen(JsonCollectionDecoder(schema, _))
    case schema: Base.Primitive[A]                => JsonPrimitiveDecoder(schema, json)
    case schema: Base.Tuple[Parent.Reader.Any, A] => ??? // Json.fromValues(JsonTupleEncoder(schema, a))
