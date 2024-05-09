package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint
import io.circe.syntax.*
import cats.data.Chain

object JsonDecoder extends Decoder[[a] =>> Schema.Reader.Any[Schema.Reader.Identity[a], a], Json]:
  override def apply[A](
      schema: Schema.Reader.Any[Schema.Reader.Identity[A], A],
      json: Json
  ): Validated[Violations[Json, Json], A] = schema match
    case schema: Collection.Reader[Schema.Reader.Identity[A], A] =>
      if json.isNull then JsonCollectionDecoder(schema, none)
      else
        json.asArray match
          case Some(values) => JsonCollectionDecoder(schema, Chain.fromSeq(values).some)
          case None         => Violations.rootNec(Violation(Constraint.Type("array"), typeOf(json).asJson)).invalid
    case schema: Primitive.Reader[A] => JsonPrimitiveDecoder(schema, json)

// override def decode[A](schema: Tuple[Schema[?], A], json: Json): Validated[Violations[Json], A] =
//   if json.isNull then JsonTupleDecoder.decode(schema, none)
//   else
//     json.asArray match
//       case Some(values) => JsonTupleDecoder.decode(schema, Chain.fromSeq(values).some)
//       case None         => Violations.rootNec(Violation(Constraint.Type("array"), typeOf(json).asJson)).invalid

// override def decode[A](schema: Union[Schema[?], A], json: Json): Validated[Violations[Json], A] =
//   JsonUnionDecoder.decode(schema, json)
