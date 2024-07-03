package io.taig.otter.json.circe

import io.taig.otter.+
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.Decoder
import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.syntax.all.*
import io.taig.otter.SchemaValidation

// TODO branch / namespace (?)
object UnionJsonDecoder:
  def apply[A](schema: Union.Reader[A], json: Json): Decoder.Result[Json, A] = ???
  //   schema match
  //   case Base.Union.Modify(self, validation, _)     => modify(self, validation, json)
  //   case Base.Union.One(schema)                     => one(schema, json)
  //   case Base.Union.Optional(self)                  => optional(self, json)
  //   case Base.Union.OrElse(left, right)             => orElse(left, right, json)
  //   case Base.Union.Reader.Modify(self, validation) => modify(self, validation, json)
  //   case Base.Union.Reader.One(schema)              => one(schema, json)
  //   case Base.Union.Reader.Optional(self)           => optional(self, json)
  //   case Base.Union.Reader.OrElse(left, right)      => orElse(left, right, json)

  // def modify[A, V1, V2, B](
  //     schema: Union.Reader[A],
  //     validation: SchemaValidation[A, V1, V2, B],
  //     json: Json
  // ): Validated[Violations[Json, Json], B] = apply(schema, json).andThen:
  //   validation(_).leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply))).leftMap(Violations.root)

  // def one[A](schema: Schema.Reader[A], json: Json): Validated[Violations[Json, Json], A] =
  //   JsonDecoder(schema, json)

  // def optional[A](self: Union.Reader[A], json: Json): Validated[Violations[Json, Json], Option[A]] =
  //   if json.isNull then none.valid else apply(self, json).map(_.some)

  // def orElse[A, B](
  //     left: Union.Reader[A],
  //     right: Schema.Reader[B],
  //     json: Json
  // ): Validated[Violations[Json, Json], A + B] =
  //   apply(left, json).map(_.asLeft).findValid(JsonDecoder(right, json).map(_.asRight))
