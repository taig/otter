package io.taig.otter.json.circe

import io.taig.otter.+
import io.taig.otter.Union
import io.taig.otter.Schema
import io.circe.Json
import io.taig.otter.Union.One
import io.taig.otter.Union.OrElse
import io.taig.otter.Union.Optional
import io.taig.otter.Union.Validate

object JsonUnionEncoder:
  def encode[A](schema: Union[Schema[?], A], value: A): Json = schema match
    case Union.One(schema)                                 => JsonEncoder.encode(schema, value)
    case Union.OrElse(left, right)                         => encode(left, right, value)
    case Union.Optional(schema)                            => ???
    case Union.Validate(schema, constraint, validation, g) => ???

  def encode[A, B](left: Union[Schema[?], A], right: Union[Schema[?], B], value: A + B): Json = value match
    case Left(a)  => encode(left, a)
    case Right(b) => encode(right, b)
