package io.taig.openapi.schema
import cats.data.{Chain, Validated}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.validation.{Constraint, Validation}

// TODO
abstract class Record[A] extends Schema[A] {
  override type Self[a] = Record[a]
  override type Codec = OpenApi
}
