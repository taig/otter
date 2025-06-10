package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Key
import cats.syntax.all.*
import cats.Functor

final class JsonFieldRenderer[S[_], V[_]: Functor, A](renderer: Renderer[Json, V[A]])
    extends Renderer[Json.Field, V[(String, A)]]:
  val self: Renderer[Json.Field, V[(String, A)]] =
    FieldTypescriptRenderer[Key, Json, V, A](
      key = KeyPrinter.Unquoted,
      value = renderer
    ).mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)

  override def render[B](schema: Json.Field[B]): V[(String, A)] = self.render(schema)
