package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript
import io.taig.otter.Key
import cats.syntax.all.*
import cats.Functor

final class JsonFieldRenderer[S[_], V[_]: Functor, A](renderer: Renderer[Json, V[Typescript[A]]])
    extends Renderer[Json.Field, V[(String, Typescript[A])]]:
  val self: Renderer[Json.Field, V[(String, Typescript[A])]] =
    FieldTypescriptRenderer[Key, Json, V, Typescript[A]](
      key = KeyPrinter.Unquoted,
      value = renderer
    ).mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self.self)

  override def render[B](schema: Json.Field[B]): V[(String, Typescript[A])] = self.render(schema)
