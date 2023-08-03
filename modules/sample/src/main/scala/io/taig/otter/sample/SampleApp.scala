package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.circe.Json
import io.taig.otter.http.{Path, Segment}
import io.taig.otter.http.syntax.*
import io.taig.otter.{CirceEncoder, OpenApi}
import io.taig.otter.schema.schemas.*

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val spec = OpenApi.schema(schemas.userProduct)
    Path.Root / "foo" / "bar" / parameter("foobar", int)
    IO.println(Json.fromJsonObject(spec).spaces2) *>
      IO.println(
        CirceEncoder.schema.encode(
          schemas.user,
          User(User.Name.unsafeFromString("Bonnie Bonus"), User.Age.unsafeFromInt(33), None)
        )
      )
