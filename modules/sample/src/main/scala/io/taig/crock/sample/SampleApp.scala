package io.taig.crock.sample

import cats.effect.{IO, IOApp}
import io.circe.Json
import io.taig.crock.http.{Path, Segment}
import io.taig.crock.http.syntax.*
import io.taig.crock.{CirceEncoder, OpenApi}
import io.taig.crock.schema.schemas.*

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
