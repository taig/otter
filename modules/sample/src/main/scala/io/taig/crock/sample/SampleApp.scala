package io.taig.crock.sample

import cats.effect.{IO, IOApp}
import io.circe.Json
import io.taig.crock.{CirceEncoder, OpenApi}

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val spec = OpenApi(CirceEncoder.schema).schema(schemas.gender)
    IO.println(Json.fromJsonObject(spec).spaces2)
