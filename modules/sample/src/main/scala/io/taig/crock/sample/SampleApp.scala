package io.taig.crock.sample

import cats.effect.{IO, IOApp}
import io.circe.Json
import io.taig.crock.OpenApi

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val spec = OpenApi.schema(schemas.userProduct)
    IO.println(Json.fromJsonObject(spec).spaces2)
