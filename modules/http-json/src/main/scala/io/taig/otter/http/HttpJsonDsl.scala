package io.taig.otter.http

import io.taig.otter.Json
import io.taig.otter.JsonDsl.*
import io.taig.otter.http.CodeDsl.*
import io.taig.otter.http.ResultDsl.*
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait HttpJsonDsl:
  def json[A](codec: => Json[A]): Body[Json, A] = BodyDsl.body(
    mediaType = MediaType(
      tpe = MediaType.Type(primary = "application", secondary = "json"),
      parameters = Parameters.Empty
    ),
    codec
  )

  def response[S[_], A](value: Results[S, A]): Response[S, Json, A] = Response(
    results = value,
    validation = result(
      unprocessableEntity,
      json(error("validation", field("violations", violations)))
    ).toResults,
    failure = result(internalServerError, json(string.nullable)).toResults
  )

object HttpJsonDsl extends HttpJsonDsl
