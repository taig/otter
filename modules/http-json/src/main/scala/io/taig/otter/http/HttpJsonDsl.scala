package io.taig.otter.http

import io.taig.otter.Json
import io.taig.otter.JsonDsl.*
import io.taig.otter.http.CodeDsl.*
import io.taig.otter.http.ResultDsl.*
import io.taig.otter.http.BodyDsl.*
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait HttpJsonDsl:
  def json[A](codec: => Json[A]): Body[Json, A] = body(
    mediaType = MediaType(
      tpe = MediaType.Type(primary = "application", secondary = "json"),
      parameters = Parameters.Empty
    ),
    codec
  )

  def response[S[_], A](results: Results[S, A]): Response[S, Json, A] = ???
  // Response(
  //   results,
  //   validation = result(
  //     unprocessableEntity,
  //     json(error("validation", field("violations", violations).toRecord))
  //   ).toResults,
  //   failure = result(internalServerError, json(string.nullable)).toResults
  // )

  def response[S[_], A](result: Result[S, A]): Response[S, Json, A] =
    response(results = result.toResults)

object HttpJsonDsl extends HttpJsonDsl
