package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.Union

/** Statically declared answers for every recoverable execution failure. */
final case class ErrorPolicy[+S[-_, +_], +E](
    envelope: Response.Schema[S, Failure, E],
    syntax: Response.Schema[S, Failure, E],
    contentType: Response.Schema[S, Failure, E],
    validation: Response.Schema[S, Failure, E],
    entityRead: Response.Schema[S, Failure, E],
    encoding: Response.Schema[S, Failure, E],
    status: Response.Schema[S, Failure, E],
    unexpected: Response.Schema[S, Failure, E]
):
  /** Selection is encoded into the schema, so the wire encoder needs no response-producing callback. */
  val responses: Responses.Schema[S, Failure, E] =
    def leaf(value: Response.Schema[S, Failure, E]): Union[Response.Schema[S, *, *], Failure, E] =
      Union.Root(Reference.now(value))
    def append(
        left: Union[Response.Schema[S, *, *], Failure, E],
        category: Failure.Category,
        right: Response.Schema[S, Failure, E]
    ): Union[Response.Schema[S, *, *], Failure, E] =
      Union.Modify(
        Union.Coproduct(left, leaf(right)),
        _.fold(identity, identity),
        failure => if failure.category == category then Right(failure) else Left(failure)
      )
    val entries = List(
      Failure.Category.Syntax -> syntax,
      Failure.Category.ContentType -> contentType,
      Failure.Category.Validation -> validation,
      Failure.Category.EntityRead -> entityRead,
      Failure.Category.Encoding -> encoding,
      Failure.Category.Status -> status,
      Failure.Category.Unexpected -> unexpected
    )
    Responses.Schema(entries.foldLeft(leaf(envelope)) { case (self, (category, response)) =>
      append(self, category, response)
    })

  def apply[T[-w, +r] >: S[w, r], AW, AR, BW, BR](
      endpoint: Endpoint.Schema[T, AW, AR, BW, BR]
  ): ComposedEndpoint[T, AW, AR, BW, BR, E] = ComposedEndpoint(endpoint, this)

object ErrorPolicy:
  /** Bodyless defaults work with every payload interpreter. Errors on the wire are identified by status. */
  val default: ErrorPolicy[Nothing, Status] =
    def response(status: Int): Response.Schema[Nothing, Failure, Status] =
      Response.Schema(Response.Value.Modify(Response.Value.Root(Status(status)), _ => Status(status), _ => ()))
    ErrorPolicy(
      response(400),
      response(400),
      response(415),
      response(422),
      response(500),
      response(500),
      response(500),
      response(500)
    )
