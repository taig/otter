package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.Union

/** Statically declared answers for every recoverable execution failure. */
final case class ErrorPolicy[+S[-w, +r], +E](
    envelope: Result.Schema[S, Failure, E],
    syntax: Result.Schema[S, Failure, E],
    contentType: Result.Schema[S, Failure, E],
    validation: Result.Schema[S, Failure, E],
    entityRead: Result.Schema[S, Failure, E],
    encoding: Result.Schema[S, Failure, E],
    status: Result.Schema[S, Failure, E],
    unexpected: Result.Schema[S, Failure, E],
    interpreter: Result.Schema[S, Failure, E]
):
  /** Selection is encoded into the schema, so the wire encoder needs no response-producing callback. */
  val responses: Results.Schema[S, Failure, E] =
    def leaf(value: Result.Schema[S, Failure, E]): Union[[w, r] =>> Result.Schema[S, w, r], Failure, E] =
      Union.Root(Reference.now(value))
    def append(
        left: Union[[w, r] =>> Result.Schema[S, w, r], Failure, E],
        category: Failure.Category,
        right: Result.Schema[S, Failure, E]
    ): Union[[w, r] =>> Result.Schema[S, w, r], Failure, E] =
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
      Failure.Category.Unexpected -> unexpected,
      Failure.Category.Interpreter -> interpreter
    )
    Results.Schema(entries.foldLeft(leaf(envelope)) { case (self, (category, result)) =>
      append(self, category, result)
    })

  def apply[T[-w, +r] >: S[w, r], AW, AR, BW, BR](
      endpoint: Endpoint.Schema[T, AW, AR, BW, BR]
  ): ComposedEndpoint[T, AW, AR, BW, BR, E] = ComposedEndpoint(endpoint, this)

object ErrorPolicy:
  /** Bodyless defaults work with every payload interpreter. Errors on the wire are identified by status. */
  val default: ErrorPolicy[Nothing, Code] =
    def result(code: Int): Result.Schema[Nothing, Failure, Code] =
      Result.Schema(Result.Value.Modify(Result.Value.Root(Code(code)), _ => Code(code), _ => ()))
    ErrorPolicy(
      result(400),
      result(400),
      result(415),
      result(422),
      result(500),
      result(500),
      result(500),
      result(500),
      result(500)
    )
