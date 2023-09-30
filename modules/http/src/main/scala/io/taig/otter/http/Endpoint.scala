package io.taig.otter.http

import cats.data.Chain

final case class Endpoint[I, O](
    request: Request[I],
    response: Response[O],
    tags: Chain[String],
    summary: Option[String],
    description: Option[String],
    operationId: Option[String]
):
  def request[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
  def response[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))

  def tags(f: Chain[String] => Chain[String]): Endpoint[I, O] = copy(tags = f(tags))
  def tags(values: Chain[String]): Endpoint[I, O] = tags(_ => values)
  def tags(values: String*): Endpoint[I, O] = tags(Chain.fromSeq(values))

  def summary(f: Option[String] => Option[String]): Endpoint[I, O] = copy(summary = f(summary))
  def summary(value: Option[String]): Endpoint[I, O] = summary(_ => value)
  def summary(value: String): Endpoint[I, O] = summary(Some(value))

  def description(f: Option[String] => Option[String]): Endpoint[I, O] = copy(description = f(description))
  def description(value: Option[String]): Endpoint[I, O] = description(_ => value)
  def description(value: String): Endpoint[I, O] = description(Some(value))

  def operationId(f: Option[String] => Option[String]): Endpoint[I, O] = copy(operationId = f(operationId))
  def operationId(value: Option[String]): Endpoint[I, O] = operationId(_ => value)
  def operationId(value: String): Endpoint[I, O] = operationId(Some(value))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, Chain.empty, None, None, None)
