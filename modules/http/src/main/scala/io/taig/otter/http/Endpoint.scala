package io.taig.otter.http

import cats.data.Chain

final case class Endpoint[I, O](
    request: Request[I],
    response: Response[O],
    description: Option[String],
    hidden: Boolean,
    operationId: Option[String],
    summary: Option[String],
    tags: Chain[String]
) extends EndpointLike[Endpoint[I, O]]:
  def request[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
  def response[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))
  def description(f: Option[String] => Option[String]): Endpoint[I, O] = copy(description = f(description))
  def hidden(f: Boolean => Boolean): Endpoint[I, O] = copy(hidden = f(hidden))
  def operationId(f: Option[String] => Option[String]): Endpoint[I, O] = copy(operationId = f(operationId))
  def summary(f: Option[String] => Option[String]): Endpoint[I, O] = copy(summary = f(summary))
  def tags(f: Chain[String] => Chain[String]): Endpoint[I, O] = copy(tags = f(tags))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, None, false, None, None, Chain.empty)
