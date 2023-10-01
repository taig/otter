package io.taig.otter.http

import cats.data.Chain

final case class Endpoint[R, I, O](
    environment: R,
    request: Request[I],
    response: Response[O],
    deprecated: Boolean,
    description: Option[String],
    hidden: Boolean,
    operationId: Option[String],
    summary: Option[String],
    tags: Chain[String]
):
  def environment[T](f: R => T): Endpoint[T, I, O] = copy(environment = f(environment))
  def environment[T](value: T): Endpoint[T, I, O] = environment(_ => value)

  def request[T](f: Request[I] => Request[T]): Endpoint[R, T, O] = copy(request = f(request))
  def request[T](value: Request[T]): Endpoint[R, T, O] = request(_ => value)

  def response[T](f: Response[O] => Response[T]): Endpoint[R, I, T] = copy(response = f(response))
  def response[T](value: Response[T]): Endpoint[R, I, T] = response(_ => value)

  def deprecated(f: Boolean => Boolean): Endpoint[R, I, O] = copy(deprecated = f(deprecated))
  def deprecated(value: Boolean): Endpoint[R, I, O] = deprecated(_ => value)

  def description(f: Option[String] => Option[String]): Endpoint[R, I, O] = copy(description = f(description))
  def description(value: Option[String]): Endpoint[R, I, O] = description(_ => value)
  def description(value: String): Endpoint[R, I, O] = description(Some(value))

  def hidden(f: Boolean => Boolean): Endpoint[R, I, O] = copy(hidden = f(hidden))
  def hidden(value: Boolean): Endpoint[R, I, O] = hidden(_ => value)

  def operationId(f: Option[String] => Option[String]): Endpoint[R, I, O] = copy(operationId = f(operationId))
  def operationId(value: Option[String]): Endpoint[R, I, O] = operationId(_ => value)
  def operationId(value: String): Endpoint[R, I, O] = operationId(Some(value))

  def summary(f: Option[String] => Option[String]): Endpoint[R, I, O] = copy(summary = f(summary))
  def summary(value: Option[String]): Endpoint[R, I, O] = summary(_ => value)
  def summary(value: String): Endpoint[R, I, O] = summary(Some(value))

  def tags(f: Chain[String] => Chain[String]): Endpoint[R, I, O] = copy(tags = f(tags))
  def tags(values: Chain[String]): Endpoint[R, I, O] = tags(_ => values)
  def tags(values: String*): Endpoint[R, I, O] = tags(Chain.fromSeq(values))

object Endpoint:
  def apply[R, I, O](environment: R, request: Request[I], response: Response[O]): Endpoint[R, I, O] =
    Endpoint(environment, request, response, false, None, false, None, None, Chain.empty)

  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[Unit, I, O] =
    Endpoint((), request, response, false, None, false, None, None, Chain.empty)
