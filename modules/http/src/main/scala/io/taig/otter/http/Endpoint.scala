package io.taig.otter.http

import cats.data.Chain

final case class Endpoint[I, O](request: Request[I], response: Response[O], tags: Chain[String]):
  def modifyRequest[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
  def modifyResponse[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))
  def tags(f: Chain[String] => Chain[String]): Endpoint[I, O] = copy(tags = f(tags))
  def tags(values: Chain[String]): Endpoint[I, O] = tags(_ => values)
  def tags(values: String*): Endpoint[I, O] = tags(Chain.fromSeq(values))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, Chain.empty)
