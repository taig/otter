//package io.taig.otter.http
//
//final case class Endpoint[I, O](request: Request[I], response: Response[O]):
//  def modifyRequest[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
//  def modifyResponse[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))
