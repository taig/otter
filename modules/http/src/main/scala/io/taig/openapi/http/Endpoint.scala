package io.taig.openapi.http

import cats.syntax.all.*
import cats.{Functor, Inject}

final case class Endpoint[I, O](input: Input[I], output: Output[O]):
  def modifyInput[T](f: Input[I] => Input[T]): Endpoint[T, O] = copy(input = f(input))
  def modifyOutput[T](f: Output[O] => Output[T]): Endpoint[I, T] = copy(output = f(output))

  def imap[T](f: O => T)(g: T => O): Endpoint[I, T] = modifyOutput(_.imap(f)(g))

object Endpoint:
  final case class Implementation[F[_], I, O](endpoint: Endpoint[I, O], implementation: I => F[O]):
    def imap[T](f: O => T)(g: T => O)(using Functor[F]): Endpoint.Implementation[F, I, T] =
      Implementation(endpoint.imap(f)(g), implementation(_).map(f))

    def ~(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = toRoutes :+ endpoint
    def toRoutes: Routes[F] = Routes.one(this)
