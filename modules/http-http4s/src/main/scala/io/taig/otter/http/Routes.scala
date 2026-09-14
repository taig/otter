package io.taig.otter.http

import cats.data.Chain

/** An ordered group of routes, retaining the requirements of every member. */
final case class Routes[F[_], +S[-_, +_]](values: Chain[Route[F, S, ?, ?]]):
  def ++[T[-_, +_]](that: Routes[F, T]): Routes[F, Body.Or[S, T]] = Routes(values ++ that.values)

object Routes:
  def apply[F[_], S[-_, +_]](routes: Route[F, S, ?, ?]*): Routes[F, S] = new Routes(Chain.fromSeq(routes))

  def empty[F[_]]: Routes[F, Nothing] = new Routes(Chain.empty)
