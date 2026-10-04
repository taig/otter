package io.taig.otter.http

import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.otter.http.codec.PathTemplate

/** An ordered group of routes, retaining the requirements of every member. */
final case class Routes[F[_], +S[-_, +_]](values: Chain[Route[F, S, ?, ?]]):
  /** Each route's method and path template, taken apart once rather than on every request nothing matched. */
  private lazy val templates: Chain[(Method, Chain[Either[String, (String, Parameter.Node[?, ?])]])] =
    values.map(route => (route.endpoint.request.method, PathTemplate(route.endpoint.request.path.value)))

  def ++[T[-_, +_]](that: Routes[F, T]): Routes[F, Body.Or[S, T]] = Routes(values ++ that.values)

  /** What these routes serve, in registration order, as the declarations a renderer takes. */
  def declarations: Chain[Endpoint.Declaration.Node] = values.map(_.declaration)

  /** What a request none of these routes matched is, as far as these routes can say.
    *
    * The methods are those of every route that spells the path, in registration order and each once. The request's own
    * method is left out, which only matters when these routes were never actually tried: a `405` naming the method it
    * refused would contradict itself, so that case is a `404` instead.
    *
    * The methods come from what is routed and never from what an [[Api]] documents, since an endpoint that is described
    * but not served is not one a request could be sent to instead.
    */
  def unrouted(method: Method, segments: Vector[String]): Unrouted =
    val allowed = templates
      .collect { case (candidate, template) if Route.addresses(template, segments) => candidate }
      .distinct
      .filterNot(_ == method)

    NonEmptyChain
      .fromChain(allowed)
      .fold[Unrouted](Unrouted.NotFound(method, segments))(Unrouted.MethodNotAllowed(method, segments, _))

object Routes:
  def apply[F[_], S[-_, +_]](routes: Route[F, S, ?, ?]*): Routes[F, S] = new Routes(Chain.fromSeq(routes))

  def empty[F[_]]: Routes[F, Nothing] = new Routes(Chain.empty)
