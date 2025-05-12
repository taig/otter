package io.taig.otter.http

import io.taig.otter.http.CodeDsl.*
import io.taig.otter.http.ResultDsl.*

trait AppDsl:
  def app[F[_], S[_], T[_], U[_]](routes: Routes[F, S, T, U], notFound: Result[U, Unit]): App[F, S, T, U] =
    App(routes, notFound)

  def app[F[_], S[_], T[_], U[_]](routes: Routes[F, S, T, U]): App[F, S, T, U] =
    app(routes, notFound = result(notFound))

object AppDsl extends AppDsl
