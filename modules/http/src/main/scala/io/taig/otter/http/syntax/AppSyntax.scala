package io.taig.otter.http.syntax

import io.taig.otter.http.App
import io.taig.otter.http.Result
import io.taig.otter.http.Routes
import io.taig.otter.http.syntax.CodeSyntax.*
import io.taig.otter.http.syntax.ResultSyntax.*

trait AppSyntax:
  def app[F[_], S[_], T[_], U[_]](routes: Routes[F, S, T, U], notFound: Result[U, Unit]): App[F, S, T, U] =
    App(routes, notFound)

  def app[F[_], S[_], T[_], U[_]](routes: Routes[F, S, T, U]): App[F, S, T, U] =
    app(routes, notFound = result(notFound))

object AppSyntax extends AppSyntax
