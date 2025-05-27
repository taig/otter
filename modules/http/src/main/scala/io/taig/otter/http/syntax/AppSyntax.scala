package io.taig.otter.http.syntax

import io.taig.otter.http.App
import io.taig.otter.http.Result
import io.taig.otter.http.Routes
import io.taig.otter.http.syntax.CodeSyntax.*
import io.taig.otter.http.syntax.ResultSyntax.*

trait AppSyntax:
  def app[F[_], S[_]](routes: Routes[F, S], notFound: Result[S, Unit]): App[F, S] =
    App(routes, notFound)

  def app[F[_], S[_], T[_], U[_]](routes: Routes[F, S]): App[F, S] =
    app(routes, notFound = result(code.notFound))

object AppSyntax extends AppSyntax
