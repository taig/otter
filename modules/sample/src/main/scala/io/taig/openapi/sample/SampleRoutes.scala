package io.taig.openapi.sample

import cats.effect.IO
import cats.syntax.all.*
import io.taig.openapi.http.Endpoint

import scala.reflect.ClassTag

abstract class SampleRoutes(authentication: Authentication):
  extension [I, O](self: AuthorizedEndpoint[I, O])
    def infallible(f: I => IO[O]): AuthorizedEndpoint.Implementation[I, O] = authentication(self)(f)

  extension [I, E <: Throwable: ClassTag, O](self: AuthorizedEndpoint[I, Either[E, O]])
    def raise(f: I => IO[O]): AuthorizedEndpoint.Implementation[I, Either[E, O]] =
      authentication(self)(f(_).attemptNarrow[E])

  extension [I, E <: Matchable, O <: Matchable](self: AuthorizedEndpoint[I, E | O])
    inline def union(f: I => IO[E | O]): AuthorizedEndpoint.Implementation[I, Either[E, O]] =
      authentication(self)(i => f(i)).imap {
        case o: O                       => Right(o)
        case e: E                       => Left(e)
        case error: Authorization.Error => error
      } {
        case Right(o)                   => o
        case Left(e)                    => e
        case error: Authorization.Error => error
      }

  extension [I, O](self: UnauthorizedEndpoint[I, O])
    def infallible(f: I => IO[O]): UnauthorizedEndpoint.Implementation[I, O] = Endpoint.Implementation(self, f)
