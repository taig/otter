package io.taig.otter.http

final class ResponseBodyOps[F[a] <: Response.Body[a], A](self: F[A]) extends AnyVal:
  def :*[B](header: Headers[B]): F[(A, B)] = ???
  def :*[B](header: Header[B]): F[(A, B)] = :*(header.toHeaders)

trait ToResponseBodyOps:
  implicit def toResponseBodyOps[F[a] <: Response.Body[a], A](self: F[A]): ResponseBodyOps[F, A] = ResponseBodyOps(self)
