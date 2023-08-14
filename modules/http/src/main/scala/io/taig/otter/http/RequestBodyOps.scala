package io.taig.otter.http

final class RequestBodyOps[F[a] <: Request.Body[a], A](self: F[A]) extends AnyVal:
  def :*[B](header: Headers[B]): F[(A, B)] = ???
  def :*[B](header: Header[B]): F[(A, B)] = :*(header.toHeaders)

trait ToRequestBodyOps:
  implicit def toRequestBodyOps[F[a] <: Request.Body[a], A](self: F[A]): RequestBodyOps[F, A] = RequestBodyOps(self)
