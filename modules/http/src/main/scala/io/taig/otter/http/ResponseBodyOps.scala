//package io.taig.otter.http
//
//final class ResponseBodyOps[F[a] <: Response.Body[a], A](val self: F[A]) extends AnyVal:
//  def :*[B](headers: Headers[B]): self.Self[(A, B)] = self.zip(headers)
//  def :*[B](header: Header[B]): self.Self[(A, B)] = :*(header.toHeaders)
//
//trait ToResponseBodyOps:
//  implicit def toResponseBodyOps[F[a] <: Response.Body[a], A](self: F[A]): ResponseBodyOps[F, A] = ResponseBodyOps(self)
