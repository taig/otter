//package io.taig.otter.http
//
//final class RequestBodyOps[F[a] <: Request.Body[a], A](val self: F[A]) extends AnyVal:
//  def :*[B](headers: Headers[B]): self.Self[(A, B)] = self.zip(headers)
//  def :*[B](header: Header[B]): self.Self[(A, B)] = :*(header.toHeaders)
//
//trait ToRequestBodyOps:
//  implicit def toRequestBodyOps[F[a] <: Request.Body[a], A](self: F[A]): RequestBodyOps[F, A] = RequestBodyOps(self)
