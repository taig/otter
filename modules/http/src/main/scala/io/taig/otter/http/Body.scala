package io.taig.otter.http

import io.taig.otter.http.header.MediaType
import io.taig.otter.Reference
import io.taig.otter.Metadata

// TODO strict vs streaming (?)
sealed abstract class Body[A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Body[B] = Body.Modify(self = this, f, g)

  final def orElse[B](body: Body[B]): Body[Either[A, B]] = Body.OrElse(left = this, right = body)
  
  final def or(body: Body[A]): Body[A] = Body.Or(left = this, right = body)

object Body:
  private[otter] final case class Empty(metadata: Metadata) extends Body[Unit]
  
  private[otter] final case class Modify[A, B](self: Body[A], f: A => B, g: B => A) extends Body[B]
  
  private[otter] final case class Or[A, B](left: Body[A], right: Body[A]) extends Body[A]

  private[otter] final case class OrElse[A, B](left: Body[A], right: Body[B]) extends Body[Either[A, B]]
  
  private[otter] final case class Root[S[_], A](mediaType: MediaType, codec: Reference[S, A]) extends Body[A]
