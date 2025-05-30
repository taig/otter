package io.taig.otter.operation

import io.taig.otter.Metadata

trait Enriched[A]:
  self =>

  def metadata(a: A): Metadata
  def modifyMetadata(a: A)(f: Metadata => Metadata): A

  def imap[B](f: A => B)(g: B => A): Enriched[B] = new Enriched[B]:
    override def metadata(b: B): Metadata = self.metadata(g(b))
    override def modifyMetadata(b: B)(h: Metadata => Metadata): B = f(self.modifyMetadata(g(b))(h))

object Enriched:
  inline def apply[A](using enriched: Enriched[A]): Enriched[A] = enriched

  given [S[_], A](using schema: SchemaInvariant[S]): Enriched[S[A]] = schema.enriched
