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
  final class Ops[A](a: A)(using enriched: Enriched[A]):
    def metadata: Metadata = enriched.metadata(a)
    def metadata(f: Metadata => Metadata): A = enriched.modifyMetadata(a)(f)
    def metadata[B](key: Metadata.Key[B]): Option[B] = enriched.metadata(a).get(key)
    def metadata[B](key: Metadata.Key[B], value: Option[B]): A =
      enriched.modifyMetadata(a)(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    def metadata[B](key: Metadata.Key[B], value: B): A = enriched.modifyMetadata(a)(_.put(key, value))
