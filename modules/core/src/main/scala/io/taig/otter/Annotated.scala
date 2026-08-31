package io.taig.otter

import cats.data.NonEmptyList
import cats.syntax.all.*

/** Read and write the [[Metadata]] of any type that carries some. */
trait Annotated[T]:
  self =>

  extension (self: T)
    def lens: (Metadata, Metadata => T)

    final def metadata: Metadata = lens._1

    final def metadata(f: Metadata => Metadata): T = lens._2(f(lens._1))

    final def attr[A](key: Metadata.Key[A]): Option[A] = metadata.get(namespace = Metadata.Namespace.Global, key)

    final def attr[A](namespace: Metadata.Namespace, namespaces: Metadata.Namespace*)(key: Metadata.Key[A]): Option[A] =
      metadata.get(namespace, namespaces*)(key)

    final def attr[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Option[A] = attr(namespace)(key)

    final def attr[A](namespaces: NonEmptyList[Metadata.Namespace], key: Metadata.Key[A]): Option[A] =
      attr(namespace = namespaces.head, namespaces = namespaces.tail*)(key)

    final def attr[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): T =
      metadata(_.put(namespace, key, value))

    final def attr[A](key: Metadata.Key[A], value: A): T = attr(namespace = Metadata.Namespace.Global, key, value)

  final def imap[U](f: T => U)(g: U => T): Annotated[U] = new Annotated[U]:
    extension (u: U) override def lens: (Metadata, Metadata => U) = self.lens(g(u)).map(_.map(f))

object Annotated:
  inline def apply[T](using annotated: Annotated[T]): Annotated[T] = annotated
