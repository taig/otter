package io.taig.otter

import cats.Invariant
import cats.data.NonEmptyList
import cats.syntax.all.*

trait Annotated[T]:
  self =>

  extension (self: T)
    def lens: (Metadata, Metadata => T)

    final def metadata: Metadata = lens._1

    final def metadata(f: Metadata => Metadata): T = lens._2(f(lens._1))

    final def attr[A](key: Metadata.Key[A]): Option[A] = metadata.get(namespace = Metadata.Namespace.Global, key)

    final def attr[A](namespace: Metadata.Namespace, namespaces: Metadata.Namespace*)(key: Metadata.Key[A]): Option[A] =
      namespaces.foldl(metadata.get(namespace, key)):
        case (None, namespace)     => metadata.get(namespace, key)
        case (result @ Some(_), _) => result

    final def attr[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Option[A] = attr(namespace)(key)

    final def attr[A](namespaces: NonEmptyList[Metadata.Namespace], key: Metadata.Key[A]): Option[A] =
      attr(namespace = namespaces.head, namespaces = namespaces.tail*)(key)

    final def attr[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): T =
      metadata(_.put(namespace, key, value))

    final def attr[A](key: Metadata.Key[A], value: A): T = attr(namespace = Metadata.Namespace.Global, key, value)

  final def imap[B](f: T => B)(g: B => T): Annotated[B] = new Annotated[B]:
    extension (b: B) override def lens: (Metadata, Metadata => B) = self.lens(g(b)).map(_.map(f))

object Annotated:
  inline def apply[A](using annotated: Annotated[A]): Annotated[A] = annotated

  given Invariant[Annotated]:
    override def imap[A, B](fa: Annotated[A])(f: A => B)(g: B => A): Annotated[B] = fa.imap(f)(g)
