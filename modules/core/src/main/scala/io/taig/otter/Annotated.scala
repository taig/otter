package io.taig.otter

import cats.data.NonEmptyList
import cats.syntax.all.*

import scala.collection.Factory

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

    /** A collection valued attribute written as its elements, so that `attr(OpenApiKeys.tags, "books", "manuals")`
      * reads the way the document it renders to does.
      *
      * The collection the key holds is built through its [[scala.collection.Factory]], so this covers every collection
      * that has one -- `List`, `Seq`, `Vector`, `Set`, and a `Map` written as pairs -- and not just the one type the
      * key happens to name.
      *
      * At least one element is required, which keeps this apart from the single value overload above: `attr(key, Nil)`
      * remains the way to write an empty collection, and a key whose value is itself a collection of collections is
      * still written whole by passing the outer one.
      */
    final def attr[A, C](namespace: Metadata.Namespace, key: Metadata.Key[C], value: A, values: A*)(using
        factory: Factory[A, C]
    ): T = metadata(_.put(namespace, key, (value +: values).to(factory)))

    final def attr[A, C](key: Metadata.Key[C], value: A, values: A*)(using factory: Factory[A, C]): T =
      attr(namespace = Metadata.Namespace.Global, key, value, values*)

  final def imap[U](f: T => U)(g: U => T): Annotated[U] = new Annotated[U]:
    extension (u: U) override def lens: (Metadata, Metadata => U) = self.lens(g(u)).map(_.map(f))

object Annotated:
  inline def apply[T](using annotated: Annotated[T]): Annotated[T] = annotated
