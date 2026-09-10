package io.taig.otter

import cats.Order
import cats.Show
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

opaque type Metadata = SortedMap[(Metadata.Namespace, Metadata.Key[Any]), Any]

object Metadata:
  opaque type Key[A] = String

  object Key:
    def apply[A](identifier: String): Metadata.Key[A] = identifier

    given [A] => (order: Order[String]) => Order[Metadata.Key[A]] = order

  opaque type Namespace = String

  object Namespace:
    val Global: Metadata.Namespace = "*"

    def apply(identifier: String): Metadata.Namespace = identifier

  extension (self: Metadata)
    inline def toSortedMap: SortedMap[(Metadata.Namespace, Metadata.Key[Any]), Any] = self

    def contains[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Boolean =
      toSortedMap.contains((namespace, key))

    /** What the key holds, taken on trust.
      *
      * `A` is a type parameter, so it erases to `Object` and the cast below compiles to nothing at all: it cannot throw
      * here, whatever the map holds. This used to catch a `ClassCastException` around it, which read as a check and was
      * not one -- the exception is raised in the caller, at the point the value is unboxed or used, with nothing left
      * to say which key produced it.
      *
      * A real check would need a [[scala.reflect.ClassTag]] on [[Metadata.Key]], which is a wider change than the
      * guarantee is worth: a key is an opaque `String` and two keys of different types sharing one identifier in one
      * namespace is a collision between the libraries that declared them, not something a schema can do by accident.
      * Stating that is more honest than a guard that never runs.
      */
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def get[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Option[A] =
      toSortedMap.get((namespace, key)).map(_.asInstanceOf[A])

    /** The value under the first namespace that has one, so that a format specific attribute can fall back to a format
      * agnostic one.
      */
    def get[A](namespace: Metadata.Namespace, namespaces: Metadata.Namespace*)(key: Metadata.Key[A]): Option[A] =
      namespaces.foldl(get[A](namespace = namespace, key = key)):
        case (None, namespace)     => get[A](namespace = namespace, key = key)
        case (result @ Some(_), _) => result

    /** The two namespace case, which is every format's own attribute falling back to the global one.
      *
      * Spelled out rather than reached through the varargs overload, which allocates a `Seq` and a fold closure per
      * lookup. A field's [[Keys.absence]] and [[Keys.tolerance]] are read once per field per write and twice per field
      * per read, to answer a question that was fixed when the schema was built.
      */
    def get[A](
        namespace: Metadata.Namespace,
        fallback: Metadata.Namespace,
        key: Metadata.Key[A]
    ): Option[A] =
      get[A](namespace = namespace, key = key).orElse(get[A](namespace = fallback, key = key))

    def put[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): Metadata =
      toSortedMap.updated((namespace, key), value)

    def remove[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Metadata =
      toSortedMap.removed((namespace, key))

    def ++(metadata: Metadata): Metadata = toSortedMap ++ metadata.toSortedMap

  val Empty: Metadata = SortedMap.empty

  def one[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): Metadata =
    SortedMap((namespace, key) -> value)

  given Show[Metadata] = _.toSortedMap.map((key, value) => s"$key=$value").mkString("[", ",", "]")
