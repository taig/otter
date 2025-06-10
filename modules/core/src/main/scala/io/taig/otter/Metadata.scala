package io.taig.otter

import cats.Show
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

final class Metadata(val toMap: SortedMap[String, Any]) extends AnyVal:
  override def toString: String = toMap.map { (key, value) => s"$key=$value" }.mkString("[", ",", "]")

object Metadata:
  sealed abstract class Key[A] extends Product with Serializable:
    def name: String

    final def imap[B](f: A => B)(g: B => A): Metadata.Key[B] = Key.Modify(self = this, f, g)

    def decode(values: SortedMap[String, Any]): Either[Throwable, Option[A]]

    def encode(values: SortedMap[String, Any])(a: A): SortedMap[String, Any]

  object Key:
    final private[otter] case class Root[A](name: String) extends Metadata.Key[A]:
      @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
      override def decode(values: SortedMap[String, Any]): Either[Throwable, Option[A]] =
        Either.catchOnly[ClassCastException](values.get(name).asInstanceOf[Option[A]])

      override def encode(values: SortedMap[String, Any])(a: A): SortedMap[String, Any] =
        values.updated(name, a)

    final private[otter] case class Modify[A, B](self: Metadata.Key[A], f: A => B, g: B => A) extends Metadata.Key[B]:
      export self.name

      override def decode(values: SortedMap[String, Any]): Either[Throwable, Option[B]] =
        self.decode(values).map(_.map(f))

      override def encode(values: SortedMap[String, Any])(b: B): SortedMap[String, Any] = self.encode(values)(g(b))

    inline def apply[A](name: String): Metadata.Key[A] = Root(name)

  extension (self: Metadata)
    inline def contains[A](key: Metadata.Key[A]): Boolean = self.toMap.contains(key.name)
    inline def get[A](key: Metadata.Key[A]): Option[A] = key.decode(self.toMap).toOption.flatten
    inline def put[A](key: Metadata.Key[A], value: A): Metadata = ??? // Metadata(self.toMap.updated(key.name, value))
    inline def remove[A](key: Metadata.Key[A]): Metadata = Metadata(self.toMap.removed(key.name))

  val Empty: Metadata = Metadata(SortedMap.empty)

  def one[A](key: Metadata.Key[A], value: A): Metadata = Metadata(SortedMap(key.name -> value))

  given Show[Metadata] = Show.fromToString
