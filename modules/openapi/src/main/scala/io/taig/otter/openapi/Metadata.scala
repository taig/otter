package io.taig.otter.openapi

import cats.syntax.all.*

trait Metadata[+A]:
  def example: Option[A]

  def name: Option[String]

  def imap[A1 >: A, B](f: A1 => B)(g: B => A1): Metadata[B] = ???

object Metadata:
  final case class Collection[+A](example: Option[A], name: Option[String]) extends Metadata[A]:
    override def imap[A1 >: A, B](f: A1 => B)(g: B => A1): Metadata[B] = copy(example = example.imap(f)(g))

  object Collection:
    val Default: Metadata.Collection[Nothing] = Collection(example = none, name = none)

  final case class Primitive[+A](example: Option[A], name: Option[String]) extends Metadata[A]

  object Primitive:
    val Default: Metadata.Primitive[Nothing] = Primitive(example = none, name = none)

  final case class Product[+A](example: Option[A], name: Option[String]) extends Metadata[A]

  object Product:
    val Default: Metadata.Product[Nothing] = Product(example = none, name = none)

  final case class Union[+A](example: Option[A], name: Option[String]) extends Metadata[A]

  object Union:
    val Default: Metadata.Union[Nothing] = Union(example = none, name = none)

  val Default: Metadata[Nothing] = new Metadata:
    override def example: Option[Nothing] = none
    override def name: Option[String] = none
