package io.taig.otter.http

import cats.syntax.all.*

trait Metadata:
  def name: Option[String]

object Metadata:
  final case class Collection(name: Option[String]) extends Metadata

  object Collection:
    val Default: Metadata.Collection = Collection(name = none)

  final case class Primitive(name: Option[String]) extends Metadata

  object Primitive:
    val Default: Metadata.Primitive = Primitive(name = none)

  final case class Product(name: Option[String]) extends Metadata

  object Product:
    val Default: Metadata.Product = Product(name = none)

  final case class Union(name: Option[String]) extends Metadata

  object Union:
    val Default: Metadata.Union = Union(name = none)

  val Default: Metadata = new Metadata:
    override def name: Option[String] = none
