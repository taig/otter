package io.taig.otter.http

trait Metadata:
  def name: Option[String]

object Metadata:
  final case class Primitive(name: Option[String]) extends Metadata
