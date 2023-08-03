package io.taig.crock.http

import cats.Eval
import cats.syntax.all.*
import io.taig.crock.schema.Schema

sealed abstract class Segment[A]:
  def name: String
  def matches(segment: String): Boolean
  final def toPath: Path[A] = Path(this)

object Segment:
  final case class Static(name: String) extends Segment[Unit]:
    override def matches(segment: String): Boolean = name === segment

  final case class Parameter[A](name: String, schema: Eval[Schema.Value[A]]) extends Segment[A]:
    override def matches(segment: String): Boolean = true
