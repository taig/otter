package io.taig.crock.http

import cats.Eval
import cats.syntax.all.*
import io.taig.crock.schema.Schema

sealed abstract class Segment[A]:
  def isOptional: Boolean
  def name: String
  final def toPath: Path[A] = Path(this)

object Segment:
  final private[crock] case class Static(name: String) extends Segment[Unit]:
    override def isOptional: Boolean = false

  final private[crock] case class Parameter[A](name: String, schema: Eval[Schema.Value[A]]) extends Segment[A]:
    override def isOptional: Boolean = schema.value.isOptional
