//package io.taig.otter.http
//
//import cats.Eval
//import cats.syntax.all.*
//import io.taig.otter.schema.Schema
//
//sealed abstract class Segment[A]:
//  def isOptional: Boolean
//  def name: String
//  final def toPath: Path[A] = Path(this)
//
//object Segment:
//  final private[otter] case class Static(name: String) extends Segment[Unit]:
//    override def isOptional: Boolean = false
//
//  final private[otter] case class Parameter[A](name: String, schema: Eval[Schema.Value[A]]) extends Segment[A]:
//    override def isOptional: Boolean = schema.value.isOptional
