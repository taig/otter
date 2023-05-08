package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Value, Violations}
import io.taig.openapi.validation.Constraint
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

// TODO multiple headers with same name (?)
// TODO default (?)
sealed abstract class Header[A]:
  def name: CIString
  def schema: Eval[Value[?]]
  final def optional: Header[Option[A]] = Header.Optional(this)
  final def toHeaders: Headers[A] = Headers(this)
  def decode(values: VectorMap[CIString, String]): Validated[Violations, (VectorMap[CIString, String], A)]
  def encode(a: A): VectorMap[CIString, String]

object Header:
  final private case class Root[A](name: CIString, schema: Eval[Value[A]]) extends Header[A]:
    override def decode(
        values: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], A)] = values
      .get(name)
      .match {
        case Some(value) => schema.value.parse(value).tupleLeft(values.removed(name))
        case None =>
          val actual = OpenApi.fromSeqMap(values.map { case (key, value) => (key.toString, OpenApi.fromString(value)) })
          val violation = Constraint.required.toViolation(actual)
          Violations.rootNec(violation).invalid
      }
      .leftMap(_.modifyHistory(name.toString /: _))
    override def encode(a: A): VectorMap[CIString, String] = VectorMap(name -> schema.value.render(a))

  final private case class Optional[A](header: Header[A]) extends Header[Option[A]]:
    export header.{name, schema}
    override def decode(
        values: VectorMap[CIString, String]
    ): Validated[Violations, (VectorMap[CIString, String], Option[A])] = values.get(name) match
      case Some(_) => header.decode(values).map(_.map(_.some))
      case None    => (values, none[A]).valid
    override def encode(a: Option[A]): VectorMap[CIString, String] = a.fold(VectorMap.empty)(header.encode)

  def apply[A](name: CIString, schema: Eval[Value[A]]): Header[A] = Root(name, schema)
