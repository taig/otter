package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean
  final def decode(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Validated[Violations, A] = decodeWithRemainders(path, queries).map(_._3)
  def decodeWithRemainders(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], A)]
  def encode(a: A): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive])

object Url:
  val Root: Url[Void] = new Url[Void]:
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      path.isEmpty
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], Void)] =
      Validated.cond(
        matches(path, queries),
        (path, queries, Void),
        Violations.rootNec(
          Constraint.text
            .equal("/")
            .toViolation(path.map(_.render).mkString_("/"))
            .mapReference(OpenApi.fromString)
            .mapActual(OpenApi.fromString)
        )
      )
    override def encode(a: Void): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      (Chain.empty, VectorMap.empty)

  final private case class SegmentProduct[A, B](url: Url[A], segment: Segment[B]) extends Url[(A, B)] {
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      url.matches(path, queries) && false // TODO
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], (A, B))] = ???
    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      val (segments, queries) = url.encode(ab._1)
      (segments :+ segment.encode(ab._2), queries)
  }
