package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Violations, Void}

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  final def decode(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Option[Validated[Violations, A]] = decodeWithRemainders(path, queries).map(_.map(_._3))

  def decodeWithRemainders(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Option[Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], A)]]

  def encode(a: A): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive])

object Url:
  val Root: Url[Void] = new Url[Void]:
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Option[Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], Void)]] =
      Option.when(path.isEmpty)((path, queries, Void).valid)
    override def encode(a: Void): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      (Chain.empty, VectorMap.empty)

  final private case class SegmentProduct[A, B](url: Url[A], segment: Segment[B]) extends Url[(A, B)] {
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Option[Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], (A, B))]] =
      url.decodeWithRemainders(path, queries) match {
        case Some(Validated.Valid((path, queries, a))) => ???
        case Some(Validated.Invalid(violations))       => ???
        case None                                      => None
      }

    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      val (segments, queries) = url.encode(ab._1)
      (segments :+ segment.encode(ab._2), queries)
  }
