package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Value

sealed trait Path[+A] extends Product, Serializable:
  def toSegments: Chain[String | Path.Parameter[?]]
  def toParameters: Chain[Path.Parameter[?]]
  final def zip[B](path: Path[B]): Path[(A, B)] = Path.Combine(this, path)

object Path:
  final case class Combine[F[+_], G[+_], A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def toSegments: Chain[String | Path.Parameter[?]] = left.toSegments ++ right.toSegments
    override def toParameters: Chain[Path.Parameter[?]] = left.toParameters ++ right.toParameters

  final case class Dynamic[A](segment: Path.Parameter[A]) extends Path[A]:
    override def toSegments: Chain[String | Path.Parameter[?]] = toParameters
    override def toParameters: Chain[Path.Parameter[?]] = Chain.one(segment)

  case object Empty extends Path[Unit]:
    override def toSegments: Chain[Nothing] = Chain.empty
    override def toParameters: Chain[Nothing] = Chain.empty

  final case class Static(name: String) extends Path[Unit]:
    override def toSegments: Chain[String] = Chain.one(name)
    override def toParameters: Chain[Nothing] = Chain.empty

  sealed trait Parameter[+A]:
    def name: String
    def schema: Value.Required.Reader[String, ?, ?]

  object Parameter:
    final case class Root[A](name: String, schema: Value.Required.Reader[String, ?, A]) extends Path.Parameter[A]
