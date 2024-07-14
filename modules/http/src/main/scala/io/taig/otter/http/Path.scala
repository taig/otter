package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Value

sealed trait Path[+A] extends Product, Serializable:
  def segments: Chain[String | Path.Parameter[?]]
  def parameters: Chain[Path.Parameter[?]]
  final def zip[B](path: Path[B]): Path[(A, B)] = Path.Combine(this, path)

object Path:
  final case class Combine[F[+_], G[+_], A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[String | Path.Parameter[?]] = left.segments ++ right.segments
    override def parameters: Chain[Path.Parameter[?]] = left.parameters ++ right.parameters

  final case class Dynamic[A](segment: Path.Parameter[A]) extends Path[A]:
    override def segments: Chain[String | Path.Parameter[?]] = parameters
    override def parameters: Chain[Path.Parameter[?]] = Chain.one(segment)

  case object Empty extends Path[Unit]:
    override def segments: Chain[Nothing] = Chain.empty
    override def parameters: Chain[Nothing] = Chain.empty

  final case class Static(name: String) extends Path[Unit]:
    override def segments: Chain[String] = Chain.one(name)
    override def parameters: Chain[Nothing] = Chain.empty

  sealed trait Parameter[+A]:
    def name: String
    def schema: Value.Required[String, ?, ?]

  object Parameter:
    final case class Root[A](name: String, schema: Value.Required[String, ?, A]) extends Path.Parameter[A]
