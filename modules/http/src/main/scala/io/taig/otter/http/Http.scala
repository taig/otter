package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Annotation
import Self.operation.PathOperation

object Http:
  type Parameter[A] = Self.Parameter[A]

  object Parameter:
    type Read[+A] = Self.Parameter.Read[A]

    type Write[-A] = Self.Parameter.Write[A]

  type Path[A] = Annotation[Self.Path[Http.Segment, A]]
  // final case class Path[A](self: Annotation[Self.Path[Http.Segment, A]]) extends Http.Path.Read[A], Http.Path.Write[A]

  // object Path:
  //   sealed trait Read[+A]:
  //     def self: Annotation[Self.Path.Read[Http.Segment.Read, A]]

  //   sealed trait Write[-A]:
  //     def self: Annotation[Self.Path.Write[Http.Segment.Write, A]]

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    override def self: Annotation[Self.Segment[A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[A]]

object Playground:
  val x: Http.Path[String] = ???
  // Annotation.test[PathOperation, Http.Path].todo
  summon[PathOperation[Http.Path]].todo
  // x.todo