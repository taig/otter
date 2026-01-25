package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Annotation

object Http:
  final case class Path[A](self: Annotation[Self.Path[Http.Segment, A]]) extends Http.Path.Read[A], Http.Path.Write[A]

  object Path:
    sealed trait Read[+A]:
      def self: Annotation[Self.Path.Read[Http.Segment.Read, A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Path.Write[Http.Segment.Write, A]]

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    override def self: Annotation[Self.Segment[A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[A]]

    sealed abstract class Parameter[A]
        extends Http.Segment[A],
          Http.Segment.Parameter.Read[A],
          Http.Segment.Parameter.Write[A]:
      override def self: Annotation[Self.Segment.Parameter[A]]

    object Parameter:
      sealed trait Read[+A] extends Http.Segment.Read[A]:
        override def self: Annotation[Self.Segment.Parameter.Read[A]]

      sealed trait Write[-A] extends Http.Segment.Write[A]:
        override def self: Annotation[Self.Segment.Parameter.Write[A]]
