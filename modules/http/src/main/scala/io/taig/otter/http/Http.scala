package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Annotation

object Http:
  type Parameter[A] = Self.Parameter[A]

  object Parameter:
    type Read[+A] = Self.Parameter.Read[A]

    type Write[-A] = Self.Parameter.Write[A]

  final case class Path[A](self: Annotation[Self.Path[A]]) extends Http.Path.Read[A], Http.Path.Write[A]

  object Path:
    sealed trait Read[+A]:
      def self: Annotation[Self.Path.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Path.Write[A]]

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    override def self: Annotation[Self.Segment[A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[A]]
