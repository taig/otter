package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Annotation

object Http:
  sealed abstract class Path[A] extends Http.Path.Read[A], Http.Path.Write[A]:
    override def self: Annotation[Self.Path[[a] =>> Annotation[Self.Segment[a]], A]]

  object Path:
    sealed trait Read[+A]:
      def self: Annotation[Self.Path.Read[[a] =>> Annotation[Self.Segment.Read[a]], A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Path.Write[[a] =>> Annotation[Self.Segment.Write[a]], A]]

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    override def self: Annotation[Self.Segment[A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[A]]
