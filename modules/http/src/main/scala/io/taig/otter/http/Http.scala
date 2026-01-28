package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Annotation
import Self.operation.SegmentOperation

object Http:
  final case class Path[A](self: Annotation[Self.Path[A]]) extends Http.Path.Read[A], Http.Path.Write[A]

  object Path:
    sealed trait Read[+A]:
      def self: Annotation[Self.Path.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Path.Write[A]]

  final case class Query[A](self: Annotation[Self.Query[A]]) extends Http.Query.Read[A], Http.Query.Write[A]

  object Query:
    sealed trait Read[+A]:
      def self: Annotation[Self.Query.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Query.Write[A]]

    export Self.Query.Parameter

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    def self: Annotation[Self.Segment[A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[A]]

    final case class Dynamic[A](self: Annotation[Self.Segment.Dynamic[A]])
        extends Http.Segment[A],
          Http.Segment.Dynamic.Read[A],
          Http.Segment.Dynamic.Write[A]

    object Dynamic:
      sealed trait Read[+A] extends Http.Segment.Read[A]:
        override def self: Annotation[Self.Segment.Dynamic.Read[A]]

      sealed trait Write[-A] extends Http.Segment.Write[A]:
        override def self: Annotation[Self.Segment.Dynamic.Write[A]]

      given SegmentOperation.Dynamic[Http.Segment.Dynamic] = SegmentOperation
        .Dynamic[[a] =>> Annotation[Self.Segment.Dynamic[a]]]
        .imapK([A] => Dynamic(_))([A] => _.self)

    final case class Static[A](self: Annotation[Self.Segment.Static[A]])
        extends Http.Segment[A],
          Http.Segment.Static.Read[A],
          Http.Segment.Static.Write[A]

    object Static:
      sealed trait Read[+A] extends Http.Segment.Read[A]:
        override def self: Annotation[Self.Segment.Static.Read[A]]

      sealed trait Write[-A] extends Http.Segment.Write[A]:
        override def self: Annotation[Self.Segment.Static.Write[A]]

    export Self.Segment.Parameter
