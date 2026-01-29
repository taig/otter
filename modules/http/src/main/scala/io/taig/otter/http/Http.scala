package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter.http as Self
import io.taig.otter.Annotation
import io.taig.otter.http.operation.SegmentOperation
import cats.Functor
import io.taig.otter.Annotated
import io.taig.otter.http.operation.QueryOperation
import cats.Contravariant
import cats.Invariant

object Http:
  final case class Path[A](self: Annotation[Self.Path[A]]) extends Http.Path.Read[A], Http.Path.Write[A]

  object Path:
    sealed trait Read[+A]:
      def self: Annotation[Self.Path.Read[A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Path.Write[A]]

  final case class Query[A](self: Annotation[Self.Query[Http.Query.Parameter, A]])
      extends Http.Query.Read[A],
        Http.Query.Write[A]

  object Query:
    sealed trait Read[+A]:
      def self: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]) = new Read[A]:
        override def self: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]] = annotation

      given Functor[Http.Query.Read] = Functor[[a] =>> Annotation[Self.Query.Read[Http.Query.Parameter.Read, a]]]
        .imapK([A] => Read(_))([A] => _.self)

      given [A] => Annotated[Http.Query.Read[A]] =
        Annotated[Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]].imap(Read.apply)(_.self)

      given QueryOperation.Read[Http.Query.Read, Http.Query.Parameter.Read] = QueryOperation
        .Read[[a] =>> Annotation[Self.Query.Read[Http.Query.Parameter.Read, a]], Http.Query.Parameter.Read]
        .imapK([A] => Read(_))([A] => _.self)

    sealed trait Write[-A]:
      def self: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]) = new Write[A]:
        override def self: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]] = annotation

      given Contravariant[Http.Query.Write] =
        Contravariant[[a] =>> Annotation[Self.Query.Write[Http.Query.Parameter.Write, a]]]
          .imapK([A] => Write(_))([A] => _.self)

      given [A] => Annotated[Http.Query.Write[A]] =
        Annotated[Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]].imap(Write.apply)(_.self)

      given QueryOperation.Write[Http.Query.Write, Http.Query.Parameter.Write] = QueryOperation
        .Write[[a] =>> Annotation[Self.Query.Write[Http.Query.Parameter.Write, a]], Http.Query.Parameter.Write]
        .imapK([A] => Write(_))([A] => _.self)

    given Invariant[Query] = Invariant[[a] =>> Annotation[Self.Query[Http.Query.Parameter, a]]]
      .imapK([A] => Query(_))([A] => _.self)

    given [A] => Annotated[Http.Query[A]] =
      Annotated[Annotation[Self.Query[Http.Query.Parameter, A]]].imap(Query.apply)(_.self)

    given QueryOperation[Http.Query, Http.Query.Parameter] = QueryOperation[
      [a] =>> Annotation[Self.Query[Http.Query.Parameter, a]],
      Http.Query.Parameter
    ].imapK([A] => Query(_))([A] => _.self)

    export Self.QueryParameter as Parameter

  sealed abstract class Segment[A] extends Http.Segment.Read[A], Http.Segment.Write[A]:
    def self: Annotation[Self.Segment[Http.Segment.Parameter, A]]

  object Segment:
    sealed trait Read[+A]:
      def self: Annotation[Self.Segment.Read[Http.Segment.Parameter.Read, A]]

    sealed trait Write[-A]:
      def self: Annotation[Self.Segment.Write[Http.Segment.Parameter.Write, A]]

    final case class Dynamic[A](self: Annotation[Self.Segment.Dynamic[Http.Segment.Parameter, A]])
        extends Http.Segment[A],
          Http.Segment.Dynamic.Read[A],
          Http.Segment.Dynamic.Write[A]

    object Dynamic:
      sealed trait Read[+A] extends Http.Segment.Read[A]:
        override def self: Annotation[Self.Segment.Dynamic.Read[Http.Segment.Parameter.Read, A]]

      sealed trait Write[-A] extends Http.Segment.Write[A]:
        override def self: Annotation[Self.Segment.Dynamic.Write[Http.Segment.Parameter.Write, A]]

      given SegmentOperation.Dynamic[Http.Segment.Dynamic, Http.Segment.Parameter] = SegmentOperation
        .Dynamic[
          [a] =>> Annotation[Self.Segment.Dynamic[Http.Segment.Parameter, a]],
          Http.Segment.Parameter
        ]
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

    export Self.SegmentParameter as Parameter
