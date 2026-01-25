package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.operation.EnumerationOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.syntax.all.*
import io.taig.otter as Self
import Self.operation.ConstantOperation
import Self.operation.UnionOperation
import Self.http.operation.SegmentOperation

sealed abstract class Segment[A] extends Segment.Read[A], Segment.Write[A]

object Segment:
  sealed trait Read[+A]

  sealed trait Write[-A]

  sealed abstract class Parameter[A] extends Segment[A], Segment.Parameter.Read[A], Segment.Parameter.Write[A]:
    override def schema: Reference[Segment.Value, ?]

  object Parameter:
    sealed trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Parameter.Read[B] = Read.Modify(self = this, f)

      def name: String

      def schema: Reference[Segment.Value.Read, ?]

    object Read:
      final case class Modify[A, B](self: Segment.Parameter.Read[A], f: A => B) extends Segment.Parameter.Read[B]:
        export self.{name, schema}

      final case class Root[A](name: String, schema: Reference[Segment.Value.Read, A]) extends Segment.Parameter.Read[A]

      given Functor[Segment.Parameter.Read]:
        override def map[A, B](segment: Segment.Parameter.Read[A])(f: A => B): Segment.Parameter.Read[B] =
          segment.map(f)

      given SegmentOperation.Parameter.Read[Segment.Parameter.Read] = ???

    sealed trait Write[-A] extends Segment.Write[A]:
      final def contramap[B](f: B => A): Segment.Parameter.Write[B] = Write.Modify(self = this, f)

      def name: String

      def schema: Reference[Segment.Value.Write, ?]

    object Write:
      final case class Modify[A, B](self: Segment.Parameter.Write[A], f: B => A) extends Segment.Parameter.Write[B]:
        export self.{name, schema}

      final case class Root[A](name: String, schema: Reference[Segment.Value.Write, A])
          extends Segment.Parameter.Write[A]

      given Contravariant[Segment.Parameter.Write]:
        override def contramap[A, B](segment: Segment.Parameter.Write[A])(f: B => A): Segment.Parameter.Write[B] =
          segment.contramap(f)

    final case class Modify[A, B](self: Segment.Parameter[A], f: A => B, g: B => A) extends Segment.Parameter[B]:
      export self.{name, schema}

    final case class Root[A](name: String, schema: Reference[Segment.Value, A]) extends Segment.Parameter[A]

    given Invariant[Segment.Parameter]:
      override def imap[A, B](self: Segment.Parameter[A])(f: A => B)(g: B => A): Segment.Parameter[B] =
        Modify(self, f, g)

  sealed abstract class Static[A] extends Segment[A], Segment.Static.Read[A], Segment.Static.Write[A]

  object Static:
    sealed trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Static.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Segment.Static.Read[A], f: A => B) extends Segment.Static.Read[B]

      given Functor[Segment.Static.Read]:
        override def map[A, B](segment: Segment.Static.Read[A])(f: A => B): Segment.Static.Read[B] =
          segment.map(f)

    sealed trait Write[-A] extends Segment.Write[A]:
      final def contramap[B](f: B => A): Segment.Static.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Segment.Static.Write[A], f: B => A) extends Segment.Static.Write[B]

      given Contravariant[Segment.Static.Write]:
        override def contramap[A, B](segment: Segment.Static.Write[A])(f: B => A): Segment.Static.Write[B] =
          segment.contramap(f)

    final case class Modify[A, B](self: Segment.Static[A], f: A => B, g: B => A) extends Segment.Static[B]

    final case class Root(name: String) extends Segment.Static[Unit]

    given Invariant[Segment.Static]:
      override def imap[A, B](self: Segment.Static[A])(f: A => B)(g: B => A): Segment.Static[B] = Modify(self, f, g)

  sealed abstract class Value[A] extends Segment.Value.Read[A], Segment.Value.Write[A]:
    override def self: Annotation[
      Self.Constant[Segment.Value.Primitive.Text, A] | Self.Enumeration[Segment.Value.Primitive.Text, A] |
        Self.Primitive.Text[A] | Self.Union[Segment.Value.Branch, A]
    ]

  object Value:
    sealed trait Read[+A]:
      def self: Annotation[
        Self.Constant.Read[Segment.Value.Primitive.Text.Read, A] |
          Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, A] | Self.Primitive.Text.Read[A] |
          Self.Union.Read[Segment.Value.Branch.Read, A]
      ]

    sealed trait Write[-A]:
      def self: Annotation[
        Self.Constant.Write[Segment.Value.Primitive.Text.Write, A] |
          Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, A] | Self.Primitive.Text.Write[A] |
          Self.Union.Write[Segment.Value.Branch.Write, A]
      ]

    final case class Constant[A](self: Annotation[Self.Constant[Segment.Value.Primitive.Text, A]])
        extends Segment.Value[A],
          Segment.Value.Constant.Read[A],
          Segment.Value.Constant.Write[A]

    object Constant:
      sealed trait Read[+A] extends Segment.Value.Read[A]:
        def self: Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]]
        ): Segment.Value.Constant.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]] = annotation

        given Functor[Segment.Value.Constant.Read] =
          Functor[[a] =>> Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Value.Constant.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Value.Constant.Read[A]] =
          Annotated[Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

        given ConstantOperation.Read[Segment.Value.Constant.Read, Segment.Value.Primitive.Text.Read] = ConstantOperation
          .Read[
            [a] =>> Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, a]],
            Segment.Value.Primitive.Text.Read
          ]
          .imapK([A] => (self: Annotation[Self.Constant.Read[Segment.Value.Primitive.Text.Read, A]]) => Read(self))(
            [A] => (parameter: Segment.Value.Constant.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Segment.Value.Write[A]:
        def self: Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]]
        ): Segment.Value.Constant.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]] = annotation

        given Contravariant[Segment.Value.Constant.Write] =
          Contravariant[[a] =>> Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]]) => Write(annotation)
          )([A] => (parameter: Segment.Value.Constant.Write[A]) => parameter.self)

        given [A] => Annotated[Segment.Value.Constant.Write[A]] =
          Annotated[Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]]].imap(Write.apply)(_.self)

        given ConstantOperation.Write[Segment.Value.Constant.Write, Segment.Value.Primitive.Text.Write] =
          ConstantOperation
            .Write[
              [a] =>> Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, a]],
              Segment.Value.Primitive.Text.Write
            ]
            .imapK([A] =>
              (annotation: Annotation[Self.Constant.Write[Segment.Value.Primitive.Text.Write, A]]) => Write(annotation)
            )([A] => (parameter: Segment.Value.Constant.Write[A]) => parameter.self)

      given Invariant[Segment.Value.Constant] =
        Invariant[[a] =>> Annotation[Self.Constant[Segment.Value.Primitive.Text, a]]].imapK([A] =>
          (self: Annotation[Self.Constant[Segment.Value.Primitive.Text, A]]) => Constant(self)
        )([A] => (parameter: Segment.Value.Constant[A]) => parameter.self)

      given [A] => Annotated[Segment.Value.Constant[A]] =
        Annotated[Annotation[Self.Constant[Segment.Value.Primitive.Text, A]]].imap(Constant.apply)(_.self)

      given ConstantOperation[Segment.Value.Constant, Segment.Value.Primitive.Text] =
        ConstantOperation[
          [a] =>> Annotation[Self.Constant[Segment.Value.Primitive.Text, a]],
          Segment.Value.Primitive.Text
        ].imapK([A] => Constant(_))([A] => _.self)

    sealed abstract class Enumeration[A]
        extends Segment.Value[A],
          Segment.Value.Enumeration.Read[A],
          Segment.Value.Enumeration.Write[A]:
      override def self: Annotation[Self.Enumeration[Segment.Value.Primitive.Text, A]]

    object Enumeration:
      sealed trait Read[+A] extends Segment.Value.Read[A]:
        override def self: Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, A]]
        ): Segment.Value.Enumeration.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, A]] = annotation

        given Functor[Segment.Value.Enumeration.Read] =
          Functor[[a] =>> Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Value.Enumeration.Read[A]) => parameter.self)

        given EnumerationOperation.Read[Segment.Value.Enumeration.Read, Segment.Value.Primitive.Text.Read] =
          EnumerationOperation
            .Read[
              [a] =>> Annotation[Self.Enumeration.Read[Segment.Value.Primitive.Text.Read, a]],
              Segment.Value.Primitive.Text.Read
            ]
            .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends Segment.Value.Write[A]:
        override def self: Annotation[Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, A]]
        ): Segment.Value.Enumeration.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, A]] = annotation

        given Contravariant[Segment.Value.Enumeration.Write] =
          Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given EnumerationOperation.Write[Segment.Value.Enumeration.Write, Segment.Value.Primitive.Text.Write] =
          EnumerationOperation
            .Write[
              [a] =>> Annotation[Self.Enumeration.Write[Segment.Value.Primitive.Text.Write, a]],
              Segment.Value.Primitive.Text.Write
            ]
            .imapK([A] => Write(_))([A] => _.self)

      def apply[A](
          annotation: Annotation[Self.Enumeration[Segment.Value.Primitive.Text, A]]
      ): Segment.Value.Enumeration[A] = new Enumeration[A]:
        override def self: Self.Annotation[Self.Enumeration[Segment.Value.Primitive.Text, A]] = annotation

      given Invariant[Segment.Value.Enumeration] =
        Invariant[[a] =>> Annotation[Self.Enumeration[Segment.Value.Primitive.Text, a]]]
          .imapK([A] => Enumeration(_))([A] => _.self)

      given EnumerationOperation[Segment.Value.Enumeration, Segment.Value.Primitive.Text] =
        EnumerationOperation[
          [a] =>> Annotation[Self.Enumeration[Segment.Value.Primitive.Text, a]],
          Segment.Value.Primitive.Text
        ].imapK([A] => Enumeration(_))([A] => _.self)

    sealed abstract class Primitive[A]
        extends Segment.Value[A],
          Segment.Value.Primitive.Read[A],
          Segment.Value.Primitive.Write[A]

    object Primitive:
      sealed trait Read[+A] extends Segment.Value.Read[A]

      sealed trait Write[-A] extends Segment.Value.Write[A]

      final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
          extends Segment.Value.Primitive.Boolean.Read[A],
            Segment.Value.Primitive.Boolean.Write[A]

      object Boolean:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Boolean.Read[A]]

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Boolean.Write[A]]

      final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
          extends Segment.Value.Primitive.Number.Read[A],
            Segment.Value.Primitive.Number.Write[A]

      object Number:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Number.Read[A]]

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Number.Write[A]]

      final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
          extends Segment.Value.Primitive[A],
            Segment.Value.Primitive.Text.Read[A],
            Segment.Value.Primitive.Text.Write[A]

      object Text:
        sealed trait Read[+A] extends Segment.Value.Read[A]:
          def self: Annotation[Self.Primitive.Text.Read[A]]

        object Read:
          def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Segment.Value.Primitive.Text.Read[A] =
            new Read[A]:
              override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

          given Functor[Segment.Value.Primitive.Text.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Segment.Value.Primitive.Text.Read[A]] =
            Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Text.Read[Segment.Value.Primitive.Text.Read] = PrimitiveOperation.Text
            .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A] extends Segment.Value.Write[A]:
          def self: Annotation[Self.Primitive.Text.Write[A]]

        object Write:
          def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Segment.Value.Primitive.Text.Write[A] =
            new Write[A]:
              override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

          given Contravariant[Segment.Value.Primitive.Text.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Segment.Value.Primitive.Text.Write[A]] =
            Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Text.Write[Segment.Value.Primitive.Text.Write] = PrimitiveOperation.Text
            .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Segment.Value.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

        given [A] => Annotated[Segment.Value.Primitive.Text[A]] =
          Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

        given PrimitiveOperation.Text[Segment.Value.Primitive.Text] = PrimitiveOperation
          .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

    final case class Union[A](self: Annotation[Self.Union[Segment.Value.Branch, A]])
        extends Segment.Value[A],
          Segment.Value.Union.Read[A],
          Segment.Value.Union.Write[A]

    object Union:
      sealed trait Read[+A] extends Segment.Value.Read[A]:
        def self: Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]]
        ): Segment.Value.Union.Read[A] =
          new Read[A]:
            override def self: Self.Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]] = annotation

        given Functor[Segment.Value.Union.Read] =
          Functor[[a] =>> Annotation[Self.Union.Read[Segment.Value.Branch.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Value.Union.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Value.Union.Read[A]] =
          Annotated[Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]]].imap(Read.apply)(_.self)

        given UnionOperation.Read[Segment.Value.Union.Read, Segment.Value.Branch.Read] = UnionOperation
          .Read[
            [a] =>> Annotation[Self.Union.Read[Segment.Value.Branch.Read, a]],
            Segment.Value.Branch.Read
          ]
          .imapK([A] => (self: Annotation[Self.Union.Read[Segment.Value.Branch.Read, A]]) => Read(self))([A] =>
            (parameter: Segment.Value.Union.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Segment.Value.Write[A]:
        def self: Annotation[Self.Union.Write[Segment.Value.Branch.Write, A]]

    final case class Branch[A](self: Annotation[Self.Branch[Segment.Value, A]])
        extends Segment.Value.Branch.Read[A],
          Segment.Value.Branch.Write[A]

    object Branch:
      sealed trait Read[+A]:
        def self: Annotation[Self.Branch.Read[Segment.Value.Read, A]]

      sealed trait Write[-A]:
        def self: Annotation[Self.Branch.Write[Segment.Value.Write, A]]
