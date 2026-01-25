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

type Segment[A] = Segment.Read[A] & Segment.Write[A]

object Segment:
  sealed trait Read[+A]

  sealed trait Write[-A]

  type Parameter[A] = Segment.Parameter.Read[A] & Segment.Parameter.Write[A]

  object Parameter:
    sealed trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Parameter.Read[B] = Read.Modify(self = this, f)

      def name: String

      def schema: Reference[Segment.Parameter.Value.Read, ?]

    object Read:
      final case class Modify[A, B](self: Segment.Parameter.Read[A], f: A => B) extends Segment.Parameter.Read[B]:
        export self.{name, schema}

      final case class Root[A](name: String, schema: Reference[Segment.Parameter.Value.Read, A])
          extends Segment.Parameter.Read[A]

      given Functor[Segment.Parameter.Read]:
        override def map[A, B](segment: Segment.Parameter.Read[A])(f: A => B): Segment.Parameter.Read[B] =
          segment.map(f)

      given SegmentOperation.Parameter.Read[Segment.Parameter.Read] = ???

    sealed trait Write[-A] extends Segment.Write[A]:
      final def contramap[B](f: B => A): Segment.Parameter.Write[B] = Write.Modify(self = this, f)

      def name: String

      def schema: Reference[Segment.Parameter.Value.Write, ?]

    object Write:
      final case class Modify[A, B](self: Segment.Parameter.Write[A], f: B => A) extends Segment.Parameter.Write[B]:
        export self.{name, schema}

      final case class Root[A](name: String, schema: Reference[Segment.Parameter.Value.Write, A])
          extends Segment.Parameter.Write[A]

      given Contravariant[Segment.Parameter.Write]:
        override def contramap[A, B](segment: Segment.Parameter.Write[A])(f: B => A): Segment.Parameter.Write[B] =
          segment.contramap(f)

    final case class Modify[A, B](self: Segment.Parameter[A], f: A => B, g: B => A)
        extends Segment.Parameter.Read[B],
          Segment.Parameter.Write[B]:
      export self.{name, schema}

    final case class Root[A](name: String, schema: Reference[Segment.Parameter.Value, A])
        extends Segment.Parameter.Read[A],
          Segment.Parameter.Write[A]

    given Invariant[Segment.Parameter]:
      override def imap[A, B](self: Segment.Parameter[A])(f: A => B)(g: B => A): Segment.Parameter[B] =
        Modify(self, f, g)

    sealed abstract class Value[A] extends Value.Read[A], Value.Write[A]:
      override def self: Annotation[
        Self.Constant[Value.Primitive.Text, A] | Self.Enumeration[Value.Primitive.Text, A] |
          Self.Primitive.Coerce[
            [a] =>> Value.Primitive.Boolean[a] | Value.Primitive.Number[a] | Value.Primitive.Text[a],
            A
          ] | Self.Primitive.Text[A] | Self.Union[Value.Branch, A]
      ]

    object Value:
      sealed trait Read[+A]:
        def self: Annotation[
          Self.Constant.Read[Value.Primitive.Text.Read, A] | Self.Enumeration.Read[Value.Primitive.Text.Read, A] |
            Self.Primitive.Coerce.Read[
              [a] =>> Value.Primitive.Boolean.Read[a] | Value.Primitive.Number.Read[a] | Value.Primitive.Text.Read[a],
              A
            ] | Self.Primitive.Text.Read[A] | Self.Union.Read[Value.Branch.Read, A]
        ]

      sealed trait Write[-A]:
        def self: Annotation[
          Self.Constant.Write[Value.Primitive.Text.Write, A] | Self.Enumeration.Write[Value.Primitive.Text.Write, A] |
            Self.Primitive.Coerce.Write[
              [a] =>> Value.Primitive.Boolean.Write[a] | Value.Primitive.Number.Write[a] |
                Value.Primitive.Text.Write[a],
              A
            ] | Self.Primitive.Text.Write[A] | Self.Union.Write[Value.Branch.Write, A]
        ]

      final case class Constant[A](self: Annotation[Self.Constant[Value.Primitive.Text, A]])
          extends Value[A],
            Value.Constant.Read[A],
            Value.Constant.Write[A]

      object Constant:
        sealed trait Read[+A] extends Value.Read[A]:
          def self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]
          ): Value.Constant.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]] = annotation

          given Functor[Value.Constant.Read] =
            Functor[[a] =>> Annotation[Self.Constant.Read[Value.Primitive.Text.Read, a]]].imapK([A] =>
              (self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]) => Read(self)
            )([A] => (parameter: Value.Constant.Read[A]) => parameter.self)

          given [A] => Annotated[Value.Constant.Read[A]] =
            Annotated[Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

          given ConstantOperation.Read[Value.Constant.Read, Value.Primitive.Text.Read] = ConstantOperation
            .Read[
              [a] =>> Annotation[Self.Constant.Read[Value.Primitive.Text.Read, a]],
              Value.Primitive.Text.Read
            ]
            .imapK([A] => (self: Annotation[Self.Constant.Read[Value.Primitive.Text.Read, A]]) => Read(self))([A] =>
              (parameter: Value.Constant.Read[A]) => parameter.self
            )

        sealed trait Write[-A] extends Value.Write[A]:
          def self: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]
          ): Value.Constant.Write[A] =
            new Write[A]:
              override def self: Self.Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]] = annotation

          given Contravariant[Value.Constant.Write] =
            Contravariant[[a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]]].imapK([A] =>
              (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
            )([A] => (parameter: Value.Constant.Write[A]) => parameter.self)

          given [A] => Annotated[Value.Constant.Write[A]] =
            Annotated[Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]].imap(Write.apply)(_.self)

          given ConstantOperation.Write[Value.Constant.Write, Value.Primitive.Text.Write] = ConstantOperation
            .Write[
              [a] =>> Annotation[Self.Constant.Write[Value.Primitive.Text.Write, a]],
              Value.Primitive.Text.Write
            ]
            .imapK([A] =>
              (annotation: Annotation[Self.Constant.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
            )([A] => (parameter: Value.Constant.Write[A]) => parameter.self)

        given Invariant[Value.Constant] =
          Invariant[[a] =>> Annotation[Self.Constant[Value.Primitive.Text, a]]].imapK([A] =>
            (self: Annotation[Self.Constant[Value.Primitive.Text, A]]) => Constant(self)
          )([A] => (parameter: Value.Constant[A]) => parameter.self)

        given [A] => Annotated[Value.Constant[A]] =
          Annotated[Annotation[Self.Constant[Value.Primitive.Text, A]]].imap(Constant.apply)(_.self)

        given ConstantOperation[Value.Constant, Value.Primitive.Text] =
          ConstantOperation[[a] =>> Annotation[Self.Constant[Value.Primitive.Text, a]], Value.Primitive.Text].imapK(
            [A] => (self: Annotation[Self.Constant[Value.Primitive.Text, A]]) => Constant(self)
          )([A] => (parameter: Value.Constant[A]) => parameter.self)

      sealed abstract class Enumeration[A] extends Value[A], Value.Enumeration.Read[A], Value.Enumeration.Write[A]:
        override def self: Annotation[Self.Enumeration[Value.Primitive.Text, A]]

      object Enumeration:
        sealed trait Read[+A] extends Value.Read[A]:
          override def self: Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, A]]
          ): Value.Enumeration.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, A]] = annotation

          given Functor[Value.Enumeration.Read] =
            Functor[[a] =>> Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, a]]].imapK([A] =>
              (self: Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, A]]) => Read(self)
            )([A] => (parameter: Value.Enumeration.Read[A]) => parameter.self)

          given EnumerationOperation.Read[Value.Enumeration.Read, Value.Primitive.Text.Read] =
            EnumerationOperation
              .Read[[a] =>> Annotation[
                Self.Enumeration.Read[Value.Primitive.Text.Read, a]
              ], Value.Primitive.Text.Read]
              .imapK([A] => (self: Annotation[Self.Enumeration.Read[Value.Primitive.Text.Read, A]]) => Read(self))(
                [A] => (parameter: Value.Enumeration.Read[A]) => parameter.self
              )

        sealed trait Write[-A] extends Value.Write[A]:
          override def self: Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, A]]
          ): Value.Enumeration.Write[A] = new Write[A]:
            override def self: Self.Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, A]] = annotation

          given Contravariant[Value.Enumeration.Write] =
            Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, a]]].imapK([A] =>
              (annotation: Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
            )([A] => (parameter: Value.Enumeration.Write[A]) => parameter.self)

          given EnumerationOperation.Write[Value.Enumeration.Write, Value.Primitive.Text.Write] =
            EnumerationOperation
              .Write[
                [a] =>> Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, a]],
                Value.Primitive.Text.Write
              ]
              .imapK([A] =>
                (annotation: Annotation[Self.Enumeration.Write[Value.Primitive.Text.Write, A]]) => Write(annotation)
              )([A] => (parameter: Value.Enumeration.Write[A]) => parameter.self)

        def apply[A](annotation: Annotation[Self.Enumeration[Value.Primitive.Text, A]]): Value.Enumeration[A] =
          new Enumeration[A]:
            override def self: Self.Annotation[Self.Enumeration[Value.Primitive.Text, A]] = annotation

        given Invariant[Value.Enumeration] =
          Invariant[[a] =>> Annotation[Self.Enumeration[Value.Primitive.Text, a]]].imapK([A] =>
            (annotation: Annotation[Self.Enumeration[Value.Primitive.Text, A]]) => Enumeration(annotation)
          )([A] => (parameter: Value.Enumeration[A]) => parameter.self)

        given EnumerationOperation[Value.Enumeration, Value.Primitive.Text] =
          EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Value.Primitive.Text, a]], Value.Primitive.Text]
            .imapK([A] =>
              (annotation: Annotation[Self.Enumeration[Value.Primitive.Text, A]]) => Enumeration(annotation)
            )([A] => (parameter: Value.Enumeration[A]) => parameter.self)

      sealed abstract class Primitive[A] extends Value[A], Value.Primitive.Read[A], Value.Primitive.Write[A]

      object Primitive:
        sealed trait Read[+A] extends Value.Read[A]

        sealed trait Write[-A] extends Value.Write[A]

        final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
            extends Value.Primitive.Boolean.Read[A],
              Value.Primitive.Boolean.Write[A]

        object Boolean:
          sealed trait Read[+A]:
            def self: Annotation[Self.Primitive.Boolean.Read[A]]

          sealed trait Write[-A]:
            def self: Annotation[Self.Primitive.Boolean.Write[A]]

        final case class Coerce[A](self: Annotation[Self.Primitive.Coerce.Text[Value.Primitive.Text, A]])
            extends Value.Primitive[A],
              Value.Primitive.Coerce.Read[A],
              Value.Primitive.Coerce.Write[A]

        object Coerce:
          sealed trait Read[+A] extends Value.Primitive.Read[A]:
            def self: Annotation[Self.Primitive.Coerce.Text.Read[Value.Primitive.Text.Read, A]]

          sealed trait Write[-A] extends Value.Primitive.Write[A]:
            def self: Annotation[Self.Primitive.Coerce.Text.Write[Value.Primitive.Text.Write, A]]

        final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
            extends Value.Primitive.Number.Read[A],
              Value.Primitive.Number.Write[A]

        object Number:
          sealed trait Read[+A]:
            def self: Annotation[Self.Primitive.Number.Read[A]]

          sealed trait Write[-A]:
            def self: Annotation[Self.Primitive.Number.Write[A]]

        final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
            extends Value.Primitive[A],
              Value.Primitive.Text.Read[A],
              Value.Primitive.Text.Write[A]

        object Text:
          sealed trait Read[+A] extends Value.Read[A]:
            def self: Annotation[Self.Primitive.Text.Read[A]]

          object Read:
            def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Value.Primitive.Text.Read[A] =
              new Read[A]:
                override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

            given Functor[Value.Primitive.Text.Read] =
              Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] =>
                (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self)
              )([A] => (parameter: Value.Primitive.Text.Read[A]) => parameter.self)

            given [A] => Annotated[Value.Primitive.Text.Read[A]] =
              Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

            given PrimitiveOperation.Text.Read[Value.Primitive.Text.Read] =
              PrimitiveOperation.Text
                .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
                .imapK([A] => (self: Annotation[Self.Primitive.Text.Read[A]]) => Read(self))([A] =>
                  (parameter: Value.Primitive.Text.Read[A]) => parameter.self
                )

          sealed trait Write[-A] extends Value.Write[A]:
            def self: Annotation[Self.Primitive.Text.Write[A]]

          object Write:
            def apply[A](annotation: Annotation[Self.Primitive.Text.Write[A]]): Value.Primitive.Text.Write[A] =
              new Write[A]:
                override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

            given Contravariant[Value.Primitive.Text.Write] =
              Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] =>
                (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self)
              )([A] => (parameter: Value.Primitive.Text.Write[A]) => parameter.self)

            given [A] => Annotated[Value.Primitive.Text.Write[A]] =
              Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

            given PrimitiveOperation.Text.Write[Value.Primitive.Text.Write] = PrimitiveOperation.Text
              .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
              .imapK([A] => (self: Annotation[Self.Primitive.Text.Write[A]]) => Write(self))([A] =>
                (parameter: Value.Primitive.Text.Write[A]) => parameter.self
              )

          given Invariant[Value.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
              (parameter: Value.Primitive.Text[A]) => parameter.self
            )

          given [A] => Annotated[Value.Primitive.Text[A]] =
            Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

          given PrimitiveOperation.Text[Value.Primitive.Text] = PrimitiveOperation
            .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
            .imapK([A] => (self: Annotation[Self.Primitive.Text[A]]) => Text(self))([A] =>
              (parameter: Value.Primitive.Text[A]) => parameter.self
            )

      final case class Union[A](self: Annotation[Self.Union[Value.Branch, A]])
          extends Value[A],
            Value.Union.Read[A],
            Value.Union.Write[A]

      object Union:
        sealed trait Read[+A] extends Value.Read[A]:
          def self: Annotation[Self.Union.Read[Value.Branch.Read, A]]

        object Read:
          def apply[A](annotation: Annotation[Self.Union.Read[Value.Branch.Read, A]]): Value.Union.Read[A] =
            new Read[A]:
              override def self: Self.Annotation[Self.Union.Read[Value.Branch.Read, A]] = annotation

          given Functor[Segment.Parameter.Value.Union.Read] =
            Functor[[a] =>> Annotation[Self.Union.Read[Value.Branch.Read, a]]].imapK([A] =>
              (self: Annotation[Self.Union.Read[Value.Branch.Read, A]]) => Read(self)
            )([A] => (parameter: Value.Union.Read[A]) => parameter.self)

          given [A] => Annotated[Value.Union.Read[A]] =
            Annotated[Annotation[Self.Union.Read[Value.Branch.Read, A]]].imap(Read.apply)(_.self)

          given UnionOperation.Read[Value.Union.Read, Value.Branch.Read] = UnionOperation
            .Read[
              [a] =>> Annotation[Self.Union.Read[Value.Branch.Read, a]],
              Value.Branch.Read
            ]
            .imapK([A] => (self: Annotation[Self.Union.Read[Value.Branch.Read, A]]) => Read(self))([A] =>
              (parameter: Value.Union.Read[A]) => parameter.self
            )

        sealed trait Write[-A] extends Value.Write[A]:
          def self: Annotation[Self.Union.Write[Value.Branch.Write, A]]

      final case class Branch[A](self: Annotation[Self.Branch[Value, A]])
          extends Value.Branch.Read[A],
            Value.Branch.Write[A]

      object Branch:
        sealed trait Read[+A]:
          def self: Annotation[Self.Branch.Read[Value.Read, A]]

        sealed trait Write[-A]:
          def self: Annotation[Self.Branch.Write[Value.Write, A]]

  type Static[A] = Segment.Static.Read[A] & Segment.Static.Write[A]

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

    final case class Modify[A, B](self: Segment.Static[A], f: A => B, g: B => A)
        extends Segment.Static.Read[B],
          Segment.Static.Write[B]

    final case class Root(name: String) extends Segment.Static.Read[Unit], Segment.Static.Write[Unit]

    given Invariant[Segment.Static]:
      override def imap[A, B](self: Segment.Static[A])(f: A => B)(g: B => A): Segment.Static[B] = Modify(self, f, g)
