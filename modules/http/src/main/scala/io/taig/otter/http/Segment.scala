package io.taig.otter.http

import io.taig.otter.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Annotation
import cats.Functor
import cats.Contravariant
import io.taig.otter.Annotated
import io.taig.otter.operation.*
import cats.Invariant
import io.taig.otter.Reference
import Self.http.operation.SegmentOperation
import scala.annotation.targetName

abstract class Segment[A] extends Segment.Read[A], Segment.Write[A]:
  def imap[B](f: A => B)(g: B => A): Segment[B]

object Segment:
  trait Read[+A]:
    def map[B](f: A => B): Segment.Read[B]

    def name: String

  object Read:
    given Functor[Segment.Read]:
      override def map[A, B](segment: Segment.Read[A])(f: A => B): Segment.Read[B] = segment.map(f)

  trait Write[-A]:
    def contramap[B](f: B => A): Segment.Write[B]

    def name: String

  object Write:
    given Contravariant[Segment.Write]:
      override def contramap[A, B](segment: Segment.Write[A])(f: B => A): Segment.Write[B] = segment.contramap(f)

  abstract class Dynamic[A] extends Segment[A], Segment.Dynamic.Read[A], Segment.Dynamic.Write[A]:
    final override def imap[B](f: A => B)(g: B => A): Segment.Dynamic[B] = Dynamic.Modify(self = this, f, g)

    override def parameter: Reference[Segment.Parameter, ?]

  object Dynamic:
    trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Dynamic.Read[B] = Read.Modify(self = this, f)

      def name: String

      def parameter: Reference[Segment.Parameter.Read, ?]

    object Read:
      final case class Modify[A, B](self: Segment.Dynamic.Read[A], f: A => B) extends Segment.Dynamic.Read[B]:
        export self.{name, parameter}

      final case class Root[A](name: String, parameter: Reference[Segment.Parameter.Read, A])
          extends Segment.Dynamic.Read[A]

      given Functor[Segment.Dynamic.Read]:
        override def map[A, B](segment: Segment.Dynamic.Read[A])(f: A => B): Segment.Dynamic.Read[B] =
          segment.map(f)

      given SegmentOperation.Dynamic.Read[Segment.Dynamic.Read]:
        @targetName("liftRead")
        override def lift[A](name: String, schema: Reference[Http.Segment.Parameter.Read, A]): Segment.Dynamic.Read[A] =
          Root(name, schema)

    trait Write[-A] extends Segment.Write[A]:
      final override def contramap[B](f: B => A): Segment.Dynamic.Write[B] = Write.Modify(self = this, f)

      def parameter: Reference[Segment.Parameter.Write, ?]

    object Write:
      final case class Modify[A, B](self: Segment.Dynamic.Write[A], f: B => A) extends Segment.Dynamic.Write[B]:
        export self.{name, parameter}

      final case class Root[A](name: String, parameter: Reference[Segment.Parameter.Write, A])
          extends Segment.Dynamic.Write[A]

      given Contravariant[Segment.Dynamic.Write]:
        override def contramap[A, B](segment: Segment.Dynamic.Write[A])(f: B => A): Segment.Dynamic.Write[B] =
          segment.contramap(f)

      given SegmentOperation.Dynamic.Write[Segment.Dynamic.Write]:
        @targetName("liftWrite")
        override def lift[A](
            name: String,
            schema: Reference[Http.Segment.Parameter.Write, A]
        ): Segment.Dynamic.Write[A] =
          Root(name, schema)

    final case class Modify[A, B](self: Segment.Dynamic[A], f: A => B, g: B => A) extends Segment.Dynamic[B]:
      export self.{name, parameter}

    final case class Root[A](name: String, parameter: Reference[Segment.Parameter, A]) extends Segment.Dynamic[A]

    given Invariant[Segment.Dynamic]:
      override def imap[A, B](self: Segment.Dynamic[A])(f: A => B)(g: B => A): Segment.Dynamic[B] =
        Modify(self, f, g)

    given SegmentOperation.Dynamic[Segment.Dynamic]:
      override def lift[A](name: String, schema: Reference[Http.Segment.Parameter, A]): Segment.Dynamic[A] =
        Root(name, schema)

  abstract class Static[A] extends Segment[A], Segment.Static.Read[A], Segment.Static.Write[A]:
    override def imap[B](f: A => B)(g: B => A): Segment.Static[B] = Segment.Static.Modify(self = this, f, g)

  object Static:
    trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Static.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Segment.Static.Read[A], f: A => B) extends Segment.Static.Read[B]:
        export self.name

      given Functor[Segment.Static.Read]:
        override def map[A, B](segment: Segment.Static.Read[A])(f: A => B): Segment.Static.Read[B] =
          segment.map(f)

      given SegmentOperation.Static.Read[Segment.Static.Read]:
        override def lift(name: String): Segment.Static.Read[Unit] = Root(name)

    trait Write[-A] extends Segment.Write[A]:
      final override def contramap[B](f: B => A): Segment.Static.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Segment.Static.Write[A], f: B => A) extends Segment.Static.Write[B]:
        export self.name

      given Contravariant[Segment.Static.Write]:
        override def contramap[A, B](segment: Segment.Static.Write[A])(f: B => A): Segment.Static.Write[B] =
          segment.contramap(f)

      given SegmentOperation.Static.Write[Segment.Static.Write]:
        override def lift(name: String): Segment.Static.Write[Unit] = Root(name)

    final case class Modify[A, B](self: Segment.Static[A], f: A => B, g: B => A) extends Segment.Static[B]:
      export self.name

    final case class Root(name: String) extends Segment.Static[Unit]

    given Invariant[Segment.Static]:
      override def imap[A, B](self: Segment.Static[A])(f: A => B)(g: B => A): Segment.Static[B] = Modify(self, f, g)

    given SegmentOperation.Static[Segment.Static]:
      override def lift(name: String): Segment.Static[Unit] = Root(name)

  given Invariant[Segment]:
    override def imap[A, B](self: Segment[A])(f: A => B)(g: B => A): Segment[B] = self match
      case segment: Segment.Dynamic[A] => segment.imap(f)(g)
      case segment: Segment.Static[A]  => segment.imap(f)(g)

  sealed abstract class Parameter[A] extends Segment.Parameter.Read[A], Segment.Parameter.Write[A]:
    override def self: Annotation[
      Self.Constant[Segment.Parameter.Primitive.Text, A] | Self.Enumeration[Segment.Parameter.Primitive.Text, A] |
        Self.Primitive.Text[A] | Self.Union[Segment.Parameter.Branch, A]
    ]

  object Parameter:
    sealed trait Read[+A]:
      def self: Annotation[
        Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A] |
          Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A] | Self.Primitive.Text.Read[A] |
          Self.Union.Read[Segment.Parameter.Branch.Read, A]
      ]

    sealed trait Write[-A]:
      def self: Annotation[
        Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A] |
          Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, A] | Self.Primitive.Text.Write[A] |
          Self.Union.Write[Segment.Parameter.Branch.Write, A]
      ]

    final case class Constant[A](self: Annotation[Self.Constant[Segment.Parameter.Primitive.Text, A]])
        extends Segment.Parameter[A],
          Segment.Parameter.Constant.Read[A],
          Segment.Parameter.Constant.Write[A]

    object Constant:
      sealed trait Read[+A] extends Segment.Parameter.Read[A]:
        def self: Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]]
        ): Segment.Parameter.Constant.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]] = annotation

        given Functor[Segment.Parameter.Constant.Read] =
          Functor[[a] =>> Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Parameter.Constant.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Constant.Read[A]] =
          Annotated[Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

        given ConstantOperation.Read[Segment.Parameter.Constant.Read, Segment.Parameter.Primitive.Text.Read] =
          ConstantOperation
            .Read[
              [a] =>> Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, a]],
              Segment.Parameter.Primitive.Text.Read
            ]
            .imapK([A] =>
              (self: Annotation[Self.Constant.Read[Segment.Parameter.Primitive.Text.Read, A]]) => Read(self)
            )([A] => (parameter: Segment.Parameter.Constant.Read[A]) => parameter.self)

      sealed trait Write[-A] extends Segment.Parameter.Write[A]:
        def self: Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]]
        ): Segment.Parameter.Constant.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]] =
            annotation

        given Contravariant[Segment.Parameter.Constant.Write] =
          Contravariant[[a] =>> Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]]) =>
              Write(annotation)
          )([A] => (parameter: Segment.Parameter.Constant.Write[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Constant.Write[A]] =
          Annotated[Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]]]
            .imap(Write.apply)(_.self)

        given ConstantOperation.Write[Segment.Parameter.Constant.Write, Segment.Parameter.Primitive.Text.Write] =
          ConstantOperation
            .Write[
              [a] =>> Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, a]],
              Segment.Parameter.Primitive.Text.Write
            ]
            .imapK([A] =>
              (annotation: Annotation[Self.Constant.Write[Segment.Parameter.Primitive.Text.Write, A]]) =>
                Write(annotation)
            )([A] => (parameter: Segment.Parameter.Constant.Write[A]) => parameter.self)

      given Invariant[Segment.Parameter.Constant] =
        Invariant[[a] =>> Annotation[Self.Constant[Segment.Parameter.Primitive.Text, a]]].imapK([A] =>
          (self: Annotation[Self.Constant[Segment.Parameter.Primitive.Text, A]]) => Constant(self)
        )([A] => (parameter: Segment.Parameter.Constant[A]) => parameter.self)

      given [A] => Annotated[Segment.Parameter.Constant[A]] =
        Annotated[Annotation[Self.Constant[Segment.Parameter.Primitive.Text, A]]].imap(Constant.apply)(_.self)

      given ConstantOperation[Segment.Parameter.Constant, Segment.Parameter.Primitive.Text] = ConstantOperation[
        [a] =>> Annotation[Self.Constant[Segment.Parameter.Primitive.Text, a]],
        Segment.Parameter.Primitive.Text
      ].imapK([A] => Constant(_))([A] => _.self)

    final case class Enumeration[A](self: Annotation[Self.Enumeration[Segment.Parameter.Primitive.Text, A]])
        extends Segment.Parameter[A],
          Segment.Parameter.Enumeration.Read[A],
          Segment.Parameter.Enumeration.Write[A]

    object Enumeration:
      sealed trait Read[+A] extends Segment.Parameter.Read[A]:
        override def self: Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A]]
        ): Segment.Parameter.Enumeration.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A]] =
            annotation

        given Functor[Segment.Parameter.Enumeration.Read] =
          Functor[[a] =>> Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Parameter.Enumeration.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Enumeration.Read[A]] =
          Annotated[Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, A]]].imap(Read.apply)(_.self)

        given EnumerationOperation.Read[Segment.Parameter.Enumeration.Read, Segment.Parameter.Primitive.Text.Read] =
          EnumerationOperation
            .Read[
              [a] =>> Annotation[Self.Enumeration.Read[Segment.Parameter.Primitive.Text.Read, a]],
              Segment.Parameter.Primitive.Text.Read
            ]
            .imapK([A] => Read(_))([A] => _.self)

      sealed trait Write[-A] extends Segment.Parameter.Write[A]:
        override def self: Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, A]]
        ): Segment.Parameter.Enumeration.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, A]] =
            annotation

        given Contravariant[Segment.Parameter.Enumeration.Write] =
          Contravariant[[a] =>> Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given [A] => Annotated[Segment.Parameter.Enumeration.Write[A]] =
          Annotated[Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, A]]].imap(Write.apply)(_.self)

        given EnumerationOperation.Write[Segment.Parameter.Enumeration.Write, Segment.Parameter.Primitive.Text.Write] =
          EnumerationOperation
            .Write[
              [a] =>> Annotation[Self.Enumeration.Write[Segment.Parameter.Primitive.Text.Write, a]],
              Segment.Parameter.Primitive.Text.Write
            ]
            .imapK([A] => Write(_))([A] => _.self)

      given Invariant[Segment.Parameter.Enumeration] =
        Invariant[[a] =>> Annotation[Self.Enumeration[Segment.Parameter.Primitive.Text, a]]]
          .imapK([A] => Enumeration(_))([A] => _.self)

      given [A] => Annotated[Segment.Parameter.Enumeration[A]] =
        Annotated[Annotation[Self.Enumeration[Segment.Parameter.Primitive.Text, A]]].imap(Enumeration.apply)(_.self)

      given EnumerationOperation[Segment.Parameter.Enumeration, Segment.Parameter.Primitive.Text] =
        EnumerationOperation[
          [a] =>> Annotation[Self.Enumeration[Segment.Parameter.Primitive.Text, a]],
          Segment.Parameter.Primitive.Text
        ].imapK([A] => Enumeration(_))([A] => _.self)

    sealed abstract class Primitive[A]
        extends Segment.Parameter[A],
          Segment.Parameter.Primitive.Read[A],
          Segment.Parameter.Primitive.Write[A]

    object Primitive:
      sealed trait Read[+A] extends Segment.Parameter.Read[A]

      sealed trait Write[-A] extends Segment.Parameter.Write[A]

      final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])
          extends Segment.Parameter.Primitive.Boolean.Read[A],
            Segment.Parameter.Primitive.Boolean.Write[A]

      object Boolean:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Boolean.Read[A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Boolean.Read[A]]
          ): Segment.Parameter.Primitive.Boolean.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Read[A]] = annotation

          given Functor[Segment.Parameter.Primitive.Boolean.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Boolean.Read[A]] =
            Annotated[Annotation[Self.Primitive.Boolean.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Boolean.Read[Segment.Parameter.Primitive.Boolean.Read] = PrimitiveOperation.Boolean
            .Read[[a] =>> Annotation[Self.Primitive.Boolean.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Boolean.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Boolean.Write[A]]
          ): Segment.Parameter.Primitive.Boolean.Write[A] = new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Boolean.Write[A]] = annotation

          given Contravariant[Segment.Parameter.Primitive.Boolean.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Boolean.Write[A]] =
            Annotated[Annotation[Self.Primitive.Boolean.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Boolean.Write[Segment.Parameter.Primitive.Boolean.Write] = PrimitiveOperation.Boolean
            .Write[[a] =>> Annotation[Self.Primitive.Boolean.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Segment.Parameter.Primitive.Boolean] =
          Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK([A] => Primitive.Boolean(_))([A] => _.self)

        given [A] => Annotated[Segment.Parameter.Primitive.Boolean[A]] =
          Annotated[Annotation[Self.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

        given PrimitiveOperation.Boolean[Segment.Parameter.Primitive.Boolean] = PrimitiveOperation
          .Boolean[[a] =>> Annotation[Self.Primitive.Boolean[a]]]
          .imapK([A] => Primitive.Boolean(_))([A] => _.self)

      final case class Number[A](self: Annotation[Self.Primitive.Number[A]])
          extends Segment.Parameter.Primitive.Number.Read[A],
            Segment.Parameter.Primitive.Number.Write[A]

      object Number:
        sealed trait Read[+A]:
          def self: Annotation[Self.Primitive.Number.Read[A]]

        object Read:
          def apply[A](
              annotation: Annotation[Self.Primitive.Number.Read[A]]
          ): Segment.Parameter.Primitive.Number.Read[A] = new Read[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Read[A]] = annotation

          given Functor[Segment.Parameter.Primitive.Number.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Number.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Number.Read[A]] =
            Annotated[Annotation[Self.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Number.Read[Segment.Parameter.Primitive.Number.Read] = PrimitiveOperation.Number
            .Read[[a] =>> Annotation[Self.Primitive.Number.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A]:
          def self: Annotation[Self.Primitive.Number.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Number.Write[A]]
          ): Segment.Parameter.Primitive.Number.Write[A] = new Write[A]:
            override def self: Self.Annotation[Self.Primitive.Number.Write[A]] = annotation

          given Contravariant[Segment.Parameter.Primitive.Number.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Number.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Number.Write[A]] =
            Annotated[Annotation[Self.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Number.Write[Segment.Parameter.Primitive.Number.Write] = PrimitiveOperation.Number
            .Write[[a] =>> Annotation[Self.Primitive.Number.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Segment.Parameter.Primitive.Number] =
          Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] => Primitive.Number(_))([A] => _.self)

        given [A] => Annotated[Segment.Parameter.Primitive.Number[A]] =
          Annotated[Annotation[Self.Primitive.Number[A]]].imap(Number.apply)(_.self)

        given PrimitiveOperation.Number[Segment.Parameter.Primitive.Number] = PrimitiveOperation
          .Number[[a] =>> Annotation[Self.Primitive.Number[a]]]
          .imapK([A] => Primitive.Number(_))([A] => _.self)

      final case class Text[A](self: Annotation[Self.Primitive.Text[A]])
          extends Segment.Parameter.Primitive[A],
            Segment.Parameter.Primitive.Text.Read[A],
            Segment.Parameter.Primitive.Text.Write[A]

      object Text:
        sealed trait Read[+A] extends Segment.Parameter.Read[A]:
          def self: Annotation[Self.Primitive.Text.Read[A]]

        object Read:
          def apply[A](annotation: Annotation[Self.Primitive.Text.Read[A]]): Segment.Parameter.Primitive.Text.Read[A] =
            new Read[A]:
              override def self: Annotation[Self.Primitive.Text.Read[A]] = annotation

          given Functor[Segment.Parameter.Primitive.Text.Read] =
            Functor[[a] =>> Annotation[Self.Primitive.Text.Read[a]]].imapK([A] => Read(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Text.Read[A]] =
            Annotated[Annotation[Self.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

          given PrimitiveOperation.Text.Read[Segment.Parameter.Primitive.Text.Read] = PrimitiveOperation.Text
            .Read[[a] =>> Annotation[Self.Primitive.Text.Read[a]]]
            .imapK([A] => Read(_))([A] => _.self)

        sealed trait Write[-A] extends Segment.Parameter.Write[A]:
          def self: Annotation[Self.Primitive.Text.Write[A]]

        object Write:
          def apply[A](
              annotation: Annotation[Self.Primitive.Text.Write[A]]
          ): Segment.Parameter.Primitive.Text.Write[A] =
            new Write[A]:
              override def self: Annotation[Self.Primitive.Text.Write[A]] = annotation

          given Contravariant[Segment.Parameter.Primitive.Text.Write] =
            Contravariant[[a] =>> Annotation[Self.Primitive.Text.Write[a]]].imapK([A] => Write(_))([A] => _.self)

          given [A] => Annotated[Segment.Parameter.Primitive.Text.Write[A]] =
            Annotated[Annotation[Self.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

          given PrimitiveOperation.Text.Write[Segment.Parameter.Primitive.Text.Write] = PrimitiveOperation.Text
            .Write[[a] =>> Annotation[Self.Primitive.Text.Write[a]]]
            .imapK([A] => Write(_))([A] => _.self)

        given Invariant[Segment.Parameter.Primitive.Text] = Invariant[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

        given [A] => Annotated[Segment.Parameter.Primitive.Text[A]] =
          Annotated[Annotation[Self.Primitive.Text[A]]].imap(Text.apply)(_.self)

        given PrimitiveOperation.Text[Segment.Parameter.Primitive.Text] = PrimitiveOperation
          .Text[[a] =>> Annotation[Self.Primitive.Text[a]]]
          .imapK([A] => Text(_))([A] => _.self)

    final case class Union[A](self: Annotation[Self.Union[Segment.Parameter.Branch, A]])
        extends Segment.Parameter[A],
          Segment.Parameter.Union.Read[A],
          Segment.Parameter.Union.Write[A]

    object Union:
      sealed trait Read[+A] extends Segment.Parameter.Read[A]:
        def self: Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]]
        ): Segment.Parameter.Union.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]] = annotation

        given Functor[Segment.Parameter.Union.Read] =
          Functor[[a] =>> Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Parameter.Union.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Union.Read[A]] =
          Annotated[Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]]].imap(Read.apply)(_.self)

        given UnionOperation.Read[Segment.Parameter.Union.Read, Segment.Parameter.Branch.Read] = UnionOperation
          .Read[
            [a] =>> Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, a]],
            Segment.Parameter.Branch.Read
          ]
          .imapK([A] => (self: Annotation[Self.Union.Read[Segment.Parameter.Branch.Read, A]]) => Read(self))([A] =>
            (parameter: Segment.Parameter.Union.Read[A]) => parameter.self
          )

      sealed trait Write[-A] extends Segment.Parameter.Write[A]:
        def self: Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]]
        ): Segment.Parameter.Union.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]] = annotation

        given Contravariant[Segment.Parameter.Union.Write] =
          Contravariant[[a] =>> Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]]) => Write(annotation)
          )([A] => (parameter: Segment.Parameter.Union.Write[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Union.Write[A]] =
          Annotated[Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]]].imap(Write.apply)(_.self)

        given UnionOperation.Write[Segment.Parameter.Union.Write, Segment.Parameter.Branch.Write] = UnionOperation
          .Write[
            [a] =>> Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, a]],
            Segment.Parameter.Branch.Write
          ]
          .imapK([A] =>
            (annotation: Annotation[Self.Union.Write[Segment.Parameter.Branch.Write, A]]) => Write(annotation)
          )([A] => (parameter: Segment.Parameter.Union.Write[A]) => parameter.self)

    given Invariant[Segment.Parameter.Union] =
      Invariant[[a] =>> Annotation[Self.Union[Segment.Parameter.Branch, a]]].imapK([A] =>
        (annotation: Annotation[Self.Union[Segment.Parameter.Branch, A]]) => Union(annotation)
      )([A] => (parameter: Segment.Parameter.Union[A]) => parameter.self)

    given [A] => Annotated[Segment.Parameter.Union[A]] =
      Annotated[Annotation[Self.Union[Segment.Parameter.Branch, A]]].imap(Union.apply)(_.self)

    given UnionOperation[Segment.Parameter.Union, Segment.Parameter.Branch] = UnionOperation[
      [a] =>> Annotation[Self.Union[Segment.Parameter.Branch, a]],
      Segment.Parameter.Branch
    ].imapK([A] => (annotation: Annotation[Self.Union[Segment.Parameter.Branch, A]]) => Union(annotation))([A] =>
      (parameter: Segment.Parameter.Union[A]) => parameter.self
    )

    final case class Branch[A](self: Annotation[Self.Branch[Segment.Parameter, A]])
        extends Segment.Parameter.Branch.Read[A],
          Segment.Parameter.Branch.Write[A]

    object Branch:
      sealed trait Read[+A]:
        def self: Annotation[Self.Branch.Read[Segment.Parameter.Read, A]]

      object Read:
        def apply[A](
            annotation: Annotation[Self.Branch.Read[Segment.Parameter.Read, A]]
        ): Segment.Parameter.Branch.Read[A] = new Read[A]:
          override def self: Self.Annotation[Self.Branch.Read[Segment.Parameter.Read, A]] = annotation

        given Functor[Segment.Parameter.Branch.Read] =
          Functor[[a] =>> Annotation[Self.Branch.Read[Segment.Parameter.Read, a]]].imapK([A] =>
            (self: Annotation[Self.Branch.Read[Segment.Parameter.Read, A]]) => Read(self)
          )([A] => (parameter: Segment.Parameter.Branch.Read[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Branch.Read[A]] =
          Annotated[Annotation[Self.Branch.Read[Segment.Parameter.Read, A]]].imap(Read.apply)(_.self)

      sealed trait Write[-A]:
        def self: Annotation[Self.Branch.Write[Segment.Parameter.Write, A]]

      object Write:
        def apply[A](
            annotation: Annotation[Self.Branch.Write[Segment.Parameter.Write, A]]
        ): Segment.Parameter.Branch.Write[A] = new Write[A]:
          override def self: Self.Annotation[Self.Branch.Write[Segment.Parameter.Write, A]] = annotation

        given Contravariant[Segment.Parameter.Branch.Write] =
          Contravariant[[a] =>> Annotation[Self.Branch.Write[Segment.Parameter.Write, a]]].imapK([A] =>
            (annotation: Annotation[Self.Branch.Write[Segment.Parameter.Write, A]]) => Write(annotation)
          )([A] => (parameter: Segment.Parameter.Branch.Write[A]) => parameter.self)

        given [A] => Annotated[Segment.Parameter.Branch.Write[A]] =
          Annotated[Annotation[Self.Branch.Write[Segment.Parameter.Write, A]]].imap(Write.apply)(_.self)

      given Invariant[Segment.Parameter.Branch] =
        Invariant[[a] =>> Annotation[Self.Branch[Segment.Parameter, a]]].imapK([A] =>
          (annotation: Annotation[Self.Branch[Segment.Parameter, A]]) => Segment.Parameter.Branch(annotation)
        )([A] => (parameter: Segment.Parameter.Branch[A]) => parameter.self)

      given [A] => Annotated[Segment.Parameter.Branch[A]] =
        Annotated[Annotation[Self.Branch[Segment.Parameter, A]]].imap(Segment.Parameter.Branch.apply)(_.self)
