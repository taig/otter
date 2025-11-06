// package io.taig.otter

// import cats.Invariant
// import cats.derived.*
// import io.taig.otter as Self
// import io.taig.otter.operation.*

// sealed abstract class Text[A] extends Product, Serializable derives Invariant

// object Text:
//   final case class Coerce[A](self: Annotation[Self.Coerce[Text.Primitive, A]]) extends Text[A] derives Invariant

//   object Coerce:
//     given [A]: Annotated[Text.Coerce[A]] =
//       Annotated[Annotation[Self.Coerce[Text.Primitive, A]]].imap(apply)(_.self)

//     // given CoerceOperation[Text.Coerce, Text.Primitive] =
//     //   CoerceOperation[[a] =>> Annotation[Self.Coerce[Text.Primitive, a]], Text.Primitive]
//     //     .imapK([A] => (self: Annotation[Self.Coerce[Text.Primitive, A]]) => Coerce(self))([A] =>
//     //       (schema: Coerce[A]) => schema.self
//     //     )

//   final case class Constant[A](self: Annotation[Self.Constant[Text, A]]) extends Text[A] derives Invariant

//   object Constant:
//     given [A]: Annotated[Text.Constant[A]] =
//       Annotated[Annotation[Self.Constant[Text, A]]].imap(apply)(_.self)

//     given ConstantOperation[Text.Constant, Text] =
//       ConstantOperation[[a] =>> Annotation[Self.Constant[Text, a]], Text]
//         .imapK([A] => (self: Annotation[Self.Constant[Text, A]]) => Constant(self))([A] =>
//           (schema: Constant[A]) => schema.self
//         )

//   final case class Enumeration[A](self: Annotation[Self.Enumeration[Text, A]]) extends Text[A] derives Invariant

//   object Enumeration:
//     given [A]: Annotated[Text.Enumeration[A]] =
//       Annotated[Annotation[Self.Enumeration[Text, A]]].imap(apply)(_.self)

//     given EnumerationOperation[Text.Enumeration, Text] =
//       EnumerationOperation[[a] =>> Annotation[Self.Enumeration[Text, a]], Text]
//         .imapK([A] => (self: Annotation[Self.Enumeration[Text, A]]) => Enumeration(self))([A] =>
//           (schema: Enumeration[A]) => schema.self
//         )

//   sealed trait Primitive[A] extends Product, Serializable derives Invariant

//   object Primitive:
//     final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A] derives Invariant

//     object Boolean:
//       given [A]: Annotated[Text.Primitive.Boolean[A]] = Annotated[Annotation[Self.Primitive.Boolean[A]]]
//         .imap(Boolean.apply)(_.self)

//       given BooleanOperation[Text.Primitive.Boolean] =
//         BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
//           (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
//         )

//     final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A] derives Invariant

//     object Number:
//       given [A]: Annotated[Text.Primitive.Number[A]] = Annotated[Annotation[Self.Primitive.Number[A]]]
//         .imap(Number.apply)(_.self)

//       given NumberOperation[Text.Primitive.Number] =
//         NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
//           (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
//         )([A] => (schema: Number[A]) => schema.self)

//     final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[A], Text.Primitive[A]
//         derives Invariant

//     object String:
//       given [A]: Annotated[Text.Primitive.String[A]] = Annotated[Annotation[Self.Primitive.String[A]]]
//         .imap(String.apply)(_.self)

//       given StringOperation[Text.Primitive.String] =
//         StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
//           (self: Annotation[Self.Primitive.String[A]]) => String(self)
//         )([A] => (schema: String[A]) => schema.self)

//     given [A]: Annotated[Text.Primitive[A]] = Annotated[Annotation[Self.Primitive[A]]]
//       .imap { annotation =>
//         annotation.self match
//           case schema: Self.Primitive.Boolean[A] => Boolean(annotation.copy(self = schema))
//           case schema: Self.Primitive.Number[A]  => Number(annotation.copy(self = schema))
//           case schema: Self.Primitive.String[A]  => String(annotation.copy(self = schema))
//       } {
//         case Text.Primitive.Boolean(annotation) => annotation
//         case Text.Primitive.Number(annotation)  => annotation
//         case Text.Primitive.String(annotation)  => annotation
//       }

//     // given PrimitiveOperation[Text.Primitive] =
//     //   PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK([A] =>
//     //     (annotation: Annotation[Self.Primitive[A]]) =>
//     //       annotation.self match
//     //         case schema: Self.Primitive.Boolean[A] => Text.Primitive.Boolean(annotation.copy(self = schema))
//     //         case schema: Self.Primitive.Number[A]  => Text.Primitive.Number(annotation.copy(self = schema))
//     //         case schema: Self.Primitive.String[A]  => Text.Primitive.String(annotation.copy(self = schema))
//     //   )([A] =>
//     //     (text: Text.Primitive[A]) =>
//     //       text match
//     //         case Text.Primitive.Boolean(self) => self
//     //         case Text.Primitive.Number(self)  => self
//     //         case Text.Primitive.String(self)  => self
//     //   )

//   final case class Union[A](self: Annotation[Self.Union[Text, A]]) extends Text[A] derives Invariant

//   object Union:
//     given [A]: Annotated[Text.Union[A]] =
//       Annotated[Annotation[Self.Union[Text, A]]].imap(apply)(_.self)

//     given UnionOperation[Text.Union, Text] =
//       UnionOperation[[a] =>> Annotation[Self.Union[Text, a]], Text]
//         .imapK([A] => (annotation: Annotation[Self.Union[Text, A]]) => Union(annotation))([A] =>
//           (text: Union[A]) => text.self
//         )
