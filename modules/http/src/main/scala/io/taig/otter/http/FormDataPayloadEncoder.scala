// package io.taig.otter.http

// import cats.data.Chain
// import cats.syntax.all.*
// import io.taig.otter.*
// import io.taig.otter.Dictionary.Root

// import java.nio.charset.StandardCharsets
// import io.taig.otter.codec.PrimitivePrinter

// final class FormDataPayloadEncoder extends PayloadEncoder[FormData]:
//   override def apply[A](codec: FormData[A], a: A): Array[Byte] =
//     val data = codec match
//       case FormData.Dictionary(self) => Chain.fromSeq(apply(codec = self, a))
//       case FormData.Record(self)     => apply(codec = self, a)

//     data
//       .map:
//         case (key, None)        => key
//         case (key, Some(value)) => s"$key=$value"
//       .mkString_("&")
//       .getBytes(StandardCharsets.UTF_8)

//   def apply[A](codec: Dictionary[FormData.Key, FormData.Value, A], a: A): List[(String, Option[String])] = codec match
//     case Dictionary.Root(key, codec, _, _, _) =>
//       a.map((name, value) => (FormDataKeyPrinter(codec = key.value, name), apply(codec = codec.value, value)))
//     case Dictionary.Modify(self, _, g) => apply(codec = self, g(a))

//   def apply[A](codec: Record[FormData.Field, A], a: A): Chain[(String, Option[String])] = ???
//   // codec match
//   //   case Record.Empty(_) => Chain.empty
//   //   case Record.Field(key, value, metadata) =>
//   //     Chain.one(FormDataKeyPrinter(codec = key.self.value, key.value) -> apply(codec = value.value, a))
//   //   case Record.Modify(self, _, g)  => apply(codec = self, g(a))
//   //   case Record.Optional(self)      => a.fold(Chain.empty)(apply(codec = self, _))
//   //   case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

//   def apply[A](codec: FormData.Value[A], a: A): Option[String] = codec match
//     case FormData.Value.Nullable(self)  => apply(codec = self, a)
//     case FormData.Value.Primitive(self) => PrimitivePrinter.Unquoted(codec = self, a).some

//   def apply[A](codec: Nullable[FormData.Value, A], a: A): Option[String] = codec match
//     case Nullable.Modify(self, _, g)             => apply(codec = self, g(a))
//     case Nullable.Default(reference, default, _) => apply(codec = reference.value, default)
//     case Nullable.Root(reference, _)             => a.flatMap(apply(codec = reference.value, _))
//     case Nullable.Void(metadata)                 => none

// object FormDataPayloadEncoder:
//   val Default: PayloadEncoder[FormData] = new FormDataPayloadEncoder()
