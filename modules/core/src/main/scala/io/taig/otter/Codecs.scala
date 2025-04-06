package io.taig.otter

// trait Codecs[S[_]] extends Types, Syntax:

//   def field[F <: Codec[A], A](name: String, codec: => F): Field.Required.Of[F, A] =
//     Base.Field.Required.Root(name, codec = Eval.later(codec), metadata = Metadata.Empty)

//   def branch[F <: Codec[A], A](name: String, codec: => F): Branch.Of[F, A] =
//     Base.Branch.Root(name, codec = Eval.later(codec), metadata = Metadata.Empty)

//   object dictionary

//   // object dynamic:
//   //   val number: Union[Data.Number] = branch("bigDecimal", jBigDecimal) |
//   //     branch("bigInteger", jBigInteger) |
//   //     branch("double", double) |
//   //     branch("float", float) |
//   //     branch("int", int) |
//   //     branch("long", long)

//   //   val primitive: Union.Of[Data.Primitive, Data.Primitive] =
//   //     number | branch("boolean", boolean) | branch("string", string)

//   //   val value: Union.Of[Data.Value, Data.Value] = primitive |
//   //     branch("object", dictionary.list(string, any).imap(Data.Object.apply)(_.values)) |
//   //     branch("array", collection.vector(any).imap(Data.Array.apply)(_.values))

//   //   // This code will trigger a warning, which might be wrong
//   //   // val any: Union.Of[Data, Data] = value | branch("null", nil.as(Data.Null))

//   //   val any: Union.Of[Data, Data.Value | Data.Null] = (value :+ branch("null", nil.as(Data.Null))).imap {
//   //     case Left(value)  => value
//   //     case Right(value) => value
//   //   } {
//   //     case value: Data.Value => Left(value)
//   //     case a: Data.Null      => Right(a)
//   //   }

//   // val void: Optional.Of[Data, Unit] = Base.Optional.Void(metadata = Metadata.Empty)

//   // val nil: Optional.Of[Data.Null, Unit] = Base.Optional.Null(metadata = Metadata.Empty)

// //   val xpath: Primitive[XPath] = parser(name = "xpath")(XPath.parse(_).toOption)(_.show)

// object Codecs extends Codecs
