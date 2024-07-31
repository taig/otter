package io.taig.otter.http

import org.typelevel.ci.CIString
import io.taig.otter.Dsl.*

trait Codecs:
  inline def header[A](
      name: CIString,
      codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], A]
  ): Header[A] = inline codec match
    case codec: Codec.Of[Data.Primitive, A]                             => Header.Default(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Array[Data.Primitive], A]                 => Header.Array(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] => Header.Object(name, codec, Metadata.Empty)

  inline def segment[A](
      name: String,
      codec: Codec.Required.Of[Data.Primitive | Data.Array[Data.Primitive], A] |
        Codec.Of[Data.Object[Data.Primitive], A]
  ): Segment.Parameter[A] = inline codec match
    case codec: Codec.Required.Of[Data.Primitive, A] => Segment.Parameter.Default(name, codec, Metadata.Empty)
    case codec: Codec.Required.Of[Data.Array[Data.Primitive], A] => Segment.Parameter.Array(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] =>
      Segment.Parameter.Object(name, codec, Metadata.Empty)

  inline def query[A](
      name: String,
      codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], A]
  ): Query[A] = inline codec match
    case codec: Codec.Of[Data.Primitive, A]                             => Query.Default(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Array[Data.Primitive], A]                 => Query.Array(name, codec, Metadata.Empty)
    case codec: Codec.Of[Data.Object[Data.Optional[Data.Primitive]], A] => Query.Object(name, codec, Metadata.Empty)

  final def endpoint[I, O](input: Request[I], output: Response[O]): Endpoint[I, O] = Endpoint(input, output)

object Codecs extends Codecs
