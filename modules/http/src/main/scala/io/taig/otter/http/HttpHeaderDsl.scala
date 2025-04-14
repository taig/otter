package io.taig.otter.http

import io.taig.otter.*
import org.typelevel.ci.CIString

trait HttpHeaderDsl
    extends CollectionDsl[Http.Header.Array.Collection, Http.Header.Value],
      ConstantDsl[Http.Header.Value.Constant, Http.Header.Value.Primitive],
      DictionaryDsl[Http.Header.Object.Dictionary, Http.Header.Value, Http.Header.Value],
      EnumerationDsl[Http.Header.Value.Enumeration, Http.Header.Value.Primitive],
      PrimitiveDsl.String[Http.Header.Value.Primitive],
      RecordDsl.Primitive.String[Http.Header.Object.Record, Http.Header.Value, Http.Header.Value],
      TupleDsl[Http.Header.Array.Tuple, Http.Header.Value],
      UnionDsl.Untagged[Http.Header.Value.Union, Http.Header.Value]:
  override protected def key: PrimitiveDsl.String[Http.Header.Value] = this

  def header[A](name: CIString, codec: => Http.Header[A]): Header[A] =
    Header.Root(name, codec = Reference.later(codec), metadata = Metadata.Empty)

object HttpHeaderDsl extends HttpHeaderDsl
