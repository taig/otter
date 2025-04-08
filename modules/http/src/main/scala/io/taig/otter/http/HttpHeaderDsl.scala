package io.taig.otter.http

import io.taig.otter.ConstantDsl
import io.taig.otter.EnumerationDsl
import io.taig.otter.PrimitiveDsl
import io.taig.otter.UnionDsl
import io.taig.otter.DictionaryDsl
import io.taig.otter.CollectionDsl
import io.taig.otter.TupleDsl
import io.taig.otter.RecordDsl
import io.taig.otter.http.Http.Header.Value
import org.typelevel.ci.CIString
import io.taig.otter.Metadata
import io.taig.otter.Reference

trait HttpHeaderDsl
    extends CollectionDsl[Http.Header.Array.Collection, Http.Header.Value],
      ConstantDsl[Http.Header.Value.Constant, Http.Header.Value.Primitive],
      DictionaryDsl[Http.Header.Object.Dictionary, Http.Header.Value, Http.Header.Value],
      EnumerationDsl[Http.Header.Value.Enumeration, Http.Header.Value.Primitive],
      PrimitiveDsl.String[Http.Header.Value.Primitive],
      RecordDsl.Primitive.String[Http.Header.Object.Record, Http.Header.Value, Http.Header.Value],
      TupleDsl[Http.Header.Array.Tuple, Http.Header.Value],
      UnionDsl.Untagged[Http.Header.Value.Union, Http.Header.Value]:
  override protected def key: PrimitiveDsl.String[Value] = this

  def header[A](name: CIString, codec: => Http.Header[A]): Header[A] =
    Header.Root(name, codec = Reference.later(codec), metadata = Metadata.Empty)

object HttpHeaderDsl extends HttpHeaderDsl
