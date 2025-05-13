package io.taig.otter.http

import io.taig.otter.*

trait HttpQueryDsl
    extends CollectionDsl[Http.Query.Array.Collection, Http.Query.Value],
      ConstantDsl[Http.Query.Value.Constant, Http.Query.Value.Primitive],
      DictionaryDsl[Http.Query.Object.Dictionary, Http.Query.Value, Http.Query.Value],
      EnumerationDsl[Http.Query.Value.Enumeration, Http.Query.Value.Primitive],
      NullableDsl[Http.Query.Optional, Http.Query],
      PrimitiveDsl.String[Http.Query.Value.Primitive],
      FieldDsl.Primitive.String[Http.Query.Field, Http.Query.Value, Http.Query.Value, Http.Query.Object.Record],
      TupleDsl[Http.Query.Array.Tuple, Http.Query.Value]:
  // UnionDsl.Untagged[Http.Query.Value.Union, Http.Query.Value]:
  override def key: PrimitiveDsl.String[Http.Query.Value] = this

  def query[A](name: String, codec: => Http.Query[A]): Query[A] =
    Query.Root(name, codec = Reference.later(codec), metadata = Metadata.Empty)

object HttpQueryDsl extends HttpQueryDsl
