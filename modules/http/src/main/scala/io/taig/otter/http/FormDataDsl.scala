package io.taig.otter.http

import io.taig.otter.PrimitiveDsl
import io.taig.otter.FieldDsl

trait FormDataDsl
    extends PrimitiveDsl.String[FormData.Value.Primitive],
      FieldDsl[FormData.Field, FormData.Key, FormData.Value, FormData.Record],
      FieldDsl.Primitive.String[FormData.Field, FormData.Key, FormData.Value, FormData.Record]:
  final override def key: PrimitiveDsl.String[FormData.Key] = FormDataKeyDsl

object FormDataDsl extends FormDataDsl
