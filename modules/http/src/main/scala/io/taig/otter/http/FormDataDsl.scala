package io.taig.otter.http

import io.taig.otter.PrimitiveDsl
import io.taig.otter.RecordDsl

trait FormDataDsl
    extends PrimitiveDsl.String[FormData.Value.Primitive],
      RecordDsl[FormData.Record, FormData.Key, FormData.Value],
      RecordDsl.Primitive.String[FormData.Record, FormData.Key, FormData.Value]:
  final override def key: PrimitiveDsl.String[FormData.Key] = FormDataKeyDsl

object FormDataDsl extends FormDataDsl
