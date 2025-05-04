package io.taig.otter.http

import io.taig.otter.PrimitiveDsl
import io.taig.otter.RecordDsl
import io.taig.otter.http.FormData.Key

trait FormDataDsl
    extends PrimitiveDsl.String[FormData.Primitive],
      RecordDsl[FormData.Record, FormData.Key, FormData],
      RecordDsl.Primitive.String[FormData.Record, FormData.Key, FormData]:
  override def key: PrimitiveDsl.String[Key] = FormDataKeyDsl

object FormDataDsl extends FormDataDsl
