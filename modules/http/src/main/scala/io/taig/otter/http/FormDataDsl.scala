package io.taig.otter.http

import io.taig.otter.PrimitiveDsl
import io.taig.otter.RecordDsl
import io.taig.otter.http.FormData.Key
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait FormDataDsl
    extends PrimitiveDsl.String[FormData.Value.Primitive],
      RecordDsl[FormData.Record, FormData.Key, FormData.Value],
      RecordDsl.Primitive.String[FormData.Record, FormData.Key, FormData.Value]:
  final override def key: PrimitiveDsl.String[Key] = FormDataKeyDsl

  final def formData[A](codec: => FormData[A]): Body[FormData, A] = BodyDsl.body(
    mediaType = MediaType(
      tpe = MediaType.Type(primary = "application", secondary = "x-www-form-urlencoded"),
      parameters = Parameters.Empty
    ),
    codec
  )

object FormDataDsl extends FormDataDsl
