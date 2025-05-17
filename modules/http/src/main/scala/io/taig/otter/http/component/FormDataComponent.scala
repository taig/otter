package io.taig.otter.http.component

import io.taig.otter.schema.RecordSchema
import io.taig.otter.http.FormData
import io.taig.otter.schema.DictionarySchema

trait FormDataComponent
    extends DictionarySchema[FormData.Dictionary, FormData.Key, FormData.Value],
      RecordSchema[FormData.Record, FormData.Field]
