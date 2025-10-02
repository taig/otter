package io.taig.otter.component

import io.taig.otter.Schema

trait SchemaComponent extends FieldComponent[Schema.Field[Schema, *], Schema], PrimitiveComponent[Schema.Primitive]

object SchemaComponent extends SchemaComponent
