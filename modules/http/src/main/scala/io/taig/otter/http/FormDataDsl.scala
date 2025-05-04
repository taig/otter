package io.taig.otter.http

import io.taig.otter.PrimitiveDsl

trait FormDataDsl extends PrimitiveDsl.String[FormData.Primitive]

object FormDataDsl extends FormDataDsl