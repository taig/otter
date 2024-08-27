package io.taig.otter.http

import io.taig.otter as Base

trait Dsl extends Base.Dsl, Codecs, Types

object Dsl extends Dsl
