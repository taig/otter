package io.taig.otter.sample.api

import io.taig.otter.dsl.*
import io.taig.otter.http.Segment
import io.taig.otter.sample.data.{Member, ReferenceOrSelf}

object parameters:
  object member:
    val referenceOrSelf: Segment[ReferenceOrSelf[Member.Reference]] =
      parameter("referenceOrSelf", codecs.referenceOrSelf(codecs.member.reference))
