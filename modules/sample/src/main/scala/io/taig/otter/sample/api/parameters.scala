package io.taig.otter.sample.api

import io.taig.otter.dsl.*
import io.taig.otter.http.Segment
import io.taig.otter.sample.data.{Member, ReferenceOrSelf}

object parameters:
  object member:
    val referenceOrSelf: Segment[ReferenceOrSelf[Member.Reference]] =
      parameter("reference", codecs.referenceOrSelf(codecs.member.reference))
        .description("If the current user is a member, use either `self` or the reference of this member. " +
          "Using the reference of a different member will cause a `memberReferenceUnknown` error. If the current user " +
          "is a librarian, any member reference can be use, but `self` will cause a `memberReferenceUnknown` error.")
