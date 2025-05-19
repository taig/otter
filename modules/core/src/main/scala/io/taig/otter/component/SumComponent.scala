package io.taig.otter.component

import io.taig.otter.schema.SumSchema

import scala.annotation.targetName

trait SumComponent[Self[_], -Branch[_]](using self: SumSchema[Self, Branch]):
  export self.{+:, :+, |, discriminator, explicit, keyed, merged, modifyDiscriminator, or, orElse, toSum}
