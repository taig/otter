package io.taig.otter

import io.circe.Json as CirceJson

/** Attributes a schema can carry to say what it renders as, beyond what a renderer derives from it.
  *
  * [[Keys.title]] and [[Keys.description]] are not here because they are not specific to JSON Schema; they already live
  * in [[Keys]] and this is simply the first interpreter to read them.
  */
trait JsonSchemaKeys:
  /** The whole document a node renders as, instead of the one derived from it. The escape hatch of last resort, and the
    * direct analogue of `TypescriptKeys.expression`.
    */
  val schema: Metadata.Key[CirceJson] = Metadata.Key("schema")

  /** Keywords merged over the derived document: a dialect's own vocabulary, an OpenAPI `discriminator`, an `x-`
    * extension. What keeps a document model that is simply JSON from needing to be reopened for any of them.
    */
  val keywords: Metadata.Key[CirceJson] = Metadata.Key("keywords")

  /** The value a schema names as its default, as a document rather than as the type it reads.
    *
    * A default is an `R`, and a renderer sees a node with its two sides unrelated, so there is no way to push one back
    * through the schema that describes it the way a [[Constant]]'s value is pushed -- [[Constant]] stores a round
    * tripping reference and the default holders wrap an already asymmetric node. Saying it as a document is saying it
    * in the only language both sides share.
    *
    * Nothing checks that it agrees with the default the schema actually applies. That is the price, and it is the price
    * [[Keys.absence]] already pays.
    */
  val default: Metadata.Key[CirceJson] = Metadata.Key("default")

  val examples: Metadata.Key[List[CirceJson]] = Metadata.Key("examples")

  val deprecated: Metadata.Key[Boolean] = Metadata.Key("deprecated")

object JsonSchemaKeys extends JsonSchemaKeys
