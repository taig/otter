package io.taig.otter.http

import io.taig.otter.Metadata

/** The attributes HTTP reads that no other format has a use for.
  *
  * A trait and an object, the way [[io.taig.otter.Keys]] is, so that a downstream module adds its own vocabulary beside
  * this one rather than editing an enumeration.
  */
trait HttpKeys:
  /** The name a [[Part]] claims the bytes it carries were saved under.
    *
    * Metadata rather than a field on the part, because a filename is a hint and not a value: it does not change what
    * the part holds, a caller may send anything or nothing, and a schema that made it a value would be describing a
    * different upload than the one browsers perform.
    */
  val filename: Metadata.Key[String] = Metadata.Key("filename")

object HttpKeys extends HttpKeys
