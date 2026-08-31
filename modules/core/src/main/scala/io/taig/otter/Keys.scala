package io.taig.otter

trait Keys:
  /** How a field renders when what it holds is absent. */
  val absence: Metadata.Key[Absence] = Metadata.Key("absence")

  val description: Metadata.Key[String] = Metadata.Key("description")
  val name: Metadata.Key[String] = Metadata.Key("name")
  val title: Metadata.Key[String] = Metadata.Key("title")

  /** Whether a field accepts only the form [[absence]] names, or either of them. */
  val tolerance: Metadata.Key[Tolerance] = Metadata.Key("tolerance")

object Keys extends Keys
