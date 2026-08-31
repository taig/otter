package io.taig.otter

/** What a field accepts for an absent value: only the form its [[Absence]] names, or either of them. */
enum Tolerance:
  case Lenient, Strict
