package io.taig.otter

/** What a field contributes when what it holds is absent.
  *
  * `Empty` names the value a format writes for nothing, which is `null` in JSON, rather than naming that value itself,
  * so that a format without one is still free to say what it does.
  */
enum Absence:
  case Omit, Empty
