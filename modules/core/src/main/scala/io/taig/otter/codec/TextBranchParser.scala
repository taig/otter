package io.taig.otter.codec

import io.taig.otter.Text

val TextBranchParser: Parser[Text.Branch] =
  BranchDecoder(decoder = TextParser).contramapK[Text.Branch]([A] => (text: Text.Branch[A]) => text.annotation.self)
