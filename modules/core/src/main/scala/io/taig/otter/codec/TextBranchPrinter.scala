package io.taig.otter.codec

import io.taig.otter.Text

val TextBranchPrinter: Printer[Text.Branch] =
  BranchEncoder(encoder = TextPrinter).contramapK([A] => (text: Text.Branch[A]) => text.annotation.self)
