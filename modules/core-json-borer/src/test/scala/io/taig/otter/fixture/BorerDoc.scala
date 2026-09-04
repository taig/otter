package io.taig.otter.fixture

import io.bullet.borer.Dom
import io.bullet.borer.Json as BorerJson

import java.nio.charset.StandardCharsets.UTF_8

/** [[Doc]] in borer's model, by borer's own parser: the only thing this hands to borer is bytes. */
object BorerDoc:
  def toBorer(doc: Doc): Dom.Element =
    BorerJson.decode(Doc.render(doc).getBytes(UTF_8)).to[Dom.Element].value
