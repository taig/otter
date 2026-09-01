package io.taig.otter

/** Which of a schema's two wire shapes an interpreter describes.
  *
  * A codec never has to ask: an [[io.taig.otter.codec.Encoder]] is handed a value to write and an
  * [[io.taig.otter.codec.Decoder]] a document to read, so the side it works on is the side it was called from. An
  * [[io.taig.otter.codec.Renderer]] involves no value and leaves both slots of the schema free, which is exactly why it
  * cannot recover the side from what it is given and has to be told.
  *
  * The two sides are not the same shape. A field written by dropping its key is read as a missing key or an explicit
  * empty alike, a field holding a default is always written and may be absent when read, and a [[Coerce]] accepts on
  * the way in what it would never write on the way out. Describing only one of them and calling it the schema would be
  * a lie in whichever direction was left out.
  *
  * This is not [[Direction]], which names the profunctor's variance slots so that a `Functor` or a `Contravariant` can
  * exist over a schema. That is a type level statement about what a schema can still do; this is a runtime choice an
  * interpreter makes about what to say.
  */
enum Side:
  case Read, Write
