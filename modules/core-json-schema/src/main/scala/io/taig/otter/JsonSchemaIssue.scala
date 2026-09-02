package io.taig.otter

/** A way in which a rendered document is not everything the schema it came from says.
  *
  * A profile that cannot say something says nothing, which is the safe direction: a document that constrains less than
  * the decoder does rejects nothing the decoder accepts, so a value that satisfies it may still be refused on the way
  * in, and never the other way round. What is not safe is not knowing, so every silence is recorded rather than left
  * for a reader to notice.
  *
  * `definition` is the `$defs` entry the issue was found under, or `None` at the root, which is as much of a path as a
  * renderer that hoists can honestly give: a definition is rendered once and referred to from everywhere.
  */
enum JsonSchemaIssue:
  /** A schema refers to itself under a profile whose consumer does not accept that. */
  case Recursive(definition: Option[String], name: String)

  /** A tuple widened to a homogeneous array, because the profile has no positional vocabulary. The one issue that is a
    * widening rather than a silence: the document admits arrays the decoder will refuse on arity.
    */
  case Positional(definition: Option[String])

  /** A dictionary rendered as a bare object, because the profile insists every object closes over listed keys. */
  case Open(definition: Option[String])

  /** A coercion rendered as the one form it writes, because the profile would rather say the canonical form than the
    * laxer ones a decoder also takes.
    */
  case Coerced(definition: Option[String])

  /** A key the decoder will not accept as empty, listed as required by a profile that admits nothing else.
    *
    * The one issue that is not safe. A strict field that drops its key rejects an explicit null, and a profile that
    * lists every key in `required` has just told a producer to send one. The fix is on the schema's side -- make the
    * field nullable, or drop its strictness -- not the renderer's.
    */
  case Total(definition: Option[String], field: String)

  /** A constraint the profile has no keyword for. */
  case Dropped(definition: Option[String], constraint: Constraint)

  /** A `format` the profile does not recognise, dropped rather than asserted. */
  case Format(definition: Option[String], name: String)
