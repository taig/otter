package io.taig.otter

trait Schema[+Of, A] extends Schema.Reader[Of, A], Schema.Writer[Of, A]:
  def ivalidate[B, C, D](validation: SchemaValidation[A, B, C, D])(f: D => A): Schema[Of, D]
  def optional: Schema[Of, Option[A]]

object Schema:
  type Any[+Of, A] = Collection[Of, A] // | Primitive[A]

  trait Reader[+Of, +A]:
    def validate[B, C, D](validation: SchemaValidation[A, B, C, D]): Schema.Reader[Of, D]
    def optional: Schema.Reader[Of, Option[A]]

  object Reader:
    type Any[+Of, +A] = Collection.Reader[Of, A]
    type Identity[A] = Fix[Schema.Reader[*, A]]

    given [Of]: SchemaFunctor[Schema.Reader[Of, *]] with
      override def validate[A, B, C, D](fa: Schema.Reader[Of, A])(
          validation: SchemaValidation[A, B, C, D]
      ): Schema.Reader[Of, D] = fa.validate(validation)
      override def optional[A](fa: Schema.Reader[Of, A]): Schema.Reader[Of, Option[A]] = fa.optional

  trait Writer[+Of, -A]:
    def contramap[B](f: B => A): Schema.Writer[Of, B]
    def optional: Schema.Writer[Of, Option[A]]

  object Writer:
    type Any[+Of, -A] = Collection.Writer[Of, A]
    type Identity[A] = Fix[Schema.Writer[*, A]]

    given [Of]: SchemaContravariant[Schema.Writer[Of, *]] with
      override def contramap[A, B](fa: Schema.Writer[Of, A])(f: B => A): Schema.Writer[Of, B] = fa.contramap(f)
      override def optional[A](fa: Schema.Writer[Of, A]): Schema.Writer[Of, Option[A]] = fa.optional

  given [Of]: SchemaInvariant[Schema[Of, *]] with
    override def ivalidate[A, B, C, D](fa: Schema[Of, A])(validation: SchemaValidation[A, B, C, D])(
        f: D => A
    ): Schema[Of, D] = fa.ivalidate(validation)(f)
    override def optional[A](fa: Schema[Of, A]): Schema[Of, Option[A]] = fa.optional
