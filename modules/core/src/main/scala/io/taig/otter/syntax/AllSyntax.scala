package io.taig.otter.syntax

trait AllSyntax
    extends CatsSyntax,
      CoerceableSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      EnumerationSyntax,
      NullableSyntax,
      RecordSyntax

object AllSyntax extends AllSyntax
