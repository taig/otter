package io.taig.otter.syntax

trait AllSyntax
    extends CatsSyntax,
      CoerceableSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      NullableSyntax

object AllSyntax extends AllSyntax
