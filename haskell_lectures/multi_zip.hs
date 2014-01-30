import Control.Applicative

(<+>) = zipWith ($)

infixl 1 <+>

--(,,) <$> "AB" <+> "CD" <+> "EF"
--(,,,) <$> "AB" <+> "CD" <+> "EF" <+> "GH"

