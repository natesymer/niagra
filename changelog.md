v 0.2.0

- `declaration` & `(.=)` functions are now of type `(Text -> Builder -> NiagraT m ())`. The syntax `declaration "color" "red"` is still acceptible when using `OverloadedStrings`.
- Property combinators! See `Data.Niagra.Properties` for more information.