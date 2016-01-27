v 0.2.0

- `declaration` & `(.=)` functions are now of type `(Text -> Builder -> NiagraT m ())`. The syntax `declaration "color" "red"` is still acceptible when using `OverloadedStrings`.
- Property combinators! See `Data.Niagra.Properties` for more information.

v 0.2.1

- Wrote 'AccumulatorT' monad transformer; used under the hood by 'NiagraT' and 'Builder.
- Performance enhancements
- 'hexadecimal' from Data.Niagra.Builder.Numbers now supports signed hexadecimal alongside unsigned hexadecimal.

v 0.2.2

- Performance & safety improvements
	- Ensure characters inserted into 'Text' buffers are supported by 'Text'.
	- Remove intermediate types such as 'Block'.
	- Replace more instances of singleton 'String's read with IsString with singleton 'Char's.