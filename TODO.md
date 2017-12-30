## Todo on Prettify

- [ ] Add package info for Cabal
- [ ] Implement fill to pad lines with spaces.
      The implementation of this seems dependent on the type of rendering
      (compact or pretty) to be done. Consider rendering Doc -> [String],
      and padding each element of the [String].
- [ ] Implement nest. As above, seems dependent on rendering / unsure how to
      handle Unions. 
