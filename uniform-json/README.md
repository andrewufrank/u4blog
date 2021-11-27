Uniform approach to use JSON and YAML

To use JSON and YAML in Haskell - connecting to other tools which expects input or produce output in those formats - is a challenge.
A set of functions which were used in a few projects - some connected to `pandoc` - are here collected and arranged to work in the uniform style (use ErrIO, `text`).  