# purescript-tidy

A syntax tidy-upper (formatter) for PureScript.

## Development

### Running `bin`

```sh
spago -x ./bin/spago.dhall build
./bin/index.js --help
```

### Running `test`

To accept snapshot tests:
```sh
spago -x ./test/spago.dhall test -a "--accept"
```

### Generating the built-in operator table

```sh
spago -x ./script/spago.dhall run -m GenerateDefaultOperatorsModule
```
