# purescript-tidy

A syntax tidy-upper (formatter) for PureScript.

## Install

```sh
npm install -g purs-tidy
```

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

### Auto-formatting in VS Code

First, install the [Run on Save](https://marketplace.visualstudio.com/items?itemName=emeraldwalk.RunOnSave) extension so that you can execute a command when your file is saved. Then, add this to your `settings.json`:

```json
  "emeraldwalk.runonsave": {
    "commands": [
      {
        "match": ".purs",
        "cmd": "$TIDY_DIR/bin/index.js format-in-place ${relativeFile}"
      }
    ]
  }
```

...where `$TIDY_DIR` is replaced with the location of a checkout of `purescript-tidy`.
