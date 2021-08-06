# purescript-tidy

A syntax tidy-upper (formatter) for PureScript.

## Install

```sh
npm install -g purs-tidy
```

## Usage

You can use `purs-tidy` to format files in place or via STDIN / STDOUT (which is useful for editor integration):

```sh
# Formatting a collection of files in place:
$ purs-tidy format-in-place "src/**/*.purs"

# Using STDIN to format a file:
$ purs-tidy format < MyFile.purs
```

### Configuration

You can view the full configuration that `purs-tidy` accepts via flags here:
https://github.com/natefaubion/purescript-tidy/blob/main/bin/Bin/FormatOptions.purs

Some common options include:

* `--indent` to set the number of spaces used in indentation, which defaults to 2 spaces
* `--arrow-first` or `--arrow-last` to control whether type signatures put arrows first on the line or last on the line (purty-style), which defaults to arrow-last.

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
