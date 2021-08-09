# purescript-tidy

A syntax tidy-upper (formatter) for PureScript.

## Install

```console
$ npm install -g purs-tidy
```

## Usage

You can use `purs-tidy` to format files in place or via STDIN / STDOUT (which is useful for editor integration):

##### Formatting a collection of files in place:

```console
$ purs-tidy format-in-place "src/**/*.purs"
```

##### Using STDIN to format a file:

```console
$ purs-tidy format < MyFile.purs
```

You can also use `purs-tidy` to verify whether files have already been formatted. This is often useful to verify, in continuous integration, that all project files are formatted according to the configuration. Files that would be changed by running `format-in-place` are listed out.


##### Verifying files are formatted

```console
$ purs-tidy check "src/**/*.purs"
All files are formatted.
```

### Configuration

You can see all configuration that `purs-tidy` accepts using the `--help` flag for the command you are using:

```console
$ purs-tidy format-in-place --help
```

Some common options include:

- `--indent` to set the number of spaces used in indentation, which defaults to 2 spaces
- `--arrow-first` or `--arrow-last` to control whether type signatures put arrows first on the line or last on the line (purty-style), which defaults to arrow-first.

You can generate a `.tidyrc.json` using the `generate-config` command. If a `.tidyrc.json` file is found, it will be used in lieu of CLI arguments.

### Operator Precedence

To support correct operator precedence without having to parse your entire
source tree (potentially for a single file), `purs-tidy` uses a pre-baked
operator precedence table. By default, `purs-tidy` ships with a table built
from the core and contrib organizations. If you need support for more
operators, you can generate your own table using the `generate-operators`
command.

```console
$ purs-tidy generate-operators $(spago sources) > .tidyoperators
$ purs-tidy generate-config --arrow-first --unicode-never --operators .tidyoperators
```

## Development

### Running `bin`

```console
$ spago -x ./bin/spago.dhall build
$ ./bin/index.js --help
```

### Running `test`

To accept snapshot tests:

```console
$ spago -x ./test/spago.dhall test -a "--accept"
```

### Generating the built-in operator table

```console
$ spago -x ./script/spago.dhall run -m GenerateDefaultOperatorsModule
```

## Editor Support

* [Vim](#vim)
* [VS Code](#vs-code)

### Vim

#### via [ALE](https://github.com/dense-analysis/ale)

Add to your other fixers `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let b:ale_fixers = { 'purescript': [ 'purs-tidy' ] }
" suggested to fix on save
let g:ale_fix_on_save = 1
```

#### via [Neoformat](https://github.com/sbdchd/neoformat)

Add to your `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let g:neoformat_enabled_purescript = ['purs-tidy']
```

### VS Code

#### via [Run on Save](https://marketplace.visualstudio.com/items?itemName=emeraldwalk.RunOnSave) 

Add this to your `settings.json`:

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
