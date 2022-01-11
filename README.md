# purescript-tidy

A syntax tidy-upper (formatter) for PureScript.

## Install

```console
$ npm install -g purs-tidy
```

Also available for [Nix](https://nixos.org/) via [Easy PureScript Nix](https://github.com/justinwoo/easy-purescript-nix)

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
$ spago sources | xargs purs-tidy generate-operators > .tidyoperators
$ purs-tidy generate-config --arrow-first --unicode-never --operators .tidyoperators
```

## Editor Support

* [Spacemacs](#spacemacs)
* [Vim](#vim)
* [VS Code](#vs-code)

### Spacemacs

[Spacemacs' Purescript layer](https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/purescript)
supports formatting using purs-tidy out of the box.

You can run the formatter manually with either `M-x spacemacs/purescript-format` or with the shortcut `SPC m =`.

To enable automatic formatting of the buffer on save, enable `purescript-fmt-on-save` in your spacemacs config:

```elisp
  (setq-default dotspacemacs-configuration-layers '(
    (purescript :variables
                purescript-fmt-on-save t)))
```


### Vim

#### via [ALE](https://github.com/dense-analysis/ale)

Add to your other fixers `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let b:ale_fixers = { 'purescript': [ 'purstidy' ] }
" suggested to fix on save
let g:ale_fix_on_save = 1
```

#### via [Neoformat](https://github.com/sbdchd/neoformat)

Add to your `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let g:neoformat_enabled_purescript = ['purstidy']
```

### VS Code

#### via [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript)

The PureScript IDE plugin for VS Code supports `purs-tidy` as a built-in formatter in versions after `0.25.1`. Choose `purs-tidy` from the list of supported formatters in the settings, or add this to your `settings.json`:

```json
"purescript.formatter": "purs-tidy"
```

## Development

### Requirements

* `purs`: 0.14
* `spago`: 0.20
* `node`: 14

### Running `bin`

```console
$ spago -x ./bin/spago.dhall build
$ ./bin/index.js --help
```

If you would like to use your local build of `purs-tidy` in your editor, use path to `bin/index.js` instead of the `purs-tidy` binary in your settings. For example, instead of setting the format command to `purs-tidy format`, set it to `$TIDY_DIR/bin/index.js format` where `$TIDY_DIR` is the location of your checkout of this repository.

### Running `test`

To accept snapshot tests:

```console
$ spago -x ./test/spago.dhall test -a "--accept"
```

### Generating the built-in operator table

```console
$ spago -x ./script/spago.dhall run -m GenerateDefaultOperatorsModule
```
