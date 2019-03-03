# CHANGELOG

## 2019-03-03 Automatically turn off completion, type on hover, and flycheck if server isn't running

- To figure out if the server isn't running we remember if the last
  connection attempt failed. By triggering an explicit request (asking
  for a type explicitly/go-to-definition/manual autocompletion) you
  can force a re-check for an externally started server. Running
  `psc-ide-server-start` also resets the behaviour.

## 2019-01-22 Detect and handle spago projects

## 2019-01-15 No longer uses shell-expansion when calling the psc-package command

- This might mean that Windows users that use a wrapped psc-package executable will
  need to put the actual executable on the path

## 2018-09-29 Automatically detects psc-package projects and sets globs accordingly

- `psc-ide-source-globs` should now only specify non-dependency source
  files (It's defaulted to `'("src/**/*.purs" "test/**/*.purs")`)

- The plugin will now detect psc-package projects and call psc-package
  sources to determine what source globs to pass to the editor

- A new `psc-ide-force-user-globs` variable was introduced to override
  the new behaviour and force the user specified globs

## 2018-05-17 Don't try to fix up temporary paths for flycheck (there shouldn't be any in the compiler output)
## 2018-05-17 Expand paths relative to the ide servers directory for go-to-definition

## 2018-01-01 Fix adding imports for symbols

## 2017-11-22 Displays docstrings with company
You can bring up the documentation for the current completion with C-h during
completion

## 2017-11-22 Adds `psc-ide-editor-mode` variable
This sets the --editor-mode flag on the server executable if set to true. It
defaults to false for backwards compatibility, but will default to true in a
while

## 2017-08-17
- Added a LICENSE file and the appropriate headers

## 2017-04-20
- Fixes a bug where large responses would cause psc-ide-send-sync to fail

## 2017-04-20
- Displays completions with their already imported qualifiers. This can be toggled
off with `(setq psc-ide-add-qualification-on-completion nil)`.

- Displays the result of the load command in the message buffer

## 2017-04-18
- Switched to purescript-0.11 compatible `purs` executable by default. The 0.10
binaries can be regained with `(setq psc-ide-use-purs nil)`, but we'll remove
this switch when purescript-0.12 is released.
