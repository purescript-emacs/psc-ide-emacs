# CHANGELOG

## 2017-04-20
- Displays completions with their already imported qualifiers. This can be toggled
off with `(setq psc-ide-add-qualification-on-completion nil)`.

- Displays the result of the load command in the message buffer

## 2017-04-18
- Switched to purescript-0.11 compatible `purs` executable by default. The 0.10
binaries can be regained with `(setq psc-ide-use-purs nil)`, but we'll remove
this switch when purescript-0.12 is released.
