# gpc.el

A general purpose cache facility for Emacs. It's light weight and buffer-local safe.

## Installation

Place `gpc.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'gpc)
```

## Documentation and Examples

### Initialization Functions

#### gpct-init `(symbol spec-list)`

{{gpc-init}}

```lisp
```

#### defcache `(symbol buffer-local doc-string &rest spec-list)`

{{defcache}}

```lisp
```

#### gpc-overwrite-with-initvals `(cache)`

{{gpc-overwrite-with-initvals}}

```lisp
```

#### gpc-make-local-variable

This is an alias of `nalist-make-local-variable`.

{{gpc-make-local-variable}}

```lisp
```

#### gpc-make-variable-buffer-local

This is an alias of `nalist-make-variable-buffer-local`.

{{gpc-make-variable-buffer-local}}

```lisp
```

### Cache Access Functions

#### gpc-val

This is an alias of `nalist-get`.

{{gpc-val}}

```lisp
```

#### gpc-fetch `(key cache)`

{{gpc-fetch}}

```lisp
```

#### gpc-fetch-all `(cache)`

{{gpc-fetch-all}}

```lisp
```

#### gpc-get `(key cache &key (force nil))`

{{gpc-get}}

```lisp
```

#### gpc-set

This is an alias of `nalist-set`.

{{gpc-set}}

```lisp
```

#### gpc-remove

This is an alias of `nalist-remove`.

{{gpc-remove}}

```lisp
```

#### gpc-clear

This is an alias of `nalist-clear`.

{{gpc-clear}}

```lisp
```

#### gpc-pairs

This is an alias of `nalist-pairs`.

{{gpc-pairs}}

```lisp
```

#### gpc-keys

This is an alias of `nalist-keys`.

{{gpc-keys}}

```lisp
```

#### gpc-values

This is an alias of `nalist-values`.

{{gpc-values}}

```lisp
```

#### gpc-pair-exist-p `(key cache &key (testfn 'eq))`

{{gpc-pair-exist-p}}

```lisp
```

#### gpc-pp `(cache)`

{{gpc-pp}}

```lisp
```

### Cache Spec Access Functions

#### gpc-set-spec `(symbol hash-table)`

{{gpc-set-spec}}

```lisp
```

#### gpc-get-spec `(cache)`

{{gpc-get-spec}}

```lisp
```

#### gpc-spec-set-entry `(key initval fetchfn cache)`

{{gpc-spec-set-entry}}

```lisp
```

#### gpc-spec-get-entry `(key cache)`

{{gpc-spec-get-entry}}

```lisp
```

#### gpc-spec-get-initval `(key cache)`

{{gpc-spec-get-initval}}

```lisp
```

#### gpc-spec-get-fetchfn `(key cache)`

{{gpc-spec-get-fetchfn}}

```lisp
```

#### gpc-spec-map `(function cache)`

{{gpc-spec-map}}

```lisp
```

#### gpc-spec-keyp `(key cache)`

{{gpc-spec-keyp}}

```lisp
```

#### gpc-pp-spec (cache)

{{gpc-pp-spec}}

```lisp
```
