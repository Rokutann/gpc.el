# gpc.el

A general purpose cache facility for Emacs. It's light weight and buffer-local safe.

## Installation

Place `gpc.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'gpc)
```

## API

### Initialization Functions

* [gpc-init](#gpct-init-symbol-spec-list) `(symbol spec-list)`
* [defcache](#defcache-symbol-buffer-local-doc-string-rest-spec-list) `(symbol buffer-local doc-string &rest spec-list)`
* [gpc-overwrite-with-initvals](#gpc-overwrite-with-initvals-cache) `(cache)`
* [gpc-make-local-variable](#gpc-make-local-variable)
* [gpc-make-variable-buffer-local](#gpc-make-variable-buffer-local)

### Cache Access Functions

* [gpc-val](#gpc-val)
* [gpc-fetch](#gpc-fetch-key-cach) `(key cache)`
* [gpc-fetch-all](#gpc-fetch-all-cache) `(cache)`
* [gpc-get](#gpc-get-key-cache-key-force-nil) `(key cache &key (force nil))`
* [gpc-set](#gpc-set)
* [gpc-remove](#gpc-remove)
* [gpc-clear](#gpc-clear)
* [gpc-pairs](#gpc-pairs)
* [gpc-keys](#gpc-keys)
* [gpc-values](#gpc-values)
* [gpc-pair-exist-p](#gpc-pair-exist-p-key-cache-key-testfn-eq) `(key cache &key (testfn 'eq))`
* [gpc-pp](#gpc-pp-cache) `(cache)`

### Cache Spec Access Functions

* [gpc-set-spec](#gpc-set-spec-symbol-hash-table) `(symbol hash-table)`
* [gpc-get-spec](#gpc-get-spec-cache) `(cache)`
* [gpc-spec-set-entry](#gpc-spec-set-entry-key-initval-fetchfn-cache) `(key initval fetchfn cache)`
* [gpc-spec-get-entry](#gpc-spec-get-entry-key-cache) `(key cache)`
* [gpc-spec-get-initval](#gpc-spec-get-initval-key-cache) `(key cache)`
* [gpc-spec-get-fetchfn](#gpc-spec-get-fetchfn-key-cache) `(key cache)`
* [gpc-spec-map](#gpc-spec-map-function-cache) `(function cache)`
* [gpc-spec-keyp](#gpc-spec-keyp-key-cache) `(key cache)`
* [gpc-pp-spec](#gpc-pp-spec-cache) (cache)


## Documentation and Examples

### gpct-init `(symbol spec-list)`

{{gpc-init}}

```lisp
(gpc-init acache
  '((buffer-size 0 (lambda ()
                     (buffer-size)))
    (uptime nil (lambda ()
                  (s-chop-suffix "\n" (shell-command-to-string "uptime"))))
    (joke "How do you make holy water? You boil the hell out of it."
          (lambda ()
            (with-temp-buffer
              (call-process "curl" nil t nil "-sb" "-H" "Accept: text/plain" "https://icanhazdadjoke.com/")
              (s-chop-suffix "\n" (buffer-string)))))
    (buffer-memory 0 (lambda ()
                       (* 8 (gpc-get 'buffer-size acache))))))
```

### defcache `(symbol buffer-local doc-string &rest spec-list)`

Define SYMBOL as a general purpose cache or gpc, and return SYMBOL.

This macro uses `defvar' internally. So, the resulting variable
is special and can have a DOC-STRING.  It makes the variable
automatically buffer-local if BUFFER-LOCAL is :buffer-local,
otherwise global.

The cache is initialized as an automatically buffer-local
variable if the value of BUFFER-LOCAL is
'buffer-local. Otherwise, as a global variable defined by
`defvar'.

SPEC-LIST defines the specification of the cache: the initial
value and fetch function for each key.  See `gpc-init' for the
detail.

```lisp
```

### gpc-overwrite-with-initvals `(cache)`

{{gpc-overwrite-with-initvals}}

```lisp
```

### gpc-make-local-variable

This is an alias of `nalist-make-local-variable`.

{{gpc-make-local-variable}}

```lisp
```

### gpc-make-variable-buffer-local

This is an alias of `nalist-make-variable-buffer-local`.

{{gpc-make-variable-buffer-local}}

```lisp
```

### gpc-val

This is an alias of `nalist-get`.

{{gpc-val}}

```lisp
```

### gpc-fetch `(key cache)`

{{gpc-fetch}}

```lisp
```

### gpc-fetch-all `(cache)`

{{gpc-fetch-all}}

```lisp
```

### gpc-get `(key cache &key (force nil))`

{{gpc-get}}

```lisp
```

### gpc-set

This is an alias of `nalist-set`.

{{gpc-set}}

```lisp
```

### gpc-remove

This is an alias of `nalist-remove`.

{{gpc-remove}}

```lisp
```

### gpc-clear

This is an alias of `nalist-clear`.

{{gpc-clear}}

```lisp
```

### gpc-pairs

This is an alias of `nalist-pairs`.

{{gpc-pairs}}

```lisp
```

### gpc-keys

This is an alias of `nalist-keys`.

{{gpc-keys}}

```lisp
```

### gpc-values

This is an alias of `nalist-values`.

{{gpc-values}}

```lisp
```

### gpc-pair-exist-p `(key cache &key (testfn 'eq))`

{{gpc-pair-exist-p}}

```lisp
```

### gpc-pp `(cache)`

{{gpc-pp}}

```lisp
```

### gpc-set-spec `(symbol hash-table)`

{{gpc-set-spec}}

```lisp
```

### gpc-get-spec `(cache)`

{{gpc-get-spec}}

```lisp
```

### gpc-spec-set-entry `(key initval fetchfn cache)`
{{gpc-spec-set-entry}}

```lisp
```

### gpc-spec-get-entry `(key cache)`

{{gpc-spec-get-entry}}

```lisp
```

### gpc-spec-get-initval `(key cache)`

{{gpc-spec-get-initval}}

```lisp
```

### gpc-spec-get-fetchfn `(key cache)`

{{gpc-spec-get-fetchfn}}

```lisp
```

### gpc-spec-map `(function cache)`

{{gpc-spec-map}}

```lisp
```

### gpc-spec-keyp `(key cache)`

{{gpc-spec-keyp}}

```lisp
```

### gpc-pp-spec (cache)

{{gpc-pp-spec}}

```lisp
```
