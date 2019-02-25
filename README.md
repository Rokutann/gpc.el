[![Build Status](https://travis-ci.org/mukuge/gpc.el.svg?branch=master)](https://travis-ci.org/mukuge/gpc.el)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# gpc.el

A general purpose cache facility for Emacs. It's light weight and buffer-local safe.

## Installation

Place `nalist.el` and `gpc.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'gpc)
```

See [the Github page](https://github.com/mukuge/nalist.el) for the detail of the `nalist` library.

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
* [gpc-copy](#gpc-copy-cache-from-buffer-to-buffer) `(cache from-buffer to-buffer)`
* [gpc-remove](#gpc-remove)
* [gpc-clear](#gpc-clear)
* [gpc-pairs](#gpc-pairs)
* [gpc-keys](#gpc-keys)
* [gpc-values](#gpc-values)
* [gpc-pair-exist-p](#gpc-pair-exist-p-key-cache-key-testfn-eq) `(key cache &key (testfn 'eq))`
* [gpc-pp](#gpc-pp-cache) `(cache)`

### Cache Lock Functions

* [gpc-lock](#gpc-lock-cache) `(cache)`
* [gpc-unlock](#gpc-unlock-cache) `(cache)`
* [gpc-get-lock-list](#gpc-get-lock-list-cache) `(cache)`
* [gpc-lock-clear](#gpc-lock-clear-cache) `(cache)`
* [gpc-lock-gc](#gpc-lock-gc-cache) `(cache)`
* [gpc-lock-pp](#gpc-lock-pp-cache) `(cache)`
* [gpc-locked-p](#gpc-locked-p-cache) `(cache)`

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

### gpc-init `(name spec-list)`

Bind NAME to a general purpose cache specified in SPEC-LIST.

General purpose cache, or ‘gpc’, is a cache facility which
enables you to store return values of fetch functions in a
variable for future reuse.

‘gpc’ uses two places to store information: One is the ordinary
variable binding , which keeps cached data, the other is the
symbol’s property list, where the specification of the cache is
kept with the key ‘gpc-cache-spec’.

Aside from the cache spec mechanism, a gpc cache is just a named
association list, or ‘nalist’.  Some of its cache access
functions is actually aliases to the corresponding functions in
the ’nalist’ library.’

A cache spec is implemented as a hash table whose key is a key of
a cache entry, and the value associated with each key is a
list, (initval fetchfn), which specifies the initial value and
fetch function of the cash entry.

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

### defcache `(name buffer-local doc-string &rest spec-list)`

Define NAME as a general purpose cache, and return a symbol.

This macro uses `defvar` internally, so the resulting symbol as a
variable is special, and DOC-STRING is stored in the symbol's
property list.

The resulting variable is initialized as an automatically
buffer-local variable if the value of BUFFER-LOCAL is
:buffer-local. Otherwise, as a global variable.

SPEC-LIST defines the specification of the cache: the initial
values and fetch functions.  See `gpc-init` for the detail.

```lisp
```

### gpc-overwrite-with-initvals `(cache)`

Overwrite the whole cache content with initvals in the CACHE spec.

```lisp
```

### gpc-make-local-variable

This is an alias of `nalist-make-local-variable`.

Create a buffer-local binding in the current buffer for NALIST.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

(fn NALIST)

```lisp
```

### gpc-make-variable-buffer-local

This is an alias of `nalist-make-variable-buffer-local`.

Mark NALIST automatically buffer local.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

It also sets the default value of NALIST to nil to avoid the
buffer-local variables in other buffers share the cons cells
through it.

(fn NALIST)

```lisp
```

### gpc-val

This is an alias of `nalist-get`.

Return the value of KEY in NALIST if found with TESTFN, otherwise DEFAULT.

The key lookup is done with TESTFN if non-nil, otherwise with
‘eq’.

(fn KEY NALIST &key DEFAULT (TESTFN 'eq))

```lisp
```

### gpc-fetch `(key cache)`

Fetch the value of KEY in CACHE by calling its fetch function.

It returns the fetched value.

```lisp
```

### gpc-fetch-all `(cache)`

Fetch values for all keys in the CACHE spec.

```lisp
```

### gpc-get `(key cache &key (force nil))`

Return the value of KEY in CACHE by calling the fetchfn if needed.

It uses fetchfn to get the value when FORCE is non-nil.

(fn KEY CACHE &key (FORCE nil))

```lisp
```

### gpc-set

This is an alias of `nalist-set`.

Find the pair with KEY in NALIST with TESTFN, and set its value to VALUE.

This macro destructively changes the value of the pair with KEY
into VALUE if the pair with KEY already exists, otherwise add a
new pair with KEY and VALUE to NALIST.

NALIST needs to be a symbol without a quote to access the correct
binding in its context.

It returns VALUE.

(fn KEY VALUE NALIST &key (TESTFN ''eq))

```lisp
```

### gpc-copy `(cache from-buffer to-buffer)`

Copy the content of CACHE from FROM-BUFFER to TO-BUFFER.

Use this function when CACHE is buffer-local or automatically
buffer-local.

```lisp
```

### gpc-remove

This is an alias of `nalist-remove`.

Remove the pair with KEY from NALIST if found with TESTFN.

(fn KEY NALIST &key (TESTFN ''eq))

```lisp
```

### gpc-clear

This is an alias of `nalist-clear`.

Set NALIST nil.

(fn NALIST)

```lisp
```

### gpc-pairs

This is an alias of `nalist-pairs`.

Return a list consisting all the pairs in NALIST.

(fn NALIST)

```lisp
```

### gpc-keys

This is an alias of `nalist-keys`.

Return a list consisting all the keys in NALIST.

(fn NALIST)

```lisp
```

### gpc-values

This is an alias of `nalist-values`.

Return a list consisting all the values in NALIST.

(fn NALIST)

```lisp
```

### gpc-pair-exist-p `(key cache &key (testfn 'eq))`

Return t if CACHE has an entry with KEY, otherwise nil.

(fn KEY CACHE &key (TESTFN 'eq))

```lisp
```

### gpc-pp `(cache)`

Pretty print and return the whole content of CACHE.

```lisp
```

### gpc-lock `(cache)`

Lock the values in CACHE.

After locking, ‘gpc-fetch’ acts like ‘gpc-val’.  This gpc lock
feature is intended to be used with buffer-local variables.

```lisp
```

### gpc-unlock `(cache)`

Unlock CACHE.

```lisp
```

### gpc-lock-clear `(cache)`

Set the lock list of CACHE nil.

```lisp
```

### gpc-lock-gc `(cache)`

Remove killed buffers from the lock list of CACHE.

```lisp
```

### gpc-lock-pp `(cache)`

Pretty print the locked buffers for CACHE.

```lisp
```

### gpc-get-lock-list `(cache)`

Return the lock list of CACHE.

```lisp
```

### gpc-locked-p `(cache)`

Return t if CACHE is locked, otherwise nil.

```lisp
```

### gpc-set-spec `(symbol hash-table)`

Set HASH-TABLE in SYMBOL’s property list as a cache spec.

HASH-TABLE should contain a cache spec following the spec
description format.  See ‘gpc-init’ for the detail.

```lisp
```

### gpc-get-spec `(cache)`

Return the spec of CACHE.

```lisp
```

### gpc-spec-set-entry `(key initval fetchfn cache)`
Set the CACHE spec entry whose key is KEY to have the value (INITVAL FETCHFN).

```lisp
```

### gpc-spec-get-entry `(key cache)`

Return a CACHE spec entry with KEY if exists, otherwise nil.

A CACHE spec entry is a list: (KEY initval fetchfn).

```lisp
```

### gpc-spec-get-initval `(key cache)`

Get the initval of the CACHE spec entry with KEY.

```lisp
```

### gpc-spec-get-fetchfn `(key cache)`

Get the fetch function of the CACHE spec entry with KEY.

```lisp
```

### gpc-spec-map `(function cache)`

Call FUNCTION for all keys and values in CACHE.

The function should have three arguments, which are filled by
this macro with a key, its initval, and its fetchfn.

```lisp
```

### gpc-spec-keyp `(key cache)`

Return t if KEY is a key in the CACHE’s spec, otherwise nil.

```lisp
```

### gpc-pp-spec (cache)

Pretty print the CACHE spec, and return it.

```lisp
```
