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

Initialize SYMBOL as a general purpose cache with SPEC-LIST.

A general purpose cache, or ‘gpc’, is a cache facility which
enables you to store return values of fetch functions for future
reuse.  It’s like memoization inside-out.

A cache entity in ‘gpc’ is a data structure with a name, which
utilizes symbol’s features to implement its mechanism between
cache and its specification.

It uses two places to store information: One is the ordinary
variable binding to the symbol, which keeps the content of cache,
the other is the symbol’s property list, where the specification
of cache is associated with the key ‘gpc-cache-spec’.

As for the cache content, a gpc is just a named association list,
or ‘nalist’.  Most of the cache access functions in ‘gpc’ is
actually aliases to the corresponding functions in nalist.’

A cache spec is a hash table whose keys are the keys of the cache
content, and the value associated with each key is in the
format (initval fetchfn), or a list of its initial value and
fetch function.

```lisp
```

#### defcache `(symbol buffer-local doc-string &rest spec-list)`

{{defcache}}

```lisp
```

#### gpc-overwrite-with-initvals `(cache)`

Overwrite the cache content with initvals in the CACHE’s spec.

```lisp
```

#### gpc-make-local-variable

This is an alias of `nalist-make-local-variable`.

Create a buffer-local binding in the current buffer for NALIST.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

(fn NALIST)

```lisp
```

#### gpc-make-variable-buffer-local

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

### Cache Access Functions

#### gpc-val

This is an alias of `nalist-get`.

Return the value of KEY in NALIST if found with TESTFN, otherwise DEFAULT.

The key lookup is done with TESTFN if non-nil, otherwise with
‘eq’.

(fn KEY NALIST &key DEFAULT (TESTFN 'eq))

```lisp
```

#### gpc-fetch `(key cache)`

Fetch the value of KEY in CACHE with its fetch function.

It returns the value associated with KEY.

```lisp
```

#### gpc-fetch-all `(cache)`

Fetch values of all keys in the CACHE’s spec.

```lisp
```

#### gpc-get `(key cache &key (force nil))`

Return the value of KEY in CACHE by calling the fetchfn if needed.

(fn KEY CACHE &key (FORCE nil))

```lisp
```

#### gpc-set

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

#### gpc-remove

This is an alias of `nalist-remove`.

Remove the pair with KEY from NALIST if found with TESTFN.

(fn KEY NALIST &key (TESTFN ''eq))

```lisp
```

#### gpc-clear

This is an alias of `nalist-clear`.

Set NALIST nil.

(fn NALIST)

```lisp
```

#### gpc-pairs

This is an alias of `nalist-pairs`.

Return a list consisting all the pairs in NALIST.

(fn NALIST)

```lisp
```

#### gpc-keys

This is an alias of `nalist-keys`.

Return a list consisting all the keys in NALIST.

(fn NALIST)

```lisp
```

#### gpc-values

This is an alias of `nalist-values`.

Return a list consisting all the values in NALIST.

(fn NALIST)

```lisp
```

#### gpc-pair-exist-p `(key cache &key (testfn 'eq))`

Return t if CACHE has an entry with KEY, otherwise nil.

(fn KEY CACHE &key (TESTFN 'eq))

```lisp
```

#### gpc-pp `(cache)`

Pretty print and return the content of CACHE.

```lisp
```

### Cache Spec Access Functions

#### gpc-set-spec `(symbol hash-table)`

Set HASH-TABLE in SYMBOL’s plist as a cache spec.

HASH-TABLE should contain a cache spec following the spec
description format.  See ‘gpc-init’ for the detail.

```lisp
```

#### gpc-get-spec `(cache)`

Return the spec of CACHE.

A cache spec is a hash table.

```lisp
```

#### gpc-spec-set-entry `(key initval fetchfn cache)`

Set the CACHE’s spec entry whose key is KEY to have the value (INITVAL FETCHFN).

```lisp
```

#### gpc-spec-get-entry `(key cache)`

Return the entry with KEY if it’s in the CACHE’s spec, otherwise nil.

```lisp
```

#### gpc-spec-get-initval `(key cache)`

Get the initval of the pair with KEY in the CACHE’s spec.

```lisp
```

#### gpc-spec-get-fetchfn `(key cache)`

Get the fetch function of the pair with KEY in the CACHE’s spec.

```lisp
```

#### gpc-spec-map `(function cache)`

Call function for all keys and values of the CACHE’s spec.

The function should have three arguments, which are filled by this macro with a key, its initval, and its fetchfn in this order.

```lisp
```

#### gpc-spec-keyp `(key cache)`

Return t if KEY is a key in the CACHE’s spec, otherwise nil.

```lisp
```

#### gpc-pp-spec (cache)

Pretty print the spec of CACHE, and return it.

```lisp
```
