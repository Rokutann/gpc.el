[![Build Status](https://travis-ci.org/mukuge/gpc.el.svg?branch=dev)](https://travis-ci.org/mukuge/gpc.el)
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

{{gpc-init}}

```lisp
(gpc-init cache
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
                       (* 8 (gpc-get 'buffer-size cache))))))
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
(defcache cache :buffer-local
  "An automatically buffer-local cache to show how to use `defcache'."
  (buffer-size 0 (lambda ()
                   (buffer-size)))
  (uptime nil (lambda ()
                (s-chop-suffix "\n" (shell-command-to-string "uptime"))))
  (joke "How do you make holy water? You boil the hell out of it."
        (lambda ()
          (with-temp-buffer
            (call-process "curl" nil t nil "-sb" "-H" "Accept: text/plain" "https://icanhazdadjoke.com/")
            (s-chop-suffix "\n" (buffer-string)))))
  (buffer-memory 0 (lambda ()
                     (* 8 (gpc-get 'buffer-size cache)))))
```

### gpc-overwrite-with-initvals `(cache)`

{{gpc-overwrite-with-initvals}}

```lisp
(gpc-overwrite-with-initvals cache) ;; => nil
```

### gpc-make-local-variable

This is an alias of `nalist-make-local-variable`.

{{gpc-make-local-variable}}

```lisp
(gpc-make-local-variable cache) ;; => cache
;; Hereafter, cache is buffer-local in this buffer.
```

### gpc-make-variable-buffer-local

This is an alias of `nalist-make-variable-buffer-local`.

{{gpc-make-variable-buffer-local}}

```lisp
(gpc-make-local-variable cache) ;; => cache
;; Hereafter, cache is automatically buffer-local.
```

### gpc-val

This is an alias of `nalist-get`.

{{gpc-val}}

```lisp
(gpc-val 'system cache) ;; => "Hurd"
```

### gpc-fetch `(key cache)`

{{gpc-fetch}}

```lisp
(gpc-fetch 'system cache) ;; => "Darwin"
```

### gpc-fetch-all `(cache)`

{{gpc-fetch-all}}

```lisp
(gpc-fetch-all cache) ;; => nil
;; All the values of cache entries are filled by calling fetchfns.
```

### gpc-get `(key cache &key (force nil))`

{{gpc-get}}

```lisp
(gpc-get 'buffer-size cache) ;; => 1256
```

### gpc-set

This is an alias of `nalist-set`.

{{gpc-set}}

```lisp
(gpc-set 'buffer-size 1000 cache) ;; => 1000

```

### gpc-copy `(cache from-buffer to-buffer)`

{{gpc-copy}}

```lisp
(gpc-copy cache parent-buffer (current-buffer))
;; => ((buffer-memory . 5608)
;;     (joke . "I couldn't get a reservation at the library. They were completely booked.")
;;     (uptime . "21:51  up 16 days,  1:18, 4 users, load averages: 2.48 2.67 2.63")
;;     (buffer-size . 701))
```

### gpc-remove

This is an alias of `nalist-remove`.

{{gpc-remove}}

```lisp
(gpc-remove 'buffer-size cache) ;; => 701
```

### gpc-clear

This is an alias of `nalist-clear`.

{{gpc-clear}}

```lisp
(gpc-clear cache) ;; => nil
```

### gpc-pairs

This is an alias of `nalist-pairs`.

{{gpc-pairs}}

```lisp
(gpc-pairs cache)
;; => ((buffer-memory . 6320)
;;     (joke . "Why couldn't the kid see the pirate movie? Because it was rated arrr!")
;;     (uptime . "21:56  up 16 days,  1:23, 4 users, load averages: 2.94 2.61 2.59")
;;     (buffer-size . 790))
```

### gpc-keys

This is an alias of `nalist-keys`.

{{gpc-keys}}

```lisp
(gpc-keys cache) ;; => (buffer-memory joke uptime buffer-size)
```

### gpc-values

This is an alias of `nalist-values`.

{{gpc-values}}

```lisp
(gpc-values cache)
;; => (6320 "Why couldn't the kid see the pirate movie? Because it was rated arrr!"
;;     "21:56 up 16 days, 1:23, 4 users, load averages: 2.94 2.61 2.59" 790)

```

### gpc-pair-exist-p `(key cache &key (testfn 'eq))`

{{gpc-pair-exist-p}}

```lisp
(gpc-pair-exist-p 'buffer-size cache) ;; => 790
```

### gpc-pp `(cache)`

{{gpc-pp}}

```lisp
(gpc-pp cache)
;; => ((buffer-memory . 6320)
;;     (joke . "Why couldn't the kid see the pirate movie? Because it was rated arrr!")
;;     (uptime . "21:56  up 16 days,  1:23, 4 users, load averages: 2.94 2.61 2.59")
;;     (buffer-size . 790))
;; Display this value in the mini-buffer.
```

### gpc-lock `(cache)`

{{gpc-lock}}

```lisp
(gpc-lock cache) ;; => (#<buffer *scratch*>)
```

### gpc-unlock `(cache)`

{{gpc-unlock}}

```lisp
(gpc-unlock cache) ;; => nil
```

### gpc-lock-clear `(cache)`

{{gpc-lock-clear}}

```lisp
(gpc-lock-clear cache) ;; => nil
```

### gpc-lock-gc `(cache)`

{{gpc-lock-gc}}

```lisp
(gpc-lock-gc cache) ;; => (#<buffer *scratch*>)
```

### gpc-lock-pp `(cache)`

{{gpc-lock-pp}}

```lisp
(gpc-lock-pp cache)
;; Display "(#<buffer *scratch*>)" in the mini-buffer.
```

### gpc-get-lock-list `(cache)`

{{gpc-get-lock-list}}

```lisp
(gpc-get-lock-list cache) ;; => (#<buffer *scratch*>)
```

### gpc-locked-p `(cache)`

{{gpc-locked-p}}

```lisp
(gpc-locked-p cache) ;; => t
```

### gpc-set-spec `(symbol hash-table)`

{{gpc-set-spec}}

```lisp
(setq spec-alist '((buffer-size 0 (lambda ()
                                    (buffer-size)))
                   (uptime nil (lambda ()
                                 (s-chop-suffix "\n" (shell-command-to-string "uptime"))))
                   (joke "How do you make holy water? You boil the hell out of it."
                         (lambda ()
                           (with-temp-buffer
                             (call-process "curl" nil t nil "-sb" "-H" "Accept: text/plain" "https://icanhazdadjoke.com/")
                             (s-chop-suffix "\n" (buffer-string)))))
                   (buffer-memory 0 (lambda ()
                                      (* 8 (gpc-get 'buffer-size cache))))))
(setq spec-ht (gpc-util-alist-to-hash spec-alist))
(gpc-set-spec bcache spec-ht) ;; => #s(hash-table ...)
```

### gpc-get-spec `(cache)`

{{gpc-get-spec}}

```lisp
(gpc-get-spec cache) ;; => #s(hash-table ...)
```

### gpc-spec-set-entry `(key initval fetchfn cache)`
{{gpc-spec-set-entry}}

```lisp
(gpc-spec-set-entry 'buffer-momory-2 0 '(lambda () (* 16 (gpc-get 'buffer-size cache))) cache) ;; => (0 (lambda nil ...))
```

### gpc-spec-get-entry `(key cache)`

{{gpc-spec-get-entry}}

```lisp
(gpc-spec-get-entry 'buffer-memory cache) ;; => (0 (lambda nil (* 8 (gpc-get ... cache))))
```

### gpc-spec-get-initval `(key cache)`

{{gpc-spec-get-initval}}

```lisp
(gpc-spec-get-initval 'buffer-memory cache) ;; => 0
```

### gpc-spec-get-fetchfn `(key cache)`

{{gpc-spec-get-fetchfn}}

```lisp
(gpc-spec-get-fetchfn 'buffer-memory cache) ;; => (lambda nil (* 8 (gpc-get (quote buffer-size) cache)))
```

### gpc-spec-map `(function cache)`

{{gpc-spec-map}}

```lisp
(gpc-spec-map #'(lambda (k v f) (message "%s: %s" k f))  cache)
;; Write out these to *Message*.
;; joke: (lambda nil (with-temp-buffer ... ))
;; buffer-momory: (lambda nil (* 8 (gpc-get (quote buffer-size) cache)))
;; buffer-momory-2: (lambda nil (* 16 (gpc-get (quote buffer-size) cache)))
;; buffer-size: (lambda nil (buffer-size))
;; uptime: (lambda nil (s-chop-suffix ...))
```

### gpc-spec-keyp `(key cache)`

{{gpc-spec-keyp}}

```lisp
(gpc-spec-keyp 'buffer-memory cache) ;; => t
```

### gpc-pp-spec (cache)

{{gpc-pp-spec}}

```lisp
(gpc-pp-spec cache)
;; => ((buffer-momory-2 0 (lambda nil (* 16 ...)))
;;     (buffer-memory 0 (lambda nil (* 8 ...)))
;;     (joke "How do you make holy water? You boil the hell out of it." (lambda nil (with-temp-buffer ... ...)))
;;     (uptime nil (lambda nil (s-chop-suffix "
;;     " ...)))
;;     (buffer-size 0 (lambda nil (buffer-size))))
;; Dispaly it in the mini-buffer as well.
```
