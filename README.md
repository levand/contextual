# contextual

A small library for Clojure and ClojureScript providing associatve
data structures that store metadata about their context in some larger
data strutcure.

## Rationale

A common idiom in functional programming is to walk a tree-like data
structure of nested maps or vectors, processing each "node" of the
tree with a function..

Sometimes, it is necessary to know "where" a given node is located
within the data structure as a whole. For example, this is often the
case when building a user intervace, so user interaction events can be
applied back to the same logical location in the tree.

Normally, this requires passing around additional book-keeping
arguments to each function. Each function must be passed not just the
node itself, but its "path" or context within the data structure as a
whole.

This library provides contextual data structures for maps and vectors
that track their *own* context, from some arbitrary root. They implement
the full set of persistent map and vector interfaces and are full
substitutes for Clojure(Script)'s map and vector datatypes.

At any point, an application may query a contextual object for its
path relative to the root. Retreiving a "child" map or vector from a
contextual data structure will yield another contextual object, with a
path reflecting its key or index within its "parent". This is entirely
seamless and requires no special functions or arguments. Intermediate
functions can be agnostic to the fact that they are operating on
contextual objects vs normal data structures.

Creating a contextual object or retrieving its path are constant-time
operations.

For the purposes of equality semantics, contextual objects are always
equivalent to their non-contextual counterparts. The path of a
contextual object is not included in equality comparisons - it is
effectively metadata (although it is not implemented using Clojure's
metadata mechanism).

## Usage

To create a contextual object, use `contextual.core/contextualize`,
passing an associative data structure to wrap and (optionally) a
path. If no path is given, an empty path with be used, implying that
the given object is the root.

```clojure
(def root (c/contextualize {:a [{:b {:c 1}} {:x 3}]}))
```

To retrieve the path of a contextual object, use
`contextual.core/context`, which returns the path relative to the
root.

```clojure
(c/context root) ;; => []
```

Use any of Clojure's built in methods for retreiving nested values:

```clojure
(def node (:b (first (root :a))))

(= node {:c 1}) ;; => true

(c/context node) ;; => [:a 0 :b]
```

Getting items via the sequence APIs returns contextual objects, too.

```clojure
(map c/context (:a root)) ;;=> ([:a 0] [:a 1])
(map c/context (vals root)) ;;=> ([:a])
```

To unwrap the contextual object and get a normal data structure back,
use `contextual.core/decontextualize`.

```clojure
(c/decontextualize node) ;; => {:c 1}
```

## Implementation

The library is implemented in terms of two protocols:

`Contextual` indicates a contextual object and supports the `context`
and `decontextualize` protocol methods.

Two implementations areprovided, `DelegateMap` and
`DelegateVec`. These wrap maps and vectors (respecively), maintaining
the object's path and delegating operations to the wrapped map or
vector implementation.

The `Contextualizable` protocol provides the `contextualize` method,
which converts the implemented type into an instance of
`Contextual`. By default it is extended to all of Clojure(Script)'s
default persistent map and vector types, clients may extend it to
additional types if necessary.

## License

Copyright © 2015 Luke VanderHart

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
