(ns contextual.core)

(defprotocol Contextual
  "An associative data structure which tracks a path of keys from some
  root that was used to obtain this value. Any 'child' values
  retrieved from a Contextual object, via any of its APIs, will also
  be Contextual (provided they also are associative), with the same
  root and the child's key added as a path element."
  (context [_] "Return the full path from the root")
  (decontextualize [_] "Return the underlying, non-contextual
  version of the object"))

(defprotocol Contextualizable
  "A value that can be contextualized."
  (contextualize [_] [_ path] "Return a contextualized version
  of the value. Uses the given path is provided, otherwise uses an
  empty path (i.e, the given object is the root)."))

(declare ->DelegateMap ->DelegateVec)

(extend-protocol Contextualizable
  #?@(:clj [clojure.lang.IPersistentVector
            (contextualize ([v] (->DelegateVec v []))
                           ([v path] (->DelegateVec v path)))
            clojure.lang.IPersistentMap
            (contextualize ([m] (->DelegateMap m []))
                           ([m path] (->DelegateMap m path)))]
       :cljs [cljs.core.PersistentVector
              (contextualize ([v] (->DelegateVec v []))
                             ([v path] (->DelegateVec v path)))
              cljs.core.Subvec
              (contextualize ([v] (->DelegateVec v []))
                             ([v path] (->DelegateVec v path)))
              cljs.core.PersistentArrayMap
              (contextualize ([m] (->DelegateMap m []))
                             ([m path] (->DelegateMap m path)))
              cljs.core.PersistentHashMap
              (contextualize ([m] (->DelegateMap m []))
                             ([m path] (->DelegateMap m path)))
              cljs.core.PersistentTreeMap
              (contextualize ([m] (->DelegateMap m []))
                             ([m path] (->DelegateMap m path)))]))

(defn contextualizable?
  "Return true if the value can be converted to a contextual one.

   We would prefer to use (satisfies? Contextualizable), however, the
   performance of satisfies? is not satisfactory."
  [value]
  (or (map? value) (vector? value)))

(defn- contextual-value
  [path key value]
  (if (contextualizable? value)
    (contextualize value (conj path key))
    value))

(defn- contextual-entry
  [path [key value :as entry]]
  (if (contextualizable? value)
    #?(:clj (clojure.lang.MapEntry. key (contextualize value (conj path key)))
       :cljs [key (contextualize value (conj path key))])
    entry))

#?(:clj
   (deftype DelegateMap [delegate path]
     Contextual
     (context [_] path)
     (decontextualize [_] delegate)

     clojure.lang.Associative
     (containsKey [_ key] (.containsKey delegate key))
     (assoc [_ key val] (->DelegateMap (.assoc delegate key val) path))
     (entryAt [_ key] (contextual-entry path (.entryAt delegate key)))

     clojure.lang.Counted
     (count [_] (.count delegate))

     clojure.lang.IFn
     (applyTo [_ [key & _ :as arglist]]
       (contextual-value path key (.applyTo delegate arglist)))
     (invoke [_ key]
       (contextual-value path key (.invoke delegate key)))
     (invoke [_ key not-found]
       (contextual-value path key (.invoke delegate key not-found)))

     clojure.lang.IHashEq
     (hasheq [_] (.hasheq delegate))

     clojure.lang.ILookup
     (valAt [_ key]
       (contextual-value path key (.valAt delegate key)))
     (valAt [_ key notfound]
       (contextual-value path key (.valAt delegate key notfound)))

     clojure.lang.IMeta
     (meta [_] (.meta delegate))

     clojure.lang.IObj
     (withMeta [_ meta] (->DelegateMap (.withMeta delegate meta) path))

     clojure.lang.MapEquivalence

     clojure.lang.IPersistentCollection
     (cons [_ o] (->DelegateMap (.cons delegate o) path))
     (empty [_] (->DelegateMap (.empty delegate) path))
     (equiv [_ o] (.equiv delegate o))

     clojure.lang.IPersistentMap
     (assocEx [_ k v] (->DelegateMap (.assocEx delegate k v) path))

     clojure.lang.Seqable
     (seq [_]
       (when-let [s (.seq delegate)]
         (map (partial contextual-entry path) s)))

     java.io.Serializable

     java.lang.Iterable
     (iterator [this] (.iterator (seq this)))

     java.lang.Runnable
     (run [this] (.invoke this))

     java.util.Map
     (containsValue [_ value] (.containsValue delegate value))
     (entrySet [this] (set (seq this)))
     (equals [_ o] (.equals delegate o))
     (get [this k] (get this k))
     (hashCode [_] (.hashCode delegate))
     (isEmpty [_] (.isEmpty delegate))
     (keySet [_] (.keySet delegate))
     (size [_] (.size delegate))
     (values [_] (map val (seq (.values delegate))))

     java.util.concurrent.Callable
     (call [this] (.invoke this))

     clojure.core.protocols/IKVReduce
     (kv-reduce [_ f init] (clojure.core.protocols/kv-reduce delegate f init))))

#?(:cljs
   (deftype DelegateMap [delegate path]
     Contextual
     (context [_] path)
     (decontextualize [_] delegate)

     Object
     (toString [_] (.toString delegate))
     (equiv [this other] (-equiv this other))

     ICloneable
     (-clone [_] (->DelegateMap delegate path))

     IWithMeta
     (-with-meta [_ meta] (->DelegateMap (-with-meta delegate meta) path))

     IMeta
     (-meta [_] (-meta delegate))

     ICollection
     (-conj [_ entry] (->DelegateMap (-conj delegate entry) path))

     ICounted
     (-count [_] (-count delegate))

     IEmptyableCollection
     (-empty [_] (->DelegateMap (-empty delegate) path))

     IEquiv
     (-equiv [_ other] (-equiv delegate other))

     IHash
     (-hash [_] (-hash delegate))

     ISeqable
     (-seq [_]
       (when-let [s (-seq delegate)]
         (map (partial contextual-entry path) s)))

     ILookup
     (-lookup [_ k] (contextual-value path k (-lookup delegate k)))
     (-lookup [_ k not-found] (contextual-value path k (-lookup delegate k not-found)))

     IAssociative
     (-assoc [_ k v] (->DelegateMap (-assoc delegate k v) path))
     (-contains-key? [_ k] (-contains-key? delegate k))

     IMap
     (-dissoc [_ k] (->DelegateMap (-dissoc delegate k) path))

     IKVReduce
     (-kv-reduce [_ f init] (-kv-reduce delegate f init))

     IFn
     (-invoke [_ k] (contextual-value path k (delegate k)))
     (-invoke [_ k not-found] (contextual-value path k (delegate k not-found)))))

#?(:clj

   (deftype DelegateVec [delegate path]
     Contextual
     (context [_] path)
     (decontextualize [_] delegate)

     clojure.lang.Associative
     (containsKey [_ key] (.containsKey delegate key))
     (assoc [_ key val] (->DelegateVec (.assoc delegate key val) path))
     (entryAt [_ key] (contextual-entry path (.entryAt delegate key)))

     clojure.lang.Counted
     (count [_] (.count delegate))

     clojure.lang.IFn
     (applyTo [_ [key & _ :as arglist]]
       (contextual-value path key (.applyTo delegate arglist)))
     (invoke [_ key]
       (contextual-value path key (.invoke delegate key)))
     (invoke [_ key not-found]
       (contextual-value path key (.invoke delegate key not-found)))

     clojure.lang.IHashEq
     (hasheq [_] (.hasheq delegate))

     clojure.lang.ILookup
     (valAt [_ key]
       (contextual-value path key (.valAt delegate key)))
     (valAt [_ key notfound]
       (contextual-value path key (.valAt delegate key notfound)))

     clojure.lang.IMeta
     (meta [_] (.meta delegate))

     clojure.lang.IObj
     (withMeta [_ meta] (->DelegateVec (.withMeta delegate meta) path))

     clojure.lang.IPersistentCollection
     (cons [_ o] (->DelegateVec (.cons delegate o) path))
     (empty [_] (->DelegateVec (.empty delegate) path))
     (equiv [_ o] (.equiv delegate o))

     clojure.lang.Seqable
     (seq [_]
       (when-let [s (.seq delegate)]
         (map-indexed (fn [i v]
                        (contextual-value path i v)) s)))

     java.io.Serializable

     java.lang.Iterable
     (iterator [this] (.iterator (seq this)))

     java.lang.Runnable
     (run [this] (.invoke this))

     java.util.concurrent.Callable
     (call [this] (.invoke this))

     clojure.lang.IReduceInit
     (reduce [_ f start] (.reduce delegate f start))

     clojure.lang.IReduce
     (reduce [_ f] (.reduce delegate f))

     clojure.lang.IPersistentStack
     (peek [_] (let [idx (dec (count delegate))]
                    (contextual-value path idx (.peek delegate))))
     (pop [_] (->DelegateVec (.pop delegate) path))

     clojure.lang.IPersistentVector
     (length [_] (.length delegate))
     (assocN [_ idx val] (->DelegateVec (.assocN delegate idx val) path))

     clojure.lang.Indexed
     (nth [_ i] (contextual-value path i (.nth delegate i)))
     (nth [_ i not-found] (contextual-value path i
                                               (.nth delegate i not-found)))

     clojure.lang.Sequential

     java.lang.Comparable
     (compareTo [_ other] (.compareTo delegate other))

     java.util.Collection
     (contains [_ obj] (.contains delegate obj))
     (containsAll [_ c] (.containsAll delegate c))
     (equals [_ o] (.equals delegate o))
     (hashCode [_] (.hashCode delegate))
     (isEmpty [_] (.isEmpty delegate))
     (size [_] (.size delegate))
     (toArray [this] (clojure.lang.RT/seqToArray (seq this)))
     (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))

     java.util.List
     (get [_ idx] (contextual-value path idx (.get delegate idx)))
     (indexOf [_ o] (.indexOf delegate o))
     (lastIndexOf [_ o] (.lastIndexOf delegate o))
     (listIterator [this]
       ;; This is quite inefficent, but much easier to
       ;; implement. Should be enough of an edge case that it doesn't
       ;; matter.
       (.listIterator (vec (seq this))))
     (listIterator [this i]
       (.listIterator (vec (seq this)) i))
     (subList [_ from to]
       (->DelegateVec (.subList delegate from to) path))

     java.util.RandomAccess
     ))

#? (:cljs
    (deftype DelegateVec [delegate path]
      Contextual
      (context [_] path)
      (decontextualize [_] delegate)

      Object
      (toString [_] (.toString delegate))
      (equiv [this other] (-equiv this other))

      ICloneable
      (-clone [_] (->DelegateVec delegate path))

      IWithMeta
      (-with-meta [_ meta] (DelegateVec. (-with-meta delegate meta) path))

      IMeta
      (-meta [_] (-meta delegate))

      IStack
      (-peek [_] (let [idx (dec (count delegate))]
                      (contextual-value path idx (-peek delegate))))
      (-pop [_] (->DelegateVec (-pop delegate) path))

      ICollection
      (-conj [_ val] (->DelegateMap (-conj delegate val) path))

      IEmptyableCollection
      (-empty [coll] (->DelegateMap (-empty delegate) path))

      ISequential
      IEquiv
      (-equiv [_ other] (-equiv delegate other))

      IHash
      (-hash [_] (-hash delegate))

      ISeqable
      (-seq [_]
        (when-let [s (-seq delegate)]
          (map-indexed (fn [i val]
                         (contextual-value path i val)) s)))

      ICounted
      (-count [_] (-count delegate))

      IIndexed
      (-nth [_ n] (contextual-value path n (-nth delegate n)))
      (-nth [_ n not-found]
        (contextual-value path n (-nth delegate n not-found)))

      ILookup
      (-lookup [_ k]
        (contextual-value path k (-lookup delegate k)))
      (-lookup [_ k not-found]
        (contextual-value path k (-lookup delegate k not-found)))

      IMapEntry
      (-key [_] (-key delegate))
      (-val [_] (contextual-value path 1 (-val delegate)))

      IAssociative
      (-assoc [_ k v] (->DelegateVec (-assoc delegate k v) path))
      (-contains-key? [_ k] (-contains-key? delegate k))

      IVector
      (-assoc-n [_ n val]
        (->DelegateVec (-assoc-n delegate n val) path))

      IReduce
      (-reduce [_ f]
        (-reduce delegate f))
      (-reduce [_ f init]
        (-reduce delegate f init))

      IKVReduce
      (-kv-reduce [_ f init]
        (-kv-reduce delegate f init))

      IFn
      (-invoke [_ k] (contextual-value path k (delegate k)))
      (-invoke [_ k not-found] (contextual-value path k (delegate k not-found)))

      IIterable
      (-iterator [this]
        (seq-iter (seq this)))))

