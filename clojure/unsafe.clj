(ns unsafe)

(def unsafe
  (.get (doto (.getDeclaredField sun.misc.Unsafe "theUnsafe")
          (.setAccessible true)) nil))

(defn allocate-instance [class]
  (.allocateInstance unsafe class))

(defn allocate-memory [bytes]
  (.allocateMemory unsafe bytes))

(defn free-memory [address]
  (.freeMemory unsafe address))

(defn get-long [address]
  (.getLong unsafe address))

(defn put-long [address value]
  (.putLong unsafe address value))
