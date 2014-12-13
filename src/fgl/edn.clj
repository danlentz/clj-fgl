(ns fgl.edn
  (:refer-clojure :exclude [])
  (:require [clojure.java.io       :as io])
  (:require [clojure.edn           :as edn])
  (:require [fgl.util  :as util]))
  

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDN Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol StringDesignator
  (as-string [x] "Stringified name of object"))

(extend-type clojure.lang.Keyword StringDesignator
             (as-string [k]
               (name k)))

(extend-type clojure.lang.Var     StringDesignator
             (as-string [v]
               (util/symbolic-name-from-var v)))

(extend-type java.lang.String     StringDesignator
             (as-string [s]
               s))

(extend-type java.lang.Object     StringDesignator
             (as-string [s]
               (.toString s)))


(defn- maybe-append-extension [ext base]
  (or (re-matches (re-pattern (str ".*\\." (as-string ext) "$")) base)
    (str base "." (as-string ext))))

(defn edn-resource [designator]
  (io/resource
    (maybe-append-extension :edn
      (as-string designator))))

(defn edn-file [designator]
  (io/file
    (edn-resource designator)))


;;; TODO: use EDN reader

(defmulti edn-resource-value type)

(defmethod edn-resource-value clojure.lang.Keyword [designator]
  (util/ignore-exceptions
    (read-string
      (slurp (edn-file designator)))))

 
