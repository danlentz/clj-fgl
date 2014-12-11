(ns fgl.logic
  (:require [fgl.util :as util])
  (:refer-clojure :exclude []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def +op+ #{'and 'or 'not})

(defn expr [op & args]
  (conj args op))

(defn atomic-clause?
  "An atomic clause has no connectives or qualifiers."
  [sentence]
  (or (not (list? sentence)
        (not (+op+ (first sentence))))))

(defn op [sentence]
  (if (atomic-clause? sentence)
    sentence
    (first sentence)))

(defn args [sentence]
  (rest sentence))

(defn arg1 [sentence]
  (second sentence))

(defn negative-clause?
  "A negative clause has NOT op"
  [sentence]
  (= 'not (first sentence)))

(defn literal-clause?
  "A literal is an atomic clause or a negated atomic clause"
  [sentence]
  (or (atomic-clause? sentence)
    (and (negative-clause? sentence)
      (atomic-clause? (arg1 sentence)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn conjuncts [sentence]
  (cond
    (= 'and (op sentence)) (args sentence)
    (= sentence true)      nil
    true                   (list sentence)))

(defn disjuncts [sentence]
  (cond
    (= 'or (op sentence))  (args sentence)
    (= sentence false)     nil
    true                   (list sentence)))

(defn conjunction [args]
  (cond
    (= 0 (count args)) true
    (= 1 (count args)) (first args)
    true               `(and ~@args)))

(defn disjunction [args]
  (cond
    (= 0 (count args)) false
    (= 1 (count args)) (first args)
    true               `(or ~@args)))

(defn merge-conjuncts
  "Return a DNF expression for the conjunction.
  'conjuncts' is a list of conjuncts, each in CNF."
  [conjuncts]
  (cond
    (= 0 (count conjuncts)) true
    (= 1 (count conjuncts)) (first conjuncts)
    true (disjunction
           (for [y (disjuncts (merge-conjuncts (rest conjuncts)))
                 x (disjuncts (first conjuncts))]
             (conjunction (concat (conjuncts x) (conjuncts y)))))))

(defn merge-disjuncts
  "Return a CNF expression for the disjunction.
  'disjuncts' is a list of disjuncts, each in CNF."
  [disjuncts]
  (cond
    (= 0 (count disjuncts)) false
    (= 1 (count disjuncts)) (first disjuncts)
    true (conjunction
           (for [y (conjuncts (merge-disjuncts (rest disjuncts)))
                 x (conjuncts (first disjuncts))]
             (disjunction (concat (disjuncts x) (disjuncts y)))))))

(defn move-not-inwards
  "Given P, return ~P, but with negation moved as far in as possible"
  [p]
  (cond
    (= '(true)  (op p)) false
    (= '(false) (op p)) true
    (= '(not)   (op p)) (arg1 p)
    (= '(and)   (op p)) (disjunction (map move-not-inwards (args p)))
    (= '(or)    (op p)) (conjunction (map move-not-inwards (args p)))
    true                (expr 'not p)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical Normal Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cnf 
  "Convert a sentence 'p' to Conjunctive Normal Form.
  Returns (and (or ...) ...) where each of the conjuncts
  has literal disjuncts."
  [p]
  (cond
    (= '(not) (op p)) (let [p2 (move-not-inwards (arg1 p))]
                        (if (literal-clause? p2)
                          p2
                          (->cnf p2)))
    (= '(and) (op p)) (conjunction
                        (util/mappend #(conjuncts (->cnf %)) (args p)))
    (= '(or)  (op p)) (merge-disjuncts (map ->cnf (args p)))
    true              p))
