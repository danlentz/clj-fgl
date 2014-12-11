(ns fgl.logic
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


    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conjunctive Normal Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


