(ns lispy.core
  (:require [clojure.reflect :as r]
            [clojure.algo.generic.math-functions :as math]))

(declare tokenize read-from-tokens 
         ->atom env- lispstr procedure eval-)

(defn all-methods 
  "(all-methods Math)"
  [x]
  (->> x r/reflect 
    :members 
    (filter :return-type)  
    (map :name) 
    sort 
    (map #(str "." %) )
    distinct))

;; http://www.rosettacode.org/wiki/User_input/Text
(defn raw-input [prompt]
  (print (format "%s: " prompt))
  (flush)
  (read-line))

;;;;;;;

(def sym str)
(def Symbol String)
(def List clojure.lang.PersistentVector)
;; (def Number number?)

(defn parse [s]
  (read-from-tokens (tokenize s)))

(defn tokenize [s]
  (remove 
    empty?
    (.split
      (.replace 
        (.replace s "(" " ( ")
        ")" " ) ") " ")))

(defn read-from-tokens [tokens]
  (let [ntokens (atom tokens)
        f (fn f[] 
            (if (zero? (count @ntokens))
              (throw (RuntimeException. 
                       "unexpected EOF while reading"))
              (let [token (first @ntokens)] 
                (swap! ntokens #(drop 1 %))
                (case token
                  "(" (let [l (atom [])]
                        (while (not= ")" (first @ntokens)) 
                          (swap! l conj (f)))
                        (swap! ntokens #(drop 1 %))
                        @l)
                  ")" (throw (RuntimeException. "unexpected )"))
                  (->atom token)))))]
    (f)))

(defn ->atom [token]
  (try 
    (Integer/parseInt token)
    (catch NumberFormatException _
      (try 
        (Integer/parseInt token)
        (catch NumberFormatException _
          (sym token))))))

(defn standard-env []
  (let [env (merge #_swap! (env-) 
                   (into {} 
                         (map (fn [[k v]] 
                                ;; v is var, if v is fn can use like (v val)
                                {(name k) v}) 
                              (ns-publics 
                                'clojure.algo.generic.math-functions))))] 
    
    (assoc #_swap! env
           "+" + "-" - "*" * "/" /
           ">" > "<" < ">=" >= "<=" <= "=" =
           "append"   conj
           "apply"    apply
           ;; can't work
           "begin"    #(do %) ;; lambda *x  x[-1]
           "car"      first
           "cdr"      rest
           "cons"     cons
           "eq?"      =
           "equal?"   =
           "length"   count
           "list"     list
           "list?"    list?
           "map"      map
           "max"      max
           "min"      min
           "not"      not
           "null?"    #(zero? (count %)) ;; lambda x  x == []
           "number?"  number?
           "procedure?"  fn?
           "round"    #(Math/round %)        
           "symbol?"  #(= Symbol (class %))
           "time"     #(time %))))

(defn env- 
  ([] (env- [] [] nil))
  ([params args outer] 
   (assoc (zipmap params args)
          :outer outer)
   #_(atom {:outer outer})))

(defn env-find [env k]
  (if (contains? env k)
    env
    (if-let [outer (:outer env)]
      (env-find outer k)
      env)))

(def global-env (atom (standard-env)))

(defn repl
  ([] (repl "lis.py> "))
  ([prompt]
   (while true
     (let [v (->> prompt
               raw-input
               parse
               eval-)]
       (when v
         (prn (lispstr v)))))))

(defn lispstr [exp]
  (if (= List (class exp))
    (str "(" (.join (map lispstr exp) " ") ")")
    (str exp)))

(defn procedure [params body env]
  (fn [& args]
    ;; (prn params)
    ;; (prn args)
    ;; (prn (env- params args env))
    (eval- body (atom (env- params args env)))))

(defn eval-
  ([x] (eval- x global-env))
  ([x env]
   (cond
     (= Symbol (class x)) (get (env-find @env x) x) 
     (not= List (class x)) x
     :else (case (first x)
             "quote" (rest x)
             "if"  (let [[_ pred conseq alt] x
                         exp (if (eval- pred env)
                               conseq
                               alt)]
                     (eval- exp env))
             "define"  (let [[_ va exp] x]
                          (swap! env 
                                 assoc 
                                 va 
                                 (eval- exp env)))
             "set!"  (let [[_ va exp] x]
                       (swap! env 
                              #(assoc (env-find % va) 
                                      va 
                                      (eval- exp env))))
             "lambda"  (let [[_ params body] x]
                         (procedure params body @env))
             (do
               ;; (prn "a " x)
               (apply (eval- (first x) env) 
                    (map #(eval- % env) (rest x))))))))

;; (repl)
