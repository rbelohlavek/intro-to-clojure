(ns intro-to-clojure.intro
  (:require [clojure.pprint :as pp]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest])
  (:import (java.util ArrayList)))

(comment







  ;Clojure core philosophy:
  ;=================================================================================================
  ;Lisp: almost no syntax, extensible
  ;Functional programming: immutable data, first class functions
  ;State management: mutation 'primitives' with state change semantics
  ;Concurrency: the previous 2 idioms means concurrency just 'works'
  ;Dynamic typing: emphasis on 'data oriented programming', not on types
  ;Hosted: leverage the power of the Jvm and all the existing libraries from Java
  ;        (same story with cljs / javascript)
  ;Practical over pure: This isn't Haskell.
  ;Non-OO:
  ; -"It is better to have 100 functions operate on one data structure
  ;   than 10 functions on 10 data structures." —Alan Perlis
  ; -Separation of data from behavior
  ; -Inheritance is not the only way to do polymorphism







  ;What are we looking at? Cursive in Intellij
  ;-project structure
  ;-sending 'forms' from the editor to the running REPL





















  ;Basic 'scalar' types
  ;=================================================================================================
  nil

  true
  false

  123
  07021
  0xfa8d
  2r01101101
  31r3T                                                     ;up to base 36

  -2.34
  123e-12

  12398374598734958734985739485897987234587923459872631405892374508979348754N
  (+ 9223372036854775807 1)

  8979879837495873458937459873495873459873452394874958734.85793483974593945873945384578754M

  44/14

  "foo"

  \b
  \u2615

  :some-keyword
  :some-other-namespace/keyword
  ::my-namespaced-keyword

  'foo













  ;the 4 basic 'core' collection types (having a literal syntax, there are others without):
  ;=================================================================================================
  '(1 2 3 4 "five")                                         ;list
  ["foo" \b '(44/14)]                                       ;vector
  {:key "val" 1 'foo}                                       ;map
  #{123 "foo" 0x7C}                                         ;set

  (->> (range)
       (take 5)
       ;(take 32)
       (map (juxt (comp keyword str) identity))
       (into {})
       type)

  (array-map :1 1 :2 2)
  (hash-map :1 1 :2 2)


  ;persistent data structures - immutability without copy-on-write
  ;https://pivovarit.github.io/talks/purely-functional-data-structures/img/clojure-trees.png


























  ;Code
  ;=================================================================================================
  ;This is an expression. This is also all there is to clojure code 'syntax':
  (+ 1 2)
  ;^ ^ ^
  ;| | |
  ;| | |-arg 1
  ;| |---arg 0
  ;|-----one of 3 things: special form, macro, function

  ;In clojure, if the above is a function, evaluation is eager:
  ;-resolve the symbol +, which is yields function
  ;-resolve the first argument, which yields a number
  ;-resolve the second argument, which also yields a number
  ;call the function + with args 1 and 2

  ;This isn't necessarily true for special forms or macros...more on this later.


  ;Why do lisps use this weird syntax for code? - homoiconicity
  ;=================================================================================================
  ;The REPL:
  ;
  ;  text   ┌─────────┐  ds   ┌──────────┐   ds   ┌─────────┐       ┌────────┐       ┌─────────┐
  ; ───────>│  read   ├──────>│  expand  ├───────>│ compile ├──────>│  eval  ├──────>│  print  ├─┐
  ;         └─────────┘       └──────────┘        └─────────┘       └────────┘       └─────────┘ │
  ;              ^                                                                               |
  ;              │                                  loop                                         │
  ;              ────────────────────────────────────────────────────────────────────────────────

  ;We can show what the REPL sees after each of the above phases manually...
  ;Simple function vs the 'or' macro
  (->>
    "'(+ 1 2)"
    ;"(or 1 2)"
    ;"'(1 2)"

    ;read-string                           ;In lisps, code is data
    ;macroexpand
    ;eval
    )







  ;Special forms
  ;=================================================================================================
  (def foo 1)                              ;creates and interns global vars

  (let [foo 2] foo)                        ;lexical symbol binding

  (fn [arg1 arg2] (or arg1 arg2))          ;general function form


  (if (= foo 1) "foo" "bar")               ;if thing then else

  (do (println foo) (println "bar"))       ;usually for side effects

  (loop [foo 2] foo)                       ;like let, but with recur target capability

  (recur)                                  ;non-stack consuming recursion (tco) - compiler enforced

  (quote (+ 1 2))                          ;don't evaluate the form

  (var foo)                                ;get the var of a symbol itself, not it's value

  ;throwing an exception
  (throw (ex-info "pebkac" {:problematic-thing foo}))

  (try                                     ;try/catch/finally are all 3 special forms
    foo
    (catch Exception e (println "foo not so good" e))
    (finally (println "glad we are moving past this...")))

  ;interop special forms: new and . ;eg:
  (let [al (new ArrayList)]                ;new up java objects
    (pp/pprint al)                         ;first usage of a required namespace, more later
    (.add al 1)                            ;. form used to access java object methods
    (pp/pprint al))

  ;other interop
  (System/getProperty "java.runtime.version");static method access
  (Math/PI)                                  ;static field access










  ;Functions - same 'syntax' as the above
  ;=================================================================================================
  (inc 1)
  (str 1)

  (< 1 2 3)

  (map inc [1 2 3])                                         ;functions as arguments
  ;note that the output type changes from '[...]' to '(...)' - more on this later

  (comp str inc)                                            ;functions that return functions
  (map (comp str inc) [1 2 3])

  (defn plus-2 [n]                                          ;named functions stored in a var
    (+ 2 n))

  (plus-2 3)
  (plus-2 3 4)                                              ;!

  (defn plus-2 [n & more]                                     ;variadic arguments
    (apply + (concat [n 2] more)))

  (plus-2 3)
  (plus-2 3 4)

  (partial + 2)                                             ;partial function application

  (defn adder [n]                                           ;named version of the above
    (partial + n))

  (adder 2)
  ((adder 2) 3 4)
  (adder)                                                   ;what happens here?

  (defn adder                                               ;multi-arity functions
    ([] (adder 0))
    ([n] (partial + n)))

  (adder)
  ((adder))
  ((adder) 3)

  ;TODO keywords as functions here


  ;destructuring - applies to function arguments and let bindings
  (defn add-only-first-and-third [& [a _ b]]      ;destructuring the variadic args, position-wise
    (+ a b))

  (add-only-first-and-third 1 2 4)
  (add-only-first-and-third 1 2 4 10)             ;destructuring doesn't break variadicity

  ;destructuring variadic args into name-value pairs
  (defn define-configs [& {:keys [password] :as m}]
    (println password)
    m)

  (def configs (define-configs :timeout 5000 :retries 3 :password "hunter2"))

  configs

  ;general destructuring of a map in a let binding:
  (let [{pwd :password :keys [timeout something] :or {something "else"} :as m} configs]
    (println (str "password=" pwd))
    (println (str "timeout=" timeout))
    (println (str "something=" something)))




  ;Macros
  ;=================================================================================================
  ; '  -> quote
  ; `  -> syntax quote
  ; ~  -> unquote
  ; ~@ -> unquote splice

  foo
  'foo
  (type 'foo)

  inc

  (inc foo)

  '(inc foo)
  `(inc foo)
  `(inc ~foo)

  ;(apply + [1 2 3])

  (let [bar [1 2 3]]
    ;`(+ ~bar)
    `(+ ~@bar)
    )

  ;example simple macro - delayed evaluation
  (defmacro unless [condition & body]
    `(if (not ~condition)                                   ;what happens during macroexpand if we remove ~ here?
       (do ~@body)))

  ;we aren't passing a function to be evaluated on the 'then' clause, we are passing a literal expression...
  foo
  (unless (= foo 1) (println "shucks"))
  (unless (= foo 2) (println "shucks"))

  (macroexpand '(unless (= foo 2) (println "shucks")))

  ;(let [condition '(= foo 2)
  ;      body '(println "shucks")]
  ;  `(if (not ~condition)
  ;     (do ~@body)))

  ;another example - altering 'syntax'
  (println (inc (+ 2 foo)))

  (->> foo
       (+ 2)
       inc
       println)

  (macroexpand
    '(-> foo
         (+ 2)
         inc
         println))









  ;Sequence abstraction & laziness
  ;=================================================================================================
  ;not all sequential things are seqs
  (seq? [1 2 3])
  (seq? (map inc [1 2 3]))
  (seq? (mapv inc [1 2 3]))

  (range 10)
  (take 10 (range))

  ;defining lazy sequences by using the 'lazy-seq' macro
  (defn fibonacci-sequence
    ([] (fibonacci-sequence 0 1))
    ([m n] (lazy-seq
             (cons n (fibonacci-sequence n (+' m n))))))

  (take 10 (fibonacci-sequence))
  (take 10 (drop 500 (fibonacci-sequence)))






  ;Mutation 'primitives': var, atom, ref, agent
  ;=================================================================================================

  ;vars - thread-local identities with a global root value
  ;-------------------------------------------------------
  ;A symbol, if it resolving to a var, will directly look up that vars value during evaluation.
  ; You have to get a handle to the var itself via a specific call, like:
  foo
  (var +)
  (type (var +))

  #'+                                                       ;shorthand for var

  ;metadata on vars (or any of these 4 state holding objects)
  (meta #'+)
  (meta #'*ns*)

  ;vars are global with a root binding
  ;-------------------------------------------------------
  ;earlier, we defined a var...
  (def foo 1)
  foo
  (binding [foo 3] (future (do (Thread/sleep 1000) (println foo))))

  ;dynamic vars are allowed to have a thread-local binding override:
  (def ^:dynamic bar 12334895)                              ;shorthand for metadata: {:dynamic true}
  bar

  (do
    (binding [bar 3] (future (do (Thread/sleep 4000) (println bar))))
    bar)



  ;atoms - uncoordinated and synchronous state change
  ;-------------------------------------------------------
  (def state (atom 0))

  state

  (deref state)                                             ;'dereference' the value in the atom
  @state                                                    ;shorthand for dereference

  (defn closure-over-atom []
    (fn [] (swap! state inc)))                              ;this uses CAS internally, like AtomicReference in java

  (->> (repeat (closure-over-atom))
       (take 20)
       (apply pcalls))

  @state



  ;refs - coordinated and synchronous state change
  ;-------------------------------------------------------
  (def ref1 (ref {:foo 1}))
  (def ref2 (ref {:bar 2}))

  @ref1
  @ref2

  (dosync                                                   ;this uses STM (software transactional memory) internally
    (alter ref1 merge @ref2)
    (alter ref2 merge @ref1))

  @ref1
  @ref2
  (= @ref1 @ref2)

  (dosync
    (alter ref1 assoc :baz 123)
    (alter ref2 assoc :baz 123)
    (throw (RuntimeException. "no baz for you")))

  @ref1
  @ref2


  ;agents - uncoordinated and asynchronous...
  ;-------------------------------------------------------
  (def agent1 (agent "og message"))

  @agent1

  (let [new-message "new message"]
    (do
      (send agent1 (fn [old-message] (do (Thread/sleep 3000)
                                         (println (str "old message was: " old-message))
                                         new-message)))
      (while (not= new-message @agent1)
        (do
          (println (str "message is still: " @agent1))
          (Thread/sleep 1000)))
      (println (str "message is now: " @agent1))))




  ;Multimethods - runtime polymorphism
  ;=================================================================================================
  ;Scenario: We need to parse a field from some data. However, the structure of that field differs
  ;          significantly based on if it is a list, and if so, how many things are in the list. We want
  ;          to just extract the inner field, then pass it to a generic function, which will handle the
  ;          details of how to handle parsing the structure

  (defmulti
    parser
    ;arbitrary dispatch function
    (fn [{:keys [things]}] (when (counted? things) (count things))))

  ;(ns-unmap *ns* 'parser)

  (defmethod parser nil [m] (str "parser for when we don't even have the things"))
  (defmethod parser 0 [m] (str "parser for when there are zero things"))
  (defmethod parser 1 [m] (str "parser for when there is just 1 thing"))
  (defmethod parser :default [m] (str "parser for when there are multiple things"))

  (parser {:foo "anything"})
  (parser {:foo "anything" :things 123})
  (parser {:foo "anything" :things []})
  (parser {:foo "anything" :things [#{"just one thing"}]})
  (parser {:foo "anything" :things [{:thing 1} {:thing 2}]})



  ;Namespaces
  ;=================================================================================================
  ;maps of symbols to vars (or java classes)
  (resolve 'foo)
  (resolve 'System)
  (resolve 'pp/pprint)

  (all-ns)

  ;namespaces are first-class 'things'
  (->> 'intro-to-clojure.intro                              ;given a symbol representing a namespace
       find-ns                                              ;find that namespace
       ns-imports                                           ;then give me all the imported things into that namespace
       )


  ;compiling an existing namespace into  class files, decompile into java
  (compile 'intro-to-clojure.core)


  ;Testing
  ;=================================================================================================
  ;unit test example in core-test ns

  ;spec: Since we are doing data oriented programming, we can do data oriented testing.
  ;inspired by QuickCheck from Haskell
  ;taken from https://clojure.org/guides/spec
  (defn ranged-rand
    "Returns random int in range start <= rand < end"
    [start end]
    (+ start (long (rand (- end start)))))

  ;(ranged-rand 10 20)

  (s/def                                                    ;define a re-usable spec
    ::increasing-int-args
    (s/and
      (s/cat :start int? :end int?)                         ;name-predicate pairs on the argument vector
      #(< (:start %) (:end %))))                            ;applying conditions on the extracted arg values

  (gen/generate (s/gen ::increasing-int-args))              ;generate some samples of the spec

  (s/fdef
    ranged-rand
    :args ::increasing-int-args                             ;re-use the spec defined above for the arguments
    :ret int?
    :fn (s/and                                              ;one-off spec for this args-return relationship
          #(<= (-> % :args :start) (:ret %))
          #(< (:ret %) (-> % :args :end))))

  (s/exercise-fn `ranged-rand)                              ;exercise a generated sample set of args and return values

  (ranged-rand 8 5)
  (stest/instrument `ranged-rand)                           ;turn on instrumentation for existing fn that is spec'ed
  (ranged-rand 8 5)                                         ;which, if enabled, will show you usage errors per the spec
  (stest/unstrument `ranged-rand)                           ;turn off instrumentation

  ;finally, we can use the generators to more exhaustively test the function
  (def test-result (stest/check `ranged-rand {:clojure.spec.test.check/opts {:num-tests 20000}}))

  test-result

  ;(stest/abbrev-result (first test-result))



  ;Reader literals - reader 'macros' applied at read time
  ;=================================================================================================
  #"\s+"
  (re-pattern "\\s+")

  #inst"2018-03-28T10:48:00.000"
  #uuid"365defdd-ac01-48ca-8ba2-0aa29b2f1152"



  ;Parallelism
  ;=================================================================================================
  ;futures, promises

  ;parallel operations (pvalues, pcalls, pmap)
  (time
    (second
      (pvalues
        (do (Thread/sleep 1000) 1)
        (do (Thread/sleep 5000) 5)
        (do (Thread/sleep 10000) 10))))


  ;Truthiness and nil punning
  ;=================================================================================================
  (let [foo
        true
        ;false
        ;nil
        ;[]
        ;:anything-else
        ]
    (if foo
      (println "foo truthy")
      (println "foo falsy")))


  ;Protocols - dynamic type polymorphism
  ;=================================================================================================
  (defprotocol thing-adder
    "Add a thing to the thing in a way that makes sense for the thing"
    (add-thing [thing new-thing]))

  (deftype my-thing-adder [s]                               ;create a new type, which implements thing-adder
    thing-adder
    (add-thing [_ foo] (str s " " foo)))

  (def instance-of-my-thing-adder (my-thing-adder. "initial string"))
  (type instance-of-my-thing-adder)
  (add-thing instance-of-my-thing-adder "new stuff")

  (add-thing "any random string" "more")                    ;java.lang.String does not implement thing-adder

  (satisfies? thing-adder instance-of-my-thing-adder)
  (satisfies? thing-adder "any string")

  (extend-type String thing-adder                           ;so lets define it to do so
    (add-thing [this more] (.concat this (str " " more))))

  (satisfies? thing-adder "any string")
  (add-thing "any random string" "more")

  (def al (new ArrayList))                                  ;do the same for java.util.ArrayList
  (add-thing al "foo")
  (satisfies? thing-adder al)
  (extend-type ArrayList thing-adder
    (add-thing [this more] (.add this more)))
  (satisfies? thing-adder al)
  (add-thing al "foo")
  al

  (add-thing nil "foo")                                     ;how about nil? nil is a type...
  (extend-type nil thing-adder
    (add-thing [_ more] (str "nil " more)))


  ;TODO derive, prefer-method, remove-method, records, reify, proxy



  ;resources:
  ;=================================================================================================
  ;https://clojure.org/index
  ;https://clojuredocs.org/

  ;books:
  ;https://www.braveclojure.com/ - great intro
  ;https://www.manning.com/books/the-joy-of-clojure-second-edition - my favorite book on clojure

  ;hamt:
  ;https://lampwww.epfl.ch/papers/idealhashtrees.pdf
  ;https://pivovarit.github.io/talks/purely-functional-data-structures/img/clojure-trees.png

  ;spec:
  ;https://clojure.org/guides/spec
  ;https://clojure.org/about/spec

  ;videos: some good talks by Rich Hickey, the creator of Clojure
  ;Are we there yet?: https://www.youtube.com/watch?v=ScEPu1cs4l0
  ;Hammock Driven Development: https://www.youtube.com/watch?v=f84n5oFoZBc
  ;Simple made easy: https://www.youtube.com/watch?v=oytL881p-nQ
  ;Simplicity matters: https://www.youtube.com/watch?v=rI8tNMsozo0
  ;The value of values: https://www.youtube.com/watch?v=-6BsiVyC1kM
  ;The language of the system: https://www.youtube.com/watch?v=ROor6_NGIWU
  ;Effective programs - 10 years of clojure: https://www.youtube.com/watch?v=2V1FtfBDsLU

  )