(ns omkamra.pygen.core-test
  (:require [clojure.string :as str]
            [omkamra.pygen.core :as py]
            [clojure.test :refer [deftest is testing]]))

(defn py-lines [& lines]
  (str (str/join "\n" lines) "\n"))

(defn linked-ident [v]
  (let [var-name (-> v meta :name name)
        ns-name (str (-> v meta :ns ns-name))
        digest (java.security.MessageDigest/getInstance "SHA-1")
        bytes (.digest digest (.getBytes (str ns-name "/" var-name) "UTF-8"))
        hex (apply str (map (fn [b] (format "%02x" (bit-and b 0xff)))
                            bytes))
        hash8 (subs hex 0 8)
        base (str/replace var-name #"[.\-]" "_")]
    (str base "__" hash8)))

(def default-env
  (py/value {"PATH" "/usr/bin"
             "SHELL" "/bin/bash"}))

(def exec-command
  (py/function [cmd env]
    (return (py-tuple cmd env))))

(def run-command
  (py/function [cmd]
    (return (::exec-command
             cmd
             ::default-env))))

(def floor-value
  (py/function [x]
    {:imports [math]}
    (return (math.floor x))))

(def floor-value-2
  (py/function [x]
    {:imports [math]}
    (return (::floor-value x))))

(def json-dumps
  (py/function [x]
    {:imports [json]}
    (return (json.dumps x :sort-keys true))))

(def main-linked
  (py/function []
    (return (::run-command "date"))))

(def unused-helper
  (py/function []
    (return "unused")))

(py/define define-v 5)

(py/define (define-inc x)
  (+ x 1))

(py/define (define-main x)
  (return (::define-inc (+ x ::define-v))))

(deftest transpile
  (is (= (py/transpile '((def helper [x] (return (* x 2)))
                         (def main [] (print (helper 21)))))
         (py-lines
            "def helper(x):"
            "    return x * 2"
            ""
            "def main():"
            "    print(helper(21))"))))

(deftest transpile-supports-general-module-level-statements
  (is (= (py/transpile '((assign! x 1)
                         (if (> x 0)
                           (update! x + 1))
                         (assert (> x 0))
                         (def main []
                           (return x))))
         (py-lines
          "x = 1"
          ""
          "if x > 0:"
          "    x += 1"
          ""
          "assert x > 0"
          ""
          "def main():"
          "    return x"))))

(deftest transpile-rejects-module-level-control-flow-statements
  (doseq [form ['((return 1))
                '((break))
                '((continue))
                '((yield 1))
                '((yield-from xs))
                '((await x))
                '((async-for x xs
                    (pass)))
                '((async-with [(open path) f]
                    (pass)))]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"module level"
         (py/transpile form)))))

(deftest transpile-rejects-single-function-form
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"module"
       (py/transpile '(def helper [x] (return x))))))

(deftest transpile-rejects-non-symbol-identifiers
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"symbol"
       (py/transpile '((def bad [x]
                         (assign! "y" (+ x 1))
                         (return x)))))))

(deftest assign-and-return
  (is (= (py/transpile '((def incr [x]
                           (assign! y (+ x 1))
                           (return y))))
         (py-lines
          "def incr(x):"
          "    y = x + 1"
          "    return y"))))

(deftest if-else-return
  (is (= (py/transpile '((def clamp-positive [x]
                           (if (> x 0)
                             (return x)
                             (return 0)))))
         (py-lines
          "def clamp_positive(x):"
          "    if x > 0:"
          "        return x"
          "    else:"
          "        return 0"))))

(deftest if-without-else
  (is (= (py/transpile '((def normalize [x]
                           (if (< x 0)
                             (assign! x 0))
                           (return x))))
         (py-lines
          "def normalize(x):"
          "    if x < 0:"
          "        x = 0"
          "    return x"))))

(deftest chained-comparison
  (is (= (py/transpile '((def between [x]
                           (if (< 0 x 10)
                             (return true)
                             (return false)))))
         (py-lines
          "def between(x):"
          "    if 0 < x < 10:"
          "        return True"
          "    else:"
          "        return False"))))

(deftest boolop-and-in-condition
  (is (= (py/transpile '((def in-range [x]
                           (if (and (> x 0) (< x 10))
                             (return true)
                             (return false)))))
         (py-lines
          "def in_range(x):"
          "    if x > 0 and x < 10:"
          "        return True"
          "    else:"
          "        return False"))))

(deftest boolop-or-in-condition
  (is (= (py/transpile '((def is-edge [x]
                           (if (or (== x 0) (== x 10))
                             (return true)
                             (return false)))))
         (py-lines
          "def is_edge(x):"
          "    if x == 0 or x == 10:"
          "        return True"
          "    else:"
          "        return False"))))

(deftest compare-equals-alias
  (is (= (py/transpile '((def is-zero [x]
                           (if (= x 0)
                             (return true)
                             (return false)))))
         (py-lines
          "def is_zero(x):"
          "    if x == 0:"
          "        return True"
          "    else:"
          "        return False"))))

(deftest unary-not-in-condition
  (is (= (py/transpile '((def outside-range [x]
                           (if (not (and (> x 0) (< x 10)))
                             (return true)
                             (return false)))))
         (py-lines
          "def outside_range(x):"
          "    if not (x > 0 and x < 10):"
          "        return True"
          "    else:"
          "        return False"))))

(deftest unary-factor-ops
  (is (= (py/transpile '((def unary-demo [x]
                           (assign! a (+ x))
                           (assign! b (- x))
                           (assign! c (bit-not x))
                           (return [a b c]))))
         (py-lines
          "def unary_demo(x):"
          "    a = +x"
          "    b = -x"
          "    c = ~x"
          "    return [a, b, c]"))))

(deftest precedence-parentheses
  (is (= (py/transpile '((def f [x]
                           (return (* (+ x 1) 2)))))
         (py-lines
          "def f(x):"
          "    return (x + 1) * 2"))))

(deftest while-loop
  (is (= (py/transpile '((def countdown [n]
                           (while (> n 0)
                             (assign! n (- n 1)))
                           (return n))))
         (py-lines
          "def countdown(n):"
          "    while n > 0:"
          "        n = n - 1"
          "    return n"))))

(deftest while-break
  (is (= (py/transpile '((def until-zero [n]
                           (while true
                             (if (== n 0)
                               (break))
                             (assign! n (- n 1)))
                           (return n))))
         (py-lines
          "def until_zero(n):"
          "    while True:"
          "        if n == 0:"
          "            break"
          "        n = n - 1"
          "    return n"))))

(deftest while-else
  (is (= (py/transpile '((def find-index [xs target]
                           (assign! i 0)
                             (while (< i (len xs))
                             (if (== (py-at xs i) target)
                               (return i))
                             (update! i + 1)
                             (else
                               (return -1))))))
         (py-lines
          "def find_index(xs, target):"
          "    i = 0"
          "    while i < len(xs):"
          "        if xs[i] == target:"
          "            return i"
          "        i += 1"
          "    else:"
          "        return -1"))))

(deftest while-body-allows-vector-expression
  (is (= (py/transpile '((def weird [x]
                           (while (> x 0)
                             [x 1]
                             (break))
                           (return x))))
         (py-lines
          "def weird(x):"
          "    while x > 0:"
          "        [x, 1]"
          "        break"
          "    return x"))))

(deftest for-loop
  (is (= (py/transpile '((def sum-to [n]
                           (assign! total 0)
                           (for x (range n)
                             (assign! total (+ total x)))
                           (return total))))
         (py-lines
          "def sum_to(n):"
          "    total = 0"
          "    for x in range(n):"
          "        total = total + x"
          "    return total"))))

(deftest for-else
  (is (= (py/transpile '((def contains [xs target]
                           (for x xs
                             (if (== x target)
                               (return true))
                             (else
                               (return false))))))
         (py-lines
          "def contains(xs, target):"
          "    for x in xs:"
          "        if x == target:"
          "            return True"
          "    else:"
          "        return False"))))

(deftest loop-else-must-be-last
  (doseq [form ['((def bad-while [n]
                   (while (> n 0)
                     (else (return n))
                     (assign! n (- n 1)))))
                '((def bad-for [xs]
                   (for x xs
                     (else (return x))
                     (pass))))]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"else clause must be last"
         (py/transpile form)))))

(deftest update-augassign
  (is (= (py/transpile '((def sum-to [n]
                           (assign! total 0)
                           (for x (range n)
                             (update! total + x))
                           (return total))))
         (py-lines
          "def sum_to(n):"
          "    total = 0"
          "    for x in range(n):"
          "        total += x"
          "    return total"))))

(deftest for-continue
  (is (= (py/transpile '((def sum-odd [n]
                           (assign! total 0)
                           (for x (range n)
                             (if (== (% x 2) 0)
                               (continue))
                             (assign! total (+ total x)))
                           (return total))))
         (py-lines
          "def sum_odd(n):"
          "    total = 0"
          "    for x in range(n):"
          "        if x % 2 == 0:"
          "            continue"
          "        total = total + x"
          "    return total"))))

(deftest with-statement
  (is (= (py/transpile '((def read-data [path lock]
                           (with [(open path) f
                                  (acquire lock) nil]
                             (assign! data (f.read))
                             (return data)))))
         (py-lines
          "def read_data(path, lock):"
          "    with open(path) as f, acquire(lock):"
          "        data = f.read()"
          "        return data"))))

(deftest assert-statement
  (is (= (py/transpile '((def check [x]
                           (assert (> x 0))
                           (assert (< x 10) "x must be < 10")
                           (return x))))
         (py-lines
          "def check(x):"
          "    assert x > 0"
          "    assert x < 10, \"x must be < 10\""
          "    return x"))))

(deftest explicit-pass
  (is (= (py/transpile '((def noop []
                           (pass))))
         (py-lines
          "def noop():"
          "    pass"))))

(deftest delete-statement
  (is (= (py/transpile '((def prune [obj arr i]
                           (del (. obj cache)
                                (py-at arr i)
                                (py-slice arr 1 3))
                           (return arr))))
         (py-lines
          "def prune(obj, arr, i):"
          "    del obj.cache, arr[i], arr[1:3]"
          "    return arr"))))

(deftest global-statement
  (is (= (py/transpile '((def init []
                           (global state count)
                           (assign! state {})
                           (assign! count 0)
                           (return count))))
         (py-lines
          "def init():"
          "    global state, count"
          "    state = {}"
          "    count = 0"
          "    return count"))))

(deftest nonlocal-statement
  (is (= (py/transpile '((def outer [x]
                           (def inner [y]
                             (nonlocal x)
                             (assign! x (+ x y))
                             (return x))
                           (return (inner 1)))))
         (py-lines
          "def outer(x):"
          "    def inner(y):"
          "        nonlocal x"
          "        x = x + y"
          "        return x"
          "    return inner(1)"))))

(deftest yield-and-yield-from
  (is (= (py/transpile '((def generate [xs]
                           (yield)
                           (yield 1)
                           (yield-from xs))))
         (py-lines
          "def generate(xs):"
          "    yield"
          "    yield 1"
          "    yield from xs"))))

(deftest yield-parenthesized-in-call-arguments
  (is (= (py/transpile '((def relay [x]
                           (return (f (yield x)
                                      :k (yield-from (g x)))))))
         (py-lines
          "def relay(x):"
          "    return f((yield x), k=(yield from g(x)))"))))

(deftest async-def-and-await
  (is (= (py/transpile '((async-def fetch-json [client url]
                           (assign! resp (await (client.get url)))
                           (return (await (resp.json))))))
         (py-lines
          "async def fetch_json(client, url):"
          "    resp = await client.get(url)"
          "    return await resp.json()"))))

(deftest async-for-with-else
  (is (= (py/transpile '((async-def drain [xs]
                           (assign! last nil)
                           (async-for x xs
                             (assign! last x)
                             (else
                               (assign! last "done")))
                           (return last))))
         (py-lines
          "async def drain(xs):"
          "    last = None"
          "    async for x in xs:"
          "        last = x"
          "    else:"
          "        last = \"done\""
          "    return last"))))

(deftest async-with
  (is (= (py/transpile '((async-def read-first [factory path]
                           (async-with [(factory path) f]
                             (return (await (f.read)))))))
         (py-lines
          "async def read_first(factory, path):"
          "    async with factory(path) as f:"
          "        return await f.read()"))))

(deftest try-except-else-finally-and-raise
  (is (= (py/transpile '((def process [x]
                           (try
                             (assign! y (/ 10 x))
                             (except [ZeroDivisionError e]
                               (return "zero"))
                             (except [[TypeError ValueError] e]
                               (raise))
                             (else
                               (update! count + 1))
                             (finally
                               (cleanup)))
                           (return y))))
         (py-lines
          "def process(x):"
          "    try:"
          "        y = 10 / x"
          "    except ZeroDivisionError as e:"
          "        return \"zero\""
          "    except (TypeError, ValueError) as e:"
          "        raise"
          "    else:"
          "        count += 1"
          "    finally:"
          "        cleanup()"
          "    return y"))))

(deftest try-bare-except-and-raise-expression
  (is (= (py/transpile '((def wrap [f]
                           (try
                             (return (f))
                             (except []
                               (raise (RuntimeError "failed")))))))
         (py-lines
          "def wrap(f):"
          "    try:"
          "        return f()"
          "    except:"
          "        raise RuntimeError(\"failed\")"))))

(deftest raise-with-cause
  (is (= (py/transpile '((def wrap [f]
                           (try
                             (return (f))
                             (except [Exception e]
                               (raise (RuntimeError "failed") e))))))
         (py-lines
          "def wrap(f):"
          "    try:"
          "        return f()"
          "    except Exception as e:"
          "        raise RuntimeError(\"failed\") from e"))))

(deftest attribute-read-write
  (is (= (py/transpile '((def touch [obj]
                           (assign! (. obj count) 1)
                           (update! (. obj count) + 2)
                           (return (. obj count)))))
         (py-lines
          "def touch(obj):"
          "    obj.count = 1"
          "    obj.count += 2"
          "    return obj.count"))))

(deftest dotted-symbols-normalize-to-attribute-access
  (is (= (py/transpile '((def dotted-demo []
                           (logging.warning "foo")
                           (assign! logging.level 1)
                           (update! logging.level + 1)
                           (return logging.warning))))
         (py-lines
          "def dotted_demo():"
          "    logging.warning(\"foo\")"
          "    logging.level = 1"
          "    logging.level += 1"
          "    return logging.warning"))))

(deftest subscript-read-write
  (is (= (py/transpile '((def touch-index [arr i]
                           (assign! (py-at arr i) 1)
                           (update! (py-at arr i) + 2)
                           (return (py-at arr i)))))
         (py-lines
          "def touch_index(arr, i):"
          "    arr[i] = 1"
          "    arr[i] += 2"
          "    return arr[i]"))))

(deftest py-slice
  (is (= (py/transpile '((def slice-demo [arr]
                           (assign! head (py-slice arr nil 3))
                           (assign! middle (py-slice arr 1 4))
                           (assign! tail (py-slice arr 2 nil))
                           (assign! stride (py-slice arr nil nil 2))
                           (assign! (py-slice arr 1 3) [9 9])
                           (return [head middle tail stride arr]))))
         (py-lines
          "def slice_demo(arr):"
          "    head = arr[:3]"
          "    middle = arr[1:4]"
          "    tail = arr[2:]"
          "    stride = arr[::2]"
          "    arr[1:3] = [9, 9]"
          "    return [head, middle, tail, stride, arr]"))))

(deftest py-tuple
  (is (= (py/transpile '((def tuple-demo [x y]
                           (assign! pair (py-tuple x y))
                           (assign! single (py-tuple x))
                           (assign! empty (py-tuple))
                           (return (py-tuple pair single empty)))))
         (py-lines
          "def tuple_demo(x, y):"
          "    pair = (x, y)"
          "    single = (x,)"
          "    empty = ()"
          "    return (pair, single, empty)"))))

(deftest dict-literal
  (is (= (py/transpile '((def dict-demo [x]
                           (assign! d {"a" 1 "b" x})
                           (assign! empty {})
                           (return (py-tuple d empty)))))
         (py-lines
          "def dict_demo(x):"
          "    d = {\"a\": 1, \"b\": x}"
          "    empty = {}"
          "    return (d, empty)"))))

(deftest set-literal
  (is (= (py/transpile '((def set-demo [x]
                           (assign! s #{1 2})
                           (assign! empty #{})
                           (return (py-tuple s empty)))))
         (py-lines
          "def set_demo(x):"
          "    s = {1, 2}"
          "    empty = set()"
          "    return (s, empty)"))))

(deftest imports
  (is (= (py/transpile '((import math [numpy np])
                         (from os.path import join [dirname dn])
                         (def main []
                           (return 0))))
         (py-lines
          "import math, numpy as np"
          "from os.path import join, dirname as dn"
          ""
          "def main():"
          "    return 0"))))

(deftest call-keyword-args
  (is (= (py/transpile '((def call-demo [x]
                           (return (f x :indent 2 :sort-keys true)))))
         (py-lines
          "def call_demo(x):"
          "    return f(x, indent=2, sort_keys=True)"))))

(deftest class-def
  (is (= (py/transpile '((class Counter [BaseCounter]
                           (def __init__ [self start]
                             (assign! (. self value) start))
                           (def inc [self]
                             (update! (. self value) + 1)
                             (return (. self value))))
                         (def main []
                           (return 0))))
         (py-lines
          "class Counter(BaseCounter):"
          "    def __init__(self, start):"
          "        self.value = start"
          "    def inc(self):"
          "        self.value += 1"
          "        return self.value"
          ""
          "def main():"
          "    return 0"))))

(deftest list-comprehension
  (is (= (py/transpile '((def comp-demo [xs ys]
                           (return [(py-tuple x y)
                                    :for [x xs]
                                    :for [y ys]
                                    :if (< x y)]))))
         (py-lines
          "def comp_demo(xs, ys):"
          "    return [(x, y) for x in xs for y in ys if x < y]"))))

(deftest list-comp-form
  (is (= (py/transpile '((def comp-demo-2 [xs ys]
                           (return (list-comp (py-tuple x y)
                                              :for [x xs]
                                              :for [y ys]
                                              :if (< x y))))))
         (py-lines
          "def comp_demo_2(xs, ys):"
          "    return [(x, y) for x in xs for y in ys if x < y]"))))

(deftest set-comprehension
  (is (= (py/transpile '((def set-comp-demo [xs]
                           (return (set-comp (* x 2)
                                             :for [x xs]
                                             :if (> x 0))))))
         (py-lines
          "def set_comp_demo(xs):"
          "    return {x * 2 for x in xs if x > 0}"))))

(deftest dict-comprehension
  (is (= (py/transpile '((def dict-comp-demo [xs]
                           (return (dict-comp x (* x x)
                                              :for [x xs]
                                              :if (> x 0))))))
         (py-lines
          "def dict_comp_demo(xs):"
          "    return {x: x * x for x in xs if x > 0}"))))

(deftest generator-expression
  (is (= (py/transpile '((def gen-comp-demo [xs]
                           (return (gen-comp (* x x)
                                             :for [x xs]
                                             :if (> x 0))))))
         (py-lines
          "def gen_comp_demo(xs):"
          "    return (x * x for x in xs if x > 0)"))))

(deftest py-compare-mixed-operators
  (is (= (py/transpile '((def within-range [x]
                           (if (py-compare 0 < x <= 10)
                             (return true)
                             (return false)))))
         (py-lines
          "def within_range(x):"
          "    if 0 < x <= 10:"
          "        return True"
          "    else:"
          "        return False"))))

(deftest py-compare-python-specific-operators
  (is (= (py/transpile '((def classify [x xs]
                           (if (py-compare x in xs)
                             (return (py-compare x is-not nil))
                             (return false)))))
         (py-lines
          "def classify(x, xs):"
          "    if x in xs:"
          "        return x is not None"
          "    else:"
          "        return False"))))

(deftest compare-in-and-not-in-surface-syntax
  (is (= (py/transpile '((def membership [x]
                           (if (in x [5 3])
                             (return (not-in x [0]))
                             (return false)))))
         (py-lines
          "def membership(x):"
          "    if x in [5, 3]:"
          "        return x not in [0]"
          "    else:"
          "        return False"))))

(deftest if-else-expression
  (is (= (py/transpile '((def classify [x]
                           (return (if-else (> x 0)
                                            "pos"
                                            (if-else (== x 0)
                                              "zero"
                                              "neg"))))))
         (py-lines
          "def classify(x):"
          "    return \"pos\" if x > 0 else \"zero\" if x == 0 else \"neg\""))))

(deftest if-else-expression-parenthesizes-then-branch-when-needed
  (is (= (py/transpile '((def choose [x y]
                           (return (if-else (== x 0)
                                            (if-else (> y 0) "a" "b")
                                            "c")))))
         (py-lines
          "def choose(x, y):"
          "    return (\"a\" if y > 0 else \"b\") if x == 0 else \"c\""))))

(deftest py-named-expression
  (is (= (py/transpile '((def find-first [xs]
                           (if (> (py-named n (len xs)) 0)
                             (return n)
                             (return 0)))))
         (py-lines
          "def find_first(xs):"
          "    if (n := len(xs)) > 0:"
          "        return n"
          "    else:"
          "        return 0"))))

(deftest py-named-expression-statement-is-parenthesized
  (is (= (py/transpile '((def demo []
                           (py-named x 1)
                           (return x))))
         (py-lines
          "def demo():"
          "    (x := 1)"
          "    return x"))))

(deftest py-named-in-call-keyword-value-is-parenthesized
  (is (= (py/transpile '((def send [x]
                           (return (f :k (py-named y x))))))
         (py-lines
          "def send(x):"
          "    return f(k=(y := x))"))))

(deftest lambda-expression
  (is (= (py/transpile '((def make-inc [n]
                           (return (lambda [x] (+ x n))))))
         (py-lines
          "def make_inc(n):"
          "    return lambda x: x + n"))))

(deftest lambda-call-parenthesized
  (is (= (py/transpile '((def apply-twice [x]
                           (return ((lambda [y] (* y 2)) x)))))
         (py-lines
          "def apply_twice(x):"
          "    return (lambda y: y * 2)(x)"))))

(deftest function-params-varargs-and-kwargs
  (is (= (py/transpile '((def collect [x *args **kwargs]
                           (return (py-tuple x args kwargs)))))
         (py-lines
          "def collect(x, *args, **kwargs):"
          "    return (x, args, kwargs)"))))

(deftest function-params-full-model-surface-syntax
  (is (= (py/transpile '((def f [a / b [c 10] * d [e 5] **kw]
                           (return (py-tuple a b c d e kw)))))
         (py-lines
          "def f(a, /, b, c=10, *, d, e=5, **kw):"
          "    return (a, b, c, d, e, kw)"))))

(deftest lambda-params-full-model-surface-syntax
  (is (= (py/transpile '((def make-lam []
                           (return (lambda [x [y 1] / *args z [w 2] **kw]
                                     (py-tuple x y args z w kw))))))
         (py-lines
          "def make_lam():"
          "    return lambda x, y=1, /, *args, z, w=2, **kw: (x, y, args, z, w, kw)"))))

(deftest kwonly-default-none
  (is (= (py/transpile '((def f [* [x nil]]
                           (return x))))
         (py-lines
          "def f(*, x=None):"
          "    return x"))))

(deftest params-reject-non-default-after-default
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"non-default positional parameter follows default parameter"
       (py/transpile '((def bad [[x 1] y]
                         (return y)))))))

(deftest call-args-varargs-and-kwargs
  (is (= (py/transpile '((def invoke [f args extra kwargs]
                           (return (f :mode "fast" *args *extra :indent 2 **kwargs)))))
         (py-lines
          "def invoke(f, args, extra, kwargs):"
          "    return f(*args, *extra, mode=\"fast\", indent=2, **kwargs)"))))

(deftest call-rejects-star-after-double-star
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"iterable argument unpacking follows keyword argument unpacking"
       (py/transpile '((def bad [f args kwargs]
                         (return (f **kwargs *args))))))))

(deftest raise-rejects-three-arguments
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"raise expects zero, one, or two arguments"
       (py/transpile '((def bad [exc cause]
                         (raise exc from cause)))))))

(deftest match-statement-with-sequence-mapping-guard-and-wildcard
  (is (= (py/transpile '((def classify [v ready]
                           (match v
                             (case [head & rest]
                               (return rest))
                             (case {"x" x & rest}
                               (return x))
                             (case (= Color.RED) :if ready
                               (return "red"))
                             (case _
                               (return nil))))))
         (py-lines
          "def classify(v, ready):"
          "    match v:"
          "        case [head, *rest]:"
          "            return rest"
          "        case {\"x\": x, **rest}:"
          "            return x"
          "        case Color.RED if ready:"
          "            return \"red\""
          "        case _:"
          "            return None"))))

(deftest match-class-or-and-as-patterns
  (is (= (py/transpile '((def decode [node]
                           (match node
                             (case (Point :x x :y y)
                               (return (+ x y)))
                             (case (as (or 0 1) bit)
                               (return bit))
                             (case _
                               (return nil))))))
         (py-lines
          "def decode(node):"
          "    match node:"
          "        case Point(x=x, y=y):"
          "            return x + y"
          "        case 0 | 1 as bit:"
          "            return bit"
          "        case _:"
          "            return None"))))

(deftest transpile-links-reachable-dependencies-from-top-level-forms
  (let [default-env-id (linked-ident #'default-env)
        exec-command-id (linked-ident #'exec-command)
        run-command-id (linked-ident #'run-command)]
    (is (= (py/transpile '((def main-linked []
                             (return (::run-command "date")))))
           (py-lines
            (str "def " exec-command-id "(cmd, env):")
            "    return (cmd, env)"
            ""
            (str default-env-id " = {\"PATH\": \"/usr/bin\", \"SHELL\": \"/bin/bash\"}")
            ""
            (str "def " run-command-id "(cmd):")
            (str "    return " exec-command-id "(cmd, " default-env-id ")")
            ""
            "def main_linked():"
            (str "    return " run-command-id "(\"date\")"))))))

(deftest define-macro-supports-values-and-functions
  (let [define-v-id (linked-ident #'define-v)
        define-inc-id (linked-ident #'define-inc)
        define-main-id (linked-ident #'define-main)]
    (is (= (py/transpile '((def entry [x]
                             (return (::define-main x)))))
           (py-lines
            (str "def " define-inc-id "(x):")
            "    x + 1"
            ""
            (str define-v-id " = 5")
            ""
            (str "def " define-main-id "(x):")
            (str "    return " define-inc-id "(x + " define-v-id ")")
            ""
            "def entry(x):"
            (str "    return " define-main-id "(x)"))))))

(deftest transpile-links-and-aggregates-imports-from-function-metadata
  (let [floor-id (linked-ident #'floor-value)
        json-id (linked-ident #'json-dumps)]
    (is (= (py/transpile '((def entry [x]
                             (return (py-tuple (::floor-value x)
                                               (::json-dumps {"x" x}))))))
           (py-lines
            "import math"
            "import json"
            ""
            (str "def " floor-id "(x):")
            "    return math.floor(x)"
            ""
            (str "def " json-id "(x):")
            "    return json.dumps(x, sort_keys=True)"
            ""
            "def entry(x):"
            (str "    return (" floor-id "(x), " json-id "({\"x\": x})" ")"))))))

(deftest transpile-deduplicates-imports-from-linked-functions
  (let [floor-id (linked-ident #'floor-value)
        floor2-id (linked-ident #'floor-value-2)]
    (is (= (py/transpile '((def entry [x]
                             (return (::floor-value-2 x)))))
           (py-lines
            "import math"
            ""
            (str "def " floor-id "(x):")
            "    return math.floor(x)"
            ""
            (str "def " floor2-id "(x):")
            (str "    return " floor-id "(x)")
            ""
            "def entry(x):"
            (str "    return " floor2-id "(x)"))))))
