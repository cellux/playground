(ns omkamra.pygen.emit-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [omkamra.pygen.ast :as ast]
            [omkamra.pygen.emit :as emit]))

(defn py-lines [& lines]
  (str (str/join "\n" lines) "\n"))

(deftest emit-empty-module
  (is (= "" (emit/emit-module (ast/module)))))

(deftest emit-general-top-level-statements
  (let [module (-> (ast/module)
                   (ast/module-add (ast/assign 'x 1))
                   (ast/module-add (ast/assert-stmt (ast/compare 'x [:>] [0])))
                   (ast/module-add (ast/function-def 'main []
                                      [(ast/return 'x)])))]
    (is (= (py-lines
            "x = 1"
            ""
            "assert x > 0"
            ""
            "def main():"
            "    return x")
           (emit/emit-module module)))))

(deftest emit-empty-function-def-body
  (let [module (-> (ast/module)
                   (ast/module-add (ast/function-def 'noop [] [])))]
    (is (= (py-lines
            "def noop():"
            "    pass")
           (emit/emit-module module)))))

(deftest emit-return-binops
  (doseq [[op op-text] [[:+ "+"]
                        [:- "-"]
                        [:* "*"]
                        [:/ "/"]]]
    (testing (str "emits binary op " op)
      (let [module (-> (ast/module)
                       (ast/module-add
                        (ast/function-def 'calc ['x 'y]
                          [(ast/return (ast/binop op 'x 'y))])))]
        (is (= (py-lines
                "def calc(x, y):"
                (str "    return x " op-text " y"))
               (emit/emit-module module)))))))

(deftest emit-expression-statements
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'main []
                      [(ast/expr-stmt
                        (ast/call 'print
                                  ['helper
                                   21
                                   "ok"
                                   true
                                   false
                                   nil]))])))]
    (is (= (py-lines
            "def main():"
            "    print(helper, 21, \"ok\", True, False, None)")
           (emit/emit-module module)))))

(deftest emit-symbol-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'main []
                      [(ast/expr-stmt 'helper)])))]
    (is (= (py-lines
            "def main():"
            "    helper")
           (emit/emit-module module)))))

(deftest emit-list-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'main ['x]
                      [(ast/expr-stmt (ast/list-expr ['x 1]))])))]
    (is (= (py-lines
            "def main(x):"
            "    [x, 1]")
           (emit/emit-module module)))))

(deftest emit-if-else
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'clamp-positive ['x]
                      [(ast/if-stmt
                        (ast/compare 'x [:>] [0])
                        [(ast/return 'x)]
                        [(ast/return 0)])])))]
    (is (= (py-lines
            "def clamp_positive(x):"
            "    if x > 0:"
            "        return x"
            "    else:"
            "        return 0")
           (emit/emit-module module)))))

(deftest emit-if-without-else
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'normalize ['x]
                      [(ast/if-stmt
                        (ast/compare 'x [:<] [0])
                        [(ast/assign 'x 0)])
                       (ast/return 'x)])))]
    (is (= (py-lines
            "def normalize(x):"
            "    if x < 0:"
            "        x = 0"
            "    return x")
           (emit/emit-module module)))))

(deftest emit-while-loop
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'countdown ['n]
                      [(ast/while-stmt
                        (ast/compare 'n [:>] [0])
                        [(ast/assign 'n (ast/binop :- 'n 1))])
                       (ast/return 'n)])))]
    (is (= (py-lines
            "def countdown(n):"
            "    while n > 0:"
            "        n = n - 1"
            "    return n")
           (emit/emit-module module)))))

(deftest emit-while-else
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'find-index ['xs 'target]
                      [(ast/assign 'i 0)
                       (ast/while-stmt
                        (ast/compare 'i [:<] [(ast/call 'len ['xs])])
                        [(ast/if-stmt
                          (ast/compare (ast/subscript 'xs 'i) [:==] ['target])
                          [(ast/return 'i)])
                         (ast/augassign 'i :+ 1)]
                        [(ast/return -1)])])))]
    (is (= (py-lines
            "def find_index(xs, target):"
            "    i = 0"
            "    while i < len(xs):"
            "        if xs[i] == target:"
            "            return i"
            "        i += 1"
            "    else:"
            "        return -1")
           (emit/emit-module module)))))

(deftest emit-while-break
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'until-zero ['n]
                      [(ast/while-stmt true
                         [(ast/if-stmt
                           (ast/compare 'n [:==] [0])
                           [(ast/break-stmt)])
                          (ast/assign 'n (ast/binop :- 'n 1))])
                       (ast/return 'n)])))]
    (is (= (py-lines
            "def until_zero(n):"
            "    while True:"
            "        if n == 0:"
            "            break"
            "        n = n - 1"
            "    return n")
           (emit/emit-module module)))))

(deftest emit-chained-compare
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'between ['x]
                      [(ast/return (ast/compare 0 [:< :<] ['x 10]))])))]
    (is (= (py-lines
            "def between(x):"
            "    return 0 < x < 10")
           (emit/emit-module module)))))

(deftest emit-python-specific-compare-ops
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'classify ['x 'xs]
                      [(ast/if-stmt
                        (ast/compare 'x [:in] ['xs])
                        [(ast/return (ast/compare 'x [:is-not] [nil]))]
                        [(ast/return false)])])))]
    (is (= (py-lines
            "def classify(x, xs):"
            "    if x in xs:"
            "        return x is not None"
            "    else:"
            "        return False")
           (emit/emit-module module)))))

(deftest emit-boolop-chain
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'flag ['x]
                      [(ast/return
                        (ast/boolop :or
                                    [(ast/compare 'x [:<] [0])
                                     (ast/compare 'x [:>] [10])
                                     (ast/compare 'x [:==] [5])]))])))]
    (is (= (py-lines
            "def flag(x):"
            "    return x < 0 or x > 10 or x == 5")
           (emit/emit-module module)))))

(deftest emit-unary-not
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'outside-range ['x]
                      [(ast/return
                        (ast/unaryop :not
                                     (ast/boolop :and
                                                 [(ast/compare 'x [:>] [0])
                                                  (ast/compare 'x [:<] [10])])))])))]
    (is (= (py-lines
            "def outside_range(x):"
            "    return not (x > 0 and x < 10)")
           (emit/emit-module module)))))

(deftest emit-unary-factor-ops
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'unary-demo ['x]
                      [(ast/return
                        (ast/list-expr
                         [(ast/unaryop :uadd 'x)
                          (ast/unaryop :usub 'x)
                          (ast/unaryop :invert 'x)]))])))]
    (is (= (py-lines
            "def unary_demo(x):"
            "    return [+x, -x, ~x]")
           (emit/emit-module module)))))

(deftest emit-for-loop
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'sum-to ['n]
                      [(ast/assign 'total 0)
                       (ast/for-stmt 'x
                                     (ast/call 'range ['n])
                                     [(ast/assign 'total (ast/binop :+ 'total 'x))])
                       (ast/return 'total)])))]
    (is (= (py-lines
            "def sum_to(n):"
            "    total = 0"
            "    for x in range(n):"
            "        total = total + x"
            "    return total")
           (emit/emit-module module)))))

(deftest emit-for-else
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'contains ['xs 'target]
                      [(ast/for-stmt 'x
                                     'xs
                                     [(ast/if-stmt
                                       (ast/compare 'x [:==] ['target])
                                       [(ast/return true)])]
                                     [(ast/return false)])])))]
    (is (= (py-lines
            "def contains(xs, target):"
            "    for x in xs:"
            "        if x == target:"
            "            return True"
            "    else:"
            "        return False")
           (emit/emit-module module)))))

(deftest emit-continue
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'skip-even ['n]
                      [(ast/for-stmt 'x
                                     (ast/call 'range ['n])
                                     [(ast/if-stmt
                                       (ast/compare (ast/binop :% 'x 2) [:==] [0])
                                       [(ast/continue-stmt)])
                                      (ast/expr-stmt (ast/call 'print ['x]))])])))]
    (is (= (py-lines
            "def skip_even(n):"
            "    for x in range(n):"
            "        if x % 2 == 0:"
            "            continue"
            "        print(x)")
           (emit/emit-module module)))))

(deftest emit-with-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'read-data ['path 'lock]
                      [(ast/with-stmt
                        [(ast/with-item (ast/call 'open ['path]) 'f)
                         (ast/with-item (ast/call 'acquire ['lock]) nil)]
                        [(ast/assign 'data (ast/call (ast/attribute 'f 'read) []))
                         (ast/return 'data)])])))]
    (is (= (py-lines
            "def read_data(path, lock):"
            "    with open(path) as f, acquire(lock):"
            "        data = f.read()"
            "        return data")
           (emit/emit-module module)))))

(deftest emit-assert-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'check ['x]
                      [(ast/assert-stmt (ast/compare 'x [:>] [0]))
                       (ast/assert-stmt (ast/compare 'x [:<] [10]) "x must be < 10")
                       (ast/return 'x)])))]
    (is (= (py-lines
            "def check(x):"
            "    assert x > 0"
            "    assert x < 10, \"x must be < 10\""
            "    return x")
           (emit/emit-module module)))))

(deftest emit-augassign
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'mutate ['x]
                      [(ast/augassign 'x :+ 1)
                       (ast/augassign 'x :% 2)
                       (ast/return 'x)])))]
    (is (= (py-lines
            "def mutate(x):"
            "    x += 1"
            "    x %= 2"
            "    return x")
           (emit/emit-module module)))))

(deftest emit-pass
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'noop []
                      [(ast/pass-stmt)])))]
    (is (= (py-lines
            "def noop():"
            "    pass")
           (emit/emit-module module)))))

(deftest emit-delete-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'prune ['obj 'arr 'i]
                      [(ast/delete-stmt [(ast/attribute 'obj 'cache)
                                         (ast/subscript 'arr 'i)
                                         (ast/subscript 'arr (ast/slice-expr 1 3 nil))])
                       (ast/return 'arr)])))]
    (is (= (py-lines
            "def prune(obj, arr, i):"
            "    del obj.cache, arr[i], arr[1:3]"
            "    return arr")
           (emit/emit-module module)))))

(deftest emit-global-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'init []
                      [(ast/global-stmt ['state 'count])
                       (ast/assign 'state (ast/dict [] []))
                       (ast/assign 'count 0)
                       (ast/return 'count)])))]
    (is (= (py-lines
            "def init():"
            "    global state, count"
            "    state = {}"
            "    count = 0"
            "    return count")
           (emit/emit-module module)))))

(deftest emit-nonlocal-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'outer ['x]
                      [(ast/function-def 'inner ['y]
                         [(ast/nonlocal-stmt ['x])
                          (ast/assign 'x (ast/binop :+ 'x 'y))
                          (ast/return 'x)])
                       (ast/return (ast/call 'inner [1]))])))]
    (is (= (py-lines
            "def outer(x):"
            "    def inner(y):"
            "        nonlocal x"
            "        x = x + y"
            "        return x"
            "    return inner(1)")
           (emit/emit-module module)))))

(deftest emit-yield-and-yield-from
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'generate ['xs]
                      [(ast/expr-stmt (ast/yield-expr nil))
                       (ast/expr-stmt (ast/yield-expr 1))
                       (ast/expr-stmt (ast/yield-from-expr 'xs))])))]
    (is (= (py-lines
            "def generate(xs):"
            "    yield"
            "    yield 1"
            "    yield from xs")
           (emit/emit-module module)))))

(deftest emit-yield-parenthesized-in-call-arguments
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'relay ['x]
                      [(ast/return (ast/call 'f
                                             [(ast/yield-expr 'x)]
                                             [(ast/keyword-arg 'k
                                                               (ast/yield-from-expr
                                                                (ast/call 'g ['x])))]))])))]
    (is (= (py-lines
            "def relay(x):"
            "    return f((yield x), k=(yield from g(x)))")
           (emit/emit-module module)))))

(deftest emit-async-def-and-await
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/async-function-def 'fetch-json ['client 'url]
                      [(ast/assign 'resp
                                   (ast/await-expr
                                    (ast/call (ast/attribute 'client 'get) ['url])))
                       (ast/return
                        (ast/await-expr
                         (ast/call (ast/attribute 'resp 'json) [])))])))]
    (is (= (py-lines
            "async def fetch_json(client, url):"
            "    resp = await client.get(url)"
            "    return await resp.json()")
           (emit/emit-module module)))))

(deftest emit-async-for-with-else
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/async-function-def 'drain ['xs]
                      [(ast/assign 'last nil)
                       (ast/async-for-stmt
                        'x
                        'xs
                        [(ast/assign 'last 'x)]
                        [(ast/assign 'last "done")])
                       (ast/return 'last)])))]
    (is (= (py-lines
            "async def drain(xs):"
            "    last = None"
            "    async for x in xs:"
            "        last = x"
            "    else:"
            "        last = \"done\""
            "    return last")
           (emit/emit-module module)))))

(deftest emit-async-with
  (let [module (ast/module-add
                (ast/module)
                (ast/async-function-def
                 'read-first
                 ['factory 'path]
                 [(ast/async-with-stmt
                   [(ast/with-item (ast/call 'factory ['path]) 'f)]
                   [(ast/return
                     (ast/await-expr
                      (ast/call (ast/attribute 'f 'read) [])))])]))]
    (is (= (py-lines
            "async def read_first(factory, path):"
            "    async with factory(path) as f:"
            "        return await f.read()")
           (emit/emit-module module)))))

(deftest emit-try-except-else-finally-and-raise
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'process ['x]
                      [(ast/try-stmt
                        [(ast/assign 'y (ast/binop :/ 10 'x))]
                        [(ast/except-handler ['ZeroDivisionError] 'e
                                             [(ast/return "zero")])
                         (ast/except-handler ['TypeError 'ValueError] 'e
                                             [(ast/raise-stmt)])]
                        [(ast/augassign 'count :+ 1)]
                        [(ast/expr-stmt (ast/call 'cleanup []))])
                       (ast/return 'y)])))]
    (is (= (py-lines
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
            "    return y")
           (emit/emit-module module)))))

(deftest emit-try-bare-except-and-raise-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'wrap ['f]
                      [(ast/try-stmt
                        [(ast/return (ast/call 'f []))]
                        [(ast/except-handler nil nil
                                             [(ast/raise-stmt
                                               (ast/call 'RuntimeError ["failed"]))])]
                        nil
                        nil)])))]
    (is (= (py-lines
            "def wrap(f):"
            "    try:"
            "        return f()"
            "    except:"
            "        raise RuntimeError(\"failed\")")
           (emit/emit-module module)))))

(deftest emit-raise-with-cause
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'wrap ['f]
                      [(ast/try-stmt
                        [(ast/return (ast/call 'f []))]
                        [(ast/except-handler ['Exception] 'e
                                             [(ast/raise-stmt
                                               (ast/call 'RuntimeError ["failed"])
                                               'e)])]
                        nil
                        nil)])))]
    (is (= (py-lines
            "def wrap(f):"
            "    try:"
            "        return f()"
            "    except Exception as e:"
            "        raise RuntimeError(\"failed\") from e")
           (emit/emit-module module)))))

(deftest emit-attribute-read-write
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'touch ['obj]
                      [(ast/assign (ast/attribute 'obj 'count) 1)
                       (ast/augassign (ast/attribute 'obj 'count) :+ 2)
                       (ast/return (ast/attribute 'obj 'count))])))]
    (is (= (py-lines
            "def touch(obj):"
            "    obj.count = 1"
            "    obj.count += 2"
            "    return obj.count")
           (emit/emit-module module)))))

(deftest emit-subscript-read-write
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'touch-index ['arr 'i]
                      [(ast/assign (ast/subscript 'arr 'i) 1)
                       (ast/augassign (ast/subscript 'arr 'i) :+ 2)
                       (ast/return (ast/subscript 'arr 'i))])))]
    (is (= (py-lines
            "def touch_index(arr, i):"
            "    arr[i] = 1"
            "    arr[i] += 2"
            "    return arr[i]")
           (emit/emit-module module)))))

(deftest emit-get-slice
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'slice-demo ['arr]
                      [(ast/assign 'head (ast/subscript 'arr (ast/slice-expr nil 3 nil)))
                       (ast/assign 'middle (ast/subscript 'arr (ast/slice-expr 1 4 nil)))
                       (ast/assign 'tail (ast/subscript 'arr (ast/slice-expr 2 nil nil)))
                       (ast/assign 'stride (ast/subscript 'arr (ast/slice-expr nil nil 2)))
                       (ast/assign (ast/subscript 'arr (ast/slice-expr 1 3 nil))
                                   (ast/list-expr [9 9]))
                       (ast/return (ast/list-expr ['head 'middle 'tail 'stride 'arr]))])))]
    (is (= (py-lines
            "def slice_demo(arr):"
            "    head = arr[:3]"
            "    middle = arr[1:4]"
            "    tail = arr[2:]"
            "    stride = arr[::2]"
            "    arr[1:3] = [9, 9]"
            "    return [head, middle, tail, stride, arr]")
           (emit/emit-module module)))))

(deftest emit-tuple
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'tuple-demo ['x 'y]
                      [(ast/assign 'pair (ast/tuple ['x 'y]))
                       (ast/assign 'single (ast/tuple ['x]))
                       (ast/assign 'empty (ast/tuple []))
                       (ast/return (ast/tuple ['pair 'single 'empty]))])))]
    (is (= (py-lines
            "def tuple_demo(x, y):"
            "    pair = (x, y)"
            "    single = (x,)"
            "    empty = ()"
            "    return (pair, single, empty)")
           (emit/emit-module module)))))

(deftest emit-dict
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'dict-demo ['x]
                      [(ast/assign 'd (ast/dict ["a" "b"] [1 'x]))
                       (ast/assign 'empty (ast/dict [] []))
                       (ast/return (ast/tuple ['d 'empty]))])))]
    (is (= (py-lines
            "def dict_demo(x):"
            "    d = {\"a\": 1, \"b\": x}"
            "    empty = {}"
            "    return (d, empty)")
           (emit/emit-module module)))))

(deftest emit-set
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'set-demo ['x]
                      [(ast/assign 's (ast/set-expr [1 2]))
                       (ast/assign 'empty (ast/set-expr []))
                       (ast/return (ast/tuple ['s 'empty]))])))]
    (is (= (py-lines
            "def set_demo(x):"
            "    s = {1, 2}"
            "    empty = set()"
            "    return (s, empty)")
           (emit/emit-module module)))))

(deftest emit-imports
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/import-stmt
                     [(ast/alias 'math)
                      (ast/alias 'numpy 'np)]))
                   (ast/module-add
                    (ast/import-from-stmt
                     'os.path
                     [(ast/alias 'join)
                      (ast/alias 'dirname 'dn)]))
                   (ast/module-add
                    (ast/function-def 'main []
                      [(ast/return 0)])))]
    (is (= (py-lines
            "import math, numpy as np"
            ""
            "from os.path import join, dirname as dn"
            ""
            "def main():"
            "    return 0")
           (emit/emit-module module)))))

(deftest emit-call-keyword-args
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'call-demo ['x]
                      [(ast/return
                        (ast/call 'f
                                  ['x]
                                  [(ast/keyword-arg 'indent 2)
                                   (ast/keyword-arg 'sort-keys true)]))])))]
    (is (= (py-lines
            "def call_demo(x):"
            "    return f(x, indent=2, sort_keys=True)")
           (emit/emit-module module)))))

(deftest emit-class-def
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/class-def 'Counter
                                   ['BaseCounter]
                                   [(ast/function-def '__init__ ['self 'start]
                                      [(ast/assign (ast/attribute 'self 'value) 'start)])
                                    (ast/function-def 'inc ['self]
                                      [(ast/augassign (ast/attribute 'self 'value) :+ 1)
                                       (ast/return (ast/attribute 'self 'value))])]))
                   (ast/module-add
                    (ast/function-def 'main []
                      [(ast/return 0)])))]
    (is (= (py-lines
            "class Counter(BaseCounter):"
            "    def __init__(self, start):"
            "        self.value = start"
            "    def inc(self):"
            "        self.value += 1"
            "        return self.value"
            ""
            "def main():"
            "    return 0")
           (emit/emit-module module)))))

(deftest emit-list-comprehension
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'comp-demo ['xs 'ys]
                      [(ast/return
                        (ast/list-comp
                         (ast/tuple ['x 'y])
                         [(ast/comprehension 'x 'xs [])
                          (ast/comprehension 'y 'ys [(ast/compare 'x [:<] ['y])])]))])))]
    (is (= (py-lines
            "def comp_demo(xs, ys):"
            "    return [(x, y) for x in xs for y in ys if x < y]")
           (emit/emit-module module)))))

(deftest emit-set-comprehension
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'set-comp-demo ['xs]
                      [(ast/return
                        (ast/set-comp
                         (ast/binop :* 'x 2)
                         [(ast/comprehension 'x 'xs [(ast/compare 'x [:>] [0])])]))])))]
    (is (= (py-lines
            "def set_comp_demo(xs):"
            "    return {x * 2 for x in xs if x > 0}")
           (emit/emit-module module)))))

(deftest emit-dict-comprehension
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'dict-comp-demo ['xs]
                      [(ast/return
                        (ast/dict-comp
                         'x
                         (ast/binop :* 'x 'x)
                         [(ast/comprehension 'x 'xs [(ast/compare 'x [:>] [0])])]))])))]
    (is (= (py-lines
            "def dict_comp_demo(xs):"
            "    return {x: x * x for x in xs if x > 0}")
           (emit/emit-module module)))))

(deftest emit-generator-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'gen-comp-demo ['xs]
                      [(ast/return
                        (ast/generator-exp
                         (ast/binop :* 'x 'x)
                         [(ast/comprehension 'x 'xs [(ast/compare 'x [:>] [0])])]))])))]
    (is (= (py-lines
            "def gen_comp_demo(xs):"
            "    return (x * x for x in xs if x > 0)")
           (emit/emit-module module)))))

(deftest emit-if-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'classify ['x]
                      [(ast/return
                        (ast/if-exp
                         (ast/compare 'x [:>] [0])
                         "pos"
                         (ast/if-exp (ast/compare 'x [:==] [0])
                                     "zero"
                                     "neg")))])))]
    (is (= (py-lines
            "def classify(x):"
            "    return \"pos\" if x > 0 else \"zero\" if x == 0 else \"neg\"")
           (emit/emit-module module)))))

(deftest emit-if-expression-parenthesized-body
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'choose ['x 'y]
                      [(ast/return
                        (ast/if-exp
                         (ast/compare 'x [:==] [0])
                         (ast/if-exp (ast/compare 'y [:>] [0]) "a" "b")
                         "c"))])))]
    (is (= (py-lines
            "def choose(x, y):"
            "    return (\"a\" if y > 0 else \"b\") if x == 0 else \"c\"")
           (emit/emit-module module)))))

(deftest emit-py-named-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'find-first ['xs]
                      [(ast/if-stmt
                        (ast/compare (ast/named-expr 'n
                                                     (ast/call 'len ['xs]))
                                     [:>]
                                     [0])
                        [(ast/return 'n)]
                        [(ast/return 0)])])))]
    (is (= (py-lines
            "def find_first(xs):"
            "    if (n := len(xs)) > 0:"
            "        return n"
            "    else:"
            "        return 0")
           (emit/emit-module module)))))

(deftest emit-py-named-expression-statement-parenthesized
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'demo []
                      [(ast/expr-stmt (ast/named-expr 'x 1))
                       (ast/return 'x)])))]
    (is (= (py-lines
            "def demo():"
            "    (x := 1)"
            "    return x")
           (emit/emit-module module)))))

(deftest emit-py-named-in-call-keyword-value-parenthesized
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'send ['x]
                      [(ast/return (ast/call 'f
                                             []
                                             [(ast/keyword-arg 'k
                                                               (ast/named-expr 'y 'x))]))])))]
    (is (= (py-lines
            "def send(x):"
            "    return f(k=(y := x))")
           (emit/emit-module module)))))

(deftest emit-lambda-expression
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'make-inc ['n]
                      [(ast/return
                        (ast/lambda-expr ['x]
                                         (ast/binop :+ 'x 'n)))])))]
    (is (= (py-lines
            "def make_inc(n):"
            "    return lambda x: x + n")
           (emit/emit-module module)))))

(deftest emit-lambda-call-parenthesized
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'apply-twice ['x]
                      [(ast/return
                        (ast/call (ast/lambda-expr ['y]
                                                   (ast/binop :* 'y 2))
                                  ['x]))])))]
    (is (= (py-lines
            "def apply_twice(x):"
            "    return (lambda y: y * 2)(x)")
           (emit/emit-module module)))))

(deftest emit-varargs-and-kwargs-in-def-and-call
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'invoke ['f '*args '**kwargs]
                      [(ast/return
                        (ast/call 'f
                                  [(ast/starred 'args)]
                                  [(ast/keyword-arg 'indent 2)
                                   (ast/keyword-arg nil 'kwargs)]))])))]
    (is (= (py-lines
            "def invoke(f, *args, **kwargs):"
            "    return f(*args, indent=2, **kwargs)")
           (emit/emit-module module)))))

(deftest emit-structured-arguments-node
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'f
                      (ast/arguments
                       [(ast/arg 'a)]
                       [(ast/arg 'b) (ast/arg 'c)]
                       (ast/arg 'args)
                       [(ast/arg 'd) (ast/arg 'e)]
                       [ast/kw-required 5]
                       (ast/arg 'kwargs)
                       [10])
                      [(ast/return 0)])))]
    (is (= (py-lines
            "def f(a, /, b, c=10, *args, d, e=5, **kwargs):"
            "    return 0")
           (emit/emit-module module)))))

(deftest emit-match-statement
  (let [module (-> (ast/module)
                   (ast/module-add
                    (ast/function-def 'classify ['v 'ready]
                      [(ast/match-stmt
                        'v
                        [(ast/match-case
                          (ast/match-sequence [(ast/match-as nil 'head)
                                               (ast/match-star 'rest)])
                          [(ast/return 'rest)])
                         (ast/match-case
                          (ast/match-mapping ["x"] [(ast/match-as nil 'x)] 'rest)
                          [(ast/return 'x)])
                         (ast/match-case
                          (ast/match-value (ast/attribute 'Color 'RED))
                          'ready
                          [(ast/return "red")])
                         (ast/match-case
                          (ast/match-as nil nil)
                          [(ast/return nil)])])])))]
    (is (= (py-lines
            "def classify(v, ready):"
            "    match v:"
            "        case [head, *rest]:"
            "            return rest"
            "        case {\"x\": x, **rest}:"
            "            return x"
            "        case Color.RED if ready:"
            "            return \"red\""
            "        case _:"
            "            return None")
           (emit/emit-module module)))))
