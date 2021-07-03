(ns refactor-nrepl.find.find-macros-test
  (:require [clojure.test :refer :all]
            [refactor-nrepl.find.find-macros :refer [find-macro]]))

(defn- found? [regexp occurrences]
  (first (filter #(re-find regexp (:match %)) occurrences)))

(def ignore-errors?
  "Currently errors while analysing the classpath (most relevantly our `test-resources` dir) are ignored.

  This is because some files in that dir are intentionally invalid, which intentionally represents a use case
  (e.g. users can have WIP code, files not meant to be vanilla namespaces, etc)."
  true)

(deftest find-macro-test
  (let [occurrences (find-macro "com.example.macro-def/my-macro" ignore-errors?)
        {:keys [line-beg col-beg ^String file match]}
        (first (filter #(.contains ^String (:match %) "defmacro") occurrences))]
    (testing "finds the macro definition"
      (is (found? #"defmacro my-macro" occurrences)))
    (testing "finds fully qualified reference"
      (is (found? #"com.example.macro-def/my-macro :fully-qualified"
                  occurrences)))
    (testing "finds referred reference"
      (is (found? #"my-macro :referred" occurrences)))
    (testing "finds aliased reference"
      (is (found? #"m/my-macro :aliased" occurrences)))
    (testing "doesn't find macro shadowed by function parameter"
      (is (not (found? #"my-macro :shadowed-by-function-param" occurrences))))
    (testing "doesn't find macro shadowed by let parameter"
      (is (not (found? #"my-macro :shadowed-by-let" occurrences))))
    (testing "has the right meta"
      (is (= "(defmacro my-macro [& body])" match) "match")
      (is (= 3 line-beg) "line-beg")
      (is (= 11 col-beg) "col-beg")
      (is (.endsWith file "macro_def.clj")))))

(deftest find-regular-symbol-test
  (is (nil? (find-macro "sym" ignore-errors?))))

(deftest find-fully-qualified-random-name
  (is (nil? (find-macro "asdf" ignore-errors?))))

(deftest find-fully-qualified-fn
  (is (nil? (find-macro "refactor-nrepl.find.find-macros/find-macro" ignore-errors?))))

(deftest finds-macro-defined-in-cljc-file
  (is (found? #"defmacro cljc-macro"
              (find-macro "com.example.macro-def-cljc/cljc-macro" ignore-errors?))))

(deftest finds-macro-defined-in-cljc-file-and-used-in-clj-file
  (is (found? #"(com.example.macro-def-cljc/cljc-macro :fully-qualified)"
              (find-macro "com.example.macro-def-cljc/cljc-macro" ignore-errors?))))

(deftest macro-definitions-are-cached
  (find-macro "com.example.macro-def/my-macro" ignore-errors?)
  (with-redefs [refactor-nrepl.find.find-macros/put-cached-macro-definitions
                (fn [& _] (throw (ex-info "Cache miss!" {})))]
    (is (found? #"defmacro my-macro" (find-macro "com.example.macro-def/my-macro" ignore-errors?))))
  (reset! @#'refactor-nrepl.find.find-macros/macro-defs-cache {})
  (with-redefs [refactor-nrepl.find.find-macros/put-cached-macro-definitions
                (fn [& _] (throw (Exception. "Expected!")))]
    (is (thrown-with-msg? Exception #"Expected!"
                          (find-macro "com.example.macro-def/my-macro" false)))))
