(ns refactor-nrepl.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [refactor-nrepl.config :as config]
   [refactor-nrepl.core :as sut]))

(defmacro assert-ignored-paths
  [paths pred]
  `(doseq [p# ~paths]
     (is (~pred (sut/ignore-dir-on-classpath? p#)))))

(deftest test-ignore-dir-on-classpath?
  (let [not-ignored ["/home/user/project/test"
                     "/home/user/project/src"
                     "/home/user/project/target/classes"]
        sometimes-ignored ["/home/user/project/checkouts/subproject"
                           "/home/user/project/resources"]
        always-ignored ["/home/user/project/target/srcdeps"]]
    (testing "predicate to ignore dirs on classpath with default config"
      (assert-ignored-paths (concat not-ignored sometimes-ignored) false?)
      (assert-ignored-paths always-ignored true?))
    (testing "predicate to ignore dirs on classpath with custom config"
      (binding [config/*config* (assoc config/*config*
                                       :ignore-paths
                                       [#".+checkouts/.+" #"resources"])]
        (assert-ignored-paths not-ignored false?)
        (assert-ignored-paths (concat always-ignored sometimes-ignored) true?)))))

(deftest readable-file?
  (let [[unreadable-file
         readable-file-incorrect-aliases
         readable-file-incorrect-data-readers
         :as all-files] (->> ["unreadable_file.clj"
                              "readable_file_incorrect_aliases.clj"
                              "readable_file_incorrect_data_readers.clj"]
                             (map (comp io/as-file io/resource)))]
    (doseq [file all-files]
      (assert (try
                (-> file slurp read-string)
                false
                (catch Exception _
                  true))
              "All files are *generally* unreadable (an assumption that makes this deftest logically valid)"))
    (are [input expected] (= expected
                             (sut/readable-file? input))
      unreadable-file                      false
      readable-file-incorrect-aliases      true
      readable-file-incorrect-data-readers true)))
