(ns hotaru.methods.test-lexicon-wire
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [babashka.fs :as fs]))

(deftest generated-wire-matches-canonical-edn
  (doseq [source (sort (fs/glob "lex" "*.edn"))]
    (let [doc (edn/read-string (slurp (str source)))
          parts (.split ^String (:id doc) "\\.")
          target (fs/path "lexicons" (apply fs/path (butlast parts))
                          (str (last parts) ".json"))]
      (is (fs/exists? target) (str "wire output exists for " (:id doc)))
      (is (= doc (json/parse-string (slurp (str target)) true))
          (str "wire output matches canonical EDN for " (:id doc))))))
