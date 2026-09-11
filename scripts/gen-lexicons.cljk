#!/usr/bin/env bb
(ns gen-lexicons
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]))

(def source-dir "lex")
(def target-root "lexicons")

(defn target-for [doc]
  (let [parts (.split ^String (:id doc) "\\.")]
    (str (fs/path target-root
                  (apply fs/path (butlast parts))
                  (str (last parts) ".json")))))

(defn generated [source]
  (let [doc (edn/read-string (slurp (str source)))]
    [(target-for doc) (str (json/generate-string doc {:pretty true}) "\n")]))

(defn -main [& args]
  (let [check? (some #{"--check"} args)
        outputs (map generated (sort (fs/glob source-dir "*.edn")))
        stale (filter (fn [[target body]]
                        (or (not (fs/exists? target)) (not= body (slurp target))))
                      outputs)]
    (if check?
      (when (seq stale)
        (doseq [[target] stale] (binding [*out* *err*] (println "stale generated lexicon:" target)))
        (System/exit 1))
      (doseq [[target body] outputs]
        (fs/create-dirs (fs/parent target))
        (spit target body)
        (println "generated" target)))))

(apply -main *command-line-args*)
