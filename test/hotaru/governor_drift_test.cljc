(ns hotaru.governor-drift-test
  "The governor holds copies of things declared elsewhere — manifest.edn,
  .well-known/did.json, lex/*.edn. Copies drift. This suite reads the
  declarations and asserts the agreement, so the governor cannot come to
  enforce a charter the actor no longer has.

  Same intent as methods/test_charter_gates.cljc, which pins manifest against
  lexicon. This pins CODE against both. clj-only: it reads files."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])
            [hotaru.governor :as gov]))

#?(:clj
   (do
     (defn- repo-file
       "clojure -M:test runs at the repo root."
       [rel]
       (io/file (System/getProperty "user.dir") rel))

     (defn- read-edn [rel]
       (let [f (repo-file rel)]
         ;; Refuse to answer rather than report a pass from a file we could not
         ;; read: a drift test that silently skips is a drift test that is green
         ;; for the same reason whether or not the code agrees with the charter.
         (assert (.exists f) (str "cannot read " rel " — refusing to report agreement"))
         (edn/read-string (slurp f))))

     (def manifest (delay (read-edn "manifest.edn")))
     (def did-json-text (delay (slurp (repo-file ".well-known/did.json"))))

     (defn- lex-field [lexicon field k]
       (get-in (read-edn (str "lex/" lexicon ".edn"))
               [:defs :main :record :properties field k]))

     (deftest declared-collections-match-the-manifest
       (let [ids (mapv :lex/id (:actor/lex @manifest))]
         (is (seq ids) "manifest must declare lexicons")
         (is (= (set ids) (set gov/declared-lexicons))
             "governor/declared-lexicons must equal manifest.edn :actor/lex")
         (is (= (into #{} (map gov/collection) ids) gov/declared-collections))))

     (deftest actor-phase-matches-the-manifest
       (is (= (:actor/status @manifest) gov/actor-phase)
           "governor/actor-phase must equal manifest.edn :actor/status — G8 is tied to it"))

     (deftest open-licenses-match-the-lexicon-enum
       (is (= (set (lex-field "processKnowledge" :sourceLicense :enum))
              gov/open-licenses)
           "G1: governor/open-licenses must equal processKnowledge.sourceLicense enum")
       (is (not-any? #(str/includes? (str/lower-case %) "proprietary") gov/open-licenses)))

     (deftest council-level-matches-the-review-lexicon-const
       (is (= (lex-field "silenHotaruReview" :councilLevel :const)
              gov/council-level-required)
           "G8: governor/council-level-required must equal silenHotaruReview.councilLevel const"))

     (deftest claimed-dids-match-the-did-document
       (let [declared (set (map #(str/replace % #"#.*$" "")
                                (re-seq #"did:web:[A-Za-z0-9._%:-]+" @did-json-text)))]
         (is (seq declared) "did.json must contain at least one did:web")
         (is (contains? declared (:actor/did @manifest))
             "manifest :actor/did must appear in did.json")
         (is (= declared gov/claimed-dids)
             (str "governor/claimed-dids must equal the did:web set of did.json; "
                  "declared=" (pr-str (vec (sort declared)))))))

     (deftest every-numbered-gate-the-governor-cites-is-declared
       (let [declared (set (map :gate/id (:actor/gates @manifest)))
             ;; the gates this governor can actually return, read off the source
             ;; rather than restated here
             cited (set (re-seq #"\"G\d+\""
                                (slurp (repo-file "src/hotaru/governor.cljc"))))
             cited (set (map #(str/replace % "\"" "") cited))]
         (is (seq declared) "manifest must declare gates")
         (is (seq cited) "the governor must cite at least one numbered gate")
         (is (empty? (remove declared cited))
             (str "governor cites gates the manifest does not declare: "
                  (pr-str (vec (sort (remove declared cited))))))))))
