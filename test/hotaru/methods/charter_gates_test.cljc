(ns hotaru.methods.charter-gates-test
  "hotaru 蛍 — constitutional-gate conformance tests (manifest + local lexicons).

  Substrate-native Clojure (clj + datomic first tier). hotaru is the III-V/InP substrate
  open-publication COMMONS — it is NOT a fab: a fabricated wafer is structurally unrepresentable,
  and it publishes only open-IP process knowledge. It reads the first-tier `lex/*.edn` via
  clojure.edn and the manifest via cheshire. Its 11 gates are declared in the manifest and
  encoded as enum/const across the 6 lexicons. This suite pins them so a future R-phase cell wave
  cannot silently drift them:

    G2  design-only / NOT fabricated — fabricated const false on every spec/design; the review +
        readiness report pin fabricationProhibited true (the headline 'not a fab' gate)
    G1  open-IP only — processKnowledge.sourceLicense ∈ open set (no proprietary license)
    G4  conflict-mineral sourcing — In/Ga sourcing ∈ {recycled, conflict-free-attested}
    G9/G11 process-safety + export-control honesty — acute-toxic/pyrophoric acknowledged + cleared;
        exportControls recorded
    G8  outward-gated at Lv7+ — silenHotaruReview.councilLevel const Lv7+
    G7  sourcing-honesty — records carry a sourcing ∈ {representative, verified}; knowledge screened

  Reads manifest via cheshire + local lexicons via clojure.edn. It weakens no gate; it asserts
  them. G5 (no-server-key) + Murakumo-only (G6) are manifest-level and untouched."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.edn :as edn]))

#?(:clj
   (do
     (defn- lex [name]
       (edn/read-string (slurp (str "contracts/lexicons/" name ".edn"))))
     (defn- manifest []
       (edn/read-string (slurp "manifest.edn")))))

(defn- record-node [doc] (get-in doc [:defs :main :record]))
(defn- enum-of [doc field]
  (let [p (get-in (record-node doc) [:properties field])]
    (set (or (:enum p) (get-in p [:items :enum])))))   ;; scalar enum OR array-of-enum
(defn- const-of [doc field] (get-in (record-node doc) [:properties field :const]))

;; ── 11 gates + non-goals declared ──
(deftest gates-and-nongoals-declared
  (let [gn (->> (:actor/gates (manifest)) (map :gate/id)
                (keep #(second (re-matches #"G(\d+).*" %)))
                (map #(Integer/parseInt %)) set)]
    (is (= (set (range 1 12)) gn) "manifest must declare G1–G11")
    (is (seq (:actor/non-goals (manifest))) "manifest must declare nonGoals")))

;; ── G2 — design-only / NOT fabricated (the headline 'not a fab' gate) ──
(deftest g2-not-fabricated
  (doseq [n ["waferSpec" "crystalGrowthDesign"]]
    (is (= false (const-of (lex n) :fabricated))
        (str "G2: " n ".fabricated const false (a fabricated wafer is unrepresentable)")))
  (is (= true (const-of (lex "silenHotaruReview") :fabricationProhibited))
      "G2: silenHotaruReview.fabricationProhibited const true")
  (is (= true (const-of (lex "commonsReadinessReport") :fabricationProhibited))
      "G2: commonsReadinessReport.fabricationProhibited const true"))

;; ── G1 — open-IP only ──
(deftest g1-open-ip-only
  (let [e (enum-of (lex "processKnowledge") :sourceLicense)]
    (is (= #{"academic-oa" "patent-expired" "textbook-public" "standard-public" "own-rnd"} e)
        (str "G1: sourceLicense must be the open-IP set, got " e))
    (is (not (some #(clojure.string/includes? (clojure.string/lower-case %) "proprietary") e))
        "G1: no proprietary license representable")))

;; ── G4 — conflict-mineral sourcing ──
(deftest g4-conflict-mineral-sourcing
  (doseq [n ["crystalGrowthDesign" "precursorSafetyAttestation"]]
    (is (= #{"recycled" "conflict-free-attested"} (enum-of (lex n) :inSourcing))
        (str "G4: " n ".inSourcing must be {recycled, conflict-free-attested}"))))

;; ── G9/G11 — process-safety + export-control honesty ──
(deftest g9-g11-precursor-safety
  (let [p (lex "precursorSafetyAttestation")]
    (is (= true (const-of p :acuteToxicAcknowledged)) "G11: acute-toxic/pyrophoric acknowledged")
    (is (= true (const-of p :cleared)) "G11: precursor safety cleared")
    (is (= #{"none" "ear" "itar"} (enum-of p :exportControls)) "G9: export-control posture recorded")))

;; ── G8 — outward-gated at Council Lv7+ ──
(deftest g8-outward-gated-lv7
  (is (= "Lv7+" (const-of (lex "silenHotaruReview") :councilLevel))
      "G8: silenHotaruReview.councilLevel const Lv7+"))

;; ── G7 — sourcing-honesty ──
(deftest g7-sourcing-honesty
  (doseq [n ["waferSpec" "crystalGrowthDesign" "processKnowledge" "precursorSafetyAttestation"]]
    (is (= #{"representative" "verified"} (enum-of (lex n) :sourcing))
        (str "G7: " n ".sourcing must be {representative, verified}")))
  (is (= true (const-of (lex "processKnowledge") :screened))
      "G7: process knowledge is screened (const true)"))

#?(:clj
   (defn -main [& _]
     (let [r (run-tests 'hotaru.methods.test-charter-gates)]
       (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))))
