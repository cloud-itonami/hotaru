(ns hotaru.governor-test
  "Both directions of the charter gate over hotaru's outward effects.

  The discipline every refusal test here follows: start from ONE baseline
  effect that the governor permits, change EXACTLY ONE field, and assert the
  reason literal that names the thing that was changed. A test that only
  asserted 'it refused' would also pass when the effect was refused for some
  unrelated reason, which is how a gate comes to be believed for a job it is
  not doing."
  (:require [clojure.test :refer [deftest is testing]]
            [hotaru.governor :as gov]))

;; ── the one effect this actor could lawfully publish, and the authority to ──
;; ── publish it. Everything below is this, minus one thing.                 ──

(def authorized
  {:council-level "Lv7+" :operator-signature "operator-sig-1"})

(def permitted-effect
  {:op :mst/put-record
   :actor "did:web:etzhayyim.com:actor:hotaru"
   :collection "com.etzhayyim.hotaru.processKnowledge"
   :rkey "rk-1"
   :record {:$type "com.etzhayyim.hotaru.processKnowledge"
            :procId "inp-lec-01"
            :stage "bulk-growth"
            :sourceLicense "academic-oa"
            :sourceCite "doi:10.0000/example"
            :maturity "open-mature"
            :screened true
            :fabricated false}})

(defn- decide
  ([effect] (decide effect authorized))
  ([effect ctx] (gov/review-effect effect ctx)))

;; ── the positive control ───────────────────────────────────────────────────
;; If this ever stops permitting, every refusal test below has stopped proving
;; what it claims: they would all be refusing for whatever broke this instead.

(deftest permits-a-declared-record-under-council-authority
  (let [d (decide permitted-effect)]
    (is (= :permit (:decision d)) (str "expected a permit, got " (pr-str d)))
    (is (gov/permitted? d))))

;; ── G8 — the headline. R0 is offline-only. ─────────────────────────────────

(deftest g8-refuses-outward-reach-at-r0-without-council-authority
  (testing "no authority at all"
    (let [d (decide permitted-effect {})]
      (is (= :refuse (:decision d)))
      (is (= "G8" (:gate d)))
      (is (= :outward-gated-at-r0 (:reason d)))))
  (testing "Council level below Lv7+ is not enough"
    (let [d (decide permitted-effect (assoc authorized :council-level "Lv6+"))]
      (is (= :outward-gated-at-r0 (:reason d)))))
  (testing "Council Lv7+ without an operator signature is not enough — G8 is both"
    (let [d (decide permitted-effect (dissoc authorized :operator-signature))]
      (is (= :outward-gated-at-r0 (:reason d)))))
  (testing "an empty operator signature is not a signature"
    (let [d (decide permitted-effect (assoc authorized :operator-signature ""))]
      (is (= :outward-gated-at-r0 (:reason d))))))

;; ── G2 — design-only. A fabricated wafer is unrepresentable through R3. ────

(deftest g2-refuses-a-fabricated-record
  (doseq [v [true "true"]]
    (let [d (decide (assoc-in permitted-effect [:record :fabricated] v))]
      (is (= :refuse (:decision d)))
      (is (= "G2" (:gate d)))
      (is (= :fabricated-record (:reason d)) (str "fabricated " (pr-str v)))))
  (testing "fabricated false is the lawful value and does not refuse"
    (is (gov/permitted? (decide (assoc-in permitted-effect [:record :fabricated] false)))))
  (testing "string key spelling is read too"
    (let [e (update permitted-effect :record #(-> % (dissoc :fabricated) (assoc "fabricated" true)))]
      (is (= :fabricated-record (:reason (decide e)))))))

(deftest g2-refuses-denying-the-fabrication-prohibition
  (let [d (decide (assoc-in permitted-effect [:record :fabricationProhibited] false))]
    (is (= "G2" (:gate d)))
    (is (= :fabrication-prohibition-denied (:reason d))))
  (testing "affirming it is fine"
    (is (gov/permitted? (decide (assoc-in permitted-effect [:record :fabricationProhibited] true))))))

;; ── G1 — open-IP only. ─────────────────────────────────────────────────────

(deftest g1-refuses-a-closed-source-license
  (doseq [lic ["vendor-proprietary" "patent-active" "trade-secret" :vendor-proprietary]]
    (let [d (decide (assoc-in permitted-effect [:record :sourceLicense] lic))]
      (is (= :refuse (:decision d)))
      (is (= "G1" (:gate d)))
      (is (= :closed-source-license (:reason d)) (str "license " (pr-str lic)))))
  (testing "every member of the open set is accepted, keyword or string"
    (doseq [lic gov/open-licenses]
      (is (gov/permitted? (decide (assoc-in permitted-effect [:record :sourceLicense] lic))))
      (is (gov/permitted? (decide (assoc-in permitted-effect [:record :sourceLicense] (keyword lic))))))))

;; ── G5 — no server-held key. ───────────────────────────────────────────────

(deftest g5-refuses-a-server-signature
  (let [d (decide permitted-effect (assoc authorized :server-signature "server-sig"))]
    (is (= "G5" (:gate d)))
    (is (= :server-held-key (:reason d))))
  (testing "a record that admits a server-held key is refused the same way"
    (let [d (decide (assoc-in permitted-effect [:record :serverHeldKey] true))]
      (is (= :server-held-key (:reason d))))))

;; ── attribution and manifest conformance ───────────────────────────────────

(deftest refuses-an-unclaimed-actor-did
  (testing "the DID the boundary used before this governor existed"
    (let [d (decide (assoc permitted-effect :actor "did:web:hotaru.etzhayyim.com"))]
      (is (= :refuse (:decision d)))
      (is (= "did-primary" (:gate d)))
      (is (= :unclaimed-actor-did (:reason d)))))
  (testing "every claimed DID is accepted"
    (doseq [did gov/claimed-dids]
      (is (gov/permitted? (decide (assoc permitted-effect :actor did)))))))

(deftest refuses-an-undeclared-collection
  (testing "the scaffold collection, which has no lexicon in this repo"
    (let [d (decide (assoc permitted-effect :collection "com.etzhayyim.hotaru.null"))]
      (is (= :refuse (:decision d)))
      (is (= "manifest-conformance" (:gate d)))
      (is (= :undeclared-collection (:reason d)))))
  (testing "every declared collection is accepted"
    (doseq [c gov/declared-collections]
      (is (gov/permitted? (decide (assoc permitted-effect :collection c)))))))

;; ── the third outcome: could not decide is not a yes ───────────────────────

(deftest undecidable-is-not-a-permit
  (testing "an effect that is not a map"
    (let [d (decide "not-an-effect")]
      (is (= :undecidable (:decision d)))
      (is (= :effect/not-a-map (:reason d)))
      (is (not (gov/permitted? d)))))
  (testing "an effect with no :op"
    (let [d (decide (dissoc permitted-effect :op))]
      (is (= :undecidable (:decision d)))
      (is (= :effect/no-op (:reason d)))
      (is (not (gov/permitted? d)))))
  (testing "an op in neither outward-ops nor inward-ops"
    (let [d (decide (assoc permitted-effect :op :mst/delete-the-universe))]
      (is (= :undecidable (:decision d)))
      (is (= :effect/unknown-op (:reason d)))
      (is (not (gov/permitted? d)))))
  (testing "an outward effect whose record cannot be read — G1/G2 uncheckable"
    (let [d (decide (assoc permitted-effect :record "opaque"))]
      (is (= :undecidable (:decision d)))
      (is (= :record/unreadable (:reason d)))
      (is (not (gov/permitted? d))))))

;; ── the evidence floor ─────────────────────────────────────────────────────

(deftest reviewing-nothing-is-not-permitting-everything
  (is (false? (gov/permit-all? []))
      "an empty decision set must not be reported as a clean review")
  (is (false? (gov/permit-all? nil)))
  (is (true? (gov/permit-all? (gov/review-effects [permitted-effect] authorized))))
  (is (false? (gov/permit-all? (gov/review-effects [permitted-effect] {})))))

(deftest review-effects-keeps-each-effect-beside-its-decision
  (let [ds (gov/review-effects [permitted-effect
                                (assoc permitted-effect :collection "com.etzhayyim.hotaru.null")]
                               authorized)]
    (is (= 2 (count ds)))
    (is (= [:permit :refuse] (mapv :decision ds)))
    (is (= [permitted-effect] (mapv :effect (filter gov/permitted? ds))))
    (is (= 1 (count (gov/refusals ds))))))

;; ── ordering: a specific gate is named before the blanket phase gate ───────

(deftest specific-gates-are-named-before-the-blanket-phase-gate
  (testing "a fabricated record at R0 with no authority is reported as G2, not G8"
    (let [d (decide (assoc-in permitted-effect [:record :fabricated] true) {})]
      (is (= "G2" (:gate d)))
      (is (= :fabricated-record (:reason d)))))
  (testing "and a clean record with no authority is reported as G8"
    (is (= "G8" (:gate (decide permitted-effect {}))))))
