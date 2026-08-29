(ns hotaru.murakumo-test
  "Generic contract tests for the manifest-migration-scaffold cljc actor
  boundary (hotaru.murakumo): gate-value / missing-gates / put-record-effect /
  records-for / cell-plan / all-cell-plans. Introspects `cell-specs` rather
  than hardcoding cell names, so it holds regardless of which cells this
  actor's manifest declares.

  NOTE the cost of that generality: this suite was green while `cell-specs`
  described a single placeholder cell `:null` and manifest.edn declared five
  real ones. Being green here says nothing about whether the boundary knows
  what this actor is. `hotaru.governor-drift-test` is where code is held
  against the declarations."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hotaru.murakumo :as m]))

(def full-attestations
  (into {}
        (map (fn [gate] [gate (str "attested-" (name gate))]))
        (distinct (mapcat :required-gates (vals m/cell-specs)))))

(deftest gate-value-handles-map-and-set-attestations
  (testing "map attestations, keyword key"
    (is (= "yes" (m/gate-value {:g "yes"} :g))))
  (testing "map attestations, string key fallback"
    (is (= "yes" (m/gate-value {"g" "yes"} :g))))
  (testing "set attestations, keyword member"
    (is (= :g (m/gate-value #{:g} :g))))
  (testing "set attestations, string member fallback"
    (is (= "g" (m/gate-value #{"g"} :g))))
  (testing "missing gate returns nil"
    (is (nil? (m/gate-value {} :g)))))

(deftest missing-gates-computes-the-diff
  (let [some-spec (first (vals m/cell-specs))
        all-gates (:required-gates some-spec)]
    (testing "no attestations -> every required gate is missing"
      (is (= all-gates (m/missing-gates some-spec {}))))
    (testing "all attested -> nothing missing"
      (is (empty? (m/missing-gates some-spec full-attestations))))
    (when (seq all-gates)
      (testing "partially attested -> only the unattested gate is missing"
        (let [partial (dissoc full-attestations (first all-gates))]
          (is (= [(first all-gates)] (m/missing-gates some-spec partial))))))))

(deftest put-record-effect-shape
  (let [effect (m/put-record-effect "com.example.coll" "rk-1" {:a 1})]
    (is (= :mst/put-record (:op effect)))
    (is (= m/actor-did (:actor effect)))
    (is (= "com.example.coll" (:collection effect)))
    (is (= "rk-1" (:rkey effect)))
    (is (= {:a 1} (:record effect)))))

(deftest records-for-produces-one-record-per-collection
  (doseq [[cell-key spec] m/cell-specs]
    (let [recs (m/records-for spec {:request-id (str "req-" (name cell-key))})]
      (is (= (count (:collections spec)) (count recs))
          (str cell-key ": one record per declared collection"))
      (doseq [{:keys [collection record rkey]} recs]
        (is (contains? (set (:collections spec)) collection))
        (is (= m/actor-did (:actorDid record)))
        (is (true? (:scaffold record)))
        (is (= (:legacy-cell spec) (:legacyCell record)))
        (is (string? rkey))
        (is (not (str/blank? rkey)))))))

(deftest records-for-honors-explicit-record-override
  (let [[_ spec] (first (filter (fn [[_ s]] (= 1 (count (:collections s))))
                                 m/cell-specs))]
    (when spec
      (let [recs (m/records-for spec {:record {:rkey "custom-rk" :note "override"}})]
        (is (= "custom-rk" (:rkey (first recs))))
        (is (= "override" (:note (:record (first recs)))))))))

(deftest cell-plan-blocks-when-gates-missing
  (doseq [cell-key (keys m/cell-specs)]
    (let [plan (m/cell-plan cell-key {})]
      (is (= :blocked (:status plan)))
      (is (empty? (:effects plan)))
      (is (= (get-in m/cell-specs [cell-key :required-gates]) (:missing-gates plan))))))

(deftest baseline-attestations-do-not-by-themselves-authorize-publication
  ;; This used to assert :ready. It was asserting the defect: the seven baseline
  ;; attestations are generic, none of them is one of the eleven charter gates
  ;; this actor declares, and satisfying them was enough to emit an atproto
  ;; put-record while the actor is at R0. The baselines are now a precondition
  ;; for being HEARD by the governor, not an authorization.
  (doseq [cell-key (keys m/cell-specs)]
    (let [plan (m/cell-plan cell-key {:attestations full-attestations :request-id "req-1"})]
      (is (empty? (:missing-gates plan)) "the baselines themselves are satisfied")
      (is (= :refused (:status plan))
          "no charter authority was presented, so nothing may leave the actor")
      (is (empty? (:effects plan)) "a refused plan emits no effects")
      (is (seq (:refusals plan)) "and it says why"))))

(deftest cell-plan-refusals-name-a-gate-and-a-reason
  (doseq [cell-key (keys m/cell-specs)]
    (let [plan (m/cell-plan cell-key {:attestations full-attestations :request-id "req-1"})]
      (doseq [r (:refusals plan)]
        (is (contains? #{:refuse :undecidable} (:decision r)))
        (is (some? (:gate r)))
        (is (keyword? (:reason r)))
        (is (string? (:detail r)))))))

(deftest the-scaffold-cell-cannot-be-published-even-with-full-authority
  ;; cell-specs still describes one placeholder cell, :null, whose collection
  ;; com.etzhayyim.hotaru.null has no lexicon in lex/ and is not in manifest.edn
  ;; :actor/lex. Council authority does not make an undeclared collection
  ;; publishable, and the governor says so by name.
  (let [plan (m/cell-plan :null {:attestations full-attestations
                                 :request-id "req-1"
                                 :council-level "Lv7+"
                                 :operator-signature "operator-sig-1"})]
    (is (= :refused (:status plan)))
    (is (= [:undeclared-collection] (mapv :reason (:refusals plan))))))

(deftest governor-ctx-passes-only-the-claimed-authority
  (is (= {:council-level "Lv7+" :operator-signature "s" :server-signature "x"}
         (m/governor-ctx {:council-level "Lv7+" :operator-signature "s"
                          :server-signature "x" :request-id "req-1" :records {}})))
  (is (= {} (m/governor-ctx {:request-id "req-1"}))))

(deftest cell-plan-throws-on-unknown-cell
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
               (m/cell-plan :totally-not-a-real-cell {}))))

(deftest all-cell-plans-covers-every-cell
  (let [plans (m/all-cell-plans {:attestations full-attestations :request-id "req-1"})]
    (is (= (set (keys m/cell-specs)) (set (keys plans))))
    (is (every? #(= :refused (:status %)) (vals plans))
        "every cell is governed, not just the one a test happened to name")))
