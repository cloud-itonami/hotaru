(ns hotaru.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold.

  Outward effects planned here are decided by `hotaru.governor` before they are
  emitted. The baseline attestations below are a PRECONDITION, not the charter:
  satisfying them gets a plan as far as the governor, which then asks the gates
  this actor actually declares in manifest.edn."
  (:require [clojure.string :as str]
            [hotaru.governor :as gov]))

(def actor-did
  "manifest.edn :actor/did and the `id` of .well-known/did.json. The previous
  value here, did:web:hotaru.etzhayyim.com, appeared nowhere else in the repo
  and is not in did.json alsoKnownAs — the boundary was attributing its effects
  to a DID the actor does not claim."
  "did:web:etzhayyim.com:actor:hotaru")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.hotaru." name))

(def cell-specs {
  :null {:legacy-cell "null"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "null")]
     :required-gates common-gates
     :trigger "manifest cell null"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn governor-ctx
  "The authority the caller claims to hold, handed to the governor. Absent keys
  mean absent authority — the governor refuses rather than assuming."
  [input]
  (select-keys input [:council-level :operator-signature :server-signature]))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)
               planned-effects (mapv (fn [{:keys [collection record rkey]}]
                                       (put-record-effect collection rkey record))
                                     planned-records)
               decisions (gov/review-effects planned-effects (governor-ctx input))]
           (if (gov/permit-all? decisions)
             {:status :ready
              :records (vec planned-records)
              :effects planned-effects
              :governor-decisions decisions}
             ;; Fail closed on the whole set rather than emitting the permitted
             ;; subset: a partially governed publication is not a governed one.
             {:status :refused
              :records (vec planned-records)
              :effects []
              :governor-decisions decisions
              :refusals (gov/refusals decisions)})))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
