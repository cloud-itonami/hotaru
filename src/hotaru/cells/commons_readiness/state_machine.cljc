(ns hotaru.cells.commons-readiness.state-machine
  "Phase state machine for the hotaru 蛍 commons_readiness cell.

  1:1 port of `cells/commons_readiness/state_machine.py` (III-V/InP substrate
  open-publication commons; ADR-2606051200, gate parent ADR-2605265500 §2).

  Aggregates per-stage open-publication coverage into a commonsReadinessReport
  routed to the ADR-2605265500 §2 R4+ gate evaluation, and STRUCTURALLY enforces
  non-adjudication.

  Invariant enforced:
    G3 — non-adjudicating: the report carries `fabricationProhibited` forced
         true and `r4GateSatisfiable` as a COMPUTED fact about the commons; it
         can NEVER carry a 'fabrication opened/permitted/decided' field. Any
         attempt to mark the gate as decided/opened here raises — opening the
         gate is Council Lv7+, never this actor.

  Maturity scoring (substrate stages only): open-mature=1.0, open-emerging=0.5,
  gap=0.0, absent=0.0, averaged over the 4 substrate stages → a single 0..1
  commons-maturity score.

  Conventions (mimamori/methods/bond.cljc + shionome house style):
    - @dataclass ReadinessState → a plain map with kebab keyword keys
    - Python \":…\" string identities stay strings (maturity labels)
    - ReadinessPhase enum value identities (\"init\"/\"assessed\"/\"reported\") stay strings
    - transitions are pure fns; closed-vocab / illegal-transition / G3 → ex-info"
  (:require [clojure.string :as str]))

;; ── module constants (Python module-level) ──────────────────────

(def substrate-stages
  "The 4 substrate stages, in order (SUBSTRATE_STAGES)."
  ["synthesis" "bulk-growth" "wafering" "surface-prep"])

(def stage-weight
  "Per-maturity weight (_STAGE_WEIGHT). Unknown maturity → not a member → raise."
  {"open-mature" 1.0 "open-emerging" 0.5 "gap" 0.0 "absent" 0.0})

(def forbidden-keys
  "Fields that would turn a report into an adjudication — forbidden (G3)."
  ["fabricationOpened" "fabricationPermitted" "gateDecided" "gateOpened"])

;; ── ReadinessPhase (enum — Python value identities preserved) ────

(def readiness-phases
  "The closed ReadinessPhase vocabulary, keyed by the idiomatic Clojure enum
  keyword; the value is the Python `ReadinessPhase.<X>.value` string identity."
  {:init     "init"
   :assessed "assessed"
   :reported "reported"})

(def readiness-phase-init     (:init readiness-phases))      ;; "init"
(def readiness-phase-assessed (:assessed readiness-phases))  ;; "assessed"
(def readiness-phase-reported (:reported readiness-phases))  ;; "reported"

;; ── ReadinessState (dataclass → plain map, kebab keys, field defaults) ──

(def readiness-state
  "ReadinessState default value — the @dataclass field defaults as a plain map."
  {:phase                   readiness-phase-init   ;; ReadinessPhase.INIT.value
   :per-stage               {}                     ;; {stage best-maturity-string}
   :epitaxy-open-mature     false
   :stages-covered          0
   :substrate-commons-ready false
   :r4-gate-satisfiable     false
   :maturity-score          0.0
   :conflict-flagged        0
   :sourcing                "derived"
   :payload                 {}})

(defn make-readiness-state
  "Construct a ReadinessState map from a partial cell-state map, filling the
  dataclass defaults (ReadinessState(**state.get(\"cell_state\", {}))). Unknown
  keys → ex-info (closed ReadinessState surface — ReadinessState(**...) would
  TypeError on an unexpected kwarg)."
  [cs]
  (let [cs (or cs {})
        allowed (set (keys readiness-state))
        extra (remove allowed (keys cs))]
    (when (seq extra)
      (throw (ex-info (str "unknown ReadinessState field(s): " (vec extra))
                      {:hotaru/closed-vocab true :extra (vec extra)})))
    (merge readiness-state cs)))

;; ── helpers ──────────────────────────────────────────────────────

(defn- round4
  "round(x, 4) — round to 4 decimal places. Returns a double."
  [x]
  (/ (Math/round (* (double x) 10000.0)) 10000.0))

(defn- norm
  "Python `_norm`: (v or '').lstrip(':') — coerce nil → \"\", strip leading ':'
  (so an EDN keyword-form input like \":open-mature\" reads as \"open-mature\")."
  [v]
  (let [s (if (nil? v) "" (str v))]
    (str/replace s #"^:+" "")))

;; ── transitions ──────────────────────────────────────────────────

(defn transition-to-assessed
  "Port of `transition_to_assessed(state)`. Compute coverage + maturity score.
  G3: reject any adjudicating input key. Pure: takes the wrapper state map
  {:cell-state … :per-stage … :epitaxy-open-mature … :conflict-flagged …} and
  returns {:cell-state <next>}.

  per-stage / epitaxy-open-mature / conflict-flagged from the top-level state
  override the cell-state's own, matching the Python `state.get(k, cs.k)` form."
  [state]
  (let [state (or state {})
        cs (make-readiness-state (:cell-state state))]
    ;; G3 — reject any adjudicating input key present on the top-level state.
    (doseq [k forbidden-keys]
      (when (contains? state k)
        (throw (ex-info
                (str "G3 violation: commons_readiness is non-adjudicating; it "
                     "cannot carry " (pr-str k) ". Opening the ADR-2605265500 §2 "
                     "R4+ gate is Council Lv7+ only — this cell reports the "
                     "commons, it never decides fabrication.")
                {:hotaru/gate-violation true :gate "G3" :key k}))))
    (let [raw (or (get state :per-stage (:per-stage cs)) {})
          per-stage (reduce-kv (fn [m k v] (assoc m (norm k) (norm v))) {} raw)
          epitaxy-open-mature (boolean (get state :epitaxy-open-mature
                                            (:epitaxy-open-mature cs)))
          conflict-flagged (long (get state :conflict-flagged (:conflict-flagged cs)))
          [covered score-sum]
          (reduce
           (fn [[cov ssum] st]
             (let [m (get per-stage st "absent")]
               (when-not (contains? stage-weight m)
                 (throw (ex-info (str "unknown maturity " (pr-str m)
                                      " for stage " (pr-str st))
                                 {:hotaru/closed-vocab true :maturity m :stage st})))
               [(if (= m "open-mature") (inc cov) cov)
                (+ ssum (stage-weight m))]))
           [0 0.0]
           substrate-stages)
          substrate-commons-ready (= covered (count substrate-stages))
          cs (assoc cs
                    :per-stage per-stage
                    :epitaxy-open-mature epitaxy-open-mature
                    :conflict-flagged conflict-flagged
                    :stages-covered covered
                    :maturity-score (round4 (/ score-sum (count substrate-stages)))
                    :substrate-commons-ready substrate-commons-ready
                    ;; R4+ gate satisfiable from the commons only if the WHOLE
                    ;; chain incl. epitaxy is open-mature. Reported, NOT decided (G3).
                    :r4-gate-satisfiable (and substrate-commons-ready
                                              epitaxy-open-mature)
                    :phase readiness-phase-assessed)]
      {:cell-state cs})))

(defn transition-to-reported
  "Port of `transition_to_reported(state)`. Materialize the
  commonsReadinessReport record (non-adjudicating, G3). Requires a prior
  assessment, else raises (illegal transition)."
  [state]
  (let [state (or state {})
        cs (make-readiness-state (:cell-state state))]
    (when-not (= (:phase cs) readiness-phase-assessed)
      (throw (ex-info "report requires an assessment first"
                      {:hotaru/illegal-transition true :phase (:phase cs)})))
    (let [cs (assoc cs
                    :payload {"stagesCovered" (:stages-covered cs)
                              "stagesTotal" (count substrate-stages)
                              "substrateCommonsReady" (:substrate-commons-ready cs)
                              "epitaxyOpenMature" (:epitaxy-open-mature cs)
                              "r4GateSatisfiable" (:r4-gate-satisfiable cs)
                              "maturityScore" (:maturity-score cs)
                              "conflictFlagged" (:conflict-flagged cs)
                              ;; G3 — invariant, this report never opens fabrication
                              "fabricationProhibited" true
                              "sourcing" "derived"}
                    :phase readiness-phase-reported)]
      {:cell-state cs})))
