(ns hotaru.methods.analyze
  "hotaru 蛍 — III-V / InP open-publication substrate-commons analyzer.
  1:1 Clojure port of `methods/analyze.py` (ADR-2606051200).

  Reads a kotoba-EDN III-V substrate graph (:iiiv.material/* compounds, :iiiv.proc/*
  open-publication process knowledge, :iiiv.crystal/* growth DESIGNS, :iiiv.wafer/*
  SPECS, :iiiv.precursor/* materials + safety) and emits:
    1. an aggregate-first commons-readiness report (out/commons-readiness.md) — the
       per-stage open-publication coverage of the InP substrate chain, the :epitaxy GAP,
       the precursor-safety + conflict-mineral picture, and an HONEST verdict on the
       ADR-2605265500 §2 R4+ re-evaluation gate.
    2. the derived readiness datoms (out/iii-v-readiness.kotoba.edn), flagged :derived.

  CONSTITUTIONAL framing (enforced HERE as enforcement-point #3 of 3):
    G1 — every :iiiv.proc/source-license MUST be in the practiceable-open set
         {:academic-oa :patent-expired :textbook-public :standard-public :own-rnd}.
         Any other value (e.g. :vendor-proprietary) raises (ex-info) — the model
         cannot hold a proprietary recipe → the graph stays a *commons*.
    G2 — every :iiiv.crystal/fabricated and :iiiv.wafer/fabricated MUST be false.
         A true value raises (ex-info); the model is design/spec ONLY through R3.
    G4 — any crystal consuming conflict-mineral In/Ga MUST declare :in-sourcing ∈
         {:recycled :conflict-free-attested}; :unverified is flagged.
    G3 — non-adjudicating: hotaru reports commons coverage; Council decides the gate.

  House style: Python ':…' keyword strings stay strings; pure fns; file I/O only at
  #?(:clj) edges via clojure.java.io. Portable .cljc."
  (:require [clojure.string :as str]))

;; ── minimal EDN reader (subset: [] {} :kw \"str\" num bool nil) — ported from nusa.
;; Mirrors analyze.py's _TOK / _tokens / _atom / _parse faithfully. Keywords are kept as
;; \":ns/name\" strings (NOT clojure keywords) so the whole pipeline stays string-keyed,
;; byte-for-byte the same as the Python port.

(def ^:private tok-re
  ;; _TOK = re.compile(r'[\s,]+|;[^\n]*|(\[|\]|\{|\}|"(?:\\.|[^"\\])*"|[^\s,\[\]{}]+)')
  #"[\s,]+|;[^\n]*|(\[|\]|\{|\}|\"(?:\\.|[^\"\\])*\"|[^\s,\[\]{}]+)")

(defn tokens
  "Lazy seq of significant tokens (group 1 of each tok-re match that captured)."
  [s]
  (let [m (re-matcher tok-re s)]
    ((fn step []
       (lazy-seq
        (when (.find m)
          (let [t (.group m 1)]
            (if (nil? t)
              (step)
              (cons t (step))))))))))

(defn atom-of
  "Port of _atom: \"…\" → unescaped string; true/false/nil → bool/nil; \":…\" kept as string;
  int → long; else float; else raw string."
  [t]
  (cond
    (str/starts-with? t "\"")
    (-> (subs t 1 (dec (count t)))
        (str/replace "\\\"" "\"")
        (str/replace "\\\\" "\\"))
    (= t "true") true
    (= t "false") false
    (= t "nil") nil
    (str/starts-with? t ":") t
    :else
    (let [as-long (try (Long/parseLong t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
      (if (not= as-long ::nan)
        as-long
        (let [as-dbl (try (Double/parseDouble t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
          (if (not= as-dbl ::nan) as-dbl t))))))

(def ^:private end-marker ::end)

(defn- parse-step
  "Consume one form from the token vector at index i. Returns [value next-i] or
  [end-marker next-i] when a closing ] or } is hit (matching _parse's _END sentinel)."
  [toks i]
  (let [t (nth toks i)
        i (inc i)]
    (cond
      (= t "[")
      (loop [i i, out []]
        (let [[x i] (parse-step toks i)]
          (if (= x end-marker)
            [out i]
            (recur i (conj out x)))))

      (= t "{")
      (loop [i i, out (array-map)]
        (let [[k i] (parse-step toks i)]
          (if (= k end-marker)
            [out i]
            (let [[v i] (parse-step toks i)]
              (recur i (assoc out k v))))))

      (or (= t "]") (= t "}"))
      [end-marker i]

      :else
      [(atom-of t) i])))

(defn read-edn
  "Parse the first top-level form from EDN text (matches read_edn → _parse(_tokens(text)))."
  [text]
  (let [toks (vec (tokens text))]
    (first (parse-step toks 0))))

#?(:clj
   (defn load-edn
     "Read + parse an EDN file. File I/O only at this edge (mirrors load_edn)."
     [path]
     (read-edn (slurp (str path)))))

;; G1: the ONLY practiceable-open licenses (mirrors nusa ALLOWED_THC_CLASSES).
(def ALLOWED-LICENSES
  [":academic-oa" ":patent-expired" ":textbook-public" ":standard-public" ":own-rnd"])
;; G4: conflict-mineral In/Ga sourcing that satisfies the gate.
(def CLEAN-SOURCING [":recycled" ":conflict-free-attested"])

;; The substrate chain hotaru's scope COVERS. :epitaxy is deliberately NOT here.
(def SUBSTRATE-STAGES [":synthesis" ":bulk-growth" ":wafering" ":surface-prep"])
(def EPITAXY-STAGE ":epitaxy")

;; ── schema-conformance (load_allowed_map / validate_against_schema) ──────────
(defn load-allowed-map
  "Read a parsed ontology form and return {attribute -> allowed-value-vector} for every
  attribute that declares :db/allowed."
  [onto]
  (let [attrs (get onto ":attributes" {})]
    (reduce (fn [m [a spec]]
              (if (and (map? spec) (contains? spec ":db/allowed"))
                (assoc m a (get spec ":db/allowed"))
                m))
            (array-map)
            attrs)))

(defn validate-against-schema
  "Validate every datom against the ontology's :db/allowed constraints. Returns a vector
  of violation maps {\"attr\" \"value\" \"allowed\"}; empty == schema-conformant."
  [rows allowed-map]
  (reduce
   (fn [violations r]
     (if-not (map? r)
       violations
       (reduce
        (fn [vs [attr val]]
          (if (and (contains? allowed-map attr)
                   (not (some #(= % val) (get allowed-map attr))))
            (conj vs {"attr" attr "value" val "allowed" (get allowed-map attr)})
            vs))
        violations
        r)))
   []
   rows))

;; ── classify the flat datom vector into entity buckets (insertion-ordered) ───
(defn classify
  "Classify rows into [materials procs crystals wafers precursors], each an insertion-
  ordered map keyed by id (mirrors the Python dicts; first-key-wins iteration order)."
  [rows]
  (reduce
   (fn [[materials procs crystals wafers precursors :as acc] r]
     (if-not (map? r)
       acc
       (cond
         (contains? r ":iiiv.material/id")
         [(assoc materials (get r ":iiiv.material/id") r) procs crystals wafers precursors]
         (contains? r ":iiiv.proc/id")
         [materials (assoc procs (get r ":iiiv.proc/id") r) crystals wafers precursors]
         (contains? r ":iiiv.crystal/id")
         [materials procs (assoc crystals (get r ":iiiv.crystal/id") r) wafers precursors]
         (contains? r ":iiiv.wafer/id")
         [materials procs crystals (assoc wafers (get r ":iiiv.wafer/id") r) precursors]
         (contains? r ":iiiv.precursor/id")
         [materials procs crystals wafers (assoc precursors (get r ":iiiv.precursor/id") r)]
         :else acc)))
   [(array-map) (array-map) (array-map) (array-map) (array-map)]
   rows))

(defn screen-licenses
  "G1 enforcement point #3: refuse any process whose source-license is not practiceable-open."
  [procs]
  (doseq [[pid p] procs]
    (let [lic (get p ":iiiv.proc/source-license")]
      (when-not (some #(= % lic) ALLOWED-LICENSES)
        (throw (ex-info
                (str "G1 violation: process " (pr-str pid) " has :source-license " (pr-str lic)
                     "; only " ALLOWED-LICENSES " are permitted (hotaru is an OPEN-PUBLICATION "
                     "commons; vendor-proprietary / patent-active / trade-secret recipes are "
                     "excluded by construction, per ADR-2605265500 §2).")
                {:g1 true :pid pid :license lic}))))))

(defn screen-fabrication
  "G2 enforcement point #3: refuse any crystal/wafer marked fabricated."
  [crystals wafers]
  (doseq [[cid c] crystals]
    (when-not (false? (get c ":iiiv.crystal/fabricated"))
      (throw (ex-info
              (str "G2 violation: crystal " (pr-str cid) " has :fabricated "
                   (pr-str (get c ":iiiv.crystal/fabricated")) "; only false is permitted "
                   "(III-V fabrication PROHIBITED through R3, ADR-2605265500 §2).")
              {:g2 true :cid cid}))))
  (doseq [[wid w] wafers]
    (when-not (false? (get w ":iiiv.wafer/fabricated"))
      (throw (ex-info
              (str "G2 violation: wafer " (pr-str wid) " has :fabricated "
                   (pr-str (get w ":iiiv.wafer/fabricated")) "; only false is permitted.")
              {:g2 true :wid wid})))))

(defn stage-coverage
  "Per substrate stage: is it covered by ≥1 :open-mature process? Returns
  [per-stage covered total epitaxy]."
  [procs]
  (let [pvals (vals procs)
        by-stage (group-by #(get % ":iiiv.proc/stage") pvals)
        per-stage (reduce
                   (fn [m st]
                     (let [ps (get by-stage st [])
                           mature (filter #(= (get % ":iiiv.proc/maturity") ":open-mature") ps)
                           emerging (filter #(= (get % ":iiiv.proc/maturity") ":open-emerging") ps)]
                       (assoc m st {"n" (count ps) "mature" (count mature)
                                    "emerging" (count emerging)
                                    "covered" (>= (count mature) 1)})))
                   (array-map)
                   SUBSTRATE-STAGES)
        covered (count (filter #(get-in per-stage [% "covered"]) SUBSTRATE-STAGES))
        epitaxy (get by-stage EPITAXY-STAGE [])
        epitaxy-mature (boolean (some #(= (get % ":iiiv.proc/maturity") ":open-mature") epitaxy))]
    [per-stage covered (count SUBSTRATE-STAGES)
     {"n" (count epitaxy) "open_mature" epitaxy-mature}]))

;; maturity weights for the commons-maturity score
(def ^:private STAGE-WEIGHT {":open-mature" 1.0 ":open-emerging" 0.5 ":gap" 0.0})

(defn- round4
  "Python round(x, 4) — HALF_EVEN on the exact double value."
  [x]
  #?(:clj (.doubleValue (.setScale (java.math.BigDecimal. (double x)) 4 java.math.RoundingMode/HALF_EVEN))
     :cljs (let [f 10000.0 r (/ (Math/round (* (double x) f)) f)] r)))

(defn maturity-metrics
  "Returns [maturity-score per-material]."
  [procs materials]
  (let [pvals (vals procs)
        ;; stage -> #{materials with a process there}
        by-stage-mat (reduce (fn [m p]
                               (update m (get p ":iiiv.proc/stage") (fnil conj #{})
                                       (get p ":iiiv.proc/material")))
                             {} pvals)
        ;; substrate stage -> best maturity weight (any material)
        ;; material -> {stage -> best maturity weight}
        [best-by-stage best-by-mat-stage]
        (reduce (fn [[bbs bbms] p]
                  (let [st (get p ":iiiv.proc/stage")
                        mat (get p ":iiiv.proc/material")]
                    (if (some #(= % st) SUBSTRATE-STAGES)
                      (let [w (get STAGE-WEIGHT (get p ":iiiv.proc/maturity") 0.0)]
                        [(update bbs st #(max (or % 0.0) w))
                         (update-in bbms [mat st] #(max (or % 0.0) w))])
                      [bbs bbms])))
                [{} {}] pvals)
        score (/ (reduce + 0.0 (map #(get best-by-stage % 0.0) SUBSTRATE-STAGES))
                 (count SUBSTRATE-STAGES))
        per-material (reduce
                      (fn [m mid]
                        (let [covered (count (filter #(contains? (get by-stage-mat % #{}) mid)
                                                     SUBSTRATE-STAGES))
                              mat-score (/ (reduce + 0.0
                                                   (map #(get-in best-by-mat-stage [mid %] 0.0)
                                                        SUBSTRATE-STAGES))
                                           (count SUBSTRATE-STAGES))]
                          (assoc m mid {"covered" covered "total" (count SUBSTRATE-STAGES)
                                        "fraction" (round4 (/ (double covered)
                                                              (count SUBSTRATE-STAGES)))
                                        "score" (round4 mat-score)})))
                      (array-map)
                      (keys materials))]
    [(round4 score) per-material]))

(defn gap-register
  "Enumerate exactly what the commons is MISSING. Returns a sorted vector of
  {\"material\" \"stage\" \"status\"} + the structural epitaxy entry."
  [materials procs]
  (let [bulk (set (for [[mid m] materials
                        :when (= (get m ":iiiv.material/form") ":bulk-substrate")] mid))
        rank {"absent" 0 ":gap" 1 ":open-emerging" 2 ":open-mature" 3}
        label {"absent" "absent" ":gap" "gap" ":open-emerging" "emerging" ":open-mature" "open-mature"}
        ;; best status per (material, substrate stage), defaulting to "absent"
        best (reduce
              (fn [b p]
                (let [mat (get p ":iiiv.proc/material")
                      st (get p ":iiiv.proc/stage")
                      mt (get p ":iiiv.proc/maturity")]
                  (if (and (contains? bulk mat)
                           (some #(= % st) SUBSTRATE-STAGES)
                           (> (get rank mt 0) (get rank (get b [mat st] "absent"))))
                    (assoc b [mat st] mt)
                    b)))
              {} (vals procs))
        gaps (vec
              (for [mid (sort bulk)
                    st SUBSTRATE-STAGES
                    :let [status (get best [mid st] "absent")]
                    :when (not= status ":open-mature")]
                {"material" mid "stage" (str/replace-first st ":" "")
                 "status" (get label status status)}))]
    (conj gaps {"material" "*" "stage" "epitaxy"
                "status" "gap (vendor-IP; out of substrate scope)"})))

(defn substrate-material-metrics
  "Honest coverage = full-chain bulk-substrate materials / all bulk-substrate materials.
  :epitaxial-only materials are EXCLUDED from the denominator."
  [materials per-material]
  (let [bulk (vec (for [[mid m] materials
                        :when (= (get m ":iiiv.material/form") ":bulk-substrate")] mid))
        epi-only (vec (sort (for [[mid m] materials
                                  :when (= (get m ":iiiv.material/form") ":epitaxial-only")] mid)))
        full-chain (vec (for [mid bulk
                              :when (= (get-in per-material [mid "fraction"]) 1.0)] mid))]
    {"bulk_substrate" (count bulk)
     "full_chain" (count full-chain)
     "full_chain_materials" (vec (sort full-chain))
     "coverage" (if (seq bulk) (round4 (/ (double (count full-chain)) (count bulk))) 0.0)
     "epitaxial_only" epi-only}))

(defn- counter
  "Counter(seq) → ordered map value->count, first-touch insertion order (mirrors Counter)."
  [coll]
  (reduce (fn [m v]
            (if (contains? m v)
              (update m v inc)
              (assoc m v 1)))
          (array-map) coll))

(defn safety-metrics
  "Precursor-safety + export-control coverage."
  [precursors]
  (let [pvals (vals precursors)
        acute (count (filter #(contains? #{":acute-toxic" ":acute-toxic-pyrophoric"}
                                         (get % ":iiiv.precursor/hazard-class" "")) pvals))
        cm (vec (for [p pvals :when (true? (get p ":iiiv.precursor/conflict-mineral"))]
                  (get p ":iiiv.precursor/formula")))
        ec (counter (map #(get % ":iiiv.precursor/export-control" ":none") pvals))]
    {"acute_toxic" acute
     "conflict_mineral" (count cm)
     "conflict_mineral_formulas" (vec (sort cm))
     "export_control" ec
     "itar_present" (> (get ec ":itar" 0) 0)
     "ear_present" (> (get ec ":ear" 0) 0)}))

(defn conflict-mineral-screen
  "G4: which precursor elements are conflict-mineral; which crystals consume one without
  a clean :in-sourcing. Returns [cm-elements flagged]."
  [crystals precursors]
  (let [cm-elements (set (for [p (vals precursors)
                               :when (true? (get p ":iiiv.precursor/conflict-mineral"))]
                           (get p ":iiiv.precursor/formula")))
        flagged (reduce
                 (fn [m [cid c]]
                   (if-not (some #(= % (get c ":iiiv.crystal/in-sourcing")) CLEAN-SOURCING)
                     (assoc m cid (get c ":iiiv.crystal/in-sourcing"))
                     m))
                 (array-map) crystals)]
    [cm-elements flagged]))

(defn analyze
  [materials procs crystals wafers precursors]
  (screen-licenses procs)
  (screen-fabrication crystals wafers)
  (let [[per-stage covered total epitaxy] (stage-coverage procs)
        [maturity-score per-material] (maturity-metrics procs materials)
        substrate-materials (substrate-material-metrics materials per-material)
        gaps (gap-register materials procs)
        safety (safety-metrics precursors)
        [cm-elements cm-flagged] (conflict-mineral-screen crystals precursors)
        license-breakdown (counter (map #(get % ":iiiv.proc/source-license") (vals procs)))
        direct-materials (vec (filter #(= (get % ":iiiv.material/bandgap-type") ":direct")
                                      (vals materials)))
        substrate-commons-ready (= covered total)
        r4-gate-satisfiable (and substrate-commons-ready (get epitaxy "open_mature"))]
    {"per_stage" per-stage "covered" covered "total" total "epitaxy" epitaxy
     "maturity_score" maturity-score "per_material" per-material "safety" safety
     "substrate_materials" substrate-materials "gaps" gaps
     "cm_elements" cm-elements "cm_flagged" cm-flagged
     "license_breakdown" license-breakdown "direct_materials" direct-materials
     "substrate_commons_ready" substrate-commons-ready
     "r4_gate_satisfiable" (boolean r4-gate-satisfiable)}))

;; ── report rendering (matches render_report's f-strings) ─────────────────────

(defn- py-bool [b] (if b "True" "False"))

(defn- fmt2
  "Python f'{v:.2f}' — fixed 2 fraction digits, HALF_EVEN on the exact double."
  [v]
  #?(:clj (.toPlainString (.setScale (java.math.BigDecimal. (double v)) 2 java.math.RoundingMode/HALF_EVEN))
     :cljs (.toFixed (double v) 2)))

(defn- lstrip-colon [s] (if (and (string? s) (str/starts-with? s ":")) (subs s 1) s))

(defn- py-str-list
  "Python repr of a list of strings: ['a', 'b'] (single quotes, comma-space)."
  [xs]
  (str "[" (str/join ", " (map #(str "'" % "'") xs)) "]"))

(defn render-report
  [materials procs crystals wafers precursors a]
  (let [L (transient [])
        P #(conj! L %)
        stage-label {":synthesis" "synthesis (合成)" ":bulk-growth" "bulk-growth (単結晶育成)"
                     ":wafering" "wafering (ウェハ加工)" ":surface-prep" "surface-prep (エピ面)"}]
    (P "# hotaru 蛍 — III-V / InP substrate open-commons readiness report")
    (P "")
    (P (str "> ADR-2606051200 · **aggregate-first** · open-publication commons framing. "
            "OPEN-PUBLICATION process knowledge ONLY — vendor-proprietary recipes are "
            "unrepresentable (`:source-license` invariant, G1); crystals + wafers are "
            "design/spec only (`:fabricated false`, G2); III-V **fabrication remains "
            "PROHIBITED through R3 per ADR-2605265500 §2** — this report does NOT change "
            "that (G3, non-adjudicating). All sourcing `:representative` (G5/G7)."))
    (P "")
    (P (str "- materials: **" (count materials) "** (" (count (get a "direct_materials"))
            " direct-bandgap)  ·  open-publication processes: **" (count procs)
            "**  ·  crystal designs: **" (count crystals) "**  ·  wafer specs: **"
            (count wafers) "**  ·  precursors: **" (count precursors) "**"))
    (P "")

    (P "## Substrate-chain commons coverage (生成 → 製造)")
    (P "")
    (P (str "Each stage is *covered* when ≥1 `:open-mature` process exists in the commons. "
            "`:epitaxy` is deliberately out of hotaru's substrate scope — it is the "
            "vendor-IP-dense device stage ADR-2605265500 §2 keeps prohibited, tracked here "
            "only as a gap."))
    (P "")
    (P "| stage | processes | open-mature | open-emerging | covered |")
    (P "|---|---:|---:|---:|:---:|")
    (doseq [st SUBSTRATE-STAGES]
      (let [s (get-in a ["per_stage" st])]
        (P (str "| " (get stage-label st) " | " (get s "n") " | " (get s "mature") " | "
                (get s "emerging") " | " (if (get s "covered") "✅" "❌") " |"))))
    (let [ep (get a "epitaxy")]
      (P (str "| epitaxy (エピtaxy — OUT of scope) | " (get ep "n") " | "
              (if (get ep "open_mature") "≥1" "0 (gap)") " | — | "
              (if (get ep "open_mature") "✅" "⛔ gap") " |")))
    (P "")
    (P (str "- **substrate commons readiness**: " (get a "covered") "/" (get a "total")
            " stages open-mature → **" (if (get a "substrate_commons_ready") "READY" "INCOMPLETE") "**"))
    (P (str "- **commons-maturity score** (open-mature=1.0 / open-emerging=0.5 / gap=0.0, over "
            "4 substrate stages): **" (fmt2 (get a "maturity_score")) "**"))
    (P "")
    (P "### Per-material substrate-chain completeness")
    (P "")
    (let [sm (get a "substrate_materials")]
      (P (str "Bulk-substrate materials with a FULL open chain: **" (get sm "full_chain") "/"
              (get sm "bulk_substrate") "** (" (fmt2 (get sm "coverage")) ") → "
              (py-str-list (get sm "full_chain_materials")) ". Epitaxial-only materials "
              "(grown ON a substrate, NOT as boules — correctly excluded, not a gap): **"
              (if (seq (get sm "epitaxial_only")) (py-str-list (get sm "epitaxial_only")) "—")
              "**.")))
    (P "")
    (P (str "Fraction of the 4 substrate stages that have ≥1 open process for each material "
            "(maturity-weighted score in the last column)."))
    (P "")
    (P "| material | covered stages | completeness | maturity-weighted |")
    (P "|---|---:|---:|---:|")
    (doseq [mid (sort-by (fn [m] (- (get-in a ["per_material" m "score"])))
                         (keys (get a "per_material")))]
      (let [pm (get-in a ["per_material" mid])]
        (P (str "| `" mid "` | " (get pm "covered") "/" (get pm "total") " | "
                (fmt2 (get pm "fraction")) " | " (fmt2 (get pm "score")) " |"))))
    (P "")

    (P "## Gap register — what the open commons still lacks")
    (P "")
    (P (str "Every (material, stage) below is tracked but NOT yet `:open-mature`. This is the "
            "honest list of work between today's commons and the ADR-2605265500 §2 R4+ gate."))
    (P "")
    (P "| material | stage | status |")
    (P "|---|---|---|")
    (doseq [g (get a "gaps")]
      (P (str "| `" (get g "material") "` | " (get g "stage") " | " (get g "status") " |")))
    (P "")

    (P "## ADR-2605265500 §2 R4+ re-evaluation gate")
    (P "")
    (P (str "The gate opens III-V *fabrication* re-evaluation only when an open-source III-V "
            "**wafer + epitaxy** IP commons exists. hotaru reports the two legs; Council decides."))
    (P "")
    (P (str "- substrate (wafer) commons leg: **"
            (if (get a "substrate_commons_ready") "READY" "INCOMPLETE") "**"))
    (P (str "- epitaxy commons leg: **"
            (if (get-in a ["epitaxy" "open_mature"]) "READY" "GAP — vendor-proprietary") "**"))
    (P (str "- **R4+ gate satisfiable from the commons alone**: **" (py-bool (get a "r4_gate_satisfiable"))
            "** → fabrication stays PROHIBITED through R3 (unchanged). The binding gap is "
            "**epitaxy/device stack-up**, not substrate growth."))
    (P "")

    (P "## Process provenance (G1 invariant)")
    (P "")
    (P (str "Every process is practiceable-open. `:vendor-proprietary` is not a representable "
            "license — the data model cannot hold a proprietary recipe."))
    (P "")
    (P "| source-license | processes |")
    (P "|---|---:|")
    (doseq [lic (sort (keys (get a "license_breakdown")))]
      (P (str "| `" lic "` | " (get-in a ["license_breakdown" lic]) " |")))
    (P "")

    (P "## Conflict-mineral sourcing (G4 — inherits hikari/himawari §G2)")
    (P "")
    (let [cm (str/join ", " (sort (get a "cm_elements")))]
      (P (str "- conflict-mineral elements in precursor set: **" (if (seq cm) cm "—") "**")))
    (P (str "- crystal designs consuming a conflict-mineral element WITHOUT clean "
            "`:in-sourcing`: **" (count (get a "cm_flagged")) "**"
            (if (empty? (get a "cm_flagged")) ""
                (str " → " (py-str-list (sort (keys (get a "cm_flagged"))))))))
    (let [sf (get a "safety")
          ec (str/join "  ·  " (map #(str (lstrip-colon %) ": " (get-in sf ["export_control" %]))
                                    (sort (keys (get sf "export_control")))))]
      (P (str "- acute-toxic precursors: **" (get sf "acute_toxic") "**  ·  conflict-mineral precursors: "
              "**" (get sf "conflict_mineral") "** ("
              (let [f (str/join ", " (get sf "conflict_mineral_formulas"))] (if (seq f) f "—")) ")"))
      (P (str "- export-control posture (G9): " ec "  ·  ITAR present: **" (py-bool (get sf "itar_present"))
              "**  ·  EAR present: **" (py-bool (get sf "ear_present")) "**")))
    (P "")
    (P "| precursor | formula | hazard | conflict-mineral | export-control |")
    (P "|---|---|---|:---:|---|")
    (doseq [pid (sort (keys precursors))]
      (let [p (get precursors pid)
            cmf (if (get p ":iiiv.precursor/conflict-mineral") "⚠️ yes" "no")]
        (P (str "| " (get p ":iiiv.precursor/name" pid) " | `" (get p ":iiiv.precursor/formula") "` | `"
                (get p ":iiiv.precursor/hazard-class") "` | " cmf " | `"
                (get p ":iiiv.precursor/export-control") "` |"))))
    (P "")
    (P (str "> PH₃ (phosphine) is acute-toxic + pyrophoric; In/Ga are conflict-minerals "
            "(In/Ga explicitly barred from hikari/himawari panel sourcing) and Ga carries "
            "EAR export controls. Any live process is Council Lv7+ + operator gated (G8/G6)."))
    (P "")
    (P (str "> hotaru builds the commons; it does not grow a crystal. The light-emitting "
            "III-V substrate (蛍, direct-bandgap) is the sibling of the iwakura/fuigo "
            "indirect-bandgap silicon track (ADR-2605242500). Fabrication is the Council's "
            "decision (G3), gated by ADR-2605265500 §2 — not this report."))
    (P "")
    (str/join "\n" (persistent! L))))

(defn- py-float
  "Python repr of a float that is integral-valued (e.g. 1.0 → \"1.0\"). The maturity-score is
  round4'd; render it the way Python str() would in the f-string."
  [x]
  (let [d (double x)]
    (if (== d (Math/rint d))
      (str (long d) ".0")
      (let [s (str d)]
        s))))

(defn render-datoms
  [materials procs crystals wafers a]
  (let [L (transient [])
        P #(conj! L %)]
    (P ";; hotaru 蛍 — DERIVED readiness datoms (ADR-2606051200)")
    (P ";; :derived — analyzer output, NOT re-ingested as authoritative fact (G5).")
    (P "[")
    (doseq [st SUBSTRATE-STAGES]
      (let [s (get-in a ["per_stage" st])]
        (P (str " {:hotaru.derived/stage " st " :hotaru.derived/covered "
                (str/lower-case (py-bool (get s "covered"))) " "
                ":hotaru.derived/open-mature " (get s "mature") " :hotaru.derived/sourcing :derived}"))))
    (let [sf (get a "safety")]
      (P (str " {:hotaru.derived/substrate-commons-ready "
              (str/lower-case (py-bool (get a "substrate_commons_ready"))) " "
              ":hotaru.derived/r4-gate-satisfiable "
              (str/lower-case (py-bool (get a "r4_gate_satisfiable"))) " "
              ":hotaru.derived/maturity-score " (py-float (get a "maturity_score")) " "
              ":hotaru.derived/acute-toxic-precursors " (get sf "acute_toxic") " "
              ":hotaru.derived/conflict-mineral-precursors " (get sf "conflict_mineral") " "
              ":hotaru.derived/itar-present " (str/lower-case (py-bool (get sf "itar_present"))) " "
              ":hotaru.derived/open-gaps " (count (get a "gaps")) " "
              ":hotaru.derived/conflict-flagged " (count (get a "cm_flagged"))
              " :hotaru.derived/sourcing :derived}")))
    (P "]")
    (str/join "\n" (persistent! L))))

#?(:clj
   (defn -main
     "CLI entry: analyze a seed EDN graph → out/commons-readiness.md + out/iii-v-readiness.kotoba.edn."
     [& argv]
     (let [argv (vec argv)
           file-here (when (and *file* (not= *file* "NO_SOURCE_PATH"))
                       (some-> *file* clojure.java.io/file .getParentFile))
           seed (if (and (seq argv) (not (str/starts-with? (first argv) "--")))
                  (clojure.java.io/file (first argv))
                  (clojure.java.io/file (.getParentFile file-here) "data" "seed-iii-v-substrate.kotoba.edn"))
           ;; methods dir = seed's parent's parent / methods (when seed given) else file-here
           here (or file-here (clojure.java.io/file (.getParentFile (.getParentFile seed)) "methods"))
           actor-dir (.getParentFile here)
           outdir (if (some #{"--out"} argv)
                    (clojure.java.io/file (nth argv (inc (.indexOf argv "--out"))))
                    (clojure.java.io/file here "out"))
           onto-path (clojure.java.io/file (.getParentFile (.getParentFile actor-dir))
                                           "00-contracts" "schemas" "iii-v-substrate-ontology.kotoba.edn")
           rows (load-edn seed)
           violations (when (.exists onto-path)
                        (validate-against-schema rows (load-allowed-map (load-edn onto-path))))]
       (.mkdirs outdir)
       (if (and violations (seq violations))
         (do (binding [*out* *err*]
               (println (str "hotaru: SCHEMA VIOLATIONS (" (count violations) "): " (take 3 violations))))
             2)
         (let [[materials procs crystals wafers precursors] (classify rows)
               a (analyze materials procs crystals wafers precursors)
               report (render-report materials procs crystals wafers precursors a)
               datoms (render-datoms materials procs crystals wafers a)]
           (spit (clojure.java.io/file outdir "commons-readiness.md") report)
           (spit (clojure.java.io/file outdir "iii-v-readiness.kotoba.edn") datoms)
           (when violations
             (println (str "hotaru: schema-conformant (" (count rows)
                           " datoms validated against ontology :db/allowed)")))
           (println (str "hotaru: " (count procs) " open processes, substrate " (get a "covered")
                         "/" (get a "total") " stages open-mature (maturity " (fmt2 (get a "maturity_score"))
                         ") → commons " (if (get a "substrate_commons_ready") "READY" "INCOMPLETE")
                         "; epitaxy " (if (get-in a ["epitaxy" "open_mature"]) "open" "GAP")
                         "; R4+ gate satisfiable=" (py-bool (get a "r4_gate_satisfiable")) " → " outdir))
           0)))))
