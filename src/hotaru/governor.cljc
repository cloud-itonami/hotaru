(ns hotaru.governor
  "hotaru 蛍 — the independent charter gate over this actor's OUTWARD effects.

  Why this exists. `hotaru.murakumo` plans `:mst/put-record` effects — atproto
  publication, the actor's only outward reach. Before this namespace existed it
  emitted them whenever seven GENERIC baseline attestations were present, and
  not one of the eleven charter gates this actor declares in `manifest.edn`
  (G1–G11) was consulted on that path. Measured 2026-08-29 on the R0 tree: with
  the seven baselines attested, `cell-plan` returned `:status :ready` and one
  `:mst/put-record` effect — while `:actor/status` is `:r0` and G8 says outward
  reach is Council Lv7+ **and** operator gated, R0 being 'offline design +
  commons-readiness reporting only'. The gates were real and written down; the
  code path that reaches the outside never asked them.

  The governor is deliberately SEPARATE from the cells. Each cell state machine
  already refuses on its own subject matter (commons_ingest on G1, precursor_
  safety on G3/G4/G9/G11). Those are subject gates and they are not on the
  runtime classpath. This is the phase-and-attribution gate, and it sits on the
  one path that leaves the actor.

  THREE OUTCOMES, NOT TWO. `:permit`, `:refuse` (a gate decided against the
  effect) and `:undecidable` (the governor could not read what it needed to
  decide). `:undecidable` is not a permit and is not a refusal: it is the
  governor declining to answer, kept distinct so that 'could not check' can
  never be read out of the output as 'checked and fine'. `permitted?` is the
  ONLY way to obtain a yes, and it tests for `:permit` by name rather than
  testing for the absence of `:refuse`.

  Every constant below mirrors a declaration that lives somewhere else in this
  repo — `manifest.edn`, `.well-known/did.json`, `lex/*.edn`. It is not a second
  source of truth. `hotaru.governor-drift-test` reads those files and asserts
  the agreement, in the same spirit as `methods/test_charter_gates.cljc`, so the
  governor cannot silently drift away from the charter it enforces."
  (:require [clojure.string :as str]))

;; ── what the actor declares about itself (mirrors; drift-tested) ────────────

(def actor-phase
  "manifest.edn :actor/status. G8 ties outward reach to the phase."
  :r0)

(def council-level-required
  "lex/silenHotaruReview.edn councilLevel :const — Lv7+, not Lv6+, because
  III-V is constitutionally gated through R3 (ADR-2605265500 §2)."
  "Lv7+")

(def claimed-dids
  "The DIDs this actor actually claims: manifest.edn :actor/did plus the did:web
  entries of .well-known/did.json alsoKnownAs. Anything else is not hotaru."
  #{"did:web:etzhayyim.com:actor:hotaru"
    "did:web:etzhayyim.github.io:com-etzhayyim-hotaru"})

(def declared-lexicons
  "manifest.edn :actor/lex — the only record types this actor may publish."
  ["processKnowledge" "crystalGrowthDesign" "waferSpec"
   "precursorSafetyAttestation" "commonsReadinessReport" "silenHotaruReview"])

(defn collection
  "Lexicon id -> NSID, matching hotaru.murakumo/collection."
  [nm]
  (str "com.etzhayyim.hotaru." nm))

(def declared-collections (into #{} (map collection) declared-lexicons))

(def open-licenses
  "G1 — lex/processKnowledge.edn sourceLicense :enum. Practiceable-open only."
  #{"academic-oa" "patent-expired" "textbook-public" "standard-public" "own-rnd"})

(def outward-ops
  "Operations that leave the actor. These are what this gate governs."
  #{:mst/put-record})

(def inward-ops
  "Operations that do not leave the actor. Empty today: the boundary plans
  nothing but put-record. Kept explicit so that an op which is in NEITHER set
  is undecidable rather than quietly permitted."
  #{})

;; ── decisions ──────────────────────────────────────────────────────────────

(def permit {:decision :permit})

(defn- refuse [gate reason detail]
  {:decision :refuse :gate gate :reason reason :detail detail})

(defn- undecidable [reason detail]
  {:decision :undecidable :gate :undecidable :reason reason :detail detail})

(defn permitted?
  "The only way to get a yes. Tests for :permit BY NAME — an :undecidable
  decision is not permitted, and neither is anything this namespace has not
  seen before."
  [decision]
  (= :permit (:decision decision)))

;; ── reading effect payloads ────────────────────────────────────────────────

(defn- as-str
  "Records arrive with either keyword or string keys and either keyword or
  string values (`:academic-oa` and \"academic-oa\" are the same license).
  Normalise to a plain string, dropping one leading colon."
  [v]
  (cond
    (nil? v)     nil
    (keyword? v) (name v)
    (string? v)  (str/replace v #"^:+" "")
    :else        (str v)))

(defn- rget
  "Look a lexicon field up under either a keyword or a string key."
  [record field]
  (let [v (get record (keyword field))]
    (if (some? v) v (get record field))))

(defn- explicitly-false?
  "A G2 record must carry `fabricated false`. Accept the boolean and the string
  spelling; treat everything else, including absence, as not-false."
  [v]
  (or (false? v) (= "false" (as-str v))))

(defn- explicitly-true? [v]
  (or (true? v) (= "true" (as-str v))))

;; ── the gate ───────────────────────────────────────────────────────────────

(defn review-effect
  "Decide one planned effect. `ctx` carries the authority the caller claims to
  hold: {:council-level \"Lv7+\" :operator-signature <sig> :server-signature <sig>}.

  Ordering is deliberate: the SPECIFIC charter gates are asked before the
  blanket phase gate, so that a fabricated record at R0 is refused as G2 (the
  precise thing that is wrong with it) rather than being swallowed by G8, which
  at R0 would otherwise refuse everything and mask every other defect."
  [effect ctx]
  (cond
    (not (map? effect))
    (undecidable :effect/not-a-map
                 (str "cannot review a " (pr-str (type effect)) " as an effect"))

    (nil? (:op effect))
    (undecidable :effect/no-op "effect carries no :op; nothing to decide")

    (contains? inward-ops (:op effect))
    permit

    (not (contains? outward-ops (:op effect)))
    (undecidable :effect/unknown-op
                 (str "op " (pr-str (:op effect)) " is in neither outward-ops nor "
                      "inward-ops; this governor does not know whether it leaves the actor"))

    (not (map? (:record effect)))
    (undecidable :record/unreadable
                 "outward effect carries no readable :record; G1/G2 cannot be checked")

    :else
    (let [record (:record effect)
          did    (as-str (:actor effect))
          coll   (as-str (:collection effect))]
      (cond
        (not (contains? claimed-dids did))
        (refuse "did-primary" :unclaimed-actor-did
                (str "effect is attributed to " (pr-str did) ", which this actor does not "
                     "claim; claimed: " (pr-str (vec (sort claimed-dids)))))

        (not (contains? declared-collections coll))
        (refuse "manifest-conformance" :undeclared-collection
                (str "collection " (pr-str coll) " is not declared in manifest.edn "
                     ":actor/lex; declared: " (pr-str (vec (sort declared-collections)))))

        (let [v (rget record "fabricated")]
          (and (some? v) (not (explicitly-false? v))))
        (refuse "G2" :fabricated-record
                (str "record carries fabricated " (pr-str (rget record "fabricated"))
                     "; a grown boule / manufactured wafer is unrepresentable through R3 "
                     "(ADR-2605265500 §2)"))

        (let [v (rget record "fabricationProhibited")]
          (and (some? v) (not (explicitly-true? v))))
        (refuse "G2" :fabrication-prohibition-denied
                (str "record carries fabricationProhibited "
                     (pr-str (rget record "fabricationProhibited"))
                     "; the prohibition is not this actor's to lift (G3: Council decides)"))

        (let [v (as-str (rget record "sourceLicense"))]
          (and (some? v) (not (contains? open-licenses v))))
        (refuse "G1" :closed-source-license
                (str "sourceLicense " (pr-str (as-str (rget record "sourceLicense")))
                     " is outside the open-IP set " (pr-str (vec (sort open-licenses)))
                     "; hotaru is a commons by construction"))

        (or (some? (:server-signature ctx))
            (explicitly-true? (rget record "serverHeldKey")))
        (refuse "G5" :server-held-key
                "authorization carries a server signature; member/operator signature required")

        (and (= :r0 actor-phase)
             (not (and (= council-level-required (as-str (:council-level ctx)))
                       (seq (str (or (:operator-signature ctx) ""))))))
        (refuse "G8" :outward-gated-at-r0
                (str "actor is at " (pr-str actor-phase) "; atproto publication requires Council "
                     council-level-required " AND an operator signature, got council-level "
                     (pr-str (as-str (:council-level ctx))) " operator-signature "
                     (pr-str (boolean (seq (str (or (:operator-signature ctx) "")))))))

        :else permit))))

(defn review-effects
  "Decide a whole planned effect set, keeping each effect beside its decision."
  [effects ctx]
  (mapv (fn [e] (assoc (review-effect e ctx) :effect e)) effects))

(defn permit-all?
  "True only when there was something to review AND every decision was :permit.

  An EMPTY decision set is not a permit. Reviewing nothing and finding nothing
  wrong produce the same shape otherwise, and the first of those must never be
  reported as the second."
  [decisions]
  (boolean (and (seq decisions) (every? permitted? decisions))))

(defn refusals
  "The decisions that were not permits — refusals and undecidables alike."
  [decisions]
  (filterv (complement permitted?) decisions))
