(ns hotaru.cells.bulk-crystal-design.state-machine
  "Phase state machine for the hotaru 蛍 bulk_crystal_design cell.
  1:1 port of cells/bulk_crystal_design/state_machine.py (ADR-2606051200). Single-crystal InP boule
  growth DESIGN (LEC/VGF/VB). G2 design-only (fabricated forced false; any true raises), G4 In/Ga
  clean sourcing, bulk-growth method only (epitaxy refused). ValueError → ex-info."
  (:require [clojure.string :as str]))

(def allowed-methods #{"lec" "vgf" "vertical-bridgman"})
(def clean-sourcing #{"recycled" "conflict-free-attested"})
(def known-dopants #{"sulfur" "iron" "tin" "zinc" "undoped"})

(def state-defaults
  {"phase" "init" "crystal_id" "" "material" "inp" "method" "lec" "dopant" "undoped"
   "target_wafer" "" "in_sourcing" "conflict-free-attested" "fabricated" false
   "screened" false "sourcing" "representative" "payload" {}})

(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))

(defn transition-to-screened [state]
  (let [cs (cell-state state)
        cs (assoc cs "crystal_id" (get state "crystal_id" (get cs "crystal_id"))
                  "material" (let [m (norm (get state "material" (get cs "material")))] (if (seq m) m "inp")))
        fab (get state "fabricated" (get cs "fabricated"))]
    (when-not (false? fab)
      (throw (ex-info (str "G2 violation: crystal " (pr-str (get cs "crystal_id")) " cannot be :fabricated " (pr-str fab)
                           "; only false is permitted (III-V fabrication PROHIBITED through R3, ADR-2605265500 §2 "
                           "— a grown boule is unrepresentable).") {:gate "G2"})))
    (let [method (norm (get state "method" (get cs "method")))]
      (when-not (contains? allowed-methods method)
        (throw (ex-info (str "bulk-growth method " (pr-str method) " not in " (pr-str (vec (sort allowed-methods)))
                             "; epitaxy methods (movpe/mbe) are not bulk crystal growth and are out of substrate scope.") {})))
      (let [insrc (norm (get state "in_sourcing" (get cs "in_sourcing")))]
        (when-not (contains? clean-sourcing insrc)
          (throw (ex-info (str "G4 violation: crystal " (pr-str (get cs "crystal_id")) " :in-sourcing " (pr-str insrc)
                               "; clean sourcing " (pr-str (vec (sort clean-sourcing))) " required (inherits hikari/himawari §G2).") {:gate "G4"})))
        (let [dopant (norm (get state "dopant" (get cs "dopant")))]
          (when-not (contains? known-dopants dopant)
            (throw (ex-info (str "unknown dopant " (pr-str dopant) "; expected one of " (pr-str (vec (sort known-dopants)))) {})))
          {"cell_state" (assoc cs "fabricated" false "method" method "in_sourcing" insrc "dopant" dopant
                               "target_wafer" (get state "target_wafer" (get cs "target_wafer"))
                               "screened" true "phase" "screened")})))))

(defn transition-to-designed [state]
  (let [cs (cell-state state)]
    (when-not (get cs "screened")
      (throw (ex-info "growth design requires a passed G2/G4 screen first" {})))
    {"cell_state" (assoc cs "phase" "designed"
                         "payload" {"crystalId" (get cs "crystal_id") "material" (get cs "material")
                                    "method" (get cs "method") "dopant" (get cs "dopant")
                                    "targetWafer" (get cs "target_wafer") "inSourcing" (get cs "in_sourcing")
                                    "fabricated" false "sourcing" (get cs "sourcing")})}))

(defn solve [_input-state]
  (throw (ex-info "hotaru R0 scaffold: activate bulk_crystal_design via Council ADR (post-2606051200 ratification)" {:scaffold true})))
