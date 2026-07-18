(ns hotaru.cells.wafer-fab-design.state-machine
  "Phase state machine for the hotaru 蛍 wafer_fab_design cell.
  1:1 port of cells/wafer_fab_design/state_machine.py (ADR-2606051200). Boule → wire-saw → lap → CMP
  → epi-ready surface SPEC design. G2 design-only (fabricated forced false), spec-sanity (known
  diameter/orientation, EPD positive int). ValueError → ex-info."
  (:require [clojure.string :as str]))

(def known-diameters-um #{50800 76200 100000})   ; 2-inch / 3-inch / 4-inch
(def known-orientations #{"(100)" "(111)" "(110)"})

(def state-defaults
  {"phase" "init" "wafer_id" "" "material" "inp" "diameter_um" 50800 "orientation" "(100)"
   "epd_cm2" 5000 "doping" "sulfur-n" "fabricated" false "screened" false "sourcing" "representative" "payload" {}})

(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))

(defn transition-to-screened [state]
  (let [cs (cell-state state)
        cs (assoc cs "wafer_id" (get state "wafer_id" (get cs "wafer_id"))
                  "material" (let [m (norm (get state "material" (get cs "material")))] (if (seq m) m "inp")))
        fab (get state "fabricated" (get cs "fabricated"))]
    (when-not (false? fab)
      (throw (ex-info (str "G2 violation: wafer " (pr-str (get cs "wafer_id")) " cannot be :fabricated " (pr-str fab)
                           "; only false is permitted (a manufactured wafer is unrepresentable through R3).") {:gate "G2"})))
    (let [dia (get state "diameter_um" (get cs "diameter_um"))]
      (when-not (contains? known-diameters-um dia)
        (throw (ex-info (str "diameter " (pr-str dia) " um not in known substrate sizes " (pr-str (vec (sort known-diameters-um)))) {})))
      (let [ori (get state "orientation" (get cs "orientation"))]
        (when-not (contains? known-orientations ori)
          (throw (ex-info (str "orientation " (pr-str ori) " not in " (pr-str (vec known-orientations))) {})))
        (let [epd (get state "epd_cm2" (get cs "epd_cm2"))]
          (when-not (and (integer? epd) (> epd 0))
            (throw (ex-info (str "epd-cm2 must be a positive integer; got " (pr-str epd)) {})))
          {"cell_state" (assoc cs "fabricated" false "diameter_um" dia "orientation" ori "epd_cm2" epd
                               "doping" (norm (get state "doping" (get cs "doping")))
                               "screened" true "phase" "screened")})))))

(defn transition-to-specified [state]
  (let [cs (cell-state state)]
    (when-not (get cs "screened")
      (throw (ex-info "wafer spec requires a passed G2/spec screen first" {})))
    {"cell_state" (assoc cs "phase" "specified"
                         "payload" {"waferId" (get cs "wafer_id") "material" (get cs "material")
                                    "diameterUm" (get cs "diameter_um") "orientation" (get cs "orientation")
                                    "epdCm2" (get cs "epd_cm2") "doping" (get cs "doping")
                                    "fabricated" false "sourcing" (get cs "sourcing")})}))

(defn solve [_input-state]
  (throw (ex-info "hotaru R0 scaffold: activate wafer_fab_design via Council ADR (post-2606051200 ratification)" {:scaffold true})))
