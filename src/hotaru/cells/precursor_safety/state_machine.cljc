(ns hotaru.cells.precursor-safety.state-machine
  "Phase state machine for the hotaru 蛍 precursor_safety cell — the G3/G4 safety membrane.
  1:1 port of cells/precursor_safety/state_machine.py (ADR-2606051200). A design clears review ONLY
  after: G4 every conflict-mineral element carries clean :in-sourcing; G3 every acute-toxic precursor
  is acknowledged; export-control posture recorded. A REFUSAL gate — it refuses, never clamps."
  (:require [clojure.string :as str]))

(def clean-sourcing #{"recycled" "conflict-free-attested"})
(def acute-hazards #{"acute-toxic-pyrophoric" "acute-toxic"})
(def export-postures #{"none" "ear" "itar"})

(def state-defaults
  {"phase" "init" "design_id" "" "in_sourcing" "conflict-free-attested" "precursors" [] "refusal" "" "payload" {}})

(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))

(defn review [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "design_id" (get state "design_id" (get cs "design_id"))
                  "in_sourcing" (norm (get state "in_sourcing" (get cs "in_sourcing")))
                  "precursors" (vec (get state "precursors" (get cs "precursors"))))
        precursors (get cs "precursors")
        refuse (fn [msg] {"cell_state" (assoc cs "refusal" msg "phase" "refused")})
        uses-conflict (some #(get % "conflict_mineral") precursors)
        ;; first acute-toxic-unacknowledged precursor, then first bad export posture
        bad-acute (first (filter #(and (contains? acute-hazards (norm (get % "hazard_class")))
                                       (not (get % "acknowledged"))) precursors))
        bad-export (first (filter #(not (contains? export-postures (norm (get % "export_control" "none")))) precursors))]
    (cond
      (and uses-conflict (not (contains? clean-sourcing (get cs "in_sourcing"))))
      (refuse (str "G4: design " (pr-str (get cs "design_id")) " consumes a conflict-mineral element (In/Ga) "
                   "with :in-sourcing " (pr-str (get cs "in_sourcing")) "; clean sourcing "
                   (pr-str (vec (sort clean-sourcing))) " required (inherits hikari/himawari §G2)."))

      bad-acute
      (refuse (str "G3: acute-toxic precursor " (pr-str (get bad-acute "name")) " ("
                   (pr-str (get bad-acute "hazard_class")) ") is not acknowledged; review refused."))

      bad-export
      (refuse (str "export-control posture " (pr-str (norm (get bad-export "export_control" "none")))
                   " not in " (pr-str (vec (sort export-postures)))))

      :else
      {"cell_state" (assoc cs "refusal" "" "phase" "cleared"
                           "payload" {"designId" (get cs "design_id") "inSourcing" (get cs "in_sourcing")
                                      "precursorCount" (count precursors)
                                      "exportControls" (vec (sort (set (map #(norm (get % "export_control" "none")) precursors))))
                                      "cleared" true})})))

(defn solve [_input-state]
  (throw (ex-info "hotaru R0 scaffold: activate precursor_safety via Council ADR (post-2606051200 ratification)" {:scaffold true})))
