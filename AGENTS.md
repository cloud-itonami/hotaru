# hotaru (蛍) — III-V / InP substrate open-publication commons actor

**DID**: `did:web:etzhayyim.com:actor:hotaru` · **Tier**: B · **Status**: R0 · **ADR**: 2606051200
**Gate parent**: ADR-2605265500 §2 (III-V manufacturing PROHIBITED through R3)

## What this is

The charter-clean answer to *「インジウムリン基盤の生成・製造に関わる actor は設計されているか」* (was:
**no**, and III-V *manufacturing* is constitutionally PROHIBITED through R3 per ADR-2605265500 §2).

hotaru is **not** a fab. It is the **open-publication knowledge commons** for III-V compound-
semiconductor **substrate** generation + manufacturing — InP first, then the direct-bandgap family
(GaAs/GaN/InGaAs/GaSb). It datafies the substrate chain into the kotoba Datom log and reports how far
the *open commons* is from satisfying ADR-2605265500 §2's **R4+ re-evaluation gate** — which is
explicitly conditioned on *"open-source III-V wafer + epitaxy IP becomes available (currently no such
commons exists)"*. hotaru **is the construction of that commons**.

蛍 (firefly) = direct-bandgap light emission. Silicon's **indirect** bandgap is *why* the III-V family
exists (lasers / LEDs / photodetectors / photonic ICs / the optics that run optical fibre + the
submarine cables watatsuna maps). hotaru is the light-emitting-crystal commons — the direct-bandgap
sibling of the iwakura/fuigo **indirect**-bandgap silicon track (ADR-2605242500).

The three boundary lines:
- open-publication substrate process knowledge (synthesis → bulk-growth → wafering → surface-prep) = **in scope**;
- vendor-proprietary MOCVD/epitaxy recipes = **excluded by the `:source-license` invariant** (G1);
- physical fabrication (a grown boule, a manufactured wafer) = **unrepresentable** (`:fabricated false`, G2);
- the ADR-2605265500 R4+ gate = **hotaru reports, Council Lv7+ decides** (G3, non-adjudicating).

ISIC C2611 · ISCO 2149/3119/8131 · UNSPSC 32.

## The headline (honest R0 finding)

`methods/analyze.py` on the `:representative` seed:
- substrate commons **READY** (4/4 stages synthesis/bulk-growth/wafering/surface-prep open-mature);
- **epitaxy GAP** (vendor-IP-dense device stack-up; only `:gap` maturity, no practiceable recipe);
- **R4+ gate satisfiable from the commons alone = False** → III-V **fabrication stays PROHIBITED
  through R3** (unchanged). The binding gap is **epitaxy/device**, not substrate growth.

## Cells (langgraph→WASM; Murakumo-only; `.solve()` raises at R0)

commons_ingest (dan — **coded**; open-IP license screen, `ValueError` on vendor-proprietary) ·
bulk_crystal_design (naphtali — **coded**; LEC/VGF/VB boule design, G2 `fabricated false` + G4 In-sourcing) ·
wafer_fab_design (gad — **coded**; saw/lap/CMP/surface spec, G2 `fabricated false` + spec-sanity) ·
precursor_safety (asher — **coded**; PH₃/In/Ga refusal gate, G3/G4/G9/G11) ·
commons_readiness (issachar — **coded**; G3 non-adjudicating maturity-score report; logic mirrored in `methods/analyze.py`).

## Gates (immutable)

G1 open-IP-only · G2 design-only/not-fabricated · G3 non-adjudicating-on-the-gate · G4 conflict-mineral-
sourcing · G5 no-server-key · G6 Murakumo-only · G7 sourcing-honesty · G8 outward-gated (**Lv7+**, not
Lv6+, because III-V is constitutionally gated through R3) · G9 export-control-honest · G10 civilian-only ·
G11 process-safety. (Full text: `manifest.edn` / ADR.)

## The invariants live in THREE places (mirror nusa)

1. **schema** `00-contracts/schemas/iii-v-substrate-ontology.kotoba.edn` —
   `:iiiv.proc/source-license :db/allowed [...]` (open only) + `:iiiv.crystal/fabricated :db/allowed [false]`.
2. **lexicon** `lex/processKnowledge.edn` `sourceLicense` enum (no `vendor-proprietary`) +
   `lex/crystalGrowthDesign.edn` / `lex/waferSpec.edn` `fabricated` `const false`.
3. **code** `cells/{commons_ingest,bulk_crystal_design,wafer_fab_design,precursor_safety,commons_readiness}/state_machine.py`
   + `methods/analyze.py` — raise/refuse on any non-open license, any `fabricated=true`, any unverified In/Ga.

The agreement of these three places is **machine-checked** — `methods/test_analyze.py` asserts the
schema `:db/allowed`, the lexicon `enum`/`const`, and the code sets are identical (G1 license, G2
`fabricated false`) or in the deliberate clean-set relationship (G4 in-sourcing), so the invariant
cannot silently drift. The seed is also **schema-validated**: `analyze.py` checks every datom
against the ontology's `:db/allowed` sets and refuses to run (exit 2) on any non-conformant value.

## Build / test

```
cd methods && python3 test_analyze.py           # 25/25 (or pytest with PYTEST_DISABLE_PLUGIN_AUTOLOAD=1)
cd cells   && python3 test_state_machines.py    # 28/28
cd methods && python3 analyze.py                # → out/commons-readiness.md + out/iii-v-readiness.kotoba.edn
```

## Do not

- Do not introduce a `:vendor-proprietary` / `:patent-active` / `:trade-secret` source-license, nor
  ingest a proprietary MOCVD/epitaxy recipe — G1 (unrepresentable; voids the commons claim).
- Do not set `:fabricated true` on any crystal or wafer, nor wire live fabrication — G2 / N2
  (III-V fabrication PROHIBITED through R3, ADR-2605265500 §2).
- Do not call any cell `.solve()` — R0 raises `RuntimeError`.
- Do not source In/Ga without `:in-sourcing` ∈ {`:recycled`, `:conflict-free-attested`} — G4 (§G2).
- Do not decide, advocate, or "open" the ADR-2605265500 R4+ gate — G3 (Council Lv7+ unanimity only).
- Do not design military / fire-control / weapons-seeker III-V devices — G10 / N1 (Charter §2(a)).
- Do not enable live crystal growth / wafer fab / live open-corpus ingest / publish without operator +
  **Council Lv7+** — G8.
- Do not confuse with the silicon iwakura/fuigo logic-ASIC track (ADR-2605242500) or himawari solar-Si
  PV (ADR-2606021200) — hotaru is III-V **compound** substrate only — N4.
