# hotaru 蛍 — III-V / InP substrate open-publication commons

> Tier-B actor · `did:web:etzhayyim.com:actor:hotaru` · ADR-2606051200 (R0)
> Gate parent: **ADR-2605265500 §2** — III-V *manufacturing* PROHIBITED through R3.

**インジウムリン (InP) 基板の生成・製造に関わる actor。** ただし憲章上、III-V の**製造**は
ADR-2605265500 §2 で R3 まで PROHIBITED。したがって hotaru は fab ではなく、その ADR の **R4+
再評価ゲートが前提とする「open-source III-V ウェハ IP commons」そのものを築く** design / datafication
/ simulation actor です（nusa が「解禁」を by-construction で扱ったのと同じ型）。

蛍 = 直接遷移半導体の発光。シリコンの**間接**遷移こそが III-V 族の存在理由（レーザ / LED /
フォトディテクタ / フォトニック IC / 光ファイバ + 海底ケーブルを走らせる光学系）。hotaru は
発光する結晶の commons — iwakura/fuigo の**間接**遷移シリコン・トラック（ADR-2605242500）の兄弟。

## Substrate chain (datafied)

```
synthesis (合成)  →  bulk-growth (単結晶育成)  →  wafering (ウェハ加工)  →  surface-prep (エピ面)
 In + P → poly      LEC / VGF / VB boule         saw / lap / CMP            epi-ready surface
   [open-mature]      [open-mature]                [open-mature]              [open-mature]
                                                                                   │
                                                                                   ▼
                                                                   epitaxy (エピtaxy) — ⛔ GAP
                                                                   MOVPE / MBE device stack-up
                                                                   vendor-proprietary, OUT of scope
```

## The honest R0 finding

`python3 methods/analyze.py` on the `:representative` seed:

| leg | status |
|---|---|
| substrate (wafer) commons | **READY** — 4/4 stages open-mature |
| epitaxy commons | **GAP** — vendor-proprietary device stack-up (`:gap`) |
| ADR-2605265500 §2 R4+ gate satisfiable from commons alone | **False** → fabrication stays PROHIBITED through R3 |

The binding gap is **epitaxy/device**, not substrate growth. hotaru reports this; **Council Lv7+
decides the gate** (G3, non-adjudicating).

## Charter-clean by construction

- **G1 open-IP-only** — every process carries a practiceable-open `:source-license`; a
  `:vendor-proprietary` MOCVD recipe is **unrepresentable** (schema + lexicon enum + code).
- **G2 design-only** — crystals + wafers are `:fabricated false`; a grown boule / manufactured wafer
  is **unrepresentable** until the R4+ gate (Council Lv7+).
- **G4 conflict-mineral** — In/Ga require clean `:in-sourcing` (inherits hikari/himawari §G2; In/Ga were
  explicitly barred from PV panel sourcing).
- **G10 civilian-only** — no fire-control / weapons-seeker III-V designs (Charter §2(a) force-separation).

## Layout

```
hotaru/
├── manifest.edn / manifest.jsonld   # actor manifest (11 gates, 6 non-goals)
├── AGENTS.md                        # agent rules
├── data/seed-iii-v-substrate.kotoba.edn   # :representative seed (InP-first)
├── lex/                             # 6 lexicons com.etzhayyim.hotaru.*
├── cells/                           # 5 Pregel cells (5 coded state machines) + 28 tests
└── methods/analyze.py              # commons-readiness analyzer + 25 tests
```

Vocabulary: `00-contracts/schemas/iii-v-substrate-ontology.kotoba.edn`.

## Tests

53 green: `methods/test_analyze.py` (25) + `cells/test_state_machines.py` (28).

```
cd methods && python3 test_analyze.py
cd cells   && python3 test_state_machines.py
```
