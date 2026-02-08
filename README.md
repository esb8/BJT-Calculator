# BJT Bias & Small-Signal Calculator

## Overview

This project implements a **rule-based DC bias, small-signal, and bandwidth calculator** for a **single-stage BJT common-emitter amplifier**.

The solver is **constraint-driven**. Users specify known parameters (currents, voltages, resistances, options), and the engine derives all other quantities using a prioritized rule system with conflict detection.

The design emphasizes:
- Explicit physics relationships
- Deterministic conflict resolution
- Extensibility to additional rules and checks
- Transparency of assumptions and limits

This is **not a circuit simulator**. It is a deterministic analytical calculator.

---

## Scope

### Supported
- DC bias analysis (Ic, Ib, Ie, Vb, Ve, Vc, Vce)
- Voltage-divider base bias design
- Region-of-operation classification
- Small-signal parameters (gm, re, rπ, gain)
- Optional emitter bypass capacitor
- Optional AC load (Rc || RL)
- Output swing and clipping checks
- First-order bandwidth estimation (input/output poles)
- Priority-based conflict detection
- Standard resistor series (E12 / E24)

### Not Supported
- Multiple transistor stages
- Emitter followers / buffers
- Feedback amplifiers
- Cascode stages
- MOSFETs
- Temperature-dependent β variation
- Early effect (ro)
- Large-signal transient behavior
- SPICE-level accuracy

---

## How to Use

### 1. Define Known Inputs

User inputs are provided via `raw_known`. Every value here is treated as **fixed** (priority = `user`).

```python
raw_known = {
    "Vdd": 12,
    "Ic": 1e-3,
    "Rc": 3000,
    "Re": 1000,
    "beta": 100,
    "Vbe": 0.7,
}
```

Any variable provided here cannot be overridden by derived rules.

## Input Parameters 

| Name          | Description                              |
| ------------- | ---------------------------------------- |
| Vdd           | Supply voltage                           |
| Ic            | Collector current                        |
| beta          | DC current gain                          |
| Vbe           | Base-emitter voltage                     |
| Rc            | Collector resistor                       |
| Re            | Emitter resistor                         |
| Vce_min       | Minimum VCE for forward-active operation |
| divider_ratio | Divider current to base current ratio    |
| RL            | Load resistance                          |
| Re_bypassed   | AC bypass flag for emitter resistor      |
| C_in_total    | Lumped input capacitance                 |
| C_out_total   | Lumped output capacitance                |
| Vt            | Thermal voltage                          |
| I_cutoff      | Cutoff current threshold                 |

## What Works

- Priority-aware constraint resolution
- Forward and inverse bias computation
- Automatic region-of-operation detection
- Small-signal gain and input resistance
- AC loading using Rc || RL
- Output swing and clipping checks
- Optional first-order bandwidth estimation
- E12 and E24 resistor suggestion

## What Does Not Work / Constraints
- Single BJT stage only
- Constant beta assumption
- No Early effect (ro not modeled)
- No temperature variation beyond fixed Vt
- Small-signal model valid only in forward-active region
- Bandwidth is first-order only (no Miller effect)
- No transient, distortion, or noise analysis

## How the Code Works
**Priority System**

Each variable is stored as a structured value containing:
- numerical value
- priority level
- source tag

**Priority order:**

`derived < design < user < physics`


Higher-priority values override lower-priority values. Equal-priority conflicts raise errors.

## Dependency and Loop Protection

Predefined dependency loops (for example Ic, Rc, Vc) are checked before solving. An error is raised only if all variables in a loop are user-fixed.

## Rule Engine

Rules are declarative objects with required inputs, a produced output, and a function.
```python
{
  "needs": {"Ic", "beta"},
  "produces": "Ib",
  "fn": lambda k: Ic / beta
}
```

The solver:

- Executes rules when inputs exist
- Writes outputs only if priority allows
- Iterates until no further updates occur

This is a fixed-point symbolic solver, not a numerical iterator.

## Rule Groupings

**Bias and KCL/KVL**
- Current summation, voltage drops, and node voltages.

**Design Intent**
- Divider sizing, headroom targets, inverse biasing.

**Region Detection**
- Cutoff, saturation, reverse-active, forward-active.

**Small-Signal Model**
- gm, re, rpi, gain, and input resistance.

**Bandwidth**
- Input and output poles using lumped RC models.
  
**Reporting**
- Formatted output and nearest standard resistor selection.

