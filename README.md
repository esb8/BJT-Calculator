# BJT Bias & Small-Signal Calculator

## Overview

This project implements a calculator inteded for rapid **BJT common-emitter amplifier** iterations, capable of rule-based DC bias, small-signal, and bandwidth calculations. Users can also optimize AC headroom voltages and automatically calculated a corresponding 2nd stage BJT Buffer.

The solver is **constraint-driven**. Users specify known parameters (currents, voltages, resistances, options), and the engine derives all other quantities.

This tool is **analytical, not a simulator**. It does not perform numerical integration, transient simulation, or SPICE-level modeling.

---

## Scope

### Supported
- DC bias analysis (Ic, Ib, Ie, Vb, Ve, Vc, Vce)
- Voltage-divider base bias design
- Region-of-operation classification
- Small-signal parameters (gm, re, rπ, gain)
- Optional emitter bypass capacitor
- Optional AC load (Rc || RL)
- Output resistance (Rout = Rc_ac)
- Output swing and clipping checks
- First-order bandwidth estimation (input/output poles)
- Priority-based conflict detection
- Standard resistor series (E12 / E24)
- Bias centering suggestions (design_optimize_vc)
- Post-solve emitter-follower buffer design (design_second_stage_buffer)

### Not Supported
- Feedback amplifiers
- Cascode stages
- MOSFETs
- Temperature-dependent β variation
- Early effect (ro)
- Large-signal transient behavior
- SPICE-level accuracy
- Miller capacitance
- Transient, distortion, or noise analysis

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

Any variable provided here cannot be overridden by derived rules. And are then calculated and printed

### 2. Constrain Check, Solve and Print

```python
known = normalize_user_known(raw_known)

check_overconstrained(known, DEPENDENCY_LOOPS)
known = solve(known, RULES)

print_report(known)
```

## Input Parameters 

| Name          | Description                    |
| ------------- | ------------------------------ |
| Vdd           | Supply voltage                 |
| Ic            | Collector current              |
| beta          | DC current gain                |
| Vbe           | Base-emitter voltage           |
| Rc            | Collector resistor             |
| Re            | Emitter resistor               |
| Vce_min       | Minimum VCE for forward-active |
| divider_ratio | Divider current / base current |
| Re_bypassed   | AC bypass flag for Re          |
| load_enabled  | Enables AC load RL             |
| RL            | Load resistance (AC)           |
| Vt            | Thermal voltage                |
| I_cutoff      | Cutoff current threshold       |
| ac_in         | Small-signal input peak        |
| C_in_total    | Lumped input capacitance       |
| C_out_total   | Lumped output capacitance      |

## Options

### Load Resistance
Options can be selected by setting the load_enabled value to true or false/

- No Early effect modeled
- Output resistance is:

```python
Rout = Rc_ac
```
```python
Where:
Rc_ac = Rc              (load_enabled = False)
Rc_ac = Rc || RL        (load_enabled = True)
```

### Gain Options
Controls whether the emitter resistor is AC-bypassed.

**Unbypassed:**
```python
Re_bypassed = False
```
Unbypassed emitter: \( A_v = -\frac{R_{C,\mathrm{ac}}}{r_e + R_E} \)

**Bypassed:**
```python
Re_bypassed = True
```
Bypassed emitter: \( A_v = -\frac{R_{C,\mathrm{ac}}}{r_e} \)


## Vc Headroom Optimization

```python
design_optimize_vc(known, target="center")
```

Suggests adjustments to:
- Rc
- Ic (via bias)
- Re
- Divider resistors

Targets:
- "center" – symmetric swing
- "max_swing"
- "low_distortion" minimum swing

This does not modify the circuit automatically.


## Second-Stage Buffer (Emitter Follower)
```python
design_second_stage_buffer(known)
```
This buffer calculator utilizes the output values from first BJT stage solver engine to provide its inputs, it does not utilize the inputs from the headroom optimizer.

It:
- Designs a DC-biased emitter follower
- Accounts for first-stage Rout
- Computes attenuation, headroom, and input resistance
- Does not feed back into the solver
Small-signal assumptions:
- Forward-active
- 𝑅𝑖𝑛 ≈ (𝛽 + 1)*𝑅𝐸​

# Solver 

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

## Dependency and Loop Protection

Predefined dependency loops (for example Ic, Rc, Vc) are checked before solving. An error is raised only if all variables in a loop are user-fixed.
```python
DEPENDENCY_LOOPS = [
    {"Ic", "Rc", "Vc"},     # collector loop
    {"Ie", "Re", "Ve"},     # emitter loop
    {"Vc", "Ve", "Vce"},    # vertical voltage loop
    {"Ib", "beta", "Ic"},   # base-current loop
]
```

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



