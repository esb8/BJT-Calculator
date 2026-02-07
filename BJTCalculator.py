import math

mA = 1e-3  # milliampere


# ============================================================
# STANDARD RESISTOR SERIES
# ============================================================

E12_BASE = [
    1.0, 1.2, 1.5, 1.8, 2.2, 2.7,
    3.3, 3.9, 4.7, 5.6, 6.8, 8.2
]

E24_BASE = [
    1.0, 1.1, 1.2, 1.3, 1.5, 1.6, 1.8, 2.0, 2.2, 2.4, 2.7, 3.0,
    3.3, 3.6, 3.9, 4.3, 4.7, 5.1, 5.6, 6.2, 6.8, 7.5, 8.2, 9.1
]


def generate_resistors(base, decades=(-1, 6)):
    values = []
    for d in range(decades[0], decades[1] + 1):
        for b in base:
            values.append(b * (10 ** d))
    return sorted(values)


E12_VALUES = generate_resistors(E12_BASE)
E24_VALUES = generate_resistors(E24_BASE)


def nearest_resistors(target, series, n=3):
    ranked = sorted(series, key=lambda r: abs(r - target))
    return ranked[:n]


# ============================================================
# RULE ENGINE CORE
# ============================================================

def solve(known, rules):
    """
    Fixed-point solver with OPTIONAL rule conditions:
      - if rule has key "cond", it must return True to apply.
    """
    updated = True
    while updated:
        updated = False
        for rule in rules:
            out = rule["produces"]
            if out in known:
                continue
            if not rule["needs"].issubset(known):
                continue
            if "cond" in rule and not rule["cond"](known):
                continue
            known[out] = rule["fn"](known)
            updated = True
    return known


# ============================================================
# EQUATION GENERATORS (GENERIC, REUSABLE)
# ============================================================

def linear(a, b, out):
    """
    out = a * b
    a   = out / b
    b   = out / a
    """
    return [
        {"needs": {a, b}, "produces": out, "fn": lambda k, a=a, b=b: k[a] * k[b]},
        {"needs": {out, b}, "produces": a,   "fn": lambda k, out=out, b=b: k[out] / k[b]},
        {"needs": {out, a}, "produces": b,   "fn": lambda k, out=out, a=a: k[out] / k[a]},
    ]


def diff(a, b, out):
    """
    out = a - b
    a   = out + b
    b   = a - out
    """
    return [
        {"needs": {a, b},   "produces": out, "fn": lambda k, a=a, b=b: k[a] - k[b]},
        {"needs": {out, b}, "produces": a,   "fn": lambda k, out=out, b=b: k[out] + k[b]},
        {"needs": {a, out}, "produces": b,   "fn": lambda k, a=a, out=out: k[a] - k[out]},
    ]


def sum3(a, b, out):
    """
    out = a + b
    a   = out - b
    b   = out - a
    """
    return [
        {"needs": {a, b},   "produces": out, "fn": lambda k, a=a, b=b: k[a] + k[b]},
        {"needs": {out, b}, "produces": a,   "fn": lambda k, out=out, b=b: k[out] - k[b]},
        {"needs": {out, a}, "produces": b,   "fn": lambda k, out=out, a=a: k[out] - k[a]},
    ]


# ============================================================
# RULE SET (DECLARATIVE, ORDERED BY PRIORITY)
# ============================================================

RULES = []

# ------------------------------------------------------------
# OUTPUT SWING / CLIPPING CHECKS
# ------------------------------------------------------------

RULES += [

    # Collector voltage bounds
    {"needs": {"Ve", "Vce_min"}, "produces": "Vc_min",
     "fn": lambda k: k["Ve"] + k["Vce_min"]},

    {"needs": {"Vdd"}, "produces": "Vc_max",
     "fn": lambda k: k["Vdd"]},

    # Headroom
    {"needs": {"Vc", "Vc_min"}, "produces": "headroom_down",
     "fn": lambda k: k["Vc"] - k["Vc_min"]},

    {"needs": {"Vc_max", "Vc"}, "produces": "headroom_up",
     "fn": lambda k: k["Vc_max"] - k["Vc"]},
]

RULES += [

    # Lower clipping
    {"needs": {"Vout_pk", "headroom_down"}, "produces": "clips_low",
     "fn": lambda k: k["Vout_pk"] > k["headroom_down"]},

    # Upper clipping
    {"needs": {"Vout_pk", "headroom_up"}, "produces": "clips_high",
     "fn": lambda k: k["Vout_pk"] > k["headroom_up"]},

    # Overall clipping flag
    {"needs": {"clips_low", "clips_high"}, "produces": "clips",
     "fn": lambda k: k["clips_low"] or k["clips_high"]},
]


# ----------------------------
# (1) BJT β/α relationships
# ----------------------------

RULES += [
    {"needs": {"beta"}, "produces": "alpha", "fn": lambda k: k["beta"] / (k["beta"] + 1.0)},
    {"needs": {"Ic", "beta"}, "produces": "Ib", "fn": lambda k: k["Ic"] / k["beta"]},
]

# ------------------------------------------------------------
# 2) SUM RELATIONSHIPS
# ------------------------------------------------------------

RULES += sum3("Ic", "Ib", "Ie")
RULES += sum3("Ve", "Vbe", "Vb")

# ------------------------------------------------------------
# 3) LINEAR RELATIONSHIPS
# ------------------------------------------------------------

RULES += linear("Ie", "Re", "Ve")
RULES += linear("Ic", "Rc", "Vrc")

# ------------------------------------------------------------
# 4) DIFFERENCE RELATIONSHIPS
# ------------------------------------------------------------

RULES += diff("Vdd", "Vrc", "Vc")
RULES += diff("Vc", "Ve", "Vce")

# ------------------------------------------------------------
# 5) DESIGN / INTENT RULES
# ------------------------------------------------------------

RULES += [
    {"needs": {"Ib", "divider_ratio"}, "produces": "Idiv", "fn": lambda k: k["divider_ratio"] * k["Ib"]},
    {"needs": {"Vb", "Idiv"}, "produces": "R2", "fn": lambda k: k["Vb"] / k["Idiv"]},
    {"needs": {"Vdd", "Vb", "Idiv", "Ib"}, "produces": "R1",
     "fn": lambda k: (k["Vdd"] - k["Vb"]) / (k["Idiv"] + k["Ib"])},
    {"needs": {"R1", "R2"}, "produces": "Req",
     "fn": lambda k: (k["R1"] * k["R2"]) / (k["R1"] + k["R2"])},
    {"needs": {"Vdd", "R1", "R2"}, "produces": "Vth",
     "fn": lambda k: k["Vdd"] * k["R2"] / (k["R1"] + k["R2"])},
    {"needs": {"Vth", "Ib", "Req"}, "produces": "Vb_loaded",
     "fn": lambda k: k["Vth"] - k["Ib"] * k["Req"]},
]

RULES += [
    {"needs": {"Vdd", "Ve", "Vce_min"}, "produces": "Vc",
     "fn": lambda k: 0.5 * (k["Vdd"] + (k["Ve"] + k["Vce_min"]))}
]

# ------------------------------------------------------------
# 5b) OUTPUT LOADING (AC)
# ------------------------------------------------------------

RULES += [
    # Rc_ac = Rc || RL (only if load_enabled True)
    {"needs": {"Rc", "RL", "load_enabled"}, "produces": "Rc_ac",
     "cond": lambda k: bool(k["load_enabled"]),
     "fn": lambda k: (k["Rc"] * k["RL"]) / (k["Rc"] + k["RL"])},

    # If load is disabled, just use Rc
    {"needs": {"Rc", "load_enabled"}, "produces": "Rc_ac",
     "cond": lambda k: not bool(k["load_enabled"]),
     "fn": lambda k: k["Rc"]},
]

# ------------------------------------------------------------
# 6) REGION-OF-OPERATION CHECKS
# ------------------------------------------------------------

RULES += [
    {"needs": {"Vb", "Vc"}, "produces": "Vbc", "fn": lambda k: k["Vb"] - k["Vc"]},

    {"needs": {"Vbe", "Ic", "I_cutoff"}, "produces": "is_cutoff",
     "fn": lambda k: (k["Vbe"] < 0.6) or (k["Ic"] <= k["I_cutoff"])},

    {"needs": {"Vce", "Vce_min"}, "produces": "is_saturation",
     "fn": lambda k: k["Vce"] <= k["Vce_min"]},

    {"needs": {"Vbc"}, "produces": "is_reverse_active",
     "fn": lambda k: k["Vbc"] > 0},

    {"needs": {"is_cutoff", "is_saturation", "is_reverse_active"},
     "produces": "is_forward_active",
     "fn": lambda k: not (k["is_cutoff"] or k["is_saturation"] or k["is_reverse_active"])},

    {"needs": {"is_cutoff", "is_saturation", "is_reverse_active", "is_forward_active"},
     "produces": "region",
     "fn": lambda k:
        "cutoff" if k["is_cutoff"] else
        "saturation" if k["is_saturation"] else
        "reverse-active" if k["is_reverse_active"] else
        "forward-active (linear)"},
]

RULES += [
    {"needs": {"Vce", "Vce_min"}, "produces": "sat_ok",
     "fn": lambda k: k["Vce"] > k["Vce_min"]},
]

# ------------------------------------------------------------
# 7) SMALL SIGNAL (ONLY VALID IN FORWARD-ACTIVE)
# ------------------------------------------------------------

RULES += [
    # AC emitter resistance depends on bypass capacitor:
    {"needs": {"Re", "Re_bypassed"}, "produces": "Re_ac",
     "fn": lambda k: 0.0 if k["Re_bypassed"] else k["Re"]},

    # gm = Ic / Vt
    {"needs": {"Ic", "Vt", "is_forward_active"}, "produces": "gm",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: k["Ic"] / k["Vt"]},

    # re = 1/gm
    {"needs": {"gm", "is_forward_active"}, "produces": "re",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: 1.0 / k["gm"]},

    # rpi = beta/gm
    {"needs": {"beta", "gm", "is_forward_active"}, "produces": "rpi",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: k["beta"] / k["gm"]},

    # Av uses Rc_ac (which equals Rc if load_enabled False)
    {"needs": {"Rc_ac", "re", "Re_ac", "is_forward_active"}, "produces": "Av",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: -k["Rc_ac"] / (k["re"] + k["Re_ac"])},
]

# ------------------------------------------------------------
# 8) BANDWIDTH / POLES (OPTIONAL)
# ------------------------------------------------------------
# Simple RC pole model for high-frequency rolloff:
#   f_H ≈ 1/(2π * R_out * C_out)
# where:
#   R_out ≈ Rc_ac   (rough CE output resistance ignoring ro)
#   C_out = C_out_total  (user-supplied lumped capacitance at collector node)
#
# You can also model input pole similarly with R_in and C_in_total.
#
# This is "good enough" early, and scales later.

RULES += [
    # wH = 1/(R_out * C_out_total)
    {"needs": {"Rc_ac", "C_out_total", "is_forward_active"}, "produces": "wH_out",
     "cond": lambda k: bool(k["is_forward_active"]) and (k["C_out_total"] > 0),
     "fn": lambda k: 1.0 / (k["Rc_ac"] * k["C_out_total"])},

    # fH = w/(2π)
    {"needs": {"wH_out"}, "produces": "fH_out",
     "fn": lambda k: k["wH_out"] / (2.0 * math.pi)},

    # Input resistance approx:
    # Rin_base ≈ rpi + (beta+1)*Re_ac
    {"needs": {"rpi", "beta", "Re_ac", "is_forward_active"}, "produces": "Rin_base",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: k["rpi"] + (k["beta"] + 1.0) * k["Re_ac"]},

    # Total input resistance seen by source (if divider exists): Rin_total = Rin_base || Req
    {"needs": {"Rin_base", "Req", "is_forward_active"}, "produces": "Rin_total",
     "cond": lambda k: bool(k["is_forward_active"]),
     "fn": lambda k: (k["Rin_base"] * k["Req"]) / (k["Rin_base"] + k["Req"])},

    # Input pole: fH_in ≈ 1/(2π * Rin_total * C_in_total)
    {"needs": {"Rin_total", "C_in_total", "is_forward_active"}, "produces": "wH_in",
     "cond": lambda k: bool(k["is_forward_active"]) and (k["C_in_total"] > 0),
     "fn": lambda k: 1.0 / (k["Rin_total"] * k["C_in_total"])},

    {"needs": {"wH_in"}, "produces": "fH_in",
     "fn": lambda k: k["wH_in"] / (2.0 * math.pi)},

    # Overall upper cutoff (dominant pole approximation): fH = min(fH_in, fH_out) if both exist
    {"needs": {"fH_in", "fH_out"}, "produces": "fH",
     "fn": lambda k: min(k["fH_in"], k["fH_out"])},
]


# ============================================================
# REPORTING (VIEW LAYER)
# ============================================================

def fmt(v, unit=""):
    if v is None:
        return "—"
    if abs(v) < 1e-12 and v != 0:
        return f"{v:.3e} {unit}".strip()
    if abs(v) < 1e-3 and v != 0:
        return f"{v*1e6:.2f} µ{unit}"
    if abs(v) < 1:
        return f"{v*1e3:.3f} m{unit}"
    return f"{v:.3f} {unit}"


def print_resistor_block(name, value, series):
    choices = nearest_resistors(value, series, n=3)
    print(f"{name:<8}: {fmt(value,'Ω')}  → ", end="")
    print(", ".join(f"{r:.0f}Ω ({(r-value)/value:+.1%})" for r in choices))


def print_report(k, series=E24_VALUES):
    print("\n================ CIRCUIT REPORT ================")

    print("\n[ CONSTANTS ]")
    for key, unit in (
        ("Vdd", "V"),
        ("Vbe", "V"),
        ("beta", ""),
        ("alpha", ""),
        ("divider_ratio", ""),
        ("Vt", "V"),
        ("I_cutoff", "A"),
        ("Re_bypassed", ""),
        ("load_enabled", ""),
    ):
        if key in k:
            if isinstance(k[key], bool):
                print(f"{key:<12}: {k[key]}")
            else:
                print(f"{key:<12}: {fmt(k[key], unit)}")

    print("\n[ BASE ]")
    for key, unit in (("Vb", "V"), ("Vb_loaded", "V"), ("Ib", "A"), ("Vbc", "V")):
        if key in k:
            print(f"{key:<12}: {fmt(k[key], unit)}")
    for r in ("R1", "R2", "Req"):
        if r in k:
            print_resistor_block(r, k[r], series)

    print("\n[ EMITTER ]")
    for key, unit in (("Ve", "V"), ("Ie", "A")):
        if key in k:
            print(f"{key:<12}: {fmt(k[key], unit)}")
    if "Re" in k:
        print_resistor_block("Re", k["Re"], series)
    if "Re_ac" in k:
        print(f"{'Re_ac':<12}: {fmt(k['Re_ac'], 'Ω')}")

    print("\n[ COLLECTOR ]")
    for key, unit in (("Vc", "V"), ("Vce", "V"), ("Ic", "A")):
        if key in k:
            print(f"{key:<12}: {fmt(k[key], unit)}")
    if "Rc" in k:
        print_resistor_block("Rc", k["Rc"], series)
    if "RL" in k:
        print_resistor_block("RL", k["RL"], series)
    if "Rc_ac" in k:
        print(f"{'Rc_ac':<12}: {fmt(k['Rc_ac'], 'Ω')}")

    print("\n[ REGION ]")
    if "region" in k:
        print(f"{'region':<12}: {k['region']}")
    if "sat_ok" in k:
        print(f"{'sat_ok':<12}: {k['sat_ok']}")

    print("\n[ SMALL SIGNAL ]")
    if "is_forward_active" in k and not k["is_forward_active"]:
        print("Small-signal model not valid (not in forward-active region).")
    else:
        for key, unit in (
            ("gm", "S"),
            ("re", "Ω"),
            ("rpi", "Ω"),
            ("Av", "V/V"),
            ("Rin_total", "Ω"),
        ):
            if key in k:
                print(f"{key:<12}: {fmt(k[key], unit)}")

    print("\n[ BANDWIDTH ]")
    if "is_forward_active" in k and not k["is_forward_active"]:
        print("Bandwidth model not valid (not in forward-active region).")
    else:
        any_bw = False
        for key, unit in (
            ("fH_in", "Hz"),
            ("fH_out", "Hz"),
            ("fH", "Hz"),
            ("C_in_total", "F"),
            ("C_out_total", "F"),
        ):
            if key in k:
                any_bw = True
                print(f"{key:<12}: {fmt(k[key], unit)}")
        if not any_bw:
            print("No capacitances provided; set C_in_total and/or C_out_total to estimate bandwidth.")

    # =====================================================
    # OUTPUT SWING / CLIPPING
    # =====================================================

    print("\n[ OUTPUT SWING ]")
    for key, unit in (
        ("Vc_min", "V"),
        ("Vc", "V"),
        ("Vc_max", "V"),
        ("headroom_down", "V"),
        ("headroom_up", "V"),
    ):
        if key in k:
            print(f"{key:<14}: {fmt(k[key], unit)}")

    if "Vout_pk" in k:
        print(f"\n{'Vout_pk':<14}: {fmt(k['Vout_pk'], 'V')}")
        if "clips_low" in k:
            print(f"{'clips_low':<14}: {k['clips_low']}")
        if "clips_high" in k:
            print(f"{'clips_high':<14}: {k['clips_high']}")
        if "clips" in k:
            print(f"{'clips':<14}: {k['clips']}")
            if k["clips"]:
                print("⚠️  Output will clip.")

# ------------------------------------------------------------
# HEADROOM-DRIVEN BIAS (INVERSE DESIGN)
# ------------------------------------------------------------

RULES += [

    # Vc = Vc_min + headroom_down_target
    {
        "needs": {"Ve", "Vce_min", "headroom_down_target", "set_headroom_down"},
        "produces": "Vc",
        "cond": lambda k: bool(k["set_headroom_down"]),
        "fn": lambda k: (k["Ve"] + k["Vce_min"]) + k["headroom_down_target"],
    },

    # Vc = Vc_max - headroom_up_target
    {
        "needs": {"Vdd", "headroom_up_target", "set_headroom_up"},
        "produces": "Vc",
        "cond": lambda k: bool(k["set_headroom_up"]),
        "fn": lambda k: k["Vdd"] - k["headroom_up_target"],
    },

    # symmetric headroom (already conceptually present, formalized)
    {
        "needs": {"Vdd", "Ve", "Vce_min", "set_headroom_symmetric"},
        "produces": "Vc",
        "cond": lambda k: bool(k["set_headroom_symmetric"]),
        "fn": lambda k: 0.5 * (k["Vdd"] + (k["Ve"] + k["Vce_min"])),
    },
]

# ============================================================
# RUN
# ============================================================

known = {
    "Vdd": 12,
    "Vbe": 0.7,
    "Re": 1000,
    "Rc": 3000,
    "Vce_min": 0.2,
    "Ic": 1e-3,
    "beta": 100,
    "divider_ratio": 10.0,

    # small-signal constants
    "Vt": 0.02585,
    "I_cutoff": 1e-6,

    # bypass option
    "Re_bypassed": False,

    # load option
    "load_enabled": False,  # set True to use RL
    "RL": 10_000,           # only used if load_enabled True

    # bandwidth lumped caps (set to 0 or omit to skip)
    "C_in_total": 0.0,      # F (e.g. 30e-12 for 30 pF)
    "C_out_total": 0.0,     # F (e.g. 10e-12 for 10 pF)
}

print("Initial known:")
print(known)

known = solve(known, RULES)

print("\nFinal known:")
print(known)

print_report(known)
