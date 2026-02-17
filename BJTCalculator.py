import math


def getv(known, key):
    """Read a value from `known` (no priority/meta wrapper).
    """
    if key not in known:
        return None
    return known[key]


def has(known, key):
    return key in known


def keys(known):
    return set(known.keys())


def setv(known, key, value, source="rule"):
    """Simple write: set or overwrite raw values in `known`.
    Returns True if updated, False if the value was unchanged.
    """
    if key not in known:
        known[key] = value
        return True

    old_val = known[key]
    # consider float near-equality as unchange  d
    if isinstance(old_val, (int, float)) and isinstance(value, (int, float)):
        if abs(old_val - value) <= 1e-12 * max(1.0, abs(old_val), abs(value)):
            return False
    if old_val == value:
        return False

    # Overwrite on difference (no priority system)
    known[key] = value
    return True


def set_defaults(known, defaults):
    """
    Insert defaults as *derived* (lowest) so they always yield to user/design.
    """
    for k, v in defaults.items():
        if k not in known:
            setv(known, k, v, source="default")


def normalize_user_known(raw_known):
    """
    Convert raw user dict to meta dict with priority=user.
    """
    return dict(raw_known)


# ============================================================
# DEPENDENCY LOOPS / OVERCONSTRAINT CHECKS
# ============================================================

DEPENDENCY_LOOPS = [
    {"Ic", "Rc", "Vc"},     # collector loop
    {"Ie", "Re", "Ve"},     # emitter loop
    {"Vc", "Ve", "Vce"},    # vertical voltage loop
    {"Ib", "beta", "Ic"},   # base-current loop
]


def check_overconstrained(known, loops):
    """
    Raise an error only if ALL variables in a dependency loop
    are fixed by the user (priority=user or above).
    """
    errors = []

    for loop in loops:
        fixed = set()
        for var in loop:
            if var in known:
                fixed.add(var)

        if fixed == loop:
            errors.append(loop)

    if errors:
        msg = "Overconstrained system detected:\n"
        for loop in errors:
            msg += f"  All variables fixed in loop: {sorted(loop)}\n"
        raise ValueError(msg)


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
# RULE ENGINE CORE (PRIORITY-AWARE)
# ============================================================
def solve(known, rules, tags=None):
    """
    Tag-aware fixed-point solver.

    active_tags: set[str] or None
        If provided, only rules whose tags intersect this set will run.
        Rules without 'tags' default to {"dc"}.
    """
    updated = True
    while updated:
        updated = False
        for rule in rules:

            # ----------------------------
            # TAG FILTERING
            # ----------------------------
            rule_tags = rule.get("tags", {"dc"})
            if tags is not None:
                if rule_tags.isdisjoint(tags):
                    continue

            out = rule["produces"]

            # ----------------------------
            # SKIP IF OUTPUT ALREADY KNOWN
            # ----------------------------
            if out in known:  # <-- generic check added
                continue

            # can we run?
            if not rule["needs"].issubset(keys(known)):
                continue
            if "cond" in rule and not rule["cond"](known):
                continue

            # compute candidate
            value = rule["fn"](known)

            # write
            src = rule.get("source", "rule")
            did = setv(known, out, value, source=src)
            if did:
                updated = True

    return known

# ============================================================
# EQUATION GENERATORS (GENERIC, REUSABLE)
# ============================================================

def linear(a, b, out, source="linear", tags=None):
    """
    out = a * b
    a   = out / b
    b   = out / a
    """

    return [
        {"needs": {a, b}, "produces": out, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) * getv(k, b)},
        {"needs": {out, b}, "produces": a, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) / getv(k, b)},
        {"needs": {out, a}, "produces": b, "source": source,
         "fn": lambda k, out=out, a=a: getv(k, out) / getv(k, a)},
    ]


def diff(a, b, out, source="diff", tags=None):
    """
    out = a - b
    a   = out + b
    b   = a - out
    """
    return [
        {"needs": {a, b}, "produces": out, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) - getv(k, b)},
        {"needs": {out, b}, "produces": a, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) + getv(k, b)},
        {"needs": {a, out}, "produces": b, "source": source,
         "fn": lambda k, a=a, out=out: getv(k, a) - getv(k, out)},
    ]


def sum3(a, b, out, source="sum", tags=None):
    """
    out = a + b
    a   = out - b
    b   = out - a
    """
    return [
        {"needs": {a, b}, "produces": out, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) + getv(k, b)},
        {"needs": {out, b}, "produces": a, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) - getv(k, b)},
        {"needs": {out, a}, "produces": b, "source": source,
         "fn": lambda k, out=out, a=a: getv(k, out) - getv(k, a)},
    ]

RULES = []

# Linear Rc rule is always present, but only runs if Rc is NOT bypassed

# Ic-from-Ie rule for bypassed Rc


# ============================================================
# SWING / HEADROOM  (AMP)
# ============================================================

RULES += [
    {"needs": {"Ve", "Vce_min"}, "produces": "Vc_min",
     "source": "swing",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "Ve") + getv(k, "Vce_min")},

    {"needs": {"Vdd"}, "produces": "Vc_max",
     "source": "swing",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "Vdd")},

    {"needs": {"Vc", "Vc_min"}, "produces": "headroom_down",
     "source": "swing",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "Vc") - getv(k, "Vc_min")},

    {"needs": {"Vc_max", "Vc"}, "produces": "headroom_up",
     "source": "swing",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "Vc_max") - getv(k, "Vc")},
]


# ============================================================
# CLIPPING  (AMP)
# ============================================================

RULES += [
    {"needs": {"ac_in", "headroom_down"}, "produces": "clips_low",
     "source": "clip",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "ac_in") > getv(k, "headroom_down")},

    {"needs": {"ac_in", "headroom_up"}, "produces": "clips_high",
     "source": "clip",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "ac_in") > getv(k, "headroom_up")},

    {"needs": {"clips_low", "clips_high"}, "produces": "clips",
     "source": "clip",
     "tags": {"amp"},
     "fn": lambda k: bool(getv(k, "clips_low")) or bool(getv(k, "clips_high"))},
]


# ============================================================
# (1) BJT β/α RELATIONSHIPS  (DC)
# ============================================================

RULES += [
    {"needs": {"beta"}, "produces": "alpha",
     "source": "bjt",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "beta") / (getv(k, "beta") + 1.0)},

    {"needs": {"Ic", "beta"}, "produces": "Ib",
     "source": "bjt",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Ic") / getv(k, "beta")},
]

# Only compute Ic/Ib if Rc is bypassed
RULES += [
    {"needs": {"Ie", "beta"}, "produces": "Ic",
     "tags": {"dc"},
     "cond": lambda k: bool(getv(k, "Rc_bypassed")) is True,
     "fn": lambda k: getv(k,"Ie") * getv(k,"beta") / (getv(k,"beta") + 1)
    },
    {"needs": {"Ie", "beta"}, "produces": "Ib",
     "tags": {"dc"},
     "cond": lambda k: bool(getv(k, "Rc_bypassed")) is True,
     "fn": lambda k: getv(k,"Ie") / (getv(k,"beta") + 1)
    }
]

# Skip collector drop rules if Rc is bypassed
for rule in RULES:
    if rule.get("source") == "collector_drop":
        rule["cond"] = lambda k: not bool(getv(k, "Rc_bypassed"))

# ============================================================
# (2) SUM RELATIONSHIPS  (DC)
# ============================================================

RULES += sum3("Ic", "Ib", "Ie", source="kcl", tags={"dc"})
RULES += sum3("Ve", "Vbe", "Vb", source="vbe", tags={"dc"}) ## RC bypassed necearry


# ============================================================
# (3) LINEAR RELATIONSHIPS  (DC)
# ============================================================

RULES += linear("Ie", "Re", "Ve", source="emitter", tags={"dc"})


# Suppose you have a flag `rc_bypassed` in your knowns
RULES += linear("Ic", "Rc", "Vrc", source="collector_drop", tags={"dc"})


# ============================================================
# (4) DIFFERENCE RELATIONSHIPS  (DC)
# ============================================================

RULES += diff("Vdd", "Vrc", "Vc",
              source="collector_node",
              tags={"dc"})

RULES += diff("Vc", "Ve", "Vce",
              source="vce",
              tags={"dc"})


# ============================================================
# (5) DESIGN / INTENT RULES  (DC)
# ============================================================

RULES += [
    {"needs": {"Ib", "divider_ratio"}, "produces": "Idiv",
     "source": "divider_design",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "divider_ratio") * getv(k, "Ib")},

    {"needs": {"Vb", "Idiv"}, "produces": "R2",
     "source": "divider_design",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Vb") / getv(k, "Idiv")},

    {"needs": {"Vdd", "Vb", "Idiv", "Ib"}, "produces": "R1",
     "source": "divider_design",
     "tags": {"dc"},
     "fn": lambda k: (getv(k, "Vdd") - getv(k, "Vb")) /
                    (getv(k, "Idiv") + getv(k, "Ib"))},

    {"needs": {"R1", "R2"}, "produces": "Req",
     "source": "divider",
     "tags": {"dc"},
     "fn": lambda k: (getv(k, "R1") * getv(k, "R2")) /
                    (getv(k, "R1") + getv(k, "R2"))},

    {"needs": {"Vdd", "R1", "R2"}, "produces": "Vth",
     "source": "divider",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Vdd") * getv(k, "R2") /
                    (getv(k, "R1") + getv(k, "R2"))},

    {"needs": {"Vth", "Ib", "Req"}, "produces": "Vb_loaded",
     "source": "divider",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Vth") -
                    getv(k, "Ib") * getv(k, "Req")},

    {"needs": {"Vth", "Idiv"}, "produces": "R2_th",
     "source": "divider_design",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Vth") / getv(k, "Idiv")},

    {"needs": {"Vdd", "Vth", "Idiv"}, "produces": "R1_th",
     "source": "divider_design",
     "tags": {"dc"},
     "fn": lambda k: (getv(k, "Vdd") - getv(k, "Vth")) /
                    getv(k, "Idiv")},
]


# ============================================================
# (5b) OUTPUT LOADING (AC)  (AMP)
# ============================================================

RULES += [
    {"needs": {"Rc", "RL", "load_enabled"},
     "produces": "Rc_ac",
     "cond": lambda k: bool(getv(k, "load_enabled")),
     "source": "ac_load",
     "tags": {"amp"},
     "fn": lambda k: (getv(k, "Rc") * getv(k, "RL")) /
                    (getv(k, "Rc") + getv(k, "RL"))},

    {"needs": {"Rc", "load_enabled"},
     "produces": "Rc_ac",
     "cond": lambda k: not bool(getv(k, "load_enabled")),
     "source": "ac_load",
     "tags": {"amp"},
     "fn": lambda k: getv(k, "Rc")},
]


# ============================================================
# (6) REGION OF OPERATION  (DC)
# ============================================================

RULES += [
    {"needs": {"Vb", "Vc"}, "produces": "Vbc",
     "tags": {"dc"},
     "fn": lambda k: getv(k, "Vb") - getv(k, "Vc")},

    {"needs": {"Vbe", "Ic", "I_cutoff"}, "produces": "is_cutoff",
     "tags": {"dc"},
     "fn": lambda k:
        (getv(k, "Vbe") < 0.6) or
        (getv(k, "Ic") <= getv(k, "I_cutoff"))},

    {"needs": {"Vce", "Vce_min"}, "produces": "is_saturation",
     "tags": {"dc"},
     "fn": lambda k:
        getv(k, "Vce") <= getv(k, "Vce_min")},

    {"needs": {"Vbc"}, "produces": "is_reverse_active",
     "tags": {"dc"},
     "fn": lambda k:
        getv(k, "Vbc") > 0},

    {"needs": {"is_cutoff", "is_saturation", "is_reverse_active"},
     "produces": "is_forward_active",
     "tags": {"dc"},
     "fn": lambda k: not (
         bool(getv(k, "is_cutoff")) or
         bool(getv(k, "is_saturation")) or
         bool(getv(k, "is_reverse_active"))
     )},

    {"needs": {"is_cutoff", "is_saturation",
               "is_reverse_active", "is_forward_active"},
     "produces": "region",
     "tags": {"dc"},
     "fn": lambda k:
        "cutoff" if bool(getv(k, "is_cutoff")) else
        "saturation" if bool(getv(k, "is_saturation")) else
        "reverse-active" if bool(getv(k, "is_reverse_active")) else
        "forward-active (linear)"},
]

# ------------------------------------------------------------
# 7) SMALL SIGNAL (ONLY VALID IN FORWARD-ACTIVE) (derived)
# ------------------------------------------------------------
RULES += [

    # AC emitter resistance (bypass aware)
    {"needs": {"Re", "Re_bypassed", "is_forward_active"},
     "produces": "Re_ac",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: 0.0 if bool(getv(k, "Re_bypassed"))
                     else getv(k, "Re")},

    # gm = Ic / Vt
    {"needs": {"Ic", "Vt", "is_forward_active"},
     "produces": "gm",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: getv(k, "Ic") / getv(k, "Vt")},

    # re = 1/gm
    {"needs": {"gm", "is_forward_active"},
     "produces": "re",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: 1.0 / getv(k, "gm")},

    # rπ = β / gm
    {"needs": {"beta", "gm", "is_forward_active"},
     "produces": "rpi",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: getv(k, "beta") / getv(k, "gm")},

    # Rc || RL  (AC only)
    {"needs": {"Rc", "load_enabled", "RL", "is_forward_active"},
     "produces": "Rc_ac",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: (
         (getv(k,"Rc") * getv(k,"RL")) /
         (getv(k,"Rc") + getv(k,"RL"))
         if bool(getv(k,"load_enabled"))
         else getv(k,"Rc")
     )},

    # Av = -Rc_ac / (re + Re_ac)
    {"needs": {"Rc_ac", "re", "Re_ac", "is_forward_active"},
     "produces": "Av",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: -getv(k, "Rc_ac") /
                     (getv(k, "re") + getv(k, "Re_ac"))},

    # AC peak output
    {"needs": {"ac_in", "Av", "is_forward_active"},
     "produces": "Vout_pk",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "ac_peak_calc",
     "tag": "ac",
     "fn": lambda k: getv(k, "ac_in") * getv(k, "Av")},

    # Output resistance (AC)
    {"needs": {"Rc_ac", "is_forward_active"},
     "produces": "Rout",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "small_signal",
     "tag": "ac",
     "fn": lambda k: getv(k, "Rc_ac")},
]
# Solve for Rc_ac from Av_target


# ------------------------------------------------------------
# 8) BANDWIDTH / POLES (OPTIONAL) (derived)
# ------------------------------------------------------------
RULES += [

    # Miller capacitance
    {"needs": {"C_mu", "Av_mid", "is_forward_active"},
     "produces": "C_miller",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: getv(k, "C_mu") *
                     (1.0 - getv(k, "Av_mid"))},

    # Total input HF capacitance
    {"needs": {"C_pi", "C_miller", "C_stray_in", "is_forward_active"},
     "produces": "C_in_hf",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: getv(k, "C_pi") +
                     getv(k, "C_miller") +
                     getv(k, "C_stray_in")},

    # Input HF pole
    {"needs": {"Rin_total", "C_in_hf", "is_forward_active"},
     "produces": "wH_in",
     "cond": lambda k: bool(getv(k, "is_forward_active")) and getv(k, "C_in_hf") > 0,
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: 1.0 / (getv(k, "Rin_total") * getv(k, "C_in_hf"))},

    {"needs": {"wH_in", "is_forward_active"},
     "produces": "fH_in",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: getv(k, "wH_in") / (2.0 * math.pi)},

    # Output HF capacitance
    {"needs": {"C_mu", "C_stray_out", "is_forward_active"},
     "produces": "C_out_hf",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: getv(k, "C_mu") + getv(k, "C_stray_out")},

    # Output HF pole
    {"needs": {"Rc_ac", "C_out_hf", "is_forward_active"},
     "produces": "wH_out",
     "cond": lambda k: bool(getv(k, "is_forward_active")) and getv(k, "C_out_hf") > 0,
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: 1.0 / (getv(k, "Rc_ac") * getv(k, "C_out_hf"))},

    {"needs": {"wH_out", "is_forward_active"},
     "produces": "fH_out",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: getv(k, "wH_out") / (2.0 * math.pi)},

    # Overall high-frequency cutoff
    {"needs": {"fH_in", "fH_out", "is_forward_active"},
     "produces": "fH",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "source": "bandwidth",
     "tag": "ac",
     "fn": lambda k: min(getv(k, "fH_in"), getv(k, "fH_out"))},
]

# ------------------------------------------------------------
# LOW-FREQUENCY BANDWIDTH (coupling caps) (AC)
# ------------------------------------------------------------

RULES += [

    # Input coupling pole
    {"needs": {"Rin_total", "C_in_couple"}, "produces": "wL_in",
     "cond": lambda k: getv(k, "C_in_couple") > 0,
     "tag": "ac",
     "fn": lambda k: 1.0 / (getv(k, "Rin_total") * getv(k, "C_in_couple"))},

    {"needs": {"wL_in"}, "produces": "fL_in",
     "tag": "ac",
     "fn": lambda k: getv(k, "wL_in") / (2.0 * math.pi)},

    # Output coupling pole
    {"needs": {"Rc_ac", "C_out_couple"}, "produces": "wL_out",
     "cond": lambda k: getv(k, "C_out_couple") > 0,
     "tag": "ac",
     "fn": lambda k: 1.0 / (getv(k, "Rc_ac") * getv(k, "C_out_couple"))},

    {"needs": {"wL_out"}, "produces": "fL_out",
     "tag": "ac",
     "fn": lambda k: getv(k, "wL_out") / (2.0 * math.pi)},

    # Overall low-frequency cutoff
    {"needs": {"fL_in", "fL_out"}, "produces": "fL",
     "tag": "ac",
     "fn": lambda k: max(getv(k, "fL_in"), getv(k, "fL_out"))},
]

# ============================================================
# OSCILLATOR RULES (Colpitts)
# ============================================================
import math

RULES += [

    # --- Compute C2 from C1 and feedback_ratio ---
    {
        "needs": {"C1", "feedback_ratio"},
        "produces": "C2",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"C2") is None 
                          and getv(k,"C1") not in (None,0)
                          and getv(k,"feedback_ratio") not in (None,0),
        "fn": lambda k: getv(k,"C1") / getv(k,"feedback_ratio")
    },

    # --- Compute C1 from C2 and feedback_ratio ---
    {
        "needs": {"C2", "feedback_ratio"},
        "produces": "C1",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"C1") is None 
                          and getv(k,"C2") not in (None,0)
                          and getv(k,"feedback_ratio") not in (None,0),
        "fn": lambda k: getv(k,"C2") * getv(k,"feedback_ratio")
    },

    # --- Equivalent capacitance of series C1-C2 ---
    {
        "needs": {"C1", "C2"},
        "produces": "Ceq",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"Ceq") is None 
                          and getv(k,"C1") not in (None,0) 
                          and getv(k,"C2") not in (None,0),
        "fn": lambda k: (getv(k,"C1") * getv(k,"C2")) / (getv(k,"C1") + getv(k,"C2"))
    },

    # --- Resonant frequency from freq input ---
    {
        "needs": {"freq"},
        "produces": "w0",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"w0") is None 
                          and getv(k,"freq") not in (None,0),
        "fn": lambda k: 2.0 * math.pi * getv(k,"freq")
    },

    # --- Resonant frequency from L and Ceq (only if w0 not already defined) ---
    {
        "needs": {"L", "Ceq"},
        "produces": "w0",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"w0") is None
                          and getv(k,"L") not in (None,0)
                          and getv(k,"Ceq") not in (None,0),
        "fn": lambda k: 1.0 / math.sqrt(getv(k,"L") * getv(k,"Ceq"))
    },

    # --- Frequency in Hz from w0 ---
    {
        "needs": {"w0"},
        "produces": "freq",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"freq") is None and getv(k,"w0") not in (None,0),
        "fn": lambda k: getv(k,"w0") / (2.0 * math.pi)
    },

    # Ceq from freq + L
    {
        "needs": {"freq", "L"},
        "produces": "Ceq",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"Ceq") is None 
                        and getv(k,"freq") not in (None,0)
                        and getv(k,"L") not in (None,0),
        "fn": lambda k: 1.0 / ((2 * math.pi * getv(k,"freq"))**2 * getv(k,"L"))
    },

        # C2 from Ceq + feedback_ratio
    {
        "needs": {"Ceq", "feedback_ratio"},
        "produces": "C2",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"C2") is None 
                        and getv(k,"Ceq") not in (None,0) 
                        and getv(k,"feedback_ratio") not in (None,0),
        "fn": lambda k: getv(k,"Ceq") * (1 + getv(k,"feedback_ratio")) / getv(k,"feedback_ratio")
    },

    # C1 from C2 + feedback_ratio
    {
        "needs": {"C2", "feedback_ratio"},
        "produces": "C1",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"C1") is None 
                        and getv(k,"C2") not in (None,0) 
                        and getv(k,"feedback_ratio") not in (None,0),
        "fn": lambda k: getv(k,"C2") * getv(k,"feedback_ratio")
    },

    # --- Compute L from freq + C1/C2 ---
    {
        "needs": {"Ceq", "freq"},
        "produces": "L",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"L") is None 
                          and getv(k,"Ceq") not in (None,0)
                          and getv(k,"freq") not in (None,0),
        "fn": lambda k: 1.0 / ((2*math.pi*getv(k,"freq"))**2 * getv(k,"Ceq"))
    },

    # --- Compute feedback_ratio from C1/C2 if not given ---
    {
        "needs": {"C1","C2"},
        "produces": "feedback_ratio",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"feedback_ratio") is None
                          and getv(k,"C1") not in (None,0) 
                          and getv(k,"C2") not in (None,0),
        "fn": lambda k: getv(k,"C1") / getv(k,"C2")
    },

    # --- Compute Barkhauhen Criterion ---
    {
        "needs": {"C1","C2"},
        "produces": "barkhausen_B",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"barkhausen_B") is None,                                      
        "fn": lambda k: getv(k,"C2") / (getv(k,"C1") + getv(k,"C2")),
    },
    {
        "needs": {"barkhausen_B","Av"},
        "produces": "barkhausen",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"barkhausen") is None,                                      
        "fn": lambda k: abs(getv(k,"Av")) * getv(k,"barkhausen_B"),
    },

    # --- Tank impedance Ztank ---
    {
        "needs": {"Q_tank","w0","L"},
        "produces": "Ztank",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"Ztank") is None
        and all(getv(k,x) not in (None,0) for x in ("Q_tank","w0","L")),
        "fn": lambda k: getv(k,"Q_tank") * getv(k,"w0") * getv(k,"L")
    },

    # --- Oscillator gain ---
    {
        "needs": {"gm","Ztank","is_forward_active"},
        "produces": "Av",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"Av") is None 
                          and all(getv(k,x) not in (None,0) for x in ("gm","Ztank"))
                          and bool(getv(k,"is_forward_active")),
        "fn": lambda k: (getv(k,"gm") * getv(k,"Ztank")) /(1 + getv(k,"gm") * getv(k,"Re"))
    },

    # --- Loop gain ---
    {
        "needs": {"Av","feedback_ratio"},
        "produces": "loop_gain",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"loop_gain") is None 
                          and all(getv(k,x) not in (None,0) for x in ("Av_osc","feedback_ratio")),
        "fn": lambda k: abs(getv(k,"Av_osc")) * getv(k,"feedback_ratio")
    },

    # --- Will oscillate? ---
    {
        "needs": {"loop_gain"},
        "produces": "will_oscillate",
        "tags": {"oscillator"},
        "cond": lambda k: getv(k,"will_oscillate") is None 
                          and getv(k,"loop_gain") not in (None,0),
        "fn": lambda k: getv(k,"loop_gain") >= 1.0
    },


]
# ============================================================
# REPORTING FUNCTIONS
# ============================================================
def fmt(v, unit=""):
    if v is None:
        return "—"
    abs_v = abs(v)
    if unit == "" or unit is None:
        return f"{v:.3f}"
    # Very small values
    if abs_v != 0 and abs_v < 1e-12:
        return f"{v:.3e} {unit}".strip()
    # Pico
    if abs_v < 1e-9:
        return f"{v*1e12:.2f} p{unit}"
    # Nano
    if abs_v < 1e-6:
        return f"{v*1e9:.2f} n{unit}"
    # Micro
    if abs_v < 1e-3:
        return f"{v*1e6:.2f} µ{unit}"
    # Milli
    if abs_v < 1:
        return f"{v*1e3:.3f} m{unit}"
    # Base unit
    return f"{v:.3f} {unit}"


def print_resistor_block(name, value, series):
    choices = nearest_resistors(value, series, n=3)
    print(f"{name:<8}: {fmt(value,'Ω')}  → ", end="")
    print(", ".join(f"{r:.0f}Ω ({(r-value)/value:+.1%})" for r in choices))

def print_report(k, series=E24_VALUES):
    def vv(key):
        return getv(k, key)

    print("\n================ CIRCUIT REPORT ================")

    # -------------------------
    # CONSTANTS
    # -------------------------
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
            val = vv(key)
            if isinstance(val, bool):
                print(f"{key:<12}: {val}")
            else:
                print(f"{key:<12}: {fmt(val, unit)}")

    # -------------------------
    # BASE
    # -------------------------
    print("\n[ BASE ]")
    for key, unit in (("Vb", "V"), ("Vb_loaded", "V"), ("Ib", "A"), ("Vbc", "V"), ("Vth", "V")):
        if key in k:
            print(f"{key:<12}: {fmt(vv(key), unit)}")
    for r in ("R1", "R2", "Req", "R1_th", "R2_th"):
        if r in k:
            print_resistor_block(r, vv(r), series)

    # -------------------------
    # EMITTER
    # -------------------------
    print("\n[ EMITTER ]")
    for key, unit in (("Ve", "V"), ("Ie", "A")):
        if key in k:
            print(f"{key:<12}: {fmt(vv(key), unit)}")
    if "Re" in k:
        print_resistor_block("Re", vv("Re"), series)
    if "Re_ac" in k:
        print(f"{'Re_ac':<12}: {fmt(vv('Re_ac'), 'Ω')}")

    # -------------------------
    # COLLECTOR
    # -------------------------
    print("\n[ COLLECTOR ]")
    for key, unit in (("Vc", "V"), ("Vce", "V"), ("Ic", "A")):
        if key in k:
            print(f"{key:<12}: {fmt(vv(key), unit)}")
    if "Rc" in k:
        print_resistor_block("Rc", vv("Rc"), series)
    if "RL" in k:
        print_resistor_block("RL", getv(k, "RL"), series)
    if "Rc_ac" in k:
        if k.get("load_enabled", False):
            print(f"{'Rc_ac':<12}: {fmt(getv(k, 'Rc_ac'), 'Ω')}  (= Rc, load disabled)")
        else:
            print(f"{'Rc_ac':<12}: {fmt(getv(k, 'Rc_ac'), 'Ω')}  (= Rc || RL)")

    # -------------------------
    # REGION
    # -------------------------
    print("\n[ REGION ]")
    for key in ("is_cutoff", "is_saturation", "is_reverse_active", "is_forward_active"):
        if key in k:
            print(f"{key:<20}: {vv(key)}")
    if "region" in k:
        print(f"{'region':<20}: {vv('region')}")

    # -------------------------
    # SMALL SIGNAL
    # -------------------------
    print("\n[ SMALL SIGNAL ]")
    if "is_forward_active" in k and not vv("is_forward_active"):
        print("Small-signal model not valid (not in forward-active region).")
    else:
        for key, unit in (
            ("gm", "S"),
            ("re", "Ω"),
            ("rpi", "Ω"),
            ("Av", "V/V"),
            ("Rin_total", "Ω"),
            ("Rout", "Ω"),
        ):
            if key in k:
                print(f"{key:<12}: {fmt(vv(key), unit)}")

    # -------------------------
    # BANDWIDTH
    # -------------------------
    print("\n[ BANDWIDTH ]")
    lf_keys = (
        ("fL_in", "Hz"),
        ("fL_out", "Hz"),
        ("fL", "Hz"),
        ("C_in_couple", "F"),
        ("C_out_couple", "F"),
        ("C_e_bypass", "F"),
    )
    hf_keys = (
        ("fH_in", "Hz"),
        ("fH_out", "Hz"),
        ("fH", "Hz"),
        ("C_pi", "F"),
        ("C_mu", "F"),
        ("C_miller", "F"),
        ("C_in_hf", "F"),
        ("C_out_hf", "F"),
    )

    any_bw = False
    # Low-frequency
    lf_printed = False
    for key, unit in lf_keys:
        if key in k:
            if not lf_printed:
                print("\n  Low-Frequency Poles:")
                lf_printed = True
            any_bw = True
            print(f"  {key:<14}: {fmt(vv(key), unit)}")
    # High-frequency
    hf_printed = False
    for key, unit in hf_keys:
        if key in k:
            if not hf_printed:
                print("\n  High-Frequency Poles:")
                hf_printed = True
            any_bw = True
            print(f"  {key:<14}: {fmt(vv(key), unit)}")
    if not any_bw:
        print("No capacitances provided.")
        print("Provide C_in_couple / C_out_couple for low-frequency poles.")
        print("Provide C_pi / C_mu for high-frequency poles.")

    # -------------------------
    # OUTPUT SWING
    # -------------------------
    print("\n[ OUTPUT SWING ]")
    for key, unit in (
        ("Vout_pk", "V"),
        ("Vc_min", "V"),
        ("Vc", "V"),
        ("Vc_max", "V"),
        ("headroom_down", "V"),
        ("headroom_up", "V"),
    ):
        if key in k:
            print(f"{key:<14}: {fmt(vv(key), unit)}")
    if "ac_in" in k:
        print(f"{'ac_in':<14}: {fmt(vv('ac_in'), 'V')}")
        for clip in ("clips_low", "clips_high", "clips"):
            if clip in k:
                print(f"{clip:<14}: {vv(clip)}")
                if clip == "clips" and vv(clip):
                    print("⚠️  Output will clip.")

    # -------------------------
    # OSCILLATOR
    # -------------------------
    print("\n[ OSCILLATOR ]")
    osc_keys = (
        ("C1", "F"),
        ("C2", "F"),
        ("Ceq", "F"),
        ("L", "H"),
        ("freq", "Hz"),
        ("w0", "rad/s"),
        ("feedback_ratio", ""),
        ("Q_tank", ""),
        ("Ztank", "Ω"),
        ("gm_osc", "S"),
        ("Av", "V/V"),
        ("loop_gain", ""),
        ("will_oscillate", ""),
        ("barkhausen_B", ""),
        ("barkhausen", ""),
    )
    any_osc = False
    for key, unit in osc_keys:
        if key in k:
            any_osc = True
            val = vv(key)
            if isinstance(val, bool):
                print(f"{key:<14}: {val}")
            else:
                print(f"{key:<14}: {fmt(val, unit)}")
    if not any_osc:
        print("No oscillator parameters computed or provided.")

# ============================================================
# DESIGN FUNCTIONS
# ============================================================
def design_optimize_vc(known, target="center"):
    print("\n[ VC OPTIMIZATION SUGGESTIONS ]")

    Vdd = getv(known, "Vdd")
    Ve = getv(known, "Ve")
    Vce_min = getv(known, "Vce_min")
    Vc = getv(known, "Vc")
    Ic = getv(known, "Ic")
    Rc = getv(known, "Rc")
    Re = getv(known, "Re")
    Ie = getv(known, "Ie")

    if None in (Vdd, Ve, Vce_min, Vc, Ic, Rc, Re, Ie):
        print("Cannot optimize: missing required parameters")
        return {
            "Vc_now": Vc,
            "Vc_target": None,
            "delta": None,
            "suggestions": ["Cannot optimize: missing required parameters"]
        }

    # --- Compute target collector voltage ---
    if target == "center":
        Vc_target = 0.5 * (Vdd + Ve + Vce_min)
    elif target == "max_swing":
        Vc_target = Vdd - 1.0
    elif target == "low_distortion":
        Vc_target = Ve + Vce_min + 1.5
    else:
        raise ValueError("Unknown target")

    delta_vc = Vc_target - Vc
    suggestions = []

    # --- Reporting header ---
    print(f"Current Vc : {fmt(Vc, 'V')}")
    print(f"Target Vc  : {fmt(Vc_target, 'V')}")
    print(f"Delta      : {fmt(delta_vc, 'V')}")

    if abs(delta_vc) < 0.01:
        print("Collector already near target — no adjustment needed.")
        return {
            "Vc_now": Vc,
            "Vc_target": Vc_target,
            "delta": delta_vc,
            "suggestions": []
        }

    print("Suggested adjustments:")

    # Option 1: Adjust Rc
    if Ic != 0:
        Rc_new = Rc + delta_vc / Ic
        if Rc_new > 0.0:
            msg = (
                f"Adjust Rc from {Rc:.1f}Ω → {Rc_new:.1f}Ω "
                f"to move Vc by {delta_vc:.3f}V"
            )
            suggestions.append(msg)
            print(f"  - {msg}")

    # Option 2: Adjust Ic (via bias)
    if Rc != 0:
        Ic_new = Ic + delta_vc / Rc
        if Ic_new > 0.0:
            msg = (
                f"Adjust Ic from {Ic:.3e}A → {Ic_new:.3e}A "
                f"(via base bias) to move Vc by {delta_vc:.3f}V"
            )
            suggestions.append(msg)
            print(f"  - {msg}")

    # Option 3: Adjust Re (affects Ve)
    if Ie != 0:
        Re_new = Re + delta_vc / Ie
        if Re_new > 0.0:
            msg = (
                f"Adjust Re from {Re:.1f}Ω → {Re_new:.1f}Ω "
                f"to shift Vc by changing Ve"
            )
            suggestions.append(msg)
            print(f"  - {msg}")

    # Option 4: Bias divider
    if "R1" in known and "R2" in known:
        msg = "Adjust bias divider R1/R2 to shift Ib and hence Ic (affects Vc)"
        suggestions.append(msg)
        print(f"  - {msg}")

    return {
        "Vc_now": Vc,
        "Vc_target": Vc_target,
        "delta": delta_vc,
        "suggestions": suggestions
    }


def design_second_stage_buffer(known, Vin_key="Vout_pk", Re_override=None, Ie_target=1e-3):
    """
    2nd-stage emitter-follower buffer with correct headroom based on waveform around DC point.
    Vin_key: AC input amplitude from previous stage (Vout_pk)
    Re_override: optional emitter resistor
    """
    print('\n[ 2ND STAGE BUFFER ]')

    Vdd = getv(known, "Vdd")
    Vbe = getv(known, "Vbe")
    beta = getv(known, "beta")
    Vc = getv(known, "Vc")  # use Vc from previous stage as supply for buffer

    Rout1 = getv(known, "Rout") if "Rout" in known else 0.0
    Vin_pk = getv(known, Vin_key) if Vin_key in known else 0.0

    # DC emitter voltage fixed at Vc - Vbe
    Ve_dc = Vc - Vbe

    # DC collector tied to Vdd
    Vc_buf = Vdd

    # Emitter resistor
    Re = 1e3 if Re_override is None else Re_override

    # DC emitter current
    Ie = Ve_dc / Re

    # Small-signal AC input/output resistances
    Rin_buf = (beta + 1) * Re
    Rout_buf = Re / (beta + 1)

    # AC voltage after first-stage voltage division
    
    # Absolute waveform limits
    Ve_max = Ve_dc + abs(Vin_pk)
    Ve_min = Ve_dc - abs(Vin_pk)
    V_swing = Ve_max - Ve_min

    # Correct headroom based on waveform around DC
    headroom_up = Vdd - Ve_max
    headroom_down = Ve_min - Vbe

    # Collector-emitter voltage
    Vce = Vc - Ve_dc

    # Store for reporting
    setv(known, "Ve_buf", Ve_dc)
    setv(known, "Ie_buf", Ie)
    setv(known, "Re_buf", Re)
    setv(known, "Rin_buf", Rin_buf)
    setv(known, "Rout_buf", Rout_buf)
    setv(known, "Vout_buf", Vin_pk)
    setv(known, "Vout_max_buf", Ve_max)
    setv(known, "Vout_min_buf", Ve_min)
    setv(known, "Vout_swing_buf", V_swing)
    setv(known, "Vce_buf", Vce)
    setv(known, "headroom_up_buf", headroom_up)
    setv(known, "headroom_down_buf", headroom_down)
    setv(known, "is_forward_active_buf", True)

    return {
        "Ve": Ve_dc,
        "Ie": Ie,
        "Re": Re,
        "Vc_buf": Vc_buf,
        "Vce": Vce,
        "Rin": Rin_buf,
        "Rout": Rout_buf,
        "Vout_ac": Vin_pk,
        "Ve_DC": Ve_dc,
        "Vout_max": Ve_max,
        "Vout_min": Ve_min,
        "Vout_swing": V_swing,
        "headroom_up": headroom_up,
        "headroom_down": headroom_down,
    }


def design_input_buffer(
    Vdd,
    Vin_pk,
    beta=100,
    Vbe=0.7,
    Ie_target=1e-3,
    divider_stiffness=10
):
    """
    Standalone emitter-follower input buffer design.

    Inputs:
        Vdd             : Supply voltage
        Vin_pk          : Peak input signal amplitude
        beta            : Transistor beta
        Vbe             : Base-emitter voltage
        Ie_target       : Desired emitter DC current
        divider_stiffness : Divider current multiplier over base current

    Returns dictionary of DC bias, AC swing, and bias resistor values.
    """

    print("\n[ INPUT BUFFER DESIGN ]")

    # --- DC Bias Target ---
    Ve_dc = Vdd / 2                 # mid-supply for symmetric swing
    Vb_dc = Ve_dc + Vbe

    # --- Emitter resistor ---
    Re = Ve_dc / Ie_target
    Ie = Ie_target
    Ib = Ie / beta

    # --- Bias Divider Design ---
    I_div = divider_stiffness * Ib
    R2 = Vb_dc / I_div
    R1 = (Vdd - Vb_dc) / I_div

    # --- Small Signal Parameters ---
    Rin_base = (beta + 1) * Re
    Rout_emitter = Re / (beta + 1)

    # --- AC Swing ---
    Ve_max = Ve_dc + Vin_pk
    Ve_min = Ve_dc - Vin_pk
    Vout_swing = Ve_max - Ve_min

    # --- Headroom Checks ---
    headroom_up = Vdd - Ve_max
    headroom_down = Ve_min - Vbe

    is_forward_active = headroom_up > 0 and headroom_down > 0

    return {
        "Ve_DC": Ve_dc,
        "Vb_DC": Vb_dc,
        "Ie": Ie,
        "Re": Re,
        "R1": R1,
        "R2": R2,
        "Rin": Rin_base,
        "Rout": Rout_emitter,
        "Vout_pk": Vin_pk,
        "Vout_max": Ve_max,
        "Vout_min": Ve_min,
        "Vout_swing": Vout_swing,
        "headroom_up": headroom_up,
        "headroom_down": headroom_down,
        "is_forward_active": is_forward_active,
    }
