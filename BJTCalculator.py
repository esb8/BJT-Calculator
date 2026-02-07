import math

# ============================================================
# PRIORITY SYSTEM (CONFLICT RESOLUTION)
# ============================================================
# Higher number = stronger (wins conflicts)

PRIORITY = {
    "derived": 0,   # plain consequences of equations
    "design":  1,   # "nice-to-have" intent (symmetric bias, headroom targets, etc.)
    "user":    2,   # user-fixed inputs (hard targets)
    "physics": 3,   # non-negotiable constraints (rarely used to overwrite; mostly checks)
}


def _is_meta_value(x):
    return isinstance(x, dict) and "value" in x and "priority" in x


def getv(known, key):
    """
    Read value regardless of whether known stores raw values or meta values.
    """
    v = known[key]
    return v["value"] if _is_meta_value(v) else v


def has(known, key):
    return key in known


def keys(known):
    return set(known.keys())


def setv(known, key, value, priority=PRIORITY["derived"], source="rule"):
    """
    Priority-aware write:
      - If key doesn't exist: set it
      - If key exists:
          - overwrite only if new priority > old priority
          - if equal priority and different value: raise error (hard conflict)
          - else keep existing (yield)
    Returns True if updated, False otherwise.
    """
    if key not in known:
        known[key] = {"value": value, "priority": priority, "source": source}
        return True

    # existing
    if _is_meta_value(known[key]):
        old_val = known[key]["value"]
        old_pri = known[key]["priority"]
    else:
        # legacy raw values treated as "user" by default, because they were fixed inputs
        old_val = known[key]
        old_pri = PRIORITY["user"]
        known[key] = {"value": old_val, "priority": old_pri, "source": "legacy/user"}

    # compare
    if priority > old_pri:
        known[key] = {"value": value, "priority": priority, "source": source}
        return True

    if priority == old_pri:
        # same "authority": if they disagree meaningfully, it is a real conflict
        # allow tiny float differences
        if isinstance(old_val, (int, float)) and isinstance(value, (int, float)):
            if abs(old_val - value) <= 1e-12 * max(1.0, abs(old_val), abs(value)):
                return False
        if old_val != value:
            raise ValueError(
                f"Priority conflict on '{key}': "
                f"{old_val} (source={known[key].get('source')}) vs {value} (source={source})"
            )
        return False

    # lower priority yields
    return False


def set_defaults(known, defaults):
    """
    Insert defaults as *derived* (lowest) so they always yield to user/design.
    """
    for k, v in defaults.items():
        if k not in known:
            setv(known, k, v, priority=PRIORITY["derived"], source="default")


def normalize_user_known(raw_known):
    """
    Convert raw user dict to meta dict with priority=user.
    """
    known = {}
    for k, v in raw_known.items():
        setv(known, k, v, priority=PRIORITY["user"], source="user")
    return known


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
                pri = known[var]["priority"] if _is_meta_value(known[var]) else PRIORITY["user"]
                if pri >= PRIORITY["user"]:
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

def solve(known, rules):
    """
    Priority-aware fixed-point solver.

    Rule schema:
      - needs: set[str]
      - produces: str
      - fn: lambda k: ...
      - cond: optional lambda k: bool
      - priority: optional, defaults to derived
      - source: optional human label
    """
    updated = True
    while updated:
        updated = False
        for rule in rules:
            out = rule["produces"]

            # can we run?
            if not rule["needs"].issubset(keys(known)):
                continue
            if "cond" in rule and not rule["cond"](known):
                continue

            # compute candidate
            value = rule["fn"](known)

            # write with priority (may yield)
            pri = rule.get("priority", PRIORITY["derived"])
            src = rule.get("source", rule.get("tag", "rule"))
            did = setv(known, out, value, priority=pri, source=src)
            if did:
                updated = True

    return known


# ============================================================
# EQUATION GENERATORS (GENERIC, REUSABLE)
# ============================================================

def linear(a, b, out, priority=PRIORITY["derived"], source="linear"):
    """
    out = a * b
    a   = out / b
    b   = out / a
    """
    return [
        {"needs": {a, b}, "produces": out, "priority": priority, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) * getv(k, b)},
        {"needs": {out, b}, "produces": a, "priority": priority, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) / getv(k, b)},
        {"needs": {out, a}, "produces": b, "priority": priority, "source": source,
         "fn": lambda k, out=out, a=a: getv(k, out) / getv(k, a)},
    ]


def diff(a, b, out, priority=PRIORITY["derived"], source="diff"):
    """
    out = a - b
    a   = out + b
    b   = a - out
    """
    return [
        {"needs": {a, b}, "produces": out, "priority": priority, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) - getv(k, b)},
        {"needs": {out, b}, "produces": a, "priority": priority, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) + getv(k, b)},
        {"needs": {a, out}, "produces": b, "priority": priority, "source": source,
         "fn": lambda k, a=a, out=out: getv(k, a) - getv(k, out)},
    ]


def sum3(a, b, out, priority=PRIORITY["derived"], source="sum"):
    """
    out = a + b
    a   = out - b
    b   = out - a
    """
    return [
        {"needs": {a, b}, "produces": out, "priority": priority, "source": source,
         "fn": lambda k, a=a, b=b: getv(k, a) + getv(k, b)},
        {"needs": {out, b}, "produces": a, "priority": priority, "source": source,
         "fn": lambda k, out=out, b=b: getv(k, out) - getv(k, b)},
        {"needs": {out, a}, "produces": b, "priority": priority, "source": source,
         "fn": lambda k, out=out, a=a: getv(k, out) - getv(k, a)},
    ]


# ============================================================
# RULE SET (DECLARATIVE)
# ============================================================

RULES = []

# ------------------------------------------------------------
# OUTPUT SWING / CLIPPING CHECKS (derived)
# ------------------------------------------------------------

RULES += [
    {"needs": {"Ve", "Vce_min"}, "produces": "Vc_min", "priority": PRIORITY["derived"], "source": "swing",
     "fn": lambda k: getv(k, "Ve") + getv(k, "Vce_min")},

    {"needs": {"Vdd"}, "produces": "Vc_max", "priority": PRIORITY["derived"], "source": "swing",
     "fn": lambda k: getv(k, "Vdd")},

    {"needs": {"Vc", "Vc_min"}, "produces": "headroom_down", "priority": PRIORITY["derived"], "source": "swing",
     "fn": lambda k: getv(k, "Vc") - getv(k, "Vc_min")},

    {"needs": {"Vc_max", "Vc"}, "produces": "headroom_up", "priority": PRIORITY["derived"], "source": "swing",
     "fn": lambda k: getv(k, "Vc_max") - getv(k, "Vc")},
]

RULES += [
    {"needs": {"Vout_pk", "headroom_down"}, "produces": "clips_low", "priority": PRIORITY["derived"], "source": "clip",
     "fn": lambda k: getv(k, "Vout_pk") > getv(k, "headroom_down")},

    {"needs": {"Vout_pk", "headroom_up"}, "produces": "clips_high", "priority": PRIORITY["derived"], "source": "clip",
     "fn": lambda k: getv(k, "Vout_pk") > getv(k, "headroom_up")},

    {"needs": {"clips_low", "clips_high"}, "produces": "clips", "priority": PRIORITY["derived"], "source": "clip",
     "fn": lambda k: bool(getv(k, "clips_low")) or bool(getv(k, "clips_high"))},
]

# ----------------------------
# (1) BJT β/α relationships (derived)
# ----------------------------

RULES += [
    {"needs": {"beta"}, "produces": "alpha", "priority": PRIORITY["derived"], "source": "bjt",
     "fn": lambda k: getv(k, "beta") / (getv(k, "beta") + 1.0)},
    {"needs": {"Ic", "beta"}, "produces": "Ib", "priority": PRIORITY["derived"], "source": "bjt",
     "fn": lambda k: getv(k, "Ic") / getv(k, "beta")},
]

# ------------------------------------------------------------
# 2) SUM RELATIONSHIPS (derived)
# ------------------------------------------------------------

RULES += sum3("Ic", "Ib", "Ie", priority=PRIORITY["derived"], source="kcl")
RULES += sum3("Ve", "Vbe", "Vb", priority=PRIORITY["derived"], source="vbe")

# ------------------------------------------------------------
# 3) LINEAR RELATIONSHIPS (derived)
# ------------------------------------------------------------

RULES += linear("Ie", "Re", "Ve", priority=PRIORITY["derived"], source="emitter")
RULES += linear("Ic", "Rc", "Vrc", priority=PRIORITY["derived"], source="collector_drop")

# ------------------------------------------------------------
# 4) DIFFERENCE RELATIONSHIPS (derived)
# ------------------------------------------------------------

RULES += diff("Vdd", "Vrc", "Vc", priority=PRIORITY["derived"], source="collector_node")
RULES += diff("Vc", "Ve", "Vce", priority=PRIORITY["derived"], source="vce")

# ------------------------------------------------------------
# 5) DESIGN / INTENT RULES (design priority; yield to user)
# ------------------------------------------------------------

RULES += [
    {"needs": {"Ib", "divider_ratio"}, "produces": "Idiv", "priority": PRIORITY["design"], "source": "divider_design",
     "fn": lambda k: getv(k, "divider_ratio") * getv(k, "Ib")},

    {"needs": {"Vb", "Idiv"}, "produces": "R2", "priority": PRIORITY["design"], "source": "divider_design",
     "fn": lambda k: getv(k, "Vb") / getv(k, "Idiv")},

    {"needs": {"Vdd", "Vb", "Idiv", "Ib"}, "produces": "R1", "priority": PRIORITY["design"], "source": "divider_design",
     "fn": lambda k: (getv(k, "Vdd") - getv(k, "Vb")) / (getv(k, "Idiv") + getv(k, "Ib"))},

    {"needs": {"R1", "R2"}, "produces": "Req", "priority": PRIORITY["derived"], "source": "divider",
     "fn": lambda k: (getv(k, "R1") * getv(k, "R2")) / (getv(k, "R1") + getv(k, "R2"))},

    {"needs": {"Vdd", "R1", "R2"}, "produces": "Vth", "priority": PRIORITY["derived"], "source": "divider",
     "fn": lambda k: getv(k, "Vdd") * getv(k, "R2") / (getv(k, "R1") + getv(k, "R2"))},

    {"needs": {"Vth", "Ib", "Req"}, "produces": "Vb_loaded", "priority": PRIORITY["derived"], "source": "divider",
     "fn": lambda k: getv(k, "Vth") - getv(k, "Ib") * getv(k, "Req")},
]

# ------------------------------------------------------------
# HEADROOM-DRIVEN BIAS (INVERSE DESIGN) (design; yields to user)
# ------------------------------------------------------------

RULES += [
    {
        "needs": {"Ve", "Vce_min", "headroom_down_target", "set_headroom_down"},
        "produces": "Vc",
        "cond": lambda k: bool(getv(k, "set_headroom_down")),
        "priority": PRIORITY["design"],
        "source": "bias_headroom_down",
        "fn": lambda k: (getv(k, "Ve") + getv(k, "Vce_min")) + getv(k, "headroom_down_target"),
    },
    {
        "needs": {"Vdd", "headroom_up_target", "set_headroom_up"},
        "produces": "Vc",
        "cond": lambda k: bool(getv(k, "set_headroom_up")),
        "priority": PRIORITY["design"],
        "source": "bias_headroom_up",
        "fn": lambda k: getv(k, "Vdd") - getv(k, "headroom_up_target"),
    },
    {
        "needs": {"Vdd", "Ve", "Vce_min", "set_headroom_symmetric"},
        "produces": "Vc",
        "cond": lambda k: bool(getv(k, "set_headroom_symmetric")),
        "priority": PRIORITY["design"],
        "source": "bias_headroom_sym",
        "fn": lambda k: 0.5 * (getv(k, "Vdd") + (getv(k, "Ve") + getv(k, "Vce_min"))),
    },
]

# ------------------------------------------------------------
# 5b) OUTPUT LOADING (AC) (derived)
# ------------------------------------------------------------

RULES += [
    {"needs": {"Rc", "RL", "load_enabled"}, "produces": "Rc_ac",
     "cond": lambda k: bool(getv(k, "load_enabled")),
     "priority": PRIORITY["derived"], "source": "ac_load",
     "fn": lambda k: (getv(k, "Rc") * getv(k, "RL")) / (getv(k, "Rc") + getv(k, "RL"))},

    {"needs": {"Rc", "load_enabled"}, "produces": "Rc_ac",
     "cond": lambda k: not bool(getv(k, "load_enabled")),
     "priority": PRIORITY["derived"], "source": "ac_load",
     "fn": lambda k: getv(k, "Rc")},
]

# ------------------------------------------------------------
# 6) REGION-OF-OPERATION CHECKS (derived)
# ------------------------------------------------------------

RULES += [
    {"needs": {"Vb", "Vc"}, "produces": "Vbc",
     "fn": lambda k: getv(k, "Vb") - getv(k, "Vc")},

    {"needs": {"Vbe", "Ic", "I_cutoff"}, "produces": "is_cutoff",
     "fn": lambda k: (getv(k, "Vbe") < 0.6) or (getv(k, "Ic") <= getv(k, "I_cutoff"))},

    {"needs": {"Vce", "Vce_min"}, "produces": "is_saturation",
     "fn": lambda k: getv(k, "Vce") <= getv(k, "Vce_min")},

    {"needs": {"Vbc"}, "produces": "is_reverse_active",
     "fn": lambda k: getv(k, "Vbc") > 0},

    {"needs": {"is_cutoff", "is_saturation", "is_reverse_active"},
     "produces": "is_forward_active",
     "fn": lambda k: not (
         bool(getv(k, "is_cutoff")) or
         bool(getv(k, "is_saturation")) or
         bool(getv(k, "is_reverse_active"))
     )},

    {"needs": {"is_cutoff", "is_saturation", "is_reverse_active", "is_forward_active"},
     "produces": "region",
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
    {"needs": {"Re", "Re_bypassed"}, "produces": "Re_ac", "priority": PRIORITY["derived"], "source": "small_signal",
     "fn": lambda k: 0.0 if bool(getv(k, "Re_bypassed")) else getv(k, "Re")},

    {"needs": {"Ic", "Vt", "is_forward_active"}, "produces": "gm",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "small_signal",
     "fn": lambda k: getv(k, "Ic") / getv(k, "Vt")},

    {"needs": {"gm", "is_forward_active"}, "produces": "re",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "small_signal",
     "fn": lambda k: 1.0 / getv(k, "gm")},

    {"needs": {"beta", "gm", "is_forward_active"}, "produces": "rpi",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "small_signal",
     "fn": lambda k: getv(k, "beta") / getv(k, "gm")},

    {"needs": {"Rc_ac", "re", "Re_ac", "is_forward_active"}, "produces": "Av",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "small_signal",
     "fn": lambda k: -getv(k, "Rc_ac") / (getv(k, "re") + getv(k, "Re_ac"))},
]

# ------------------------------------------------------------
# 8) BANDWIDTH / POLES (OPTIONAL) (derived)
# ------------------------------------------------------------

RULES += [
    {"needs": {"Rc_ac", "C_out_total", "is_forward_active"}, "produces": "wH_out",
     "cond": lambda k: bool(getv(k, "is_forward_active")) and (getv(k, "C_out_total") > 0),
     "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: 1.0 / (getv(k, "Rc_ac") * getv(k, "C_out_total"))},

    {"needs": {"wH_out"}, "produces": "fH_out", "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: getv(k, "wH_out") / (2.0 * math.pi)},

    {"needs": {"rpi", "beta", "Re_ac", "is_forward_active"}, "produces": "Rin_base",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: getv(k, "rpi") + (getv(k, "beta") + 1.0) * getv(k, "Re_ac")},

    {"needs": {"Rin_base", "Req", "is_forward_active"}, "produces": "Rin_total",
     "cond": lambda k: bool(getv(k, "is_forward_active")),
     "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: (getv(k, "Rin_base") * getv(k, "Req")) / (getv(k, "Rin_base") + getv(k, "Req"))},

    {"needs": {"Rin_total", "C_in_total", "is_forward_active"}, "produces": "wH_in",
     "cond": lambda k: bool(getv(k, "is_forward_active")) and (getv(k, "C_in_total") > 0),
     "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: 1.0 / (getv(k, "Rin_total") * getv(k, "C_in_total"))},

    {"needs": {"wH_in"}, "produces": "fH_in", "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: getv(k, "wH_in") / (2.0 * math.pi)},

    {"needs": {"fH_in", "fH_out"}, "produces": "fH", "priority": PRIORITY["derived"], "source": "bandwidth",
     "fn": lambda k: min(getv(k, "fH_in"), getv(k, "fH_out"))},
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
    # allow passing meta-known
    def vv(key):
        return getv(k, key)

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
            val = vv(key)
            if isinstance(val, bool):
                print(f"{key:<12}: {val}")
            else:
                print(f"{key:<12}: {fmt(val, unit)}")

    print("\n[ BASE ]")
    for key, unit in (("Vb", "V"), ("Vb_loaded", "V"), ("Ib", "A"), ("Vbc", "V")):
        if key in k:
            print(f"{key:<12}: {fmt(vv(key), unit)}")
    for r in ("R1", "R2", "Req"):
        if r in k:
            print_resistor_block(r, vv(r), series)

    print("\n[ EMITTER ]")
    for key, unit in (("Ve", "V"), ("Ie", "A")):
        if key in k:
            print(f"{key:<12}: {fmt(vv(key), unit)}")
    if "Re" in k:
        print_resistor_block("Re", vv("Re"), series)
    if "Re_ac" in k:
        print(f"{'Re_ac':<12}: {fmt(vv('Re_ac'), 'Ω')}")

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
            print(
                f"{'Rc_ac':<12}: {fmt(getv(k, 'Rc_ac'), 'Ω')}  "
                f"(= Rc, load disabled)"
            )
        else:
            print(
                f"{'Rc_ac':<12}: {fmt(getv(k, 'Rc_ac'), 'Ω')}  "
                f"(= Rc || RL)"
            )


    print("\n[ REGION ]")

    for key in (
        "is_cutoff",
        "is_saturation",
        "is_reverse_active",
        "is_forward_active",
    ):
        if key in k:
            print(f"{key:<20}: {vv(key)}")

    if "region" in k:
        print(f"{'region':<20}: {vv('region')}")


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
        ):
            if key in k:
                print(f"{key:<12}: {fmt(vv(key), unit)}")

    print("\n[ BANDWIDTH ]")
    if "is_forward_active" in k and not vv("is_forward_active"):
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
                print(f"{key:<12}: {fmt(vv(key), unit)}")
        if not any_bw:
            print("No capacitances provided; set C_in_total and/or C_out_total to estimate bandwidth.")

    print("\n[ OUTPUT SWING ]")
    for key, unit in (
        ("Vc_min", "V"),
        ("Vc", "V"),
        ("Vc_max", "V"),
        ("headroom_down", "V"),
        ("headroom_up", "V"),
    ):
        if key in k:
            print(f"{key:<14}: {fmt(vv(key), unit)}")

    if "Vout_pk" in k:
        print(f"\n{'Vout_pk':<14}: {fmt(vv('Vout_pk'), 'V')}")
        if "clips_low" in k:
            print(f"{'clips_low':<14}: {vv('clips_low')}")
        if "clips_high" in k:
            print(f"{'clips_high':<14}: {vv('clips_high')}")
        if "clips" in k:
            print(f"{'clips':<14}: {vv('clips')}")
            if vv("clips"):
                print("⚠️  Output will clip.")


# ============================================================
# RUN
# ============================================================

raw_known = {
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
    "RL": 10000,           # only used if load_enabled True

    # bandwidth lumped caps (set to 0 or omit to skip)
    "C_in_total": 0.0,      # F (e.g. 30e-12 for 30 pF)
    "C_out_total": 0.0,     # F (e.g. 10e-12 for 10 pF)

    # headroom-driven examples (uncomment to use)
    # "set_headroom_down": True,
    # "headroom_down_target": 2.0,
    # "set_headroom_up": True,
    # "headroom_up_target": 2.0,
    # "set_headroom_symmetric": True,
}

known = normalize_user_known(raw_known)

print("Initial known (user-fixed):")
print({k: getv(known, k) for k in known})

check_overconstrained(known, DEPENDENCY_LOOPS)

known = solve(known, RULES)

print("\nFinal known:")
print({k: getv(known, k) for k in known})

print_report(known)
