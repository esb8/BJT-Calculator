
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
    Fixed-point solver:
    repeatedly applies any rule whose inputs exist
    and whose output is not yet known.
    """
    updated = True
    while updated:
        updated = False
        for rule in rules:
            out = rule["produces"]
            if out not in known and rule["needs"].issubset(known):
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
# 6) CHECKS
# ------------------------------------------------------------

RULES += [
    {"needs": {"Vce", "Vce_min"}, "produces": "sat_ok",
     "fn": lambda k: k["Vce"] > k["Vce_min"]},
]


# ============================================================
# REPORTING (VIEW LAYER)
# ============================================================

def fmt(v, unit=""):
    if v is None:
        return "—"
    if abs(v) < 1e-3 and v != 0:
        return f"{v*1e6:.2f} µ{unit}"
    if abs(v) < 1:
        return f"{v*1e3:.3f} m{unit}"
    return f"{v:.3f} {unit}"

def print_resistor_block(name, value, series):
    choices = nearest_resistors(value, series, n=3)
    print(f"{name:<8}: {fmt(value,'Ω')}  → ", end="")
    print(", ".join(f"{r:.0f}Ω ({(r-value)/value:+.1%})" for r in choices))

def section(title, rows, k):
    print(f"\n=== {title.upper()} ===")
    for label, key, unit in rows:
        if key in k:
            print(f"{label:<16}: {fmt(k[key], unit)}")


def print_report(k, series=E24_VALUES):
    print("\n================ CIRCUIT REPORT ================")

    print("\n[ CONSTANTS ]")
    for key in ("Vdd", "Vbe", "beta", "alpha", "divider_ratio"):
        if key in k:
            print(f"{key:<12}: {fmt(k[key])}")

    print("\n[ BASE ]")
    for key in ("Vb", "Vb_loaded", "Ib"):
        if key in k:
            print(f"{key:<12}: {fmt(k[key],'V' if 'V' in key else 'A')}")
    for r in ("R1", "R2", "Req"):
        if r in k:
            print_resistor_block(r, k[r], series)

    print("\n[ EMITTER ]")
    for key in ("Ve", "Ie"):
        if key in k:
            print(f"{key:<12}: {fmt(k[key],'V' if 'V' in key else 'A')}")
    if "Re" in k:
        print_resistor_block("Re", k["Re"], series)

    print("\n[ COLLECTOR ]")
    for key in ("Vc", "Vce", "Ic"):
        if key in k:
            print(f"{key:<12}: {fmt(k[key],'V' if 'V' in key else 'A')}")
    if "Rc" in k:
        print_resistor_block("Rc", k["Rc"], series)

    print("\n[ CHECKS ]")
    if "sat_ok" in k:
        print(f"Saturation OK : {k['sat_ok']}")

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
}

print("Initial known:")
print(known)

known = solve(known, RULES)

print("\nFinal known:")
print(known)

print_report(known)

