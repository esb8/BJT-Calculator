from BJTCalculator import (
    normalize_user_known,
    solve,
    RULES,
    print_report,
    design_second_stage_buffer,
    design_input_buffer,
    check_overconstrained,
    DEPENDENCY_LOOPS,
    getv,
    fmt
)
## ----AMPLIFIER STAGE ----
raw_known = {
    "Vdd": 12,
    "Vbe": 0.6,
    "Vc" : 6.0,
    "Vce_min": 0.2,
    "Ic": 1e-3,
    "beta": 100,
    "divider_ratio": 10.0,
    "ac_in": 10e-6,
    "Re": 1000, # emitter resistor (Ω)
    # small-signal constants
    "Vt": 0.02585,
    "I_cutoff": 1e-6,
    
    # ---- BYPASS OPTION ----
    "Re_bypassed": True,

    # ---- LOAD OPTION ---- 
    "load_enabled": False,  # set True to use RL
    "RL": 10000,           # only used if load_enabled True

    # ---- LOW-FREQUENCY CAPS ----
    "C_in_couple": 100e-6,     # input coupling capacitor (F)
    "C_out_couple": 0.0,       # output coupling capacitor (F)
    "C_e_bypass": 0.0,         # emitter bypass capacitor (F)

    # ---- HIGH-FREQUENCY CAPS ----
    "C_pi": 0.0,               # base-emitter diffusion cap (F)
    "C_mu": 0.0,               # base-collector cap (F)
    "C_stray_in": 0.0,         # wiring / probe cap at input (F)
    "C_stray_out": 0.0,        # wiring / load cap at output (F)
}

known = normalize_user_known(raw_known)

print("Initial known (user-fixed):")
print({k: getv(known, k) for k in known})

check_overconstrained(known, DEPENDENCY_LOOPS)

known = solve(known, RULES)

print("\nFinal known:")
print({k: getv(known, k) for k in known})

print_report(known)


# ---- OUTPUT BUFFER ----- 
buffer2 = design_second_stage_buffer(
    known, 
    Vin_key="Vout_pk"
    )

for k, v in buffer2.items():
    if k in ("headroom_up", "headroom_down", "Vout", "Vce", "Ve", "Vc", "Vout_max", "Vout_min", "Vout_swing", "Vc_buf", "Ve_DC", "Vout_ac"):
        unit = "V"
    elif k.startswith("I"):
        unit = "A"
    else:
        unit = "Ω"
    print(f"{k:<12}: {fmt(v, unit)}")

# ---- INPUT BUFFER -----
buffer1 = design_input_buffer(
    Vdd=12,
    Vin_pk=10e-6,   # 10 µV RF signal
    beta=150,
    Ie_target=2e-3
    )

for k, v in buffer1.items():
    if k in ("headroom_up", "headroom_down", "Vout", "Vce", "Ve", "Vc", "Vout_max", "Vout_min", "Vout_swing", "Vc_buf", "Ve_DC", "Vout_ac"):
        unit = "V"
    elif k.startswith("I"):
        unit = "A"
    else:
        unit = "Ω"
    print(f"{k:<12}: {fmt(v, unit)}")