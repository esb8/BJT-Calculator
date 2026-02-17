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
    # These are the available user inputs. Set to None if unknown, or set to a value to fix it.
    "Vdd": 12,
    "Vbe": 0.6,
    "Vce_min": 0.2,
    "Vc": 6.0,
    "beta": 100,
    "divider_ratio": 10.0,
    "ac_in": 10e-6, # 10 µV RF input signal, rides on top of DC bias
    "Re": 1000, # emitter resistor (Ω)
    "Rc": 9000, # collector resistor (Ω)

    # ---- SMALL-SIGNAL PARAMETERS ----
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

AMP_TAGS = {"dc", "ac", "amp"}
known = solve(known, RULES, tags=AMP_TAGS)

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


# Colpitts Oscillator design

## ----OSCILLATOR STAGE (COLPITTS) ----

raw_known_osc = {
    # Power supply
    "Vdd": 12,
    "beta": 100,
    
    # Transistor bias (similar to amplifier, if using BJT)
    "Vbe": 0.6,
    "Vb" : 1.6,    # base voltage (V)
    "Vce_min": 0.2,
    "Vc": 11.999999,
    "feedback_ratio": 1,  # C1 / C2 ratio for Colpitts feedback network

    # Bias divider / resistors (user can set knowns)
    "divider_ratio": 10.0,
    "Ie": 1e-3,    # emitter resistor (Ω)
    "Ve": 1.0,    # emitter voltage (V)
    "Rc": 1,    # collector resistor (Ω)
    "Rc_bypassed": True,  # set True to bypass Rc (VDD = Vc)
    "is_forward_active": True, # set False to force cutoff/saturation bias (for testing)
    

    # Oscillator-specific components
    # feedback capacitor 1 (F)
    "L": 10e-6,
    "Q_tank": 10,
    "gm": 25.79e-3,
    "freq": 7e6, #target oscillation frequency (Hz)
    
    # Optional small-signal parameters if needed for oscillator DC calc
    "Vt": 0.02585,
    "I_cutoff": 1e-30,
    # Placeholders for calculated parameters

}

# Normalize user inputs
known_osc = normalize_user_known(raw_known_osc)

print("Initial known (oscillator, user-fixed):")
print({k: getv(known_osc, k) for k in known_osc})

check_overconstrained(known_osc, DEPENDENCY_LOOPS)

# Solve using only dc and osc tagged rules
OSC_TAGS = {"dc", "oscillator"}
known_osc = solve(known_osc, RULES, tags=OSC_TAGS)

print("\nFinal known (oscillator):")
print({k: getv(known_osc, k) for k in known_osc})

print("\nOscillator design report:")
print_report(known_osc)