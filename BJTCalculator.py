import numpy as np
import PySpice.Logging.Logging as Logging
from PySpice.Spice.Netlist import Circuit
from PySpice.Unit import *
# Program to Handle BJT Calculations and Biasing
#

cir = Circuit('Common Emitter Amplifier')
Vbase = cir.V('Vbase', 'Rb1', cir.gnd, 12@u_V)
Vcc = cir.V('Vcc', 'collector', cir.gnd, 12@u_V)
Rc = cir.R('Rc', 'collector', 'Vcc', 1@u_kOhm)
Re = cir.R('Re', 'emitter', cir.gnd, 500@u_Ohm)
Rb1 = cir.R('Rb1', 'Vbase', 'base', 100@u_kOhm)
Rb2 = cir.R('Rb2', 'base', cir.gnd, 20@u_kOhm)
Q1 = cir.BJT('Q1', 'collector', 'base', 'emitter', model='2N2222')
Q1.model('2N2222', 'NPN', bf=100, va=100@u_V)
simulator = cir.simulator(temperature=25, nominal_temperature=25)
analysis = simulator.dc(Vbase=slice(0, 12, 0.1))
