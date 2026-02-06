mA = 1e-3  # define milliampere

class BJTCalculator:
    def __init__(self, Vdd=12, Ic=None, Ib=None, Vbe=0.7, Re=100, Rc=1e3):
        # Store what the user gave us
        self.Vdd = Vdd
        self.Ic = Ic
        self.Ib = Ib
        self.Vbe = Vbe
        self.Re = Re
        self.Rc = Rc
        self.Vce_min = 0.2  # Minimum Vce for saturation
        self.Vb = None  # Base voltage, to be calculated if needed
        self.R1 = None  # Base resistor 1, to be calculated if needed
        self.R2 = None  # Base resistor 2 (to gnd), to be calculated if needed
        
        # Placeholders for calculated values
        self.Ie = None
        self.Ve = None
        self.Vc = None
        self.Vce = None
        self.Vc_min_overhead = None
        self.Vc_max_overhead = None


    def calculate_vb(self):
        self.Vdd * self.R2 / (self.R1 + self.R2) 
        return self.Vb
    
    def calculate_ib(self):
        self.Ib = (self.Vb / ((self.R1)^-1 + (self.R2)^-1)^-1) 
        return self.Ib, self.Req

    def calculate_ie(self):
        self.Ie = self.Ic + self.Ib
        return self.Ie
    
    def calculate_ve(self):
        if self.Ie is None:
            self.calculate_ie()
        self.Ve = self.Ie * self.Re
        return self.Ve
    
    def calculate_vc(self):
        self.Vc = self.Vdd - (self.Ic * self.Rc)
        return self.Vc
    
    def calculate_vce(self):
        if self.Vc is None:
            self.calculate_vc()
        if self.Ve is None:
            self.calculate_ve()
        self.Vce = self.Vc - self.Ve
        return self.Vce
    
    def calculate_overhead(self):
        Vc_min_overhead = self.Vdd - self.Vc
        Vc_max_overhead = self.Vce_min - self.Ve
        return self.Vc_min_overhead, self.Vc_max_overhead
    
    def find_bias(self):
        if self.Ic is not None:
            raise ValueError("Please provide either Ic or Ib, not both.")
        self.Ie = self.Ic
        self.calculate_ve()
        self.calculate_vc()
        self.calculate_vce()
        self.optimize_bias()
        # condition
        abs(abs(self.Vcc - self.Vc) - abs(self.Vce - self.Ve)) <=1.0  # Check if the bias is optimized within 1V tolerance
        # this doesn't work because the bias is not optimized, we need to adjust R1 and R2 to achieve the desired bias point. 
        # therefore affecting the value, 
        return self.ve
