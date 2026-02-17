$PROB
Inclisiran K-PD Model with PCSK9 and LDL-C Indirect Response

$PARAM
// PK parameters
Ke = 0.00711      // First-order elimination rate constant (mg/d)

// PCSK9 PD parameters
BASE_PCSK9 = 381  // Baseline PCSK9 (ng/mL)
KdegP = 0.163     // PCSK9 degradation rate (1/d)
IC50P = 48.5      // IC50 for PCSK9 inhibition (mg)
ImaxP = 0.887     // Maximum inhibition of PCSK9 synthesis
gammaP = 1        // Hill coefficient for PCSK9 (assumed)

// LDL-C PD parameters
BASE_LDL = 108    // Baseline LDL-C (mg/dL)
KsynL = 16.6      // LDL-C synthesis rate (mg/dL/d)
IC50L = 124       // IC50 for LDL-C inhibition (ng/mL)
ImaxL = 1         // Maximum inhibition of LDL-C degradation (fixed)
gammaL = 1        // Hill coefficient for LDL-C (assumed)

$OMEGA
0.204   // IIV on Ke (47.6% CV)
0.0596  // IIV on Base PCSK9 (24.8% CV)
0.560   // IIV on IC50P (86.6% CV)
0.0994  // IIV on Base LDL-C (32.3% CV)
1.23    // IIV on IC50L (155.6% CV)

$SIGMA
0.203   // Proportional error PCSK9 (20.3% CV)
6.61    // Additive error LDL-C (mg/dL)
0.182   // Proportional error LDL-C (18.2% CV)

$CMT
EFFECT   // Effect compartment (inclisiran amount, mg)
PCSK9    // PCSK9 concentration (ng/mL)
LDL      // LDL-C concentration (mg/dL)

$MAIN
// Individual parameters
double Ke_i = Ke * exp(ETA(1));
double BASE_PCSK9_i = BASE_PCSK9 * exp(ETA(2));
double IC50P_i = IC50P * exp(ETA(3));
double BASE_LDL_i = BASE_LDL * exp(ETA(4));
double IC50L_i = IC50L * exp(ETA(5));

double KdegL = KsynL / (BASE_LDL_i*(1 - (ImaxL * pow(BASE_PCSK9_i, gammaL)) / (pow(IC50L_i, gammaL) + pow(BASE_PCSK9_i, gammaL))));  // LDL-C degradation rate (1/d)

// Derived parameters
double KsynP = KdegP * BASE_PCSK9_i;  // PCSK9 synthesis rate (ng/mL/d)

// Initialize compartments at baseline
PCSK9_0 = BASE_PCSK9_i;
LDL_0 = BASE_LDL_i;

$ODE
// Effect compartment concentration (mg)
double Ce = EFFECT;

// PCSK9 turnover with inhibition of synthesis by inclisiran
double inhib_PCSK9_syn = 1 - (ImaxP * pow(Ce, gammaP)) / (pow(IC50P_i, gammaP) + pow(Ce, gammaP));
dxdt_EFFECT = -Ke_i * EFFECT;
dxdt_PCSK9 = KsynP * inhib_PCSK9_syn - KdegP * PCSK9;

// LDL-C turnover with inhibition of degradation by PCSK9
double inhib_LDL_deg = 1 - (ImaxL * pow(PCSK9, gammaL)) / (pow(IC50L_i, gammaL) + pow(PCSK9, gammaL));
dxdt_LDL = KsynL - KdegL * inhib_LDL_deg * LDL;

$TABLE
double DV_PCSK9 = PCSK9 * (1 + EPS(1));
double DV_LDL = LDL * (1 + EPS(3)) + EPS(2);
double Ce_conc = Ce;  // Inclisiran concentration in effect compartment

$CAPTURE
Ce_conc    // Inclisiran in effect compartment (mg)
DV_PCSK9   // Observed PCSK9 (ng/mL)
DV_LDL     // Observed LDL-C (mg/dL)