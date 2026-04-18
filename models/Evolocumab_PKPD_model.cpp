[PROB]
PCSK9-Evolocumab PK-PD Model

[PARAM]
// PK parameters
KA = 0.245        // First-order absorption rate constant (1/day)
V = 2.66          // Volume of distribution (L)
CL = 0.256        // Clearance (L/day)


// PD parameters
KDEG = 2.12       // PCSK9 degradation rate (1/day)
BASE = 5.27       // Baseline PCSK9 (nM)
KSS = 0.253       // Steady-state constant (nM)
KINT = 0.0529     // Internalization rate constant (1/day)
THETA1 = 0.637    // Fold change in baseline PCSK9: healthy subjects relative to statin-treated patients
STATIN = 0       // Population flag: 1 = statin-treated (default), 0 = healthy/non-statin

// LDL-C indirect response parameters
kout = 0.305      // LDL-C elimination rate constant (1/day)
BASELDL = 116     // Baseline LDL-C (mg/dL)
Imax = 1.0        // Maximal inhibition (fixed)
IC50 = 1.46       // Concentration for half-maximal inhibition (nM)

[CMT]
DEPOT    // Subcutaneous depot
TDA      // Total drug amount
TLC      // Total PCSK9 concentration
LDL      // LDL-C compartment

[OMEGA] @block @labels KA_IIV V_IIV CL_IIV
0.807              // var(KA)
0.250  0.158       // cov(KA,V),  var(V)
-0.579 -0.070  0.924  // cov(KA,CL), cov(V,CL), var(CL)

[OMEGA] @labels BASELDL_IIV IC50_IIV
0.0465    // var(BASELDL); kout and Imax IIV fixed to 0
0.481     // var(IC50)

[SIGMA]
0.0576    // Proportional error evolocumab
0.0942    // Proportional error PCSK9
0.0130    // Proportional error LDL-C
3612.01   // Additive error LDL-C (variance = 60.1^2; 60.1 is SD per Table 3 footnote b)

[MAIN]
// Inter-individual variability (log-normal)
double KA_i      = KA      * exp(ETA(1));
double V_i       = V       * exp(ETA(2));
double CL_i      = CL      * exp(ETA(3));
double BASELDL_i = BASELDL * exp(ETA(4));
double IC50_i    = IC50    * exp(ETA(5));

// Statin-adjusted baseline PCSK9
// THETA1 = PCSK9_healthy / PCSK9_statin, so statin patients have BASE / THETA1
double BASE_EFF = (STATIN == 1) ? BASE / THETA1 : BASE;

// Derived parameters
double KSYN = KDEG * BASE_EFF;  // PCSK9 synthesis rate (nM/day)
double kin = kout * (1 - Imax * BASE_EFF / (IC50_i + BASE_EFF)) * BASELDL_i; // LDL-C production rate (mg/dL/day)
double K = CL_i/V_i;            // Elimination rate constant (1/day)

TLC_0 = BASE_EFF;
LDL_0 = BASELDL_i;
[ODE]
double TDC = TDA/V_i;  // total drug concentration
double FDC = ((TDC-TLC-KSS)+sqrt(pow(TDC-TLC-KSS,2)+4*TDC*KSS))/2; // free drug concentration

dxdt_DEPOT = -KA_i * DEPOT;
dxdt_TDA = KA_i * DEPOT - K * FDC * V_i - KINT * TLC * FDC * V_i/(KSS + FDC);
dxdt_TLC = KSYN - KDEG * TLC - ((KINT - KDEG) * FDC * TLC)/(KSS + FDC);

// LDL-C indirect response model
// FLC is free ligand concentration (nM)
double FLC = TLC-(TDC-FDC);
dxdt_LDL = kin - kout * (1 - Imax * FLC / (IC50_i + FLC)) * LDL;

[TABLE]
// Recompute QSS quantities in TABLE scope (ODE locals are not in scope here)
double TDC_T  = TDA/V_i;
double FDC_T  = ((TDC_T-TLC-KSS)+sqrt(pow(TDC_T-TLC-KSS,2)+4*TDC_T*KSS))/2;
double FLC_T  = TLC-(TDC_T-FDC_T);

double DV_EVOL      = TDC_T * (1 + EPS(1));           // Evolocumab concentration
double DV_PCSK9     = TLC   * (1 + EPS(2));           // Total PCSK9 concentration
double DV_PCSK9_free = FLC_T;                          // Free PCSK9 concentration
double DV_FDC       = FDC_T;                           // Free drug concentration
double DV_LDL       = LDL   * (1 + EPS(3)) + EPS(4); // LDL-C with proportional and additive error

[CAPTURE]
DV_FDC
DV_EVOL
DV_PCSK9
DV_PCSK9_free
DV_LDL