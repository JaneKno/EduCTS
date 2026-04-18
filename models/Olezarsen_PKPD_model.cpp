$PROB
Olezarsen Population PK-PD Model with ApoC-III Lowering

$PARAM
// PK parameters - Population estimates (Table 120)
CL_F = 13.7        // Apparent clearance (L/h)
Vc_F = 96.3        // Apparent central volume of distribution (L)
Q_F = 4.15         // Apparent inter-compartmental clearance (L/h)
Vp_F = 3860        // Apparent peripheral volume of distribution (L)
ka = 1.17          // First order absorption rate constant (1/h)
F = 1.05           // Relative bioavailability (autoinjector)

// Covariates for PK (typical patient: 70 kg, no ADA)
WT = 70            // Body weight (kg)
ADA_binary = 0     // ADA status (0 = negative, 1 = positive)
ADA_titer = 0      // ADA titer (continuous)
RACE_JAPANESE = 0  // Japanese race (0 = no, 1 = yes)
DRUG_PRES_AI = 1   // Drug presentation autoinjector (0 = no, 1 = yes)

// PD parameters - Exposure-ApoC-III model (Table 121)
Kout = 0.0121      // First-order elimination rate (1/h)
IC50 = 0.247       // Concentration at half-maximal drug effect (ng/mL)
Imax = 0.961       // Maximum inhibitory effect (fraction)

// Covariates for PD
DISEASE_HTG_CVD = 0  // HTG or CVD patient (0 = no, 1 = yes)
DISEASE_FCS = 0      // FCS patient (0 = no, 1 = yes)
ADA_STATUS = 0       // ADA status for IC50 (0 = negative, 1 = positive)
ADA_TITER_IC50 = 0   // ADA titer for IC50

// Baseline ApoC-III
BASE_APOC3 = 100   // Baseline ApoC-III (mg/dL) - to be individualized

$OMEGA
0.1866   // IIV on CL/F (43.2% CV)
0.4624   // IIV on Vc/F (68.2% CV)
0.6970   // IIV on Q/F (83.7% CV)
1.3225   // IIV on Vp/F (115% CV)
0.0894   // IIV on ka (29.9% CV)
0.7056   // IIV on Kout (84.6% CV)
0.8930   // IIV on IC50 (94.5% CV)


$SIGMA
0.0976   // Proportional RUV for PK, ADA negative (31.6% CV)
0.1089   // Proportional RUV for PK, ADA positive (33.3% CV)
0.0640   // Proportional RUV for ApoC-III, ADA negative (25.3% CV)
0.1089   // Proportional RUV for ApoC-III, ADA positive (33.3% CV)

$CMT
DEPOT      // Absorption compartment (mg)
CENTRAL    // Central compartment (mg)
PERIPH     // Peripheral compartment (mg)
APOC3      // ApoC-III (mg/dL)

$MAIN
// Individual PK parameters with covariates
double CL_F_cov = CL_F * pow(WT/70, 0.961) * 
                  (ADA_binary == 1 ? 0.697 : 1.0) * 
                  (ADA_titer > 0 ? pow(ADA_titer/100, -0.208) : 1.0);
double CL_F_i = CL_F_cov * exp(ETA(1));

double Vc_F_cov = Vc_F * pow(WT/70, 2.23) * 
                  (RACE_JAPANESE == 1 ? 1.29 : 1.0);
double Vc_F_i = Vc_F_cov * exp(ETA(2));

double Q_F_cov = Q_F * pow(WT/70, 1.47);
double Q_F_i = Q_F_cov * exp(ETA(3));

double Vp_F_cov = Vp_F * pow(WT/70, 1.42) * 
                  (ADA_titer > 0 ? pow(ADA_titer/100, -0.241) : 1.0);
double Vp_F_i = Vp_F_cov * exp(ETA(4));

double ka_cov = ka * pow(WT/70, 0.857) * 
                (DRUG_PRES_AI == 1 ? 1.13 : 1.0);
double ka_i = ka_cov * exp(ETA(5));

double F_i = F * (DRUG_PRES_AI == 1 ? F : 1.0);

// Individual PD parameters with covariates
double Kout_cov = Kout * 
                  ((DISEASE_HTG_CVD == 1 || DISEASE_FCS == 1) ? 0.289 : 1.0) * 
                  (DISEASE_FCS == 1 ? 0.449 : 1.0);
double Kout_i = Kout_cov * exp(ETA(6));

double IC50_cov = IC50 * 
                  ((DISEASE_HTG_CVD == 1) ? 1.11 : 1.0) * 
                  (DISEASE_FCS == 1 ? 1.8 : 1.0) * 
                  (ADA_STATUS == 1 ? 1.3 : 1.0) * 
                  (ADA_TITER_IC50 > 0 ? pow(ADA_TITER_IC50/50, 0.325) : 1.0);
double IC50_i = IC50_cov * exp(ETA(7));

double Imax_i = Imax;  // Fixed, no IIV

// Derived parameter
double Kin_i = Kout_i * BASE_APOC3;  // Production rate to maintain baseline

// Initialize ApoC-III at baseline
APOC3_0 = BASE_APOC3;

$ODE
// PK equations
dxdt_DEPOT = -ka_i * DEPOT;
dxdt_CENTRAL = ka_i * F_i * DEPOT - (CL_F_i / Vc_F_i) * CENTRAL - 
               (Q_F_i / Vc_F_i) * CENTRAL + (Q_F_i / Vp_F_i) * PERIPH;
dxdt_PERIPH = (Q_F_i / Vc_F_i) * CENTRAL - (Q_F_i / Vp_F_i) * PERIPH;

// Central concentration (ng/mL, assuming dose in mg)
double Cc = (CENTRAL / Vc_F_i) * 1000;  // Convert to ng/mL

// PD equation - Indirect response with inhibition of production
double inhibition = (Imax_i * Cc) / (IC50_i + Cc);
dxdt_APOC3 = Kin_i * (1 - inhibition) - Kout_i * APOC3;

$TABLE
// Observed concentrations with residual error
double DV_Cc = (CENTRAL / Vc_F_i) * 1000 * (1 + (ADA_binary == 1 ? EPS(2) : EPS(1)));  // ng/mL

double DV_APOC3 = APOC3 * (1 + (ADA_STATUS == 1 ? EPS(4) : EPS(3)));  // mg/dL

// Additional outputs for monitoring
double Cc_out = (CENTRAL / Vc_F_i) * 1000;  // Central concentration (ng/mL)
double APOC3_pct_change = ((APOC3 - BASE_APOC3) / BASE_APOC3) * 100;  // % change from baseline

$CAPTURE
DV_Cc           // Observed olezarsen concentration (ng/mL)
DV_APOC3          // Observed ApoC-III (mg/dL)
Cc_out            // Central concentration (ng/mL)
APOC3_pct_change  // % change in ApoC-III from baseline