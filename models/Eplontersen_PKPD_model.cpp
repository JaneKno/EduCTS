$PROB
Eplontersen Population PKPD Model with 2-Compartment PK and TTR Biomarker

$PARAM
// ===== PHARMACOKINETIC PARAMETERS =====
// Population PK estimates (Table 2 - Final Model)
CL = 24.1              // Total body clearance (L/h)
Vc = 50.4              // Central compartment volume of distribution (L)
Q = 3.64               // Intercompartmental clearance (L/h)
Vp = 2790              // Peripheral compartment volume of distribution (L)
ka_arm = 0.217         // Absorption rate constant from arm injection site (1/h)
ka_ab = 0.282          // Absorption rate constant from abdomen injection site (1/h)

// ===== PHARMACODYNAMIC PARAMETERS =====
// Population PD estimates (Table 3 - TTR Biomarker)
BL_TTR = 31.4          // Baseline TTR concentration (mg/dL)
kout_TTR = 0.00398     // First-order elimination rate constant of TTR (1/h)
Imax_Epl = 0.970       // Maximum inhibitory effect of Eplontersen on TTR (%, fixed at 97%)
IC50_Epl = 0.0283      // Eplontersen concentration for 50% TTR inhibition (ng/mL)

// ===== COVARIATES (Individual patient characteristics) =====
LBM = 70               // Lean body mass (kg) [used for CL scaling]
BW = 85                // Body weight (kg) [used for Vc, Q, Vp scaling]

// ===== COVARIATE REFERENCE VALUES (Population typical values) =====
LBM_ref = 51.6         // Reference lean body mass for CL scaling (kg)
BW_ref = 72.1          // Reference body weight for Vc, Q, Vp scaling (kg)

$OMEGA
0.0436                                 // IIV on CL (21.1% CV) - from Table 2
0.2398                                 // IIV on Vc (52.1% CV) - from Table 2
0.1278                                 // IIV on Q (36.9% CV) - from Table 2
0.1816                                 // IIV on Vp (44.6% CV) - from Table 2
0.1437                                 // IIV on ka (39.3% CV) - from Table 2
0.0931                                 // IIV on BL_TTR (31.3% CV) - from Table 3
0.1883                                 // IIV on kout_TTR (45.5% CV) - from Table 3
0.5054                                 // IIV on IC50_Epl (81.1% CV) - from Table 3

$SIGMA
0.0851   // Additive error for plasma concentration (CV ~29.2%)
0.0362   // Proportional error for TTR (CV ~19.0%)

$CMT
GUT      // Absorption compartment (ng)
CENT     // Central compartment (ng/mL)
PERI     // Peripheral compartment (ng/mL)
TTR      // TTR biomarker (mg/dL)

$MAIN
// ===== COVARIATE EFFECTS ON PK PARAMETERS (Allometric scaling) =====
// From Diep et al. (2022) Table 2 - Population PKPD Model
// CL scales with LBM (metabolic rate-driven elimination)
double CL_cov = CL * pow(LBM / LBM_ref, 1.42);

// Vc, Q, Vp scale with BW (distribution volumes related to tissue/fluid compartments)
double Vc_cov = Vc * pow(BW / BW_ref, 1.89);
double Q_cov = Q * pow(BW / BW_ref, 2.53);
double Vp_cov = Vp * pow(BW / BW_ref, 2.73);

// ===== INDIVIDUAL PHARMACOKINETIC PARAMETERS WITH IIV =====
// IIV applied to covariate-adjusted values
double CL_i = CL_cov * exp(ETA(1));
double Vc_i = Vc_cov * exp(ETA(2));
double Q_i = Q_cov * exp(ETA(3));
double Vp_i = Vp_cov * exp(ETA(4));

// Absorption rates depend on injection site
// In this model structure, ka is shared but can be modified based on CMT/RATE coding
double ka_i = (ka_arm + ka_ab) / 2 * exp(ETA(5));  // Average ka with IIV

// ===== INDIVIDUAL PHARMACODYNAMIC PARAMETERS WITH IIV =====
double BL_TTR_i = BL_TTR * exp(ETA(6));
double kout_TTR_i = kout_TTR * exp(ETA(7));
double IC50_Epl_i = IC50_Epl * exp(ETA(8));

// TTR synthesis rate to maintain baseline at equilibrium
// At baseline (no drug): kin = kout * BL
double kin_TTR_i = kout_TTR_i * BL_TTR_i;

// Initialize compartments at baseline
GUT_0 = 0;     // No drug at baseline
CENT_0 = 0;    // No drug at baseline
PERI_0 = 0;    // No drug at baseline
TTR_0 = BL_TTR_i;  // Start at individual baseline

$ODE
// ===== PHARMACOKINETICS =====
// Gut (absorption) compartment: absorption to central compartment
dxdt_GUT = -ka_i * GUT;

// Central compartment: absorption input, distribution to periphery, clearance
dxdt_CENT = ka_i * GUT - CL_i/Vc_i * CENT - (Q_i / Vc_i) * CENT + (Q_i / Vp_i) * PERI;

// Peripheral compartment: gain from central, distribution back to central
dxdt_PERI = (Q_i / Vc_i) * CENT  - (Q_i / Vp_i) * PERI;

// ===== PHARMACODYNAMICS (TTR as turnover model) =====
// Central concentration in ng/mL (CENT is in ug and Vc_i in L)
double Cp = CENT / Vc_i;

// Inhibition of TTR synthesis by Eplontersen (effect compartment model implicit)
// Using Emax model: E = Imax * Cp / (IC50 + Cp)
double inhibition = (Imax_Epl * Cp) / (IC50_Epl_i + Cp);

// TTR dynamics with first-order input and inhibited synthesis
// dTTR/dt = kin * (1 - inhibition) - kout * TTR
dxdt_TTR = kin_TTR_i * (1 - inhibition) - kout_TTR_i * TTR;

$TABLE
// ===== PREDICTIONS =====
// Plasma concentration (predicted with additive error)
double IPRED_Cp = CENT / Vc_i;
double DV_Cp = IPRED_Cp + EPS(1);

// TTR concentration (predicted with proportional error)
double IPRED_TTR = TTR;
double DV_TTR = IPRED_TTR * (1 + EPS(2));

// ===== ADDITIONAL OUTPUTS =====
// Percent change from baseline
double TTR_pct_change = ((TTR - BL_TTR_i) / BL_TTR_i) * 100;

// Effective concentration for reference
double Cp_out = IPRED_Cp;

$CAPTURE
DV_Cp           // Observed Eplontersen concentration (ng/mL)
DV_TTR          // Observed TTR concentration (mg/dL)
IPRED_Cp        // Predicted Eplontersen concentration
IPRED_TTR       // Predicted TTR concentration
TTR_pct_change  // Percent change in TTR from baseline
Cp_out          // Effective plasma concentration (ng/mL)
