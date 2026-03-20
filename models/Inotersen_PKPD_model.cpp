$PROB
Inotersen Population PKPD Model with 2-Compartment PK and TTR Biomarker

$PARAM
// ===== PHARMACOKINETIC PARAMETERS =====
// Population PK estimates (Table 2 - Final Population PK Model)
CL = 3.4               // Total body clearance (L/h)
Vc = 20.7              // Central compartment volume of distribution (L)
Q = 0.266              // Intercompartmental clearance (L/h)
Vp = 230               // Peripheral compartment volume of distribution (L)
ka = 0.261             // Absorption rate constant (1/h)

// ===== PHARMACODYNAMIC PARAMETERS =====
// Population PD estimates (Table 3 - TTR Biomarker)
BL_TTR = 20.4          // Baseline TTR concentration (mg/dL)
kout = 0.00308         // First-order elimination rate constant of TTR (1/h)
Imax = 0.913           // Maximum inhibitory effect of Inotersen on TTR (fraction)
IC50 = 9.07            // Inotersen concentration for 50% TTR inhibition (ng/mL)

// ===== COVARIATES (Individual patient characteristics) =====
BW = 75                // Body weight (kg) [used for volume scaling]
DISEASE = 0            // Disease indicator (0 = healthy, 1 = diseased)

// ===== COVARIATE REFERENCE VALUES (Population typical values) =====
BW_ref = 70            // Reference body weight for scaling (kg)

$OMEGA
0.071                                  // IIV on CL - from Table 2
0.335                                  // IIV on Vc - from Table 2
0.677                                  // IIV on Vp - from Table 2
0.0537                                 // IIV on Baseline TTR - from Table 3
0.288                                  // IIV on kout - from Table 3
0.953                                  // IIV on IC50 - from Table 3

$SIGMA
0.168                  // Log additive error for plasma concentration - from Table 2
0.0169                 // Proportional error for TTR (0.13²) - from Table 3
1.8225                 // Additive error for TTR (1.35²) - from Table 3

$CMT
GUT      // Absorption compartment (ng)
CENT     // Central compartment (ng/mL)
PERI     // Peripheral compartment (ng/mL)
TTR      // TTR biomarker (mg/dL)

$MAIN
// ===== COVARIATE EFFECTS ON PK PARAMETERS =====
// Volumes and clearances scale allometrically with body weight
// Applied as power-centered effects on median (BW_ref = 70 kg)
double CL_cov = CL * pow(BW / BW_ref, 0.75);
double Vc_cov = Vc * pow(BW / BW_ref, 1.0);
double Q_cov = Q * pow(BW / BW_ref, 1.0);
double Vp_cov = Vp * pow(BW / BW_ref, 1.0);

// Disease covariate effect on baseline TTR (Table 3)
double BL_TTR_disease = BL_TTR;
if (DISEASE == 1) {
  BL_TTR_disease = BL_TTR * 1.32;  // 32% increase in disease state
}

// ===== INDIVIDUAL PHARMACOKINETIC PARAMETERS WITH IIV =====
// IIV applied to covariate-adjusted values
double CL_i = CL_cov * exp(ETA(1));
double Vc_i = Vc_cov * exp(ETA(2));
double Vp_i = Vp_cov * exp(ETA(3));
double Q_i = Q_cov;  // No IIV on Q
double ka_i = ka;    // No IIV on ka

// ===== INDIVIDUAL PHARMACODYNAMIC PARAMETERS WITH IIV =====
double BL_TTR_i = BL_TTR_disease * exp(ETA(4));
double kout_i = kout * exp(ETA(5));
double IC50_i = IC50 * exp(ETA(6));

// TTR synthesis rate to maintain baseline at equilibrium
// At baseline (no drug): kin = kout * BL
double kin_TTR_i = kout_i * BL_TTR_i;

// Initialize compartments at baseline
GUT_0 = 0;           // No drug at baseline
CENT_0 = 0;          // No drug at baseline
PERI_0 = 0;          // No drug at baseline
TTR_0 = BL_TTR_i;    // Start at individual baseline

$ODE
// ===== PHARMACOKINETICS =====
// Gut (absorption) compartment: absorption to central compartment
dxdt_GUT = -ka_i * GUT;

// Central compartment: absorption input, distribution to periphery, clearance
dxdt_CENT = ka_i * GUT - CL_i/Vc_i * CENT - (Q_i / Vc_i) * CENT + (Q_i / Vp_i) * PERI;

// Peripheral compartment: gain from central, distribution back to central
dxdt_PERI = (Q_i / Vc_i) * CENT - (Q_i / Vp_i) * PERI;

// ===== PHARMACODYNAMICS (TTR as turnover model) =====
// Central concentration in ng/mL (CENT is in ug and Vc_i in L)
double Cp = CENT / Vc_i;

// Inhibition of TTR synthesis by Inotersen using Emax model
// E = Imax * Cp / (IC50 + Cp)
double inhibition = (Imax * Cp) / (IC50_i + Cp);

// TTR dynamics with first-order input and inhibited synthesis
// dTTR/dt = kin * (1 - inhibition) - kout * TTR
dxdt_TTR = kin_TTR_i * (1 - inhibition) - kout_i * TTR;

$TABLE
// ===== PREDICTIONS =====
// Plasma concentration (predicted with log additive error)
double IPRED_Cp = CENT / Vc_i;
double DV_Cp = IPRED_Cp * exp(EPS(1));

// TTR concentration (predicted with proportional and additive errors in log space)
double IPRED_TTR = TTR;
double DV_TTR = IPRED_TTR * exp(EPS(2) + EPS(3));

// ===== ADDITIONAL OUTPUTS =====
// Percent change from baseline
double TTR_pct_change = ((TTR - BL_TTR_i) / BL_TTR_i) * 100;

// Effective concentration for reference
double Cp_out = IPRED_Cp;

$CAPTURE
DV_Cp           // Observed Inotersen concentration (ng/mL)
DV_TTR          // Observed TTR concentration (mg/dL)
IPRED_Cp        // Predicted Inotersen concentration
IPRED_TTR       // Predicted TTR concentration
TTR_pct_change  // Percent change in TTR from baseline
Cp_out          // Effective plasma concentration (ng/mL)
