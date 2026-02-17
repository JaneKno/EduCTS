$PROB
Plozasiran Population K-PD Model with APOC3 and Triglyceride Lowering

$PARAM
// K-PD parameters - Population estimates (Table 1)
Ke = 0.0004125         // Elimination rate constant from effect compartment (1/h): original value 0.00990 (1/day)
BASE_APOC3 = 28.4      // Baseline APOC3 (mg/dL)
Ksyn_APOC3 = 0.356     // Zero-order production rate constant of APOC3 (mg/dL/h)
Imax_ARO_APOC3 = 100   // Maximum inhibitory effect of ARO-APOC3 on APOC3 (%, fixed at 100)
IC50_ARO_APOC3 = 3.319 // IC50 of ARO-APOC3 on APOC3 (mg)
BASE_TG = 577          // Baseline TG (mg/dL)
Kdeg_TG = 0.724        // TG elimination rate constant (1/h, fixed)
Imax_APOC3 = 100       // Maximum inhibitory effect of APOC3 on TG (%, fixed at 100)
IC50_APOC3 = 2.46      // APOC3 concentration to achieve 50% of Imax on TG (mg/dL)

// Covariates
OBS_BASE_APOC3 = 30.5  // Observed baseline APOC3 for individual
OBS_BASE_TG = 652      // Observed baseline TG for individual

$OMEGA @block
0.4200                                    // IIV on Ke (72.2% CV)
0       0.0493                            // IIV on Baseline APOC3 (22.5% CV)
0       0       1.2996                    // IIV on IC50_ARO_APOC3 (163% CV)
0       0.0335  0       0.1361            // IIV on Baseline TG (38.2% CV), Corr=0.409 with APOC3

$SIGMA
0.1102   // Proportional error for APOC3 (33.2% CV)
0.1698   // Proportional error for TG (41.2% CV)

$CMT
EFFECT     // Effect compartment (mg)
APOC3      // Apolipoprotein C3 (mg/dL)
TG         // Triglycerides (mg/dL)

$MAIN
// Individual parameters with IIV
double Ke_i = Ke * exp(ETA(1));

// Adjust baseline APOC3 based on observed baseline and covariate effect
double BASE_APOC3_cov = BASE_APOC3 * pow(OBS_BASE_APOC3/30.5, 0.781);
double BASE_APOC3_i = BASE_APOC3_cov * exp(ETA(2));

double IC50_ARO_APOC3_i = IC50_ARO_APOC3 * exp(ETA(3));

// Adjust baseline TG based on observed baseline and covariate effect
double BASE_TG_cov = BASE_TG * pow(OBS_BASE_TG/652, 1.00);
double BASE_TG_i = BASE_TG_cov * exp(ETA(4));

// Account for correlation between APOC3 and TG baselines
double ETA_TG_corr = ETA(4) + 0.409 * ETA(2);  // 42.7% correlation

// Recalculate synthesis rates to maintain individual baselines
double inhib_TG_deg_base = (Imax_APOC3/100) * BASE_APOC3_i/ (IC50_APOC3 + BASE_APOC3_i);

double Ksyn_TG_i = Kdeg_TG* (1 - inhib_TG_deg_base) * BASE_TG_i;  // TG synthesis rate

// Initialize compartments at baseline
APOC3_0 = BASE_APOC3_i;
TG_0 = BASE_TG_i;
EFFECT_0 = 0;

$ODE
// Effect compartment - receives dose, eliminated with Ke
dxdt_EFFECT = -Ke_i * EFFECT;

// Hypothetical concentration in effect compartment
double Ce = EFFECT;  // Amount in effect compartment (mg)

// APOC3 dynamics - inhibition of synthesis by ARO-APOC3
double inhib_APOC3_syn = (Imax_ARO_APOC3/100) * Ce / (IC50_ARO_APOC3_i + Ce);

// APOC3 has zero-order synthesis (Ksyn) and appears to have first-order degradation
// Assuming first-order degradation to maintain baseline
double Kdeg_APOC3 = Ksyn_APOC3 / BASE_APOC3_i;

dxdt_APOC3 = Ksyn_APOC3 * (1 - inhib_APOC3_syn) - Kdeg_APOC3 * APOC3;

// TG dynamics - inhibition of degradation by APOC3
double inhib_TG_deg = (Imax_APOC3/100) * APOC3 / (IC50_APOC3 + APOC3);

dxdt_TG = Ksyn_TG_i - Kdeg_TG * TG * (1 - inhib_TG_deg);

$TABLE
// Observed values with proportional error
double DV_APOC3 = APOC3 * (1 + EPS(1));
double DV_TG = TG * (1 + EPS(2));

// Additional outputs
double APOC3_pct_change = ((APOC3 - BASE_APOC3_i) / BASE_APOC3_i) * 100;
double TG_pct_change = ((TG - BASE_TG_i) / BASE_TG_i) * 100;
double Ce_out = EFFECT;  // Effect compartment concentration

$CAPTURE
DV_APOC3           // Observed APOC3 (mg/dL)
DV_TG              // Observed TG (mg/dL)
APOC3_pct_change   // % change in APOC3 from baseline
TG_pct_change      // % change in TG from baseline
Ce_out             // Effect compartment amount (mg)