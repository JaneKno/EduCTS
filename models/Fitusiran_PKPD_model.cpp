$PROB
Fitusiran PopK-PD Model with AT Lowering via RISC-mediated inhibition

$PARAM
// PK parameters - Population estimates
Ka = 0.157        // Absorption rate (1/h)
V2 = 3300         // Liver central volume (g)
V3 = 6075         // Liver peripheral volume (g)
CL = 39.6         // Clearance from liver central compartment (g/h)
CL2 = 27.20       // Liver inter-compartment clearance (g/h)
minQ = 0.0225     // Minimum clearance into RISC compartment (g/h)
Qmax = 0.029      // Dose dependent clearance into RISC compartment (g/h)
RV = 9375         // RISC volume (g)
CLR = 11.2        // Clearance from RISC compartment (g/h)
IC50 = 0.0004     // RISC concentration for 50% of maximum effect (mg/g)
Imax = 0.9        // Maximum inhibition of AT production
Kin = 0.99        // AT production rate (%/h)
Kout = 0.01       // AT elimination rate (1/h)
F = 1             // Dose effect on bioavailability (fixed)
thetaQ = 0.2      // Hill's coefficient for dose dependent clearance into RISC compartment

// Derived parameter
BASE_AT = 100     // Baseline AT (%) - derived from Kin/Kout at steady state

$OMEGA
0.0661      // IIV on CL (25.7% CV)
0.3745      // IIV on IC50 (61.2% CV)
0.0016      // IIV on Imax (4% CV)
0.0144      // IIV on Kout (12% CV)
0.8950      // IIV on thetaQ (94.5% CV)

$SIGMA
0.15    // Proportional error for AT (estimated from combined error structure)

$CMT
DEPOT    // Absorption compartment (mg)
CENTRAL  // Central liver compartment (mg)
PERIPH   // Peripheral liver compartment (mg)
RISC     // RISC compartment (mg)
AT       // Antithrombin (%)

$MAIN
// Individual parameters with IIV
double CL_i = CL * exp(ETA(1));
double IC50_i = IC50 * exp(ETA(2));
double Imax_i = Imax * exp(ETA(3));
double Kout_i = Kout * exp(ETA(4));
double thetaQ_i = thetaQ * exp(ETA(5));

// Derived individual parameter
double Kin_i = Kout_i * BASE_AT;  // Maintain baseline AT at 100%

// Initialize AT compartment at baseline
AT_0 = BASE_AT;

$ODE
// Calculate dose-dependent clearance to RISC
double DOSE = DEPOT + CENTRAL + PERIPH + RISC;  // Total amount in system
double Q = minQ + (Qmax * pow(DOSE, thetaQ_i)) / (1 + pow(DOSE, thetaQ_i));

// PK equations
dxdt_DEPOT = -Ka * DEPOT;
dxdt_CENTRAL = Ka * F * DEPOT - (CL_i + CL2 + Q) * CENTRAL / V2 + CL2 * PERIPH / V3;
dxdt_PERIPH = CL2 * CENTRAL / V2 - CL2 * PERIPH / V3;
dxdt_RISC = Q * CENTRAL / V2 - CLR * RISC / RV;

// RISC concentration
double CRISC = RISC / RV;

// Inhibitory effect of fitusiran on AT production
double inhibitory_effect = 1 - (Imax_i * CRISC) / (IC50_i + CRISC);

// AT turnover with inhibited production
dxdt_AT = Kin_i * inhibitory_effect - Kout_i * AT;

$TABLE
// Observed AT with proportional error
double DV_AT = AT * (1 + EPS(1));

// RISC concentration (for monitoring)
double CRISC_out = RISC / RV;

// Central compartment concentration (for monitoring)
double C_CENTRAL = CENTRAL / V2;

$CAPTURE
DV_AT        // Observed AT (%)
CRISC_out    // RISC concentration (mg/g)
C_CENTRAL    // Central liver concentration (mg/g)
AT           // True AT (%)