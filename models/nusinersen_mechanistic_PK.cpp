// Nusinersen Mechanistic PK Model for Non-Human Primate (NHP)
// Based on NONMEM model with 9-compartment structure
// Compartments: CSF, Plasma, Cervical/Lumbar/Thoracic spinal cord, Brain, Deep brain tissue, Pons, Peripheral (ug/mL) 

[PROB]
Mechanistic PK model for nusinersen with CNS distribution in non-human primates.
Nine-compartment model including CSF, plasma, spinal cord regions, brain, deep brain tissue, pons, and peripheral compartment.

[PARAM]
// Volumes of Distribution (mL)
V1 = 13.6       // CSF
V2 = 937        // Plasma
V3 = 1.91       // Cervical spinal cord
V4 = 53.8       // Brain
V6 = 1.08       // Lumbar spinal cord
V8 = 1.52       // Thoracic spinal cord
V9 = 2.11       // Pons

// Rate Constants (1/h)
K13 = 0.00171   // CSF to Cervical spinal cord
K31 = 0.0001    // Cervical spinal cord to CSF
K14 = 0.006     // CSF to Brain
K41 = 0.0004    // Brain to CSF
K12 = 0.0891    // CSF to Plasma
K20 = 0.206     // Plasma elimination
K25 = 0.00818   // Plasma to Peripheral
K52 = 0.0001    // Peripheral to Plasma
K16 = 0.00286   // CSF to Lumbar spinal cord
K61 = 0.0003    // Lumbar spinal cord to CSF
K47 = 0.00257   // Brain to Deep brain tissue
K74 = 0.0001    // Deep brain tissue to Brain
K18 = 0.0021    // CSF to Thoracic spinal cord
K81 = 0.00045   // Thoracic spinal cord to CSF
K19 = 0.00157   // CSF to Pons
K91 = 0.0002    // Pons to CSF

// Covariate (reference weight)
WT = 6.5        // kg, typical non-human primate weight

[CMT]
CSF           // 1. Cerebrospinal fluid
PLASMA        // 2. Plasma
CERV_SPINAL   // 3. Cervical spinal cord
BRAIN         // 4. Brain
PERIPH        // 5. Peripheral compartment
LUM_SPINAL    // 6. Lumbar spinal cord
DBT           // 7. Deep brain tissue
THO_SPINAL    // 8. Thoracic spinal cord
PONS          // 9. Pons

[MAIN]
// Inter-individual variability (IIV)
double K20i = K20 * exp(ETA(1));
double K12i = K12 * exp(ETA(2));
double V1i = V1 * exp(ETA(3));
double V2i = V2 * exp(ETA(4));
double K13i = K13 * exp(ETA(5));
double K31i = K31 * exp(ETA(6));
double K14i = K14 * exp(ETA(7));
double K41i = K41 * exp(ETA(8));
double K16i = K16 * exp(ETA(9));
double K61i = K61 * exp(ETA(10));
double K18i = K18 * exp(ETA(11));
double K81i = K81 * exp(ETA(12));
double K19i = K19 * exp(ETA(13));
double K91i = K91 * exp(ETA(14));

// Scaling factors (S = volume)
double S1 = V1i;
double S2 = V2i;
double S3 = V3;
double S4 = V4;
double S6 = V6;
double S8 = V8;
double S9 = V9;

[ODE]
double K23 = K25 / V2i;
double K32 = K52 / V9;
double K24 = K12i / V2i;
double K42 = K20i / V2i;

dxdt_CSF = -K13i*CSF + K31i*CERV_SPINAL - K14i*CSF + K41i*BRAIN 
           - K12i*CSF + K24*PLASMA - K16i*CSF + K61i*LUM_SPINAL 
           - K18i*CSF + K81i*THO_SPINAL - K19i*CSF + K91i*PONS;

dxdt_PLASMA = K12i*CSF - K42*PLASMA - K23*PLASMA + K32*PERIPH;

dxdt_CERV_SPINAL = K13i*CSF - K31i*CERV_SPINAL;

dxdt_BRAIN = K14i*CSF - K41i*BRAIN - K47*BRAIN + K74*DBT;

dxdt_PERIPH = K23*PLASMA - K32*PERIPH;

dxdt_LUM_SPINAL = K16i*CSF - K61i*LUM_SPINAL;

dxdt_DBT = K47*BRAIN - K74*DBT;

dxdt_THO_SPINAL = K18i*CSF - K81i*THO_SPINAL;

dxdt_PONS = K19i*CSF - K91i*PONS;

[OMEGA]
// Inter-individual variability (IIV % CV in parentheses)
0.395       // IIV K20 (%CV ≈ 79%)
1.32        // IIV K12 (%CV ≈ 115%)
0.854       // IIV V1 (%CV ≈ 92%)
0.734       // IIV V2 (%CV ≈ 91%)
0.42        // IIV K13 (%CV ≈ 65%)
0.101       // IIV K31 (%CV ≈ 32%)
13.8        // IIV K14 (%CV ≈ 368%)
0.121       // IIV K41 (%CV ≈ 35%)
0.123       // IIV K16 (%CV ≈ 35%)
0.102       // IIV K61 (%CV ≈ 32%)
0.199       // IIV K18 (%CV ≈ 45%)
0.118       // IIV K81 (%CV ≈ 35%)
0.598       // IIV K19 (%CV ≈ 77%)
0.285       // IIV K91 (%CV ≈ 53%)

[SIGMA]
0.761       // Residual error - CSF
0.48        // Residual error - Plasma
0.101       // Residual error - Cervical spinal cord
1.91        // Residual error - Brain
0.284       // Residual error - Thoracic spinal cord
0.0239      // Residual error - Lumbar spinal cord
0.0103      // Residual error - Pons

[TABLE]
// Calculate concentrations for each compartment
double CONC_CSF = CSF / V1i;
double CONC_PLASMA = PLASMA / V2i;
double CONC_CERV = CERV_SPINAL / V3;
double CONC_BRAIN = BRAIN / V4;
double CONC_THO = THO_SPINAL / V6;
double CONC_LUM = LUM_SPINAL;
double CONC_PONS = PONS / V9;
double IPRED =0;
// Determine IPRED based on CMT 
if(CMT == 1) {
  IPRED = CONC_CSF*(1+SIGMA(1));
}
if(CMT == 2) {
  IPRED = CONC_PLASMA*(1+SIGMA(2));
}
if(CMT == 3) {
  IPRED = CONC_CERV*(1+SIGMA(3));
}
if(CMT == 4) {
  IPRED = CONC_BRAIN*(1+SIGMA(4));
}
if(CMT == 6) {
  IPRED = CONC_THO*(1+SIGMA(5));
}
if(CMT == 8) {
  IPRED = CONC_LUM*(1+SIGMA(6));
}
if(CMT == 9) {
  IPRED = CONC_PONS*(1+SIGMA(7));
}

double DV = IPRED ;

[CAPTURE]
CONC_CSF
CONC_PLASMA
CONC_CERV
CONC_BRAIN
CONC_THO
CONC_LUM
CONC_PONS
