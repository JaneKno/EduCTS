//https://bpspubs.onlinelibrary.wiley.com/doi/10.1111/bcp.16046
// ASO PK
[PROB]
Nonlinear ASO PK model with peripheral compartment

[PARAM]
// Base parameters
KA = 0.203911      // 1/h, First-order absorption rate
VMAX = 8.37004     // mg/h, Maximum velocity
KM = 3.78999       // mg/L, Michaelis constant
VC = 12.2627       // L, Central volume
Q = 6.98515        // L/h, Intercompartmental clearance
VP = 4217.41       // L, Peripheral volume
F1 = 1             // Bioavailability

// Covariate effect parameters
WT = 80            // Reference weight
BWT_VMAX = 2.93349 // Body weight effect on VMAX
BWT_KM = 5.10549   // Body weight effect on KM
BWT_VC = 1.73118   // Body weight effect on VC

[CMT]
ABS      // Absorption compartment
CENTRAL  // Central compartment
PERIPH   // Peripheral compartment

[MAIN]
// Add ETA parameters
double KAi = KA * exp(ETA(1));
double VMAXi = VMAX * exp(ETA(2));
double KMi = KM * exp(ETA(3));
double VCi = VC * exp(ETA(4));
double Qi = Q * exp(ETA(5));
double VPi = VP * exp(ETA(6));
double F1i = F1 * exp(ETA(7));

// Update rate constants with individual parameters
double K23 = Qi/VCi;
double K32 = Qi/VPi;

// Weight-based scaling with individual parameters
double TVVMAX = VMAXi * pow(WT/80, BWT_VMAX);
double TVKM = KMi * pow(WT/80, BWT_KM);
double TVVC = VCi * pow(WT/80, BWT_VC);

[ODE]
double K20 = (TVVMAX * CENTRAL)/(TVKM + CENTRAL);

dxdt_ABS = -KA * ABS;
dxdt_CENTRAL = KA * ABS - K20 - K23 * CENTRAL + K32 * PERIPH;
dxdt_PERIPH = K23 * CENTRAL - K32 * PERIPH;

[OMEGA] 
// Inter-individual variability (IIV) estimates
0.0547462       // IIV KA (%CV = 24%)
0               // IIV VMAX (not estimated)
0.121828        // IIV KM (%CV = 36%)
0.0446265       // IIV VC (%CV = 24%)
0               // IIV Q (not estimated)
0.208619        // IIV VP (%CV = 47%)
0.0560617       // IIV F1 (%CV = 26%)

[TABLE]
double IPRED = CENTRAL/TVVC;
double DV = IPRED;

[CAPTURE]
IPRED
DV