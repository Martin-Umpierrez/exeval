$PROB
#Two compt model with first order absorption
# Model_IOV_TEST Corresponds to the in-house Tacrolimus model Implemented in Monolix for the same purpose
# mapbayr package: This package will use double ETAs, one is going to be utilized in estimation
# The other one for simulation purpose
# OCC is passed from the dataset in R so we define OCC as a covariate in $PARAM
# For each IOV term we have a
# 1) ETAn: 0 : IOV on CL (OCC = N) statement in the $PARAM
# 2) ETAn :  Value in the Omega Block
# 3) ETA(n) + ETAn in every single occasion
$GLOBAL
#define CP (CENT/iV2)
#define CT (PERIPH/iV3)
$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment
PERIPH : Peripheral compartment (mass)

$PARAM @annotated
CL  :  21 : clearance
V2  :  330 : central volume
Q  :  35.44: intercompartmental clearance
V3  : 118  :peripheral volume
KA  :  2.48: absorption rate constant
ETA1 : 0 : IIV CL(L/h)
ETA2 : 0 : IIV V2 (L)
ETA3 :  0: IIV V3 (L/h)
ETA4 : 0 : IIV Q (L/h)
ETA5 : 0 : IOV on Ka (OCC = 1)
ETA6 : 0 : IOV on Ka (OCC = 2)
ETA7 : 0 : IOV on Ka (OCC = 3)
ETA8 : 0 : IOV on Ka (OCC = 4)
ETA9 : 0 : IOV on Ka (OCC = 5)
ETA10 : 0 : IOV on kA (OCC = 6)
ETA11 : 0 : IOV on Cl (OCC = 1)
ETA12 : 0 : IOV on Cl (OCC = 2)
ETA13 : 0 : IOV on Cl (OCC = 3)
ETA14 : 0 : IOV on Cl (OCC = 4)
ETA15 : 0 : IOV on Cl (OCC = 5)
ETA16 : 0 : IOV on Cl (OCC = 6)


$PARAM @annotated @covariate
LBW      : 60   : Baseline lea body weight (kg)
CYP3A5      : 3  : Most Common metabolizer in Cauccasicans Patients
HCT      : 33.5  : hematocrito
OCC     : -99  : Occasion, shall be passed by dataset imported

$ODE
dxdt_EV = -iKA*EV;
dxdt_CENT = iKA*EV  - iCL*CP - iQ*CP + iQ*CT;
dxdt_PERIPH = iQ*CP  - iQ*CT;

$MAIN

double IOVKA = 0;
if (OCC == 1) {
  IOVKA = ETA(5) + ETA5;
} else if (OCC == 2) {
 IOVKA = ETA(6) + ETA6 ;
} else if (OCC == 3) {
 IOVKA = ETA(7) + ETA7 ;
} else if (OCC == 4) {
IOVKA = ETA(8) + ETA8 ;
} else if (OCC == 5) {
IOVKA = ETA(9) + ETA9 ;
} else if (OCC == 6) {
IOVKA = ETA(10) + ETA10 ;
}

double IOVCL = 0;
if (OCC == 1) {
  IOVCL = ETA(11) + ETA11;
} else if (OCC == 2) {
 IOVCL = ETA(12) + ETA12 ;
} else if (OCC == 3) {
 IOVCL = ETA(13) + ETA13 ;
} else if (OCC == 4) {
IOVCL = ETA(14) + ETA14 ;
} else if (OCC == 5) {
IOVCL = ETA(15) + ETA15 ;
} else if (OCC == 6) {
IOVCL = ETA(16) + ETA16 ;
}

##CYP3A5 effect on Cl##
double HM = 2 ;  ####METABOLIZADOR ULTRA RAPIDO ####
double IM = 1.8 ;  ####METABOLIZADOR INTERMEDIO ####
double PM = 1 ;  ####METABOLIZADOR POBRE ####
if(CYP3A5==1) double CL_EFFECT = HM ;
if(CYP3A5==2) CL_EFFECT = IM ;
if(CYP3A5==3) CL_EFFECT = PM ;

double V_LBW    = 1;
double CL_LBW = 0.75;
double CL_HCT    = -1;

double iCL =  CL * CL_EFFECT * pow((HCT/33.5), CL_HCT) * pow((LBW / 60), CL_LBW) * exp(ETA(1) + ETA1 + IOVCL) ;
double iV2 =  V2 * pow((LBW / 60), V_LBW) * exp(ETA(2)+ ETA2);
double iQ =  Q* exp(ETA(3)+ ETA3) * pow((LBW / 60), CL_LBW);
double iV3 =  V3 * (ETA(4) + ETA4) * pow((LBW / 60), V_LBW);
double iKA =  KA * exp(IOVKA) ;



$OMEGA @name IIV
0.1296
1.41
0.3364
0.6724
$OMEGA  @name IOV
0.7396
0.7396
0.7396
0.7396
0.7396
0.7396
0.0676
0.0676
0.0676
0.0676
0.0676
0.0676
$SIGMA  @name SIGMA @annotated
ADD : 0 : ADD residual error
PROP : 0.04 : Proportional residual error


$TABLE
double IPRED = CENT/iV2;
double DV = IPRED* (1+PROP) ;

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
CT : Peripheral concentration (mass/volume)
iCL :  Clearance
iV2 : :Central Volume
iKA : KA: absorption rate constant
iQ  : intercompartmental clearance
iV3 : peripheral volume
IOVKA : IOV KA
IOVCL : IOV CL
OCC: OCCASION
DV : PREDICCION


