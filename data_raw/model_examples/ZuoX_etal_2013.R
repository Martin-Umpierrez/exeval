#---------------Zuo et al no CYP3A4------------------------------------------
ZuoX_etalfull_noCYP3A4 <-
  '$PROB
# One Comparment Model with first order absorption-
# CYP3A4*1G VARIABILITY
*1*1-------- n=73 ---- 45 %
*1*1G------- n=78----
*1G*1G------ n=10----- Combined 55 %

$GLOBAL
#define CP (CENT/iV)
$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment


$PARAM @annotated
CL  :  26.6 : clearance (L/h)
V  :  1020   : central volume (L)
KA  : 3.09 : absorption rate constant (h-1)
ETA1 : 0 : Cl (L/h)
ETA2 : 0 : V (L/h)

$PARAM @annotated @covariate
HCT   : 0   : HEMATOCRITO ON CL
EXPRESSION      : 0  : If a patients has one *1 (0=no, 1=yes)
OCC     : -99  : Occasion, shall be passed by dataset imported

$MAIN

double HM = 1.1074 ;  ####High Metbolizer#### Considering CYP3A4 proportion
double PM = 0.68315 ;  ####Poor Metabolizer#### Considering CYP3A4 proportion

if(EXPRESSION== 1 )  double CL_EFFECT = HM  ;
if(EXPRESSION== 0 )  CL_EFFECT = PM  ;

double CL_HCT = - 0.451 ;  #### Hematocrit Effect on Clerance

double iCL =  CL * exp(ETA1 + ETA(1)) * CL_EFFECT * pow((HCT/27.9), CL_HCT)  ;
double iV =  V * exp(ETA2 + ETA(2)) ;
double iKA =  KA ;

$ODE
dxdt_EV = -iKA*EV;
dxdt_CENT = iKA*EV  - iCL*CP;


$OMEGA @name IIV
0.0569
0.294

$SIGMA  @name SIGMA @annotated
ADD : 2.1609 : ADD residual error
PROP : 0.0392 : Proportional residual error

$TABLE
double IPRED = CENT/iV;
double DV = IPRED * (1 + PROP) + ADD ;

$CAPTURE @annotated
OCC : OCCASION
CP : Plasma concentration (mass/volume)
iCL :  Clearance
iV : :Central Volume
iKA : KA: absorption rate constant
EVID : EVENT ID
DV : PREDICCION
OCC: OCCASION

               '





