$PROB
# One Comparment Model with first order absorption- Ka is FIXED
$GLOBAL
#define CP (CENT/iV)
$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment#two compt model with first order absorption

$PARAM @annotated
CL  :  24.13 : Clearance for CYP3A5*3*3
V  :  716 : central volume
KA  : 4.5 : absorption rate constant
ETA1 : 0 : IIVCl (L/h)
ETA2 : 0 : IIVV (L)

$PARAM @annotated @covariate
POD    : 0   : COV POST OPERATIVE DAY
HCT      : 0  : COV HCH
WT      : 0  : COV WT
CYP3A5      : 0  : Polimorfismo CYP3A5
OCC     : -99  : Occasion, shall be passed by dataset imported

$ODE
dxdt_EV = -iKA*EV;
dxdt_CENT = iKA*EV  - iCL*CP;

$MAIN
##CYP3A5 effect on Cl##

double HM = 1.186 ;  ####Rapid Metabolizer ####
double IM = 1.13 ;  ####Intermediate Metabolizer ####
double PM = 1 ;  ####Poor Metabolizer####

if(CYP3A5==1) double CL_EFFECT = HM ;
if(CYP3A5==2) CL_EFFECT = IM ;
if(CYP3A5==3) CL_EFFECT = PM ;

double CL_HCT1 = 1.3458 ; ##Effect of HCT< 33
double CL_HCT2 = 1.124 ;  ##Effect of HCT >33

if(HCT< 33) double CL_HCT = CL_HCT1 ;
if(HCT >= 33) CL_HCT = CL_HCT2 ;

double CL_POD = - 0.00762 ;

double iCL =  CL *exp(ETA(1) + ETA1)* pow(POD, CL_POD) * CL_EFFECT * CL_HCT ;
double iV =  V *exp(ETA(2) + ETA2) * exp (0.355*WT/59.025) ;
double iKA =  KA ;


$OMEGA @name IIV
0.248
0.237

$SIGMA  @name SIGMA @annotated
ADD : 0 : ADD residual error
PROP : 0.16 : Proportional residual error


$TABLE
double IPRED = CENT/iV;
double DV = IPRED * (1 + PROP) ;

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
iCL :  Clearance
iV : :Central Volume
iKA : KA: absorption rate constant
EVID : EVENT ID
DV : PREDICCION
OCC: OCCASION
