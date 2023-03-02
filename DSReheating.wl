(* ::Package:: *)

BeginPackage["DSReheating`"];


solEqs::usage="solEqs: solutions to the reheating equations, interpolated as functions of (dimensionless) time.\nInput: (\[Gamma],w\[Chi],xLarge,prec,accu).\nOutput: {\!\(\*SubscriptBox[\(r\), \(\[Chi]\)]\), \!\(\*SubscriptBox[\(r\), \(r\)]\)}";
hub::usage="hub: the dimensionless Hubble expansion rate, as a function of (dimensionless) time.\nInput: (x).\nOutput: h(x)";
HOfx::usage="HOfx: Hubble expansion rate interpolated as a function of (dimensionless) time.\nInput: (\[Gamma],\!\(\*SubscriptBox[\(H\), \(i\)]\),w\[Chi],xLarge,prec,accu).\nOutput: H";
TOfx::usage="TOfx: dark sector temperature interpolated as a function of (dimensionless) time/\nInput: (\[Gamma],\!\(\*SubscriptBox[\(g\), \(*\)]\),\!\(\*SubscriptBox[\(H\), \(i\)]\),w\[Chi],xLarge,prec,accu).\nOutput: T";
aOfx::usage="aOfx: function to compute scale factor\nInput: (\[Gamma], \!\(\*SubscriptBox[\(a\), \(ini\)]\), sum, w\[Chi], xLarge, prec, accu).\nOutput: a";
tauOfx::usage="tauOfx: function to compute comoving time\nInput: (\[Gamma], \!\(\*SubscriptBox[\(t\), \(scale\)]\), \!\(\*SubscriptBox[\(a\), \(ini\)]\), sum, w\[Chi], xLarge, prec, accu).\nOutput: \[Tau]";
redshift::usage="redshift: function to compute the redshift \!\(\*SubscriptBox[\(z\), \(*\)]\)+1 from today to an arbitrary point \!\(\*SubscriptBox[\(x\), \(*\)]\) in the past during reheating.\nInput: (\[Gamma],\!\(\*SubscriptBox[\(H\), \(i\)]\),xRange,{\!\(\*SubsuperscriptBox[\(g\), \(d\), \(UV\)]\),\!\(\*SuperscriptBox[\(g\), \(UV\)]\)},{\!\(\*SubsuperscriptBox[\(g\), \(d\), \(IR\)]\),\!\(\*SubscriptBox[\(g\), \(d, 0\)]\),\!\(\*SuperscriptBox[\(g\), \(IR\)]\)},instantVSRH,\!\(\*SubscriptBox[\(w\), \(\[Chi]\)]\),xLarge,prec,accu,debug).\nOutput: \!\(\*SubscriptBox[\(z\), \(*\)]\)+1=\!\(\*SubsuperscriptBox[\(a\), \(*\), \(-1\)]\)";
xCross::usage="xCross: function to compute the two crossing times at which the DS temperature reaches certain value \!\(\*SubscriptBox[\(T\), \(*\)]\), as well as the time at which it reaches the maximum temperature.\nInput: (\[Gamma],\!\(\*SubscriptBox[\(r\), \(r, c\)]\),w\[Chi],xLarge,prec,accu).\nOutput: {\!\(\*SubscriptBox[\(x\), \(c1\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\),\!\(\*SubscriptBox[\(x\), \(c2\)]\)}";
gammaRate::usage="gammaRate: function to compute the log derivagive of the dark sector temperature at some time \!\(\*SubscriptBox[\(x\), \(*\)]\).\nInput: (\[Gamma],\!\(\*SubscriptBox[\(x\), \(*\)]\),w\[Chi],\[Delta],xLarge,prec,accu).\nOutput: \!\(\*FractionBox[\(d\\\ ln\\\ T\), \(d\\\ ln\\\ x\)]\)(\!\(\*SubscriptBox[\(x\), \(*\)]\))";
gsFn::usage="gsFn: function interpolating the Standard Model's relativistic degrees of freedom in entropy."
RHDict::usage="RHDict: dictionary of analytic expressions relevant for reheating.";


Begin["`Private`"];


(* ::Title:: *)
(*DSReheating.wl*)


(* ::Text:: *)
(*WRITTEN BY: MANUEL A. BUEN-ABAD, 2022.*)
(**)
(*This is a code that computes various quantities associated with the reheating of a Dark Sector (DS) due to the decay of a reheaton \[Chi], which dominates the Universe in an early matter-domination (EDM) era. For the derivation of these hard-coded results, see notebook 01_reheating.nb.*)


(* ::Chapter:: *)
(*Preamble*)


(* ::Section:: *)
(*Directory*)


SetDirectory[NotebookDirectory[]];


(* ::Section:: *)
(*Package*)


Needs["DifferentialEquations`InterpolatingFunctionAnatomy`"];


(* ::Section:: *)
(*Assumptions*)


$Assumptions=a>amin>0&&x>xmin>0&&w\[Chi]>-1&&y>ymin>0&&\[Gamma]>0&&\[Gamma]i>0&&\[Gamma]d>0&&f>1&&mPL>0&&gstar>0&&Arho>0


(* ::Section:: *)
(*Definitions*)


Clear[GeV,MeV,Mpl,mpl,second,Hz,mHz,h,\[Rho]crit,H0,H0h1,T\[Gamma]0,gs0,ge0,gSM,smallnum,A\[Rho],\[Rho]\[Chi]i]


(* ::Subsection:: *)
(*Units*)


GeV=1;eps
MeV=10^-3 GeV;
Mpl=1.22091 10^19 GeV;
mpl=Mpl/Sqrt[8\[Pi]];
second=1.519268 10^21/MeV;
Hz=1/second;
mHz=10^-3 Hz;


(* ::Subsection:: *)
(*Constants*)


h=0.67;
\[Rho]crit=8.0992*10^-47*h^2 GeV^4;
H0=Sqrt[\[Rho]crit/(3mpl^2)];
H0h1=H0/h;

T\[Gamma]0=(2.4*10^-4)*10^-9 GeV;
gs0=3.94;
ge0=3.38;
gSM=106.75;

smallnum=10^-6;


(* ::Subsection:: *)
(*Useful*)


A\[Rho]=gstar \[Pi]^2/30;
\[Rho]\[Chi]i=3 Hi^2 mpl^2;

gsdata=Import["./data/entropy_dof.csv"];
gsInt=Interpolation[gsdata,InterpolationOrder->1];
gsDomain=(gsInt//FullForm)[[1,1]]//Flatten;
gsFn[T_]:=Which[T<gsDomain[[1]],gs0,T<=gsDomain[[2]],gsInt[T],T>gsDomain[[2]],gSM]


(* ::Chapter:: *)
(*Reheating: Dark and Visible Sectors (DS & VS)*)


(* ::Text:: *)
(*We will be solving for a two-fluid Universe: one a \[Chi] fluid that starts to decay at some time t_d (equivalently, scale factor a_d and Hubble scale H_d), and the other is made of the dark radiation decay products.*)
(**)
(*The evolution equations are:*)
(**)
(*d\[Rho]_\[Chi]/dt +3H \[Rho]_\[Chi] (1+w_\[Chi])+\[CapitalGamma]*\[Rho]_\[Chi]=0*)
(*d\[Rho]_r/dt +4H \[Rho]_r -\[CapitalGamma]*\[Rho]_\[Chi]=0*)
(**)
(*It may be convenient to rewrite everything in dimensionless quantities, normalized to the value of \[Rho]_\[Chi]d at this time t_d when the decays start:*)
(**)
(*(1)   (d r_\[Chi])/(d x)+3 h r_\[Chi] (1+w_\[Chi])+\[Gamma] r_\[Chi]=0*)
(*(d r_r)/(d x)+4 h r_r-\[Gamma] r_\[Chi]=0*)
(**)
(*where we have defined*)
(**)
(*x\[Congruent]t\[CenterDot]H_d*)
(*H_d\[Congruent]Sqrt[\[Rho]_\[Chi]d/(3 m_Pl^2)]*)
(*r\[Congruent]\[Rho]/\[Rho]_\[Chi]d*)
(*h\[Congruent]H/H_d=Sqrt[r_\[Chi]+r_r]*)
(*\[Gamma]_d\[Congruent]\[CapitalGamma]/H_d*)
(**)
(*with m_Pl being the reduced Planck mass: m_Pl\[Congruent]M_Pl/Sqrt[8\[Pi]].*)
(**)
(*Defining y\[Congruent]ln(a/a_d) note that, since x\[Congruent]t\[CenterDot]H_d, then*)
(**)
(*(d y)/(d x)=1/a (d a)/(d t H_d)=H/H_d=h(y), and therefore:*)
(**)
(*x(y) = \[Integral]_y_i^y  dy'/h(y')*)
(**)
(*where y=y_i<=0 corresponds to an initial scale factor a_i<=a_d.*)
(**)
(*Note in particular that x(y=0)=H_d t(a_d)=H_d t_d is not necessarily equal to 1. This is because in the inverse of the Hubble expansion is not in general equal to time! To relate t_d and H_d we would need to know the history of the Universe prior to t_d, something about which we declare ourselves to be agnostic.*)
(**)
(*In fact, we will choose t_d=0, which does not mean H_d=0. Note that this choice of t_d is immaterial and has no impact on the physics: it is simply a choice for the start of the clock. All the physics related to the expansion of the Universe (e.g. redshift) will be taken relative to this time.*)
(**)
(*Defining A_\[Rho]\[Congruent](\[Pi]^2 g'_* )/30 then \[Rho]_r=A_\[Rho] T^4.*)
(**)
(*We can also define the factor f: \[Rho]_\[Chi]d\[Congruent]f^4 \[Rho]_rc, i.e. the initial \[Rho]_\[Chi] in terms of \[Rho]_r at the critical temperature. Thus, H_d^2=f^4 \[Rho]_rc/(3 m_Pl^2) and, since \[Rho]_\[Chi]>\[Rho]_r for all times:*)
(*\[Rho]_\[Chi]>\[Rho]_r \[Implies] f^4 \[Rho]_rc=f^4 A_\[Rho] T_c^4 > A_\[Rho] T^4\[Implies]f>T/T_c, and in particular f>1.*)
(**)
(*\[Implies] r_r\[Congruent]\[Rho]_r/\[Rho]_\[Chi]=\[Rho]_r/(f^4 \[Rho]_rc)=(T/(f T_c))^4, and r_rc=f^-4.*)


(* ::Section:: *)
(*Numerical Solution*)


(* ::Subsection:: *)
(*\[Chi]-domination: time as a function of scale factor, x(a)=H_d\[CenterDot]t(a)*)


(* ::Text:: *)
(*It is illuminating, although not necessary, to compute x(a) during an era of reheaton-domination (\[Chi]-domination, with a constant equation of state w_\[Chi]), assuming that the reheaton decays are negligible or even forbidden (if, for example, the decay products have a mass larger than the reheaton's mass at this time; their mass could afterwards change due to a phase transition).*)


Clear[xOfa]
xOfa[wchi_,aa_]:=Block[{w\[Chi]=wchi,a=aa},(2 a^((3 (1+w\[Chi]))/2))/(3 (1+w\[Chi]))]


(* ::Text:: *)
(*The (dimensionless) comoving time \[Tau]bar during reheaton domination, from x=0 to x(a=1):*)
(**)
(*\[Tau]bar_i\[Congruent]\[Integral]_0^x(a=1)  dx'/a(x').*)


Clear[taubarini]

taubarini[wchi_]:=Block[{w\[Chi]=wchi},2/(1+3 w\[Chi])]


(* ::Subsection:: *)
(*Reheating Equations & Numerical Solution (in terms of x=t\[CenterDot]H_d)*)


(* ::Text:: *)
(*First, solving for r\[Chi] at some x<xd at which the integration starts, by demanding that r\[Chi] at x=0 is equal to 1. Since x<xd there are no decays yet.*)


Clear[hub,eqsX,solEqs]

(*start the clock with x=0 at...*)
(*(*the beginning of a presumed \[Chi]-domination, before the decays are significant, or maybe before they are even allowed*)
xd=xOfa[w\[Chi],1];*)

(*decay time*)
xd=0;

(*dimensionless hubble*)
hub[x_]:=Sqrt[r\[Chi][x]+rr[x]]

(*evolution equations (in terms of x=t*Hd). Note that we have killed the decay term when r_\[Chi]<<r_r, in order to afford larger values of x*)
eqsX={D[r\[Chi][x],x]+3hub[x]r\[Chi][x](1+w\[Chi])+\[Gamma]d r\[Chi][x]*HeavisideTheta[r\[Chi][x]-rr[x]*smallnum^2]==0,D[rr[x],x]+4hub[x]rr[x]-\[Gamma]d r\[Chi][x]*HeavisideTheta[r\[Chi][x]-rr[x]*smallnum^2]==0,r\[Chi][xd]==1,rr[xd]==0};

(*the solution to the system of equations*)
solEqs[gg_?NumberQ,wx_:0,xLarge_:10^10,prec_:30,accu_:20]:=solEqs[gg,wx,xLarge,prec,accu]=Block[{\[Gamma]d=Rationalize[gg],w\[Chi]=wx,xmin,xmax,sols,dens,r\[Chi]d,hOfx,hd,res},

xmax=If[\[Gamma]d<1,Max[xLarge,xd+100/gg],xLarge];

(*initial time*)
xmin=xd;

(*solving differential equations*)
sols=NDSolve[eqsX,{r\[Chi],rr},{x,xmin,xmax},WorkingPrecision->prec,AccuracyGoal->accu]//Flatten;

(*densities*)
dens={r\[Chi],rr}/.sols;

(*value of r\[Chi] at x=xd*)
r\[Chi]d=dens[[1]][xd];

(*h(x): dimensionless hubble as a function of x*)
hOfx=hub[x]/.{r\[Chi]->dens[[1]],rr->dens[[2]]};

(*h(x_d): dimensionless hubble at decay time a=1*)
hd=(hOfx/.{x->xd});

(*results*)
res={dens[[1]],dens[[2]]}/.sols;

res
]


(* ::Subsection:: *)
(*Hubble H(x)*)


(* ::Text:: *)
(*Computing the Hubble expansion rate as a function of time, assuming a certain Hubble scale H_scale.*)


Clear[HOfx]

HOfx[gg_?NumberQ,Hscale_,wx_:0,xLarge_:10^3,prec_:30,accu_:20]:=HOfx[gg,Hscale,wx,xLarge,prec,accu]=Block[{Hi=Hscale,w\[Chi]=wx,r\[Chi],rr,xData,yData,data,Hfnx},

(*solving the reheating equations*)
{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];

(*getting the hubble parameter*)
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);
yData=Table[hub[x],{x,xData}];
yData=Hi*yData;

(*building the dataset*)
data={xData,yData}//Transpose;
Hfnx=Interpolation[data];

Hfnx
]


(* ::Subsection:: *)
(*Temperature T(x)*)


(* ::Text:: *)
(*Computing the temperature as a function of time, assuming a certain Hubble scale H_scale and a certain number of degrees of freedom g_*.*)


Clear[TOfx]

TOfx[gg_?NumberQ,gs_,Hscale_,wx_:0,xLarge_:10^3,prec_:30,accu_:20]:=TOfx[gg,gs,Hscale,wx,xLarge,prec,accu]=Block[{gstar=gs,Hi=Hscale,w\[Chi]=wx,r\[Chi],rr,xData,yData,data,Tfnx},

(*solving the reheating equations*)
{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];

(*getting the temperature*)
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[rr]//Flatten);
yData=((\[Rho]\[Chi]i*yData)/A\[Rho])^(1/4);

(*building the dataset*)
data={xData,yData}//Transpose;
Tfnx=Interpolation[data];

Tfnx
]


(* ::Subsection:: *)
(*Scale factor a(x)*)


(* ::Text:: *)
(*Computing the scale factor as a function of time.*)


Clear[aOfx]

aOfx[gg_?NumberQ,aini_:1,sum_:False,wx_:0,xLarge_:10^3,prec_:30,accu_:20]:=aOfx[gg,aini,sum,wx,xLarge,prec,accu]=Block[{w\[Chi]=wx,r\[Chi],rr,domain,xData,efolds,yData,data,afnx},

(*solving the reheating equations*)
{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];

(*interpolation function properties*)
domain=(InterpolatingFunctionDomain[rr]//Flatten);
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);

(*e-fold function*)
If[sum,
efolds[1]=0;
efolds[i_]:=efolds[i]=efolds[i-1]+((hub[xData[[i]]]+hub[xData[[i-1]]])/2)*(xData[[i]]-xData[[i-1]]),
efolds[1]=0;
efolds[i_]:=efolds[i]=efolds[i-1]+NIntegrate[hub[xx],{xx,xData[[i-1]],xData[[i]]}]
];

(*computing e-folds*)
yData=Monitor[Table[efolds[i],{i,xData//Length}],ProgressIndicator[i,{1,xData//Length}]];
Clear[efolds];

(*computing scale factor*)
yData=aini*Exp[yData];

(*final data & interpolation*)
data={xData,yData}//Transpose;
afnx=Interpolation[data];

afnx
]


(* ::Subsection:: *)
(*Comoving time\[Tau](x)*)


(* ::Text:: *)
(*Computing the comoving time since x_i, with a given timescale t_scale:*)
(**)
(*\[Tau](x)-\[Tau]_i = t_scale*\[Integral]_x_i^x dx'/a(x').*)


Clear[tauOfx]

tauOfx[gg_?NumberQ,tscale_:1,aini_:1,sum_:False,wx_:0,xLarge_:10^3,prec_:30,accu_:20]:=tauOfx[gg,tscale,aini,sum,wx,xLarge,prec,accu]=Block[{w\[Chi]=wx,taui,ax,domain,xData,tau,yData,data,taufnx},

(*initial conformal time*)
taui=If[xd==0,0,taubarini[w\[Chi]]];

(*finding the scale factor a(x)*)
ax=aOfx[gg,aini,sum,wx,xLarge,prec,accu];

(*interpolation function properties*)
domain=(InterpolatingFunctionDomain[ax]//Flatten);
xData=(InterpolatingFunctionCoordinates[ax]//Flatten);

(*comoving time difference function*)
If[sum,
tau[1]=0;
tau[i_]:=tau[i]=tau[i-1]+((ax[xData[[i]]]^-1+ax[xData[[i-1]]]^-1)/2)*(xData[[i]]-xData[[i-1]]),
tau[1]=0;
tau[i_]:=tau[i]=tau[i-1]+NIntegrate[ax[xx]^-1,{xx,xData[[i-1]],xData[[i]]}];
];

(*computing the comoving time difference*)
yData=Monitor[Table[tau[i],{i,xData//Length}],ProgressIndicator[i,{1,xData//Length}]];
Clear[tau];

(*adding initial comoving time and putting in the timescale*)
yData=tscale*(taui+yData);

(*final data & interpolation*)
data={xData,yData}//Transpose;
taufnx=Interpolation[data];

taufnx
]


(* ::Subsection:: *)
(*Redshift*)


(* ::Text:: *)
(*We compute the total redshift (1+z) from today to the time when the GW were generated. For simplicity, it's easier to start from the earliest times. We break the computation into three parts:*)
(*1. Redshift from GW generation until some time during the Dark Sector (DS) Radiation Domination (RD) era. Part of this process takes place during DS reheating (DSRH).*)
(*2. Redshift from this time until after the Visible Sector (VS) is reheated by the DS (VSRH).*)
(*3. Redshift from VSRH time until today.*)
(**)
(*For #1 the ratio a(t_ 2)/a(t_ 1)=(1+z_ 1)/(1+z_ 2) of scale factors between two times t_ 1<t_ 2 during DS reheating is simply equal to*)
(**)
(*a(t_ 2)/a(t_ 1)= exp[\[Integral]_x_1^x_2 dx' h(x') ].*)
(**)
(*For #2, VSRH can take place instantaneously or adiabatically. So we use either energy- or entropy-conservation as our guides.*)


Clear[redshift]

redshift[gg_?NumberQ,Hscale_,xRange_List,gUVs_List,IRs_List:{Automatic,0,gSM},instant_:True,wx_:0,xLarge_:10^3,prec_:30,accu_:20,debug_:False]:=redshift[gg,Hscale,xRange,gUVs,IRs,instant,wx,xLarge,prec,accu,debug]=Block[{Hi=Hscale,w\[Chi]=wx,xStar,xm,r\[Chi],rr,efolds,Tdm,gdUV,gUV,gstar,gdIR,gd0,gIR,TIR=10^2,Gfactor,Z1,Z2,Z3,res},

(*the time-range between GW production at time x_*, and an arbitrary point during DS-RD era (x_m)*)
{xStar,xm}=xRange;

(*computing the efolds between x_* and x_m*)
{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];
efolds=NIntegrate[hub[x],{x,xStar,xm}];

(*the redshift factor between x_* and x_m*)
Z1=Exp[efolds];

(*total plasma UV d.o.f.: g_*^UV=g_d^UV+g^UV*)
{gdUV,gUV}=gUVs;
gstar=gdUV+gUV;

(*computing the DS temperature at x_m*)
Tdm=TOfx[gg,gstar,Hscale,wx,xLarge,prec,accu][xm];

(*VSRH (IR) quantities: DS IR d.o.f., DS d.o.f. today, VS IR d.o.f.*)
{gdIR,gd0,gIR}=IRs;
gdIR=If[gdIR===Automatic,gdUV,gdIR];

(*computing the entropy dump from DS reheating the VS; whether instantaneously (for a non-equilibrium process, but energy-conserving and relativistic) or adiabatically (for an equilibrium process, entropy-conserving; e.g. freeze-out)*)
Gfactor=If[instant,(gstar/(gdIR+gIR))^(1/4),(gstar/(gdIR+gIR))^(1/3)];

(*the redshift factor between x_m and x_IR, at some arbitrary time shortly after VS reheating*)
Z2=Gfactor*Tdm/TIR;

(*the redshift factor from the time of VS reheating down until today*)
Z3=((gIR+(gdIR-gd0))/gs0)^(1/3)*TIR/T\[Gamma]0;

(*the total redshift. Note that the explicit dependence on the VS temperature TIR after reheating from the DS cancels out; it only appears in gIR(TIR).*)
res=If[debug,{Z1*Z2*Z3,Z1,Z2,Z3},Z1*Z2*Z3];

res
]


(* ::Subsection:: *)
(*Crossings*)


Clear[xCross]

xCross[gg_?NumberQ,rCrit_,wx_:0,xLarge_:10^3,prec_:30,accu_:20]:=xCross[gg,rCrit,wx,xLarge,prec,accu]=Block[{w\[Chi]=wx,r\[Chi],rr,xA,xB,xData,yData,xpk,xLSet,yLSet,xRSet,yRSet,xL0,xR0,xL,xR,eps=smallnum},

{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];
{xA,xB}=(InterpolatingFunctionDomain[rr]//Flatten);
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[rr]//Flatten);

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[eps,xData[[2]]],Min[xData[[2]],xA*(1+eps)]];

xpk=10^(LX/.FindMaximum[rr[10^LX]//Log10,{LX,0,Log10[xA],Log10[xB]},MaxIterations->10^6,WorkingPrecision->prec][[-1]]);

(*the initial guesses for the Left/Right crossing times*)
xLSet=Select[xData,(#<xpk)&];
yLSet=yData[[1;;Length@xLSet]];
yLSet=Abs[yLSet-rCrit];
xL0=(Position[yLSet,Min[yLSet]]//Flatten)[[1]];
xL0=xLSet[[xL0]];

xRSet=Select[xData,(#>xpk)&];
yRSet=yData[[(Length@yData-Length@xRSet)+1;;-1]];
yRSet=Abs[yRSet-rCrit];
xR0=(Position[yRSet,Min[yRSet]]//Flatten)[[1]];
xR0=xRSet[[xR0]];
Clear[xLSet,xRSet,yLSet,yRSet,xData,yData];

xL=10^(LX/.FindRoot[Log10[rr[10^LX]]==Log10[rCrit],{LX,Log10[xL0],Log10[xA],Log10[xpk]},MaxIterations->10^6,WorkingPrecision->prec]//Quiet);
xR=10^(LX/.FindRoot[Log10[rr[10^LX]]==Log10[rCrit],{LX,Log10[xR0],Log10[xpk],Log10[xB]},MaxIterations->10^6,WorkingPrecision->prec]//Quiet);

{xL,xpk,xR}
]


(* ::Subsection:: *)
(*Find rate \[Gamma]*)


Clear[gammaRate]

gammaRate[gg_?NumberQ,xCrit_,wx_:0,\[Delta]_:10^-6,xLarge_:10^3,prec_:30,accu_:20]:=gammaRate[gg,xCrit,wx,\[Delta],xLarge,prec,accu]=Block[{r\[Chi],rr,w\[Chi]=wx,\[Gamma]},

{r\[Chi],rr}=solEqs[gg,wx,xLarge,prec,accu];

\[Gamma]=(xCrit-xd)/rr[xCrit]^(1/4)*(rr[(1+\[Delta])xCrit]^(1/4)-rr[xCrit]^(1/4))/(\[Delta] xCrit);

\[Gamma]
]


(* ::Section:: *)
(*Analytical Approximations*)


(* ::Text:: *)
(*We define a dictionary of hard-coded approximations, RHDict, which we can call outside this package.*)


(* ::Subsection:: *)
(*Units, Constants, and Useful Definitions*)


(* ::Subsubsection:: *)
(*Units*)


RHDict["GeV"]=GeV;
RHDict["MeV"]=MeV;
RHDict["Mpl"]=Mpl;
RHDict["mpl"]=mpl;
RHDict["second"]=second;
RHDict["Hz"]=Hz;
RHDict["mHz"]=mHz;


(* ::Subsubsection:: *)
(*Constants*)


RHDict["h"]=h;
RHDict["\[Rho]crit"]=\[Rho]crit;
RHDict["H0"]=H0;
RHDict["H0h1"]=H0h1;
RHDict["T\[Gamma]0"]=T\[Gamma]0;
RHDict["gs0"]=gs0;
RHDict["ge0"]=ge0;
RHDict["gSM"]=gSM;


(* ::Subsubsection:: *)
(*Useful*)


RHDict["A\[Rho]"]=A\[Rho];
RHDict["\[Rho]\[Chi]i"]=\[Rho]\[Chi]i;


(* ::Subsection:: *)
(*r_rc*)


(* ::Text:: *)
(*r_rc\[Congruent]r_r(x_c)=f^-4:*)


RHDict["rrc"]=f^-4;


(* ::Subsection:: *)
(*Approximate Solutions & Useful Benchmarks*)


(* ::Subsubsection:: *)
(*\[Chi]D: reheaton domination (r_r<<r_\[Chi])*)


RHDict["solsAn"]["\[Gamma]<<1","\[Chi]D"]={r\[Chi][x]->(8-2 x (-6+(4+3 x) \[Gamma]))/(2+3 x)^3,rr[x]->4/5 (-((2 2^(2/3))/(2+3 x)^(8/3))+1/(2+3 x)) \[Gamma]};
RHDict["solsAn"]["\[Gamma]>>1","\[Chi]D"]={r\[Chi][x]->1-x (3+\[Gamma]),rr[x]->x \[Gamma]};


(* ::Subsubsection:: *)
(*\[Chi]R equality: r_eq\[Congruent]r_r(x_eq)=r_\[Chi](x_eq)*)


RHDict["xEq"]["\[Gamma]<<1"]=2/3+10/(11 \[Gamma]);
RHDict["rEq"]["\[Gamma]<<1"]=(66 \[Gamma]^2)/(15+11 \[Gamma])^2;

RHDict["xEq"]["\[Gamma]>>1"]=1/(2 \[Gamma]);
RHDict["rEq"]["\[Gamma]>>1"]=1/2-3/(4 \[Gamma]);


(* ::Subsubsection:: *)
(*RD: radiation domination (r_r>>r_\[Chi])*)


RHDict["solsAn"]["\[Gamma]<<1","RD"]={r\[Chi][x]->1/2 (33/2)^(1/4) E^(10/11+(2 \[Gamma])/3-x \[Gamma]) Sqrt[\[Gamma]/(x^3 (15+11 \[Gamma]))],rr[x]->1/(4 x^2)};

RHDict["solsAn"]["\[Gamma]>>1","RD"]={r\[Chi][x]->(E^(1/2-x \[Gamma]) (-3+2 \[Gamma]))/(4 \[Gamma]),rr[x]->1/(1+2 x)^2};


(* ::Subsubsection:: *)
(*L_r\[Congruent]1/4 (d ln r_r)/(d x)=(d ln T)/(d x)*)


RHDict["Lr"]["\[Gamma]<<1","\[Chi]D"]=-(2/(2+3 x))+5/(4 (2+3 x-(2 2^(2/3))/(2+3 x)^(2/3)));
RHDict["Lr"]["\[Gamma]<<1","RD"]=-(1/(2 x));

RHDict["Lr"]["\[Gamma]>>1","\[Chi]D"]=1/(4 x);
RHDict["Lr"]["\[Gamma]>>1","RD"]=1/(-1-2 x);


(* ::Subsubsection:: *)
(*Maximum: r_rmax\[Congruent]r_r(x_max)*)


RHDict["xMax"]["\[Gamma]<<1"]=0.5341867009072119;
RHDict["rrMax"]["\[Gamma]<<1"]=2/3 (3/8)^(8/5) \[Gamma];

RHDict["xMax"]["\[Gamma]>>1"]=-(1/(4 \[Gamma]))+1/(2 Sqrt[\[Gamma]]);
RHDict["rrMax"]["\[Gamma]>>1"]=1-4/Sqrt[\[Gamma]]+15/\[Gamma];


(* ::Subsubsection:: *)
(*L_r,max^(2) \[Congruent]d^2 lnT/dx^2|_max*)


RHDict["L2rrMax"]["\[Gamma]>>1"]=-\[Gamma];
RHDict["L2rrMax"]["\[Gamma]<<1"]=-0.4623052020517196;


(* ::Subsection:: *)
(*Reheating quantities at critical temperature*)


(* ::Subsubsection:: *)
(*f_min(\[Gamma]): r_rc<=r_rmax*)


RHDict["fmin"]["\[Gamma]<<1"]=2^(19/20)/(3^(3/20) \[Gamma]^(1/4));

RHDict["fmin"]["\[Gamma]>>1"]=\[Gamma]^(1/4)/(15 +\[Gamma]-4*Sqrt[\[Gamma]])^(1/4);


(* ::Subsubsection:: *)
(*x_c: r_rc\[Congruent]r_r(x_c)*)


RHDict["xcVals"]["\[Gamma]<<1"]={1/(f^4 \[Gamma]),Min[f^2/2,(4 f^4 \[Gamma])/15]};

RHDict["xcVals"]["\[Gamma]>>1"]={1/(f^4 \[Gamma]),f^2/2};


(* ::Subsubsection:: *)
(*L_rc\[Congruent]L_r(x_c)=1/4  ((d ln r_r)/(d x))(x_c)*)


RHDict["LrcVals"]["\[Gamma]<<1"]={(f^4 \[Gamma])/4-7/8+67/(48 f^4 \[Gamma]),If[8 f^2 \[Gamma]<=15,-(15/(16 f^4 \[Gamma])),-(1/f^2)]};

RHDict["LrcVals"]["\[Gamma]>>1"]={(f^4 \[Gamma])/4,-(1/f^2)};


(* ::Subsubsection:: *)
(*H_c\[Congruent]H(x_c)=H_dec Sqrt[r_\[Chi]+r_r]*)


HdecFactor=Assuming[Arho>0&&mPL>0,Sqrt[f^4 Arho/3 Tc^4/mPL^2]//Simplify];
RHDict["HcVals"]["\[Gamma]<<1"]=(HdecFactor*{Sqrt[1],If[8 f^2 \[Gamma]<=15,Sqrt[(50 f^4 \[Gamma])/(5+2 f^4 \[Gamma])^3],Sqrt[f^-4]//Simplify]})//Simplify;
RHDict["HcVals"]["\[Gamma]>>1"]=(HdecFactor*{Sqrt[1],Sqrt[f^-4]//Simplify})//Simplify;


(* ::Subsubsection:: *)
(*a(x_c2)/a(x_c1)*)


RHDict["acRel"]["\[Gamma]<<1"]=If[8 f^2 \[Gamma]<=15,(1+(2 f^4 \[Gamma])/5)^(2/3),(0.9049808797604288 f)/\[Gamma]^(1/6)];
RHDict["acRel"]["\[Gamma]>>1"]=f;


(* ::Chapter:: *)
(*End*)


End[];
EndPackage[];
