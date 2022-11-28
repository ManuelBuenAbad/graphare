(* ::Package:: *)

BeginPackage["DSReheating`"];


solEqs::usage="solEqs: solutions to the reheating equations, interpolated as functions of (dimensionless) time.\nInput: (\[Gamma],w\[Phi],xLarge,prec,accu).\nOutput: {\!\(\*SubscriptBox[\(r\), \(\[Phi]\)]\), \!\(\*SubscriptBox[\(r\), \(r\)]\)}";
hub::usage="hub: the dimensionless hubble expansion rate, as a function of (dimensionless) time.\nInput: (x).\nOutput: h(x)";
TOfx::usage="TOfx: dark sector temperature interpolated as a function of (dimensionless) time/\nInput: (\[Gamma],\!\(\*SubscriptBox[\(g\), \(*\)]\),\!\(\*SubscriptBox[\(H\), \(i\)]\),w\[Phi],xLarge,prec,accu).\nOutput: T";
aOfx::usafe="aOfx: function to compute scale factor\nInput: (\[Gamma], \!\(\*SubscriptBox[\(g\), \(*\)]\), \!\(\*SubscriptBox[\(H\), \(i\)]\), \!\(\*SubscriptBox[\(a\), \(ini\)]\), w\[Phi], xLarge, prec, accu).\nOutput: a";
redshift::usage="redshift: function to compute the redshift \!\(\*SubscriptBox[\(z\), \(*\)]\)+1 from today to an arbitrary point \!\(\*SubscriptBox[\(x\), \(*\)]\) in the past during reheating.\nInput: (\[Gamma],\!\(\*SubscriptBox[\(g\), \(*\)]\),\!\(\*SubscriptBox[\(H\), \(i\)]\),xRange,instant,g\!\(\*SubscriptBox[\('\), \(IR\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(IR\)]\),\!\(\*SubscriptBox[\(T\), \(IR\)]\),w\[Phi],xLarge,prec,accu,debug).\nOutput: \!\(\*SubscriptBox[\(z\), \(*\)]\)+1=\!\(\*SubsuperscriptBox[\(a\), \(*\), \(-1\)]\)";
xCross::usage="xCross: function to compute the two crossing times at which the DS temperature reaches certain value \!\(\*SubscriptBox[\(T\), \(*\)]\), as well as the time at which it reaches the maximum temperature.\nInput: (\[Gamma],\!\(\*SubscriptBox[\(r\), \(r, c\)]\),w\[Phi],xLarge,prec,accu).\nOutput: {\!\(\*SubscriptBox[\(x\), \(c1\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\),\!\(\*SubscriptBox[\(x\), \(c2\)]\)}";
gammaRate::usage="gammaRate: function to compute the log derivagive of the dark sector temperature at some time \!\(\*SubscriptBox[\(x\), \(*\)]\).\nInput: (\[Gamma],\!\(\*SubscriptBox[\(x\), \(*\)]\),w\[Phi],\[Delta],xLarge,prec,accu).\nOutput: \!\(\*FractionBox[\(d\\\ ln\\\ T\), \(d\\\ ln\\\ x\)]\)(\!\(\*SubscriptBox[\(x\), \(*\)]\))";
gsFn::usage="gsFn: function interpolating the Standard Model's relativistic degrees of freedom in entropy."
RHDict::usage="RHDict: dictionary of analytic expressions relevant for reheating.";


Begin["`Private`"];


(* ::Title:: *)
(*DSReheating.wl*)


(* ::Text:: *)
(*WRITTEN BY: MANUEL A. BUEN-ABAD, 2022.*)
(**)
(*This is a code that computes various quantities associated with the reheating of a Dark Sector (DS) due to the decay of a reheaton \[Phi], which dominates the Universe in an early matter-domination (EDM) era. For the derivation of these hard-coded results, see notebook 02_reheating.nb.*)


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


$Assumptions=a>amin>0&&x>xmin>0&&w\[Phi]>-1&&y>ymin>0&&\[Gamma]>0&&\[Gamma]i>0&&\[Gamma]d>0&&f>1&&mPL>0&&gstar>0&&Arho>0


(* ::Section:: *)
(*Definitions*)


Clear[GeV,MeV,Mpl,mpl,second,Hz,mHz,h,\[Rho]crit,H0,H0h1,T\[Gamma]0,gs0,ge0,gSM,A\[Rho],\[Rho]\[Phi]i]


(* ::Subsection:: *)
(*Units*)


GeV=1;
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


(* ::Subsection:: *)
(*Useful*)


A\[Rho]=gstar \[Pi]^2/30;
\[Rho]\[Phi]i=3 Hi^2 mpl^2;

gsdata=Import["./data/entropy_dof.csv"];
gsInt=Interpolation[gsdata,InterpolationOrder->1];
gsDomain=(gsInt//FullForm)[[1,1]]//Flatten;
gsFn[T_]:=Which[T<gsDomain[[1]],gs0,T<=gsDomain[[2]],gsInt[T],T>gsDomain[[2]],gSM]


(* ::Chapter:: *)
(*Reheating: Dark and Visible Sectors (DS & VS)*)


(* ::Text:: *)
(*We will be solving for a two-fluid Universe: one a \[Phi] fluid that starts to decay at some time Subscript[t, d] (equivalently, scale factor Subscript[a, d] and Hubble scale Subscript[H, d]), and the other is made of the dark radiation decay products.*)
(**)
(*The evolution equations are:*)
(**)
(*Subscript[\!\(\*OverscriptBox[\(\[Rho]\), \(.\)]\), \[Phi]]+3H Subscript[\[Rho], \[Phi]] (1+Subscript[w, \[Phi]])+Subscript[\[CapitalGamma]\[Rho], \[Phi]]=0*)
(*Subscript[\!\(\*OverscriptBox[\(\[Rho]\), \(.\)]\), r]+4H Subscript[\[Rho], r] -Subscript[\[CapitalGamma]\[Rho], \[Phi]]=0*)
(**)
(*It may be convenient to rewrite everything in dimensionless quantities, normalized to the value of Subscript[\[Rho], \[Phi],d] at this time Subscript[t, d] when the decays start:*)
(**)
(*(1)   (d Subscript[r, \[Phi]])/(d x)+3 h Subscript[r, \[Phi]] (1+Subscript[w, \[Phi]])+\[Gamma] Subscript[r, \[Phi]]=0*)
(*(d Subscript[r, r])/(d x)+4 h Subscript[r, r]-\[Gamma] Subscript[r, \[Phi]]=0*)
(**)
(*where we have defined*)
(**)
(*x\[Congruent]t\[CenterDot]Subscript[H, d]*)
(*Subscript[H, d]\[Congruent]Sqrt[Subscript[\[Rho], \[Phi],d]/(3 \!\(\*SubsuperscriptBox[\(m\), \(Pl\), \(2\)]\))]*)
(*r\[Congruent]\[Rho]/Subscript[\[Rho], \[Phi],d]*)
(*h\[Congruent]H/Subscript[H, d]=Sqrt[Subscript[r, \[Phi]]+Subscript[r, r]]*)
(*Subscript[\[Gamma], d]\[Congruent]\[CapitalGamma]/Subscript[H, d]*)
(**)
(*with Subscript[m, Pl] being the reduced Planck mass: Subscript[m, Pl]\[Congruent]Subscript[M, Pl]/Sqrt[8\[Pi]].*)
(**)
(*Defining y\[Congruent]ln(a/Subscript[a, d]) note that, since x\[Congruent]t\[CenterDot]Subscript[H, d], then*)
(**)
(*(d y)/(d x)=1/a (d a)/(d t Subscript[H, d])=1/Subscript[H, d] \!\(\*OverscriptBox[\(a\), \(.\)]\)/a=H/Subscript[H, d]=h(y), and therefore:*)
(**)
(*x(y) = \!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), *)
(*SubscriptBox[\(y\), \(i\)], \(y\)]*)
(*\*FractionBox[\(d\ y'\), \(h(y')\)]\)*)
(**)
(*where y=Subscript[y, i] corresponds to an initial scale factor Subscript[a, i]<<Subscript[a, d]. Note in particular that x(y=a)=Subscript[H, d]t(Subscript[a, d])=Subscript[H, d] Subscript[t, d] is not necessarily equal to 1. This is because in the inverse of the Hubble expansion is not in general equal to time!*)


(* ::Text:: *)
(*Defining Subscript[A, \[Rho]]\[Congruent](\[Pi]^2 SubStar[g'])/30 then Subscript[\[Rho], r]=Subscript[A, \[Rho]] T^4.*)
(**)
(*We can also define the factor f: Subscript[\[Rho], \[Phi],d]\[Congruent]f^4 Subscript[\[Rho], r,c], i.e. the initial Subscript[\[Rho], \[Phi]] in terms of Subscript[\[Rho], r] at the critical temperature. Thus, Subscript[H, d]=f^4 Subscript[\[Rho], r,c]/(3 \!\(\*SubsuperscriptBox[\(m\), \(Pl\), \(2\)]\)) and, since Subscript[\[Rho], \[Phi]]>Subscript[\[Rho], r] for all times:*)
(*Subscript[\[Rho], \[Phi]]>Subscript[\[Rho], r] \[Implies] f^4 Subscript[\[Rho], r,c]=f^4 Subscript[A, \[Rho]] \!\(\*SubsuperscriptBox[\(T\), \(c\), \(4\)]\)>Subscript[A, \[Rho]] T^4\[Implies]f>T/Subscript[T, c], and in particular f>1.*)
(**)
(*\[Implies] Subscript[r, r]\[Congruent]Subscript[\[Rho], r]/Subscript[\[Rho], \[Phi]]=Subscript[\[Rho], r]/(f^4 Subscript[\[Rho], r,c])=(T/(f Subscript[T, c]))^4, and Subscript[r, r,c]=f^-4.*)


(* ::Section:: *)
(*Numerical Solution*)


(* ::Subsection:: *)
(*Time as a function of scale factor: x(a)=Subscript[H, d]\[CenterDot]t(a)*)


(* ::Text:: *)
(*During reheaton-domination (\[Phi]-domination, with a constant equation of state Subscript[w, \[Phi]]), compute x(a) before the reheaton decays are allowed.*)


Clear[xOfa]
xOfa[wfi_,aa_]:=Block[{w\[Phi]=wfi,a=aa},(2 a^((3 (1+w\[Phi]))/2))/(3 (1+w\[Phi]))]


(* ::Subsection:: *)
(*Reheating Equations & Numerical Solution (in terms of x=t\[CenterDot]Subscript[H, d])*)


(* ::Text:: *)
(*First, solving for r\[Phi] at some x<xd at which the integration starts, by demanding that r\[Phi] at x=0 is equal to 1. Since x<xd there are no decays yet.*)


Clear[hub,eqsX,solEqs]

(*decay time*)
xd=xOfa[w\[Phi],1];

(*dimensionless hubble*)
hub[x_]:=Sqrt[r\[Phi][x]+rr[x]]

(*evolution equations (in terms of x=t*Hd).*)
eqsX={D[r\[Phi][x],x]+3hub[x]r\[Phi][x](1+w\[Phi])+\[Gamma]d r\[Phi][x]==0,D[rr[x],x]+4hub[x]rr[x]-\[Gamma]d r\[Phi][x]==0,r\[Phi][xd]==1,rr[xd]==0};

(*the solution to the system of equations*)
solEqs[gg_?NumberQ,wf_:0,xLarge_:10^3,prec_:30,accu_:20]:=solEqs[gg,wf,xLarge,prec,accu]=Block[{\[Gamma]d=Rationalize[gg],w\[Phi]=wf,xmin,xmax,sols,dens,r\[Phi]d,hOfx,hd,res},

xmax=If[\[Gamma]d<1,Max[xLarge,100/gg],xLarge];

(*initial time*)
xmin=xd;

(*solving differential equations*)
sols=NDSolve[eqsX,{r\[Phi],rr},{x,xmin,xmax},WorkingPrecision->prec,AccuracyGoal->accu]//Flatten;

(*densities*)
dens={r\[Phi],rr}/.sols;

(*value of r\[Phi] at x=xd*)
r\[Phi]d=dens[[1]][xd];

(*h(x): dimensionless hubble as a function of x*)
hOfx=hub[x]/.{r\[Phi]->dens[[1]],rr->dens[[2]]};

(*h(1): dimensionless hubble at decay time a=1*)
hd=(hOfx/.{x->xd});

(*results*)
res={dens[[1]],dens[[2]]}/.sols;

res
]


(* ::Subsection:: *)
(*Temperature T(x)*)


(* ::Text:: *)
(*Computing the temperature as a function of time, assuming a certain Hubble scale Subscript[H, i] and a certain number of degrees of freedom SubStar[g].*)


Clear[TOfx]

TOfx[gg_?NumberQ,gs_,Hscale_,wf_:0,xLarge_:10^3,prec_:30,accu_:20]:=TOfx[gg,gs,Hscale,wf,xLarge,prec,accu]=Block[{gstar=gs,Hi=Hscale,w\[Phi]=wf,r\[Phi],rr,xData,yData,data,Tfnx},

(*solving the reheating equations*)
{r\[Phi],rr}=solEqs[gg,wf,xLarge,prec,accu];

(*getting the temperature*)
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[rr]//Flatten);
yData=((\[Rho]\[Phi]i*yData)/A\[Rho])^(1/4);

(*building the dataset*)
data={xData,yData}//Transpose;
Tfnx=Interpolation[data];

Tfnx
]


(* ::Subsection:: *)
(*Scale factor*)


(* ::Text:: *)
(*Computing the scale factor as a function of time, assuming a certain Hubble scale Subscript[H, i] and a certain number of degrees of freedom SubStar[g].*)


Clear[aOfx]

aOfx[gg_?NumberQ,gs_,aini_:1,wf_:0,xLarge_:10^3,prec_:30,accu_:20]:=aOfx[gg,gs,aini,wf,xLarge,prec,accu]=Block[{gstar=gs,w\[Phi]=wf,r\[Phi],rr,domain,xData,efolds,yData,data,afnx},

(*solving the reheating equations*)
{r\[Phi],rr}=solEqs[gg,wf,xLarge,prec,accu];

(*interpolation function properties*)
domain=(InterpolatingFunctionDomain[rr]//Flatten);
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);

(*e-fold function*)
efolds[1]=0;
efolds[i_]:=efolds[i]=efolds[i-1]+NIntegrate[hub[xx],{xx,xData[[i-1]],xData[[i]]}];

(*computing e-folds*)
yData=Monitor[Table[efolds[i],{i,xData//Length}],ProgressIndicator[i,{1,xData//Length}]];

(*computing scale factor*)
yData=aini*Exp[yData];

(*final data & interpolation*)
data={xData,yData}//Transpose;
afnx=Interpolation[data];

afnx
]


(* ::Subsection:: *)
(*Redshift*)


(* ::Text:: *)
(*We compute the total redshift (1+z) from today to the time when the GW were generated. For simplicity, it's easier to start from the earliest times. We break the computation into three parts:*)
(*1. Redshift from GW generation until some time during the Dark Sector (DS) Radiation Domination (RD) era. Part of this process takes place during DS reheating (DSRH).*)
(*2. Redshift from this time until after the Visible Sector (VS) is reheated by the DS (VSRH).*)
(*3. Redshift from VSRH time until today.*)
(**)
(*For #1 the ratio a(Subscript[t, 2])/a(Subscript[t, 1])=(1+Subscript[z, 1])/(1+Subscript[z, 2]) of scale factors between two times Subscript[t, 1]<Subscript[t, 2] during DS reheating is simply equal to a(Subscript[t, 2])/a(Subscript[t, 1])=\!\(\*SuperscriptBox[\(e\), \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), *)
(*SubscriptBox[\(t\), \(1\)], *)
(*SubscriptBox[\(t\), \(2\)]]\[DifferentialD]t\ \ \(H(t)\)\)]\)=\!\(\*SuperscriptBox[\(e\), \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), *)
(*SubscriptBox[\(x\), \(1\)], *)
(*SubscriptBox[\(x\), \(2\)]]\[DifferentialD]x\ \ \(h(x)\)\)]\).*)
(*For #2, VSRH can take place instantaneously or adiabatically. So we use either energy- or entropy-conservation as our guides.*)


Clear[redshift]

redshift[gg_?NumberQ,gs_,Hscale_,xRange_List,instant_,gdIR_:0,xiIR_:0,TIR_:10^3,wf_:0,xLarge_:10^3,prec_:30,accu_:20,debug_:False]:=redshift[gg,gs,Hscale,xRange,instant,gdIR,xiIR,TIR,wf,xLarge,prec,accu,debug]=Block[{gstar=gs,Hi=Hscale,w\[Phi]=wf,xStar,xm,r\[Phi],rr,efolds,Tdm,gIR,Gfactor,Z1,Z2,Z3,res},

(*the time-range between GW production (Subscript[x, *]) and an arbitrary point during DS-RD era (Subscript[x, m])*)
{xStar,xm}=xRange;

(*computing the efolds between Subscript[x, *] and Subscript[x, m]*)
{r\[Phi],rr}=solEqs[gg,wf,xLarge,prec,accu];
efolds=NIntegrate[hub[x],{x,xStar,xm}];

(*the redshift factor between Subscript[x, *] and Subscript[x, m]*)
Z1=Exp[efolds];

(*computing the DS temperature at Subscript[x, m]*)
Tdm=TOfx[gg,gs,Hscale,wf,xLarge,prec,accu][xm];

(*computing the entropy dump from DS reheating the VS; whether instantaneously or adiabatically*)
gIR=gsFn[TIR];
Gfactor=If[instant,(gs/(gdIR*xiIR^4+gIR))^(1/4),(gs/(gdIR*xiIR^3+gIR))^(1/3)];

(*the redshift factor between Subscript[x, m] and Subscript[x, IR], at some arbitrary time shortly after VS reheating*)
Z2=Gfactor*Tdm/TIR;

(*the redshift factor from the time of VS reheating down until today*)
Z3=(gIR/gs0)^(1/3)*TIR/T\[Gamma]0;

(*the total redshift. Note that the explicit dependence on the VS temperature TIR after reheating from the DS cancels out; it only appears in gIR(TIR).*)
res=If[debug,{Z1*Z2*Z3,Z1,Z2,Z3},Z1*Z2*Z3];

res
]


(* ::Subsection:: *)
(*Crossings*)


Clear[xCross]

xCross[gg_?NumberQ,rCrit_,wf_:0,xLarge_:10^3,prec_:30,accu_:20]:=xCross[gg,rCrit,wf,xLarge,prec,accu]=Block[{w\[Phi]=wf,r\[Phi],rr,domain,xData,yData,xpk,xLSet,yLSet,xRSet,yRSet,xL0,xR0,xL,xR,eps=10^-4},

{r\[Phi],rr}=solEqs[gg,wf,xLarge,prec,accu];
domain=(InterpolatingFunctionDomain[rr]//Flatten);
xData=(InterpolatingFunctionCoordinates[rr]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[rr]//Flatten);

xpk=10^(LX/.FindMaximum[rr[10^LX]//Log10,{LX,0,Log10[domain[[1]]*(1+eps)],Log10[domain[[2]]]},MaxIterations->10^6,WorkingPrecision->prec][[-1]]);

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

xL=10^(LX/.FindRoot[Log10[rr[10^LX]]==Log10[rCrit],{LX,Log10[xL0],Log10[domain[[1]]*(1+eps)],Log10[xpk]},MaxIterations->10^6,WorkingPrecision->prec]//Quiet);
xR=10^(LX/.FindRoot[Log10[rr[10^LX]]==Log10[rCrit],{LX,Log10[xR0],Log10[xpk],Log10[domain[[2]]*(1-eps)]},MaxIterations->10^6,WorkingPrecision->prec]//Quiet);

{xL,xpk,xR}
]


(* ::Subsection:: *)
(*Find rate \[Gamma]*)


Clear[gammaRate]

gammaRate[gg_?NumberQ,xCrit_,wf_:0,\[Delta]_:10^-6,xLarge_:10^3,prec_:30,accu_:20]:=gammaRate[gg,xCrit,wf,\[Delta],xLarge,prec,accu]=Block[{r\[Phi],rr,w\[Phi]=wf,\[Gamma]},

{r\[Phi],rr}=solEqs[gg,wf,xLarge,prec,accu];

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
RHDict["\[Rho]\[Phi]i"]=\[Rho]\[Phi]i;


(* ::Subsection:: *)
(*Subscript[r, r,c]*)


(* ::Text:: *)
(*Subscript[r, r,c]\[Congruent]Subscript[r, r](Subscript[x, c])=f^-4:*)


RHDict["rrc"]=f^-4;


(* ::Subsection:: *)
(*Approximate Solutions & Useful Benchmarks*)


(* ::Subsubsection:: *)
(*\[Phi]D: reheaton domination (Subscript[r, r]<<Subscript[r, \[Phi]])*)


RHDict["solsAn"]["\[Gamma]<<1","\[Phi]D"]={r\[Phi][x]->4/(9 x^2)-(2 (-2+3 x) (2+3 x) \[Gamma])/(81 x^3),rr[x]->1/20 (-((32 (2/3)^(2/3))/(9 x^(8/3)))+16/(3 x)) \[Gamma]};
RHDict["solsAn"]["\[Gamma]>>1","\[Phi]D"]={r\[Phi][x]->1+(-(2/3)+x) (-3-\[Gamma]),rr[x]->(-(2/3)+x) \[Gamma]};


(* ::Subsubsection:: *)
(*\[Phi]R equality: Subscript[r, eq]\[Congruent]Subscript[r, r](Subscript[x, eq])=Subscript[r, \[Phi]](Subscript[x, eq])*)


RHDict["xEq"]["\[Gamma]<<1"]=10/(11 \[Gamma]);
RHDict["rEq"]["\[Gamma]<<1"]=(22 \[Gamma]^2)/75;

RHDict["xEq"]["\[Gamma]>>1"]=2/3+1/(2 \[Gamma]);
RHDict["rEq"]["\[Gamma]>>1"]=1/2-3/(4 \[Gamma]);


(* ::Subsubsection:: *)
(*RD: radiation domination (Subscript[r, r]>>Subscript[r, \[Phi]])*)


RHDict["solsAn"]["\[Gamma]<<1","RD"]={r\[Phi][x]->((11/6)^(1/4) E^(10/11-x \[Gamma]) Sqrt[\[Gamma]/x^3])/(2 Sqrt[5]),rr[x]->1/(4 x^2)};

RHDict["solsAn"]["\[Gamma]>>1","RD"]={r\[Phi][x]->(E^(1/2+(2 \[Gamma])/3-x \[Gamma]) (-3+2 \[Gamma]))/(4 \[Gamma]),rr[x]->9/(1-6 x)^2};


(* ::Subsubsection:: *)
(*Subscript[L, r]\[Congruent]1/4 (d ln Subscript[r, r])/(d x)=(d ln T)/(d x)*)


RHDict["Lr"]["\[Gamma]<<1","\[Phi]D"]=-(2/(3 x))+1/(-((8 (2/3)^(2/3))/(5 x^(2/3)))+(12 x)/5);
RHDict["Lr"]["\[Gamma]<<1","RD"]=-(1/(2 x));

RHDict["Lr"]["\[Gamma]>>1","\[Phi]D"]=-(3/(8-12 x));
RHDict["Lr"]["\[Gamma]>>1","RD"]=3/(1-6 x);


(* ::Subsubsection:: *)
(*Maximum: Subscript[r, r,max]\[Congruent]Subscript[r, r](Subscript[x, max])*)


RHDict["xMax"]["\[Gamma]<<1"]=2/3 (8/3)^(3/5);
RHDict["rrMax"]["\[Gamma]<<1"]=2/3 (3/8)^(8/5) \[Gamma];

RHDict["xMax"]["\[Gamma]>>1"]=2/3+1/(2 Sqrt[\[Gamma]])-1/(4 \[Gamma]);
RHDict["rrMax"]["\[Gamma]>>1"]=1-4/Sqrt[\[Gamma]]+15/\[Gamma];


(* ::Subsection:: *)
(*Reheating quantities at critical temperature*)


(* ::Subsubsection:: *)
(*Subscript[f, min](\[Gamma]): Subscript[r, r,c]<=Subscript[r, r,max]*)


RHDict["fmin"]["\[Gamma]<<1"]=2^(19/20)/(3^(3/20) \[Gamma]^(1/4));

RHDict["fmin"]["\[Gamma]>>1"]=\[Gamma]^(1/4)/(15 +\[Gamma]-4*Sqrt[\[Gamma]])^(1/4);


(* ::Subsubsection:: *)
(*Subscript[x, c]: Subscript[r, r,c]\[Congruent]Subscript[r, r](Subscript[x, c])*)


RHDict["xcVals"]["\[Gamma]<<1"]={2/3+1/(f^4 \[Gamma]),(4 f^4 \[Gamma])/15};

RHDict["xcVals"]["\[Gamma]>>1"]={2/3+1/(f^4 \[Gamma]),1/6 (1+3 f^2)};


(* ::Subsubsection:: *)
(*Subscript[L, r,c]\[Congruent]Subscript[L, r](Subscript[x, c])=1/4 Subscript[((d ln Subscript[r, r])/(d x)), Subscript[x, c]]*)


RHDict["LrcVals"]["\[Gamma]<<1"]={(f^4 \[Gamma])/4-7/8+67/(48 f^4 \[Gamma]),-(15/(16 f^4 \[Gamma]))};

RHDict["LrcVals"]["\[Gamma]>>1"]={(f^4 \[Gamma])/4,-(1/f^2)};


(* ::Subsubsection:: *)
(*Subscript[H, c]\[Congruent]H(Subscript[x, c])=Subscript[H, dec] Sqrt[Subscript[r, \[Phi]]+Subscript[r, r]]*)


HdecFactor=Assuming[Arho>0&&mPL>0,Sqrt[f^4 Arho/3 Tc^4/mPL^2]//Simplify];
HcVals["\[Gamma]<<1"]=(HdecFactor*{Sqrt[1],Sqrt[(25/4*1/(f^8 \[Gamma]^2))]})//Simplify;
HcVals["\[Gamma]>>1"]=(HdecFactor*{Sqrt[1],Sqrt[f^-4]//Simplify})//Simplify;


(* ::Chapter:: *)
(*End*)


End[];
EndPackage[];
