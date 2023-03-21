(* ::Package:: *)

BeginPackage["PhaseTransition`"];


ptParams::usage="ptParams: function to compute parameters of the first order phase transition (1PT).\nInput: (gStar,direction,vw,{Temperature Evolution},coeffs,Tcrit,method,Kfactor,full,scaleFactor,Nlx).\n\tIf method='analytic' then {Temperature Evolution}={\[Gamma], t0, tcrit};\n\telse if method='semi'/'numeric'/'alt' then {Temperature Evolution}={Tfn, tScale}.\nOutput: {\!\(\*SubscriptBox[\(T\), \(PT\)]\), \!\(\*SubscriptBox[\(t\), \(PT\)]\), \!\(\*OverscriptBox[\(\[Lambda]\), \(_\)]\), \!\(\*SubscriptBox[\(\[CapitalPhi]\), \(b\)]\), \!\(\*SubscriptBox[\(\[CapitalDelta]V\), \(T\)]\), \!\(\*SubscriptBox[\(E\), \(c\)]\), \!\(\*SubscriptBox[\(S\), \(E\)]\), \!\(\*SubscriptBox[\(S\), \(E\)]\)', \!\(\*SubscriptBox[\(S\), \(E\)]\)'', \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(nucl\)]\)/\[ScriptCapitalV], \!\(\*SubscriptBox[\(\[Beta]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Beta]\), \(2\)]\), \!\(\*SubscriptBox[\(n\), \(b\)]\), \!\(\*SubscriptBox[\(R\), \(b\)]\), <R>, \!\(\*SubscriptBox[\(\[Beta]\), \(eff\)]\), \!\(\*SubscriptBox[\(\[Alpha]\), \(\[Infinity]\)]\), {\!\(\*SubscriptBox[\(\[Alpha]\), \(n\)]\)}={\!\(\*SubscriptBox[\(\[Alpha]\), \(\[Epsilon]\)]\), \!\(\*SubscriptBox[\(\[Alpha]\), \(V\)]\), \!\(\*SubscriptBox[\(\[Alpha]\), \(L\)]\), \!\(\*SubscriptBox[\(\[Alpha]\), \(\[Theta]\)]\)}, {\!\(\*SubscriptBox[\(\[Kappa]\), \(run\)]\)}={\!\(\*SubscriptBox[\(\[Kappa]\), \(run, \[Epsilon]\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(run, V\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(run, L\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(run, \[Theta]\)]\)}}";
tTPT::usage="tTPT: function to compute the time and temperature of the 1PT.\nInput: (direction,vw,TempEvol,coeffs,Tcrit,method,Kfactor,full,scaleFactor,Nlx).\n\tIf method='analytic' then {Temperature Evolution}={\[Gamma], t0, tcrit};\n\telse if method='semi'/'numeric'/'alt' then {Temperature Evolution}={Tfn, tScale}.\nOutput: \!\(\*SubscriptBox[\(t\), \(PT\)]\),\!\(\*SubscriptBox[\(T\), \(PT\)]\)";
Lmlnh::usage="Lmlnh: interpolated function \!\(\*SubscriptBox[\(log\), \(10\)]\)(-ln[h(\!\(\*SubscriptBox[\(log\), \(10\)]\) x)]), where h is the fractional volume in the metastable phase of the 1PT.\nInput: (direction,vw,TempEvol,coeffs,Tcrit,Kfactor,full,scaleFactor,Nlx).\nOutput: \!\(\*SubscriptBox[\(log\), \(10\)]\)(-ln[h(\!\(\*SubscriptBox[\(log\), \(10\)]\)x)])";
Lnbubble::usage="Lnbubble: interpolated function \!\(\*SubscriptBox[\(log\), \(10\)]\)(\!\(\*SubscriptBox[\(n\), \(b\)]\)(\!\(\*SubscriptBox[\(log\), \(10\)]\)(x)), where \!\(\*SubscriptBox[\(n\), \(b\)]\) is the mean bubble number density.\nInput: (LmlnhLx,TempEvol,coeffs,Tcrit,xVal,Kfactor,full,scaleFactor).\nOutput: \!\(\*SubscriptBox[\(log\), \(10\)]\)(\!\(\*SubscriptBox[\(n\), \(b\)]\)(\!\(\*SubscriptBox[\(log\), \(10\)]\)(x)))";
LRbubble::usage="LRbubble: interpolated function \!\(\*SubscriptBox[\(log\), \(10\)]\)(\!\(\*SubscriptBox[\(R\), \(b\)]\)(\!\(\*SubscriptBox[\(log\), \(10\)]\)(x))), where \!\(\*SubscriptBox[\(R\), \(b\)]\) is the average bubble radius.\nInput: (LmlnhLx,LnbLx,vw,TempEvol,coeffs,Tcrit,xVal,Kfactor,full,scaleFactor).\nOutput: \!\(\*SubscriptBox[\(log\), \(10\)]\)(\!\(\*SubscriptBox[\(R\), \(b\)]\)(\!\(\*SubscriptBox[\(log\), \(10\)]\)(x)))";
EcTemp::usage="EcTemp: function to compute the energy of the critical bubble of the 1PT.\nInput: (coeffs,Tcrit,T).\nOutput: \!\(\*SubscriptBox[\(E\), \(c\)]\)";
SETemp::usage="SETemp: function to compute the Euclidean bounce action of the critical bubble of the 1PT.\nInput: (coeffs,Tcrit,T).\nOutput: \!\(\*SubscriptBox[\(S\), \(E\)]\)";
S1Rate::usage="S1Rate: function to compute the first time derivative of the Euclidean bounce action.\nInput: (dlnTdt[t],coeffs,Tcrit,T).\nOutput: \!\(\*SubscriptBox[\(S\), \(E\)]\)'(t)";
S2Rate::usage="S2Rate: function to compute the second time derivative of the Euclidean bounce action.\nInput: (dlnTdt[t],d2lnTdt2[t],coeffs,Tcrit,T).\nOutput: \!\(\*SubscriptBox[\(S\), \(E\)]\)''(t)";
\[CapitalGamma]nucl::usage="\[CapitalGamma]nucl: function to compute the bubble nucleation rate of the 1PT.\nInput: (coeffs,Tcrit,T,Kfactor,full).\nOutput: \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(nucl\)]\)/\[ScriptCapitalV]";
\[Beta]1Rate::usage="\[Beta]1Rate: function to compute the first time derivative of the log of the nucleation rate.\nInput: (dlnTdt[t],coeffs,Tcrit,T,Kfactor,full).\nOutput: \!\(\*SubscriptBox[\(\[Beta]\), \(1\)]\)(t)";
\[Beta]2Rate::usage="\[Beta]2Rate: function to compute the second time derivative of the log of the nucleation rate.\nInput: (dlnTdt[t],d2lnTdt2[t],coeffs,Tcrit,T,Kfactor,full).\nOutput: \!\(\*SubscriptBox[\(\[Beta]\), \(2\)]\)(t)";


Begin["`Private`"];


(* ::Title:: *)
(*PhaseTransition.wl*)


(* ::Text:: *)
(*WRITTEN BY: MANUEL A. BUEN-ABAD, 2022.*)
(**)
(*This is a code that computes various parameters and functions associated with bubble nucleation during First Order Phase Transitions (1PT). Many of the results here have been hard-wired into the code. This helps to make importing this code much faster and painless, but may obscure the results. For the derivation of these hard-coded results, see notebook 02_phase_transition.nb.*)


(* ::Chapter:: *)
(*0. Preamble*)


(* ::Section:: *)
(*Assumptions*)


$Assumptions=Mc>0&&M^2>0&&\[Delta]>0&&\[Mu]>0&&T>0&&T0>0&&T>T0&&A>0&&\[Lambda]>0&&4*A^2<3*\[Lambda]*\[Mu]^2&&Tc>0&&Tc>T0&&\[Lambda]bar>0&&\[Lambda]bar<9/8


(* ::Section:: *)
(*Directories*)


bubbleDir=NotebookDirectory[]<>"results/phase_transition/bubbles/";
fnsLambdaDir=NotebookDirectory[]<>"results/phase_transition/fns_lambda/";


(* ::Section:: *)
(*Test Arguments*)


Clear[noOption]

noOption[value_, options_List] := If[MemberQ[options, value] == False, Message[noOption::ArgumentsNotAllowed, value, options]; Abort[]];

noOption::ArgumentsNotAllowed = "Argument `1` not a string from options `2`.";


(* ::Section:: *)
(*Interpolation Package*)


(* ::Input::Initialization:: *)
Needs["DifferentialEquations`InterpolatingFunctionAnatomy`"];


(* ::Section:: *)
(*Definitions*)


Clear[smallnum]

smallnum=10^-6;


(* ::Chapter:: *)
(*1. Potential*)


(* ::Text:: *)
(*In this Notebook we will assume there is one single scalar \[CapitalPhi] undergoing the phase transition.*)


(* ::Section:: *)
(*1.1 Definition*)


(* ::Text:: *)
(*We first write the thermal potential (density) of the scalar field. Note that it is equal to the zero-temperature potential (T=0) contribution (from your usual QFT course) plus the thermal free energy density (\[ScriptCapitalF]) contributions from bosons and fermions coupled to it.*)
(**)
(*The only assumptions are that: \[CapitalPhi] is real, and that M^2(T)>0, \[Delta](T)>0.*)


(* ::ItemNumbered:: *)
(*\[CapitalPhi] \[Element]R^+*)


(* ::ItemNumbered:: *)
(*M^2(T)>0*)


(* ::ItemNumbered:: *)
(*\[Delta](T)>0*)


(* ::ItemNumbered:: *)
(*\[Lambda]>0*)


(* ::Text:: *)
(*M^2(T)>0 is necessary so that we're above the binodal temperature and the symmetric phase is a minimum and not a maximum. The \[Delta](T)>0 condition can be relaxed and all results will still follow upon the change of variables \[Delta](T)->\[Dash]\[Delta](T), \[CapitalPhi]->\[Dash]\[CapitalPhi].*)


V[x_]=M^2/2 x^2-\[Delta]/3 x^3+(\[Lambda]/4!) (x^4)(*M^2(T) and \[Delta](T) general functions of T, with contributions from bosons and fermions*);


(* ::Text:: *)
(*The extrema of the potential are:*)


extrema={{x->0},{x->(3 \[Delta]-Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])/\[Lambda]},{x->(3 \[Delta]+Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])/\[Lambda]}};
Fs=(x/.extrema[[1]])(*\[CapitalPhi]_s: value in symmetric minimum*);
Fm=(x/.extrema[[2]])(*\[CapitalPhi]_m: value at maximum*);
Fb=(x/.extrema[[3]])(*\[CapitalPhi]_b: value in broken minimum*);


(* ::Text:: *)
(*The potential at those values is:*)


val={0,((-3 \[Delta]+Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])^2 (3 M^2 \[Lambda]+\[Delta] (-3 \[Delta]+Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])))/(12 \[Lambda]^3),-(((3 \[Delta]+Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])^2 (-3 M^2 \[Lambda]+\[Delta] (3 \[Delta]+Sqrt[9 \[Delta]^2-6 M^2 \[Lambda]])))/(12 \[Lambda]^3))};
Vs=val[[1]];
Vm=val[[2]];
Vb=val[[3]];
Clear[extrema,val]


(* ::Text:: *)
(*As we shall see, at T=0 \[Delta]=0, and therefore the VEV and the potential value is:*)


Fb0=(Sqrt[6] Sqrt[-M^2 \[Lambda]])/\[Lambda];
Vb0=-((3 M^4)/(2 \[Lambda]));


(* ::Section:: *)
(*1.2 Critical temperature*)


(* ::Text:: *)
(*Knowing the value of the potential at the various phases we can find when both minima are degenerate, i.e. V_b = V_ s = 0, in terms of the coefficients of the parameter M of the potential. The result motivates the following definitions: M_c^2(T) \[Congruent]4/3 \[Delta](T)^2/\[Lambda] and \[Lambda]bar(T)\[Congruent]M^2(T)/M_c^2(T):*)


PotToMc\[Lambda]bar={M->Mc*Sqrt[\[Lambda]bar],\[Delta]->Sqrt[\[Lambda] Mc^2]*Sqrt[3]/2};


(* ::Text:: *)
(*Clearly, when \[Lambda]bar(T_c)=1 we have M(T_c) = M_c(T_c). This is the definition of T_c: the so-called critical temperature. One can find T_c if one knows the functional form of M(T) and of \[Delta](T).*)


(* ::Section:: *)
(*1.3 Simplification*)


(* ::Text:: *)
(*The extrema of the potential can be written in terms of these new variables, yielding more convenient expressions (with an overall dimensional scale given by M_c and a T-dependent function, via \[Lambda]bar(T).*)


(* ::Text:: *)
(*Note that M^2(T) is the \[CapitalPhi]-mass (i.e. V''(\[CapitalPhi])) in the symmetric phase (\[LeftAngleBracket]\[CapitalPhi]\[RightAngleBracket]=\[CapitalPhi]_s=0). In the broken phase (\[LeftAngleBracket]\[CapitalPhi]\[RightAngleBracket]=\[CapitalPhi]_b) the mass M_b^2(T) (V'') is:*)


Mb=(1/2 Mc Sqrt[9+3 Sqrt[9-8 \[Lambda]bar]-8 \[Lambda]bar]);


(* ::Text:: *)
(*From here, we can find the difference V_b - V_s between the two minima:*)


VbmVs=((3 Mc^4 (-9 (3+Sqrt[9-8 \[Lambda]bar])+4 (9+2 Sqrt[9-8 \[Lambda]bar]-2 \[Lambda]bar) \[Lambda]bar))/(16 \[Lambda]))(*potential difference*);


(* ::Text:: *)
(*Therefore, \[CapitalDelta]V\[Congruent]|V_b - V_s|=sign(\[Lambda]bar - 1) (V_b - V_s) is always positive:*)


\[CapitalDelta]V=(1/(16 \[Lambda]) 3 Mc^4 (-9 (3+Sqrt[9-8 \[Lambda]bar])+4 (9+2 Sqrt[9-8 \[Lambda]bar]-2 \[Lambda]bar) \[Lambda]bar) Sign[-1+\[Lambda]bar]);


(* ::Section:: *)
(*1.4 Dimensionless expressions*)


(* ::Text:: *)
(*A convenient way to write our equations is in terms of dimensionless quantities. The actual physical quantities can then be obtained from the dimensionless ones by multiplying by the appropriate scales.*)
(**)
(*We define \[CapitalPhi]bar\[Congruent]\[CapitalPhi]/\[CapitalPhi]_b and rbar=M\[CenterDot]r. We will see that the dimensionality of the potential is given by M^2 \[CapitalPhi]_b^2:*)


PotDim=(-((3 Mc^4 \[Lambda]bar (-3 (3+Sqrt[9-8 \[Lambda]bar])+4 \[Lambda]bar))/(2 \[Lambda])));


(* ::Text:: *)
(*Then, the dimensionless potential is:*)


dimlessV[x_,\[Lambda]bar_]=((x^2 (-4 x (3+Sqrt[9-8 \[Lambda]bar])+x^2 (9+3 Sqrt[9-8 \[Lambda]bar]-4 \[Lambda]bar)+8 \[Lambda]bar))/(16 \[Lambda]bar));


(* ::Text:: *)
(*The dimensionless potential energy difference between both minima is:*)


dimless\[CapitalDelta]V[\[Lambda]bar_]=(Abs[-1+\[Lambda]bar]/(-3+Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar));


(* ::Section:: *)
(*1.5 Potential coefficients M(T) & \[Delta](T)*)


(* ::Text:: *)
(*Let us recall the important functions. The potential depends on M(T) and \[Delta](T). In terms of M_c and \[Lambda]bar we used the substitutions*)


(* ::Input:: *)
(*PotToMc\[Lambda]bar*)


(* ::Text:: *)
(*Inverting those substitutions we get M_c and \[Lambda]bar in terms of M and \[Delta]:*)


defMc=Mc->(2 \[Delta])/Sqrt[3 \[Lambda]];
def\[Lambda]bar=(\[Lambda]bar->M^2/Mc^2)/.defMc;
Mc\[Lambda]barToPot={defMc,def\[Lambda]bar};


(* ::Text:: *)
(*For some theories the M(T) and \[Delta](T) coefficients have the following temperature dependence:*)


PotToCoeffs={M->\[Mu] Sqrt[(T^2-T0^2)],\[Delta]->A T};


(* ::Text:: *)
(*where T_0 is the binodal temperature, and \[Mu] and A are dimensionless parameters. M_c and \[Lambda]bar are therefore:*)


Mc\[Lambda]barToCoeffs=Mc\[Lambda]barToPot/.PotToCoeffs;


(* ::Text:: *)
(*From here we can find the binodal temperature in terms of the critical temperature T_c, at which \[Lambda]bar(T_c)=1:*)


Tbinodal=({T0->(Tc Sqrt[-((4 A^2)/(3 \[Lambda]))+\[Mu]^2])/\[Mu]});


(* ::Text:: *)
(*Clearly the condition for there to be a critical temperature is*)


cond1[\[Mu]_,A_,\[Lambda]_]=(1-4/3 A^2/(\[Lambda] \[Mu]^2)>0);


(* ::Text:: *)
(*The spinodal temperature, on the other hand, is when \[Lambda]bar=9/8 and the second minimum vanishes:*)


Tspinodal=({Ts->Tc/Sqrt[1+A^2/(8 A^2-6 \[Lambda] \[Mu]^2)]});


(* ::Text:: *)
(*This occurs for parameter values that satisfy the following condition*)


cond2[\[Mu]_,A_,\[Lambda]_]=(1+A^2/(8 A^2-6 \[Lambda] \[Mu]^2)>0);


(* ::Text:: *)
(*We hereby define some functions of the coefficients of the potential that allow us to perform error handling whenever we stray from the physical parameter space.*)


noCritTemp[coeffs_]:=If[cond1[coeffs[[1]],coeffs[[2]],coeffs[[3]]],None,Message[noCritTemp::ParameterSpace,coeffs];Abort[]];
noCritTemp::ParameterSpace="The parameters {\[Mu],A,\[Lambda]}=`1` in the potential do not allow for a critical temperature, nor a first order phase transition.";


(* ::Text:: *)
(*Finally, this leads to the updated rules, in terms of {T, T_c, \[Mu], A}:*)


Mc\[Lambda]barToCoeffs=({Mc->(2 A T)/(Sqrt[3] Sqrt[\[Lambda]]),\[Lambda]bar->(4 Tc^2+(3 (T-Tc) (T+Tc) \[Lambda] \[Mu]^2)/A^2)/(4 T^2)});
PotToCoeffs=({M->Sqrt[T^2+1/3 Tc^2 (-3+(4 A^2)/(\[Lambda] \[Mu]^2))] \[Mu],\[Delta]->A T});


(* ::Text:: *)
(*We can also write T in terms of \[Lambda]bar for a given set of parameters*)


TTo\[Lambda]bar=Tc Sqrt[(4 A^2-3 \[Lambda] \[Mu]^2)/(4 A^2 \[Lambda]bar-3 \[Lambda] \[Mu]^2)];


(* ::Text:: *)
(*And therefore M_c in terms of \[Lambda]bar:*)


McTo\[Lambda]bar={Mc->2 A Tc Sqrt[(4 A^2-3 \[Lambda] \[Mu]^2)/(12 A^2 \[Lambda] \[Lambda]bar-9 \[Lambda]^2 \[Mu]^2)]};


(* ::Text:: *)
(*The variable \[CapitalDelta]\[Congruent](4/3)A^2/(\[Lambda] \[Mu]^2) is very useful, since \[CapitalDelta]=0 corresponds to no cubic term (and thus no broken phase), whereas \[CapitalDelta]=1 corresponds to the 1PT condition not being satisfied, and \[CapitalDelta]=8/9 to the spinodal temperature going to T_s->\[Infinity]:*)


ARule={A->Sqrt[3]/2 Sqrt[\[Lambda]]\[Mu] Sqrt[\[CapitalDelta]]};


(* ::Text:: *)
(*From here we can then write T and M_c in terms of \[Lambda]bar and \[CapitalDelta], which is particularly simple:*)


TTo\[Lambda]bar\[CapitalDelta]={T->Tc Sqrt[(1-\[CapitalDelta])/(1-\[CapitalDelta]*\[Lambda]bar)]}
McTo\[Lambda]bar\[CapitalDelta]={Mc->Tc \[Mu] Sqrt[\[CapitalDelta]] Sqrt[(1-\[CapitalDelta])/(1-\[CapitalDelta]*\[Lambda]bar)]}


(* ::Text:: *)
(*For the T=0 case the substitution for the potential coefficients are:*)


PotToCoeffs0Temp={M->Tc Sqrt[-1+(4 A^2)/(3 \[Lambda] \[Mu]^2)] \[Mu],\[Delta]->0};


(* ::Text:: *)
(*For future reference, we compute (d \[Lambda]bar)/dT, (d \[Lambda]bar)/(d lnT), (d^2 \[Lambda]bar)/(d ln T^2), (d ln \[Lambda]bar)/(d ln T), and (d^2 ln \[Lambda]bar)/(d ln T^2):*)


d\[Lambda]dT=((Tc^2 (-4 A^2+3 \[Lambda] \[Mu]^2))/(2 A^2 T^3));

d\[Lambda]dlnT=d\[Lambda]dT*T;

d2\[Lambda]dlnT2=(Tc^2 (4 A^2-3 \[Lambda] \[Mu]^2))/(A^2 T^2);

dln\[Lambda]dlnT=((2 Tc^2 (-4 A^2+3 \[Lambda] \[Mu]^2))/(4 A^2 Tc^2+3 (T^2-Tc^2) \[Lambda] \[Mu]^2));

d2ln\[Lambda]dlnT2=-((12 T^2 Tc^2 \[Lambda] \[Mu]^2 (-4 A^2+3 \[Lambda] \[Mu]^2))/(4 A^2 Tc^2+3 (T^2-Tc^2) \[Lambda] \[Mu]^2)^2);


(* ::Chapter:: *)
(*2. Critical Bubble*)


(* ::Text:: *)
(*The critical bubble is the profile for \[CapitalPhi]_bubble(x) \[Congruent] \[CapitalPhi]_bub that corresponds to a stationary solution interpolating between the two minima \[CapitalPhi]=constant minima (i.e. \[CapitalPhi]_s & \[CapitalPhi]_b), and are solutions to the stationary equation for the Euclidean action (S_E), which can be written in terms of the energy of the critical bubble, E_T:*)
(**)
(*S_E[\[CapitalPhi]] = S_E[\[CapitalPhi]]/T=T^-1 \[Integral]d^3 x  1/2 (\[Del]\[CapitalPhi])^2+ V_T(\[CapitalPhi])*)
(**)
(*\[Delta]E_T[\[CapitalPhi]]/\[Delta]\[CapitalPhi]=0   \[DoubleRightArrow]   -\[Del]^2\[CapitalPhi]+\[PartialD]V_T(\[CapitalPhi])/\[PartialD]\[CapitalPhi]=0*)
(**)
(*Note that we will be dealing with spherically-symmetric, time-independent expressions, so this equation reduces to:*)
(**)
(*-(1/r^2) d/dr (r^2 d\[CapitalPhi]_bub/dr) + V_T'(\[CapitalPhi]_bub)=0*)
(**)
(*The dimensionality of this equation is then M^2 \[CapitalPhi]_b. Dividing the whole expression by this dimension (M^2 \[CapitalPhi]_b) allows us to find its dimensionless form, which we will use in the next section. On the other hand, the dimensionality of the E_T energy is (\[CapitalPhi]_b^2)/M, and thus the action has a prefactor P_S\[Congruent](\[CapitalPhi]_b^2)/(M T).*)
(**)
(*This prefactor can be rewritten in terms of \[Lambda]bar. It, and its first and second derivatives w.r.t. \[Lambda]bar, are given by:*)


PS=(8Sqrt[3] A)/(\[Lambda]^(3/2) \[Lambda]bar^(1/2)) ((3+Sqrt[9-8 \[Lambda]bar])/4)^2;

dPSd\[Lambda]=-((Sqrt[3] A (9 (3+Sqrt[9-8 \[Lambda]bar])+4 Sqrt[9-8 \[Lambda]bar] \[Lambda]bar))/(2 \[Lambda]bar Sqrt[\[Lambda]^3 (9-8 \[Lambda]bar) \[Lambda]bar]));

d2PSd\[Lambda]2=-((Sqrt[3] A (-243 (3+Sqrt[9-8 \[Lambda]bar])+4 \[Lambda]bar (216+45 Sqrt[9-8 \[Lambda]bar]+8 Sqrt[9-8 \[Lambda]bar] \[Lambda]bar)))/(4 (\[Lambda] (9-8 \[Lambda]bar))^(3/2) \[Lambda]bar^(5/2)));


(* ::Section:: *)
(*2.1 Differential Equation*)


(* ::Text:: *)
(*The differential equation is:*)
(**)
(*-(1/r^2) d/dr (r^2 d\[CapitalPhi]_bub/dr) + V_T'(\[CapitalPhi]_bub)=0,*)
(**)
(*which has dimensionality M^2 \[CapitalPhi]_b. Dividing the whole expression by this dimension (M^2 \[CapitalPhi]_b) allows us to find its dimensionless form.*)
(**)
(*The term with the derivative in the potential is:*)


dimlessVprime[x_,\[Lambda]bar_]=x+(x^2 (-9-3 Sqrt[9-8 \[Lambda]bar]))/(4 \[Lambda]bar)+(x^3 (9+3 Sqrt[9-8 \[Lambda]bar]-4 \[Lambda]bar))/(4 \[Lambda]bar);


(* ::Text:: *)
(*In terms of these dimensionless quantities, the bubble solution satisfies the following boundary conditions:*)
(**)
(*\[CapitalPhi]bar(rbar->\[Infinity]) \[Congruent] \[CapitalPhi]bar_fv  is the false vacuum initial value of the scalar field \[CapitalPhi]bar in the phase transition: \[CapitalPhi]bar_fv = \[CapitalPhi]bar_s=0 (\[CapitalPhi]bar_fv = \[CapitalPhi]bar_b = 1) for a cold/subcritical PT (hot/supercritical PT)*)
(**)
(*\[CapitalPhi]bar(rbar=0) \[Congruent] \[CapitalPhi]bar_tv is the true vacuum final value of the scalar field \[CapitalPhi]bar in the phase transition: close to \[CapitalPhi]bar_tv = \[CapitalPhi]bar_b = 1 (\[CapitalPhi]bar_tv = \[CapitalPhi]bar_s = 0) for a cold/subcritical PT (hot/supercritical PT)*)
(**)
(*Recall that whether we have a hot (supercritical)/cold (subcritical) phase transition depends on the value of \[Lambda]bar (\[GreaterLess]1), and thus whether we tunnel from the broken to the symmetric phase, or vice versa.*)


(* ::Text:: *)
(*In terms of these dimensionless quantities, the energy in the critical bubble is given by:*)
(**)
(*E_T[\[CapitalPhi]_bub]=\[Integral]d^3 x  1/2 (\[Del]\[CapitalPhi]_bub)^2+ V_T(\[CapitalPhi]_bub) = (\[CapitalPhi]_b^2)/M 4\[Pi] \[Integral] drbar rbar^2 ( 1/2 (d\[CapitalPhi]bar_bub/drbar)^2 + Vbar_T(\[CapitalPhi]bar_bub) )*)
(**)
(*with \[CapitalPhi]_bub = \[CapitalPhi]_b \[CapitalPhi]bar_bub and Vbar_T(\[CapitalPhi]bar) = V_T(\[CapitalPhi]bar \[CapitalPhi]_b)/(\[CapitalPhi]_b^2 M^2)*)
(**)
(*As we will see more clearly in the thin wall approximation, this expression is formally infinite. However, what the physics depends on is the (finite) difference between the energies of the initial constant-\[CapitalPhi] configuration (\[CapitalPhi]bar_fv) and the bubble \[CapitalPhi]bar_bub configuration. This is called the critical energy, and is given by:*)
(**)
(*E_c\[Congruent]E_T[\[CapitalPhi]_bub]-E_T[\[CapitalPhi]_fv].*)


(* ::Text:: *)
(*Finally, the dimensionless critical bubble equation is:*)
(**)
(*\[CapitalPhi]bar_bub''(r) + (2/rbar) \[CapitalPhi]bar_bub'(r) - Vbar_T(\[CapitalPhi]bar_bub) = 0*)


(* ::Section:: *)
(*2.2 Analytic solution: thin wall approximation*)


(* ::Text:: *)
(*In the thin wall approximation (twa) \[Epsilon]\[Congruent]\[CapitalDelta]V/V_m<<1, which means that the gain in the vacuum energy from the bubble (~\[CapitalDelta]V\[CenterDot]R^3\[Proportional]\[Epsilon]\[CenterDot]R^3), only becomes larger than the energy from the bubble surface (~(R^2)) for a large bubble radius R, much larger than the thickness of the bubble wall.*)
(**)
(*One can show that in the twa \[CapitalPhi]' (r)^2 = 2 V_T(\[CapitalPhi]), and that the (dimensionless) critical energy can be written as:*)
(**)
(*Ebar_c=Ebar_T[\[CapitalPhi]bar_bub]-Ebar_T[\[CapitalPhi]bar_fv]=[(4\[Pi])/3 Rbar_c^3 Vbar_T(\[CapitalPhi]bar_tv) + 4\[Pi] Rbar_c^2 \[Sigma]bar+(4\[Pi])/3 Vbar_T(\[CapitalPhi]bar_fv) (\[Infinity]^3-Rbar_c^3)] - (4\[Pi])/3 Vbar_T(\[CapitalPhi]bar_fv) \[Infinity]^3 = -(4\[Pi])/3 Rbar_c^3 \[CapitalDelta] Vbar_T + 4\[Pi] Rbar_c^2 \[Sigma]bar ,*)
(**)
(*where R_c is the critical radius, and we have split the contributions to the energy in three parts: the inside (rbar<Rbar_c), the surface (rbar\[TildeTilde]Rbar_c), and the outside (rbar>Rbar_c). The (infinite) energy from the constant-\[CapitalPhi] initial configuration cancels out the (infinite) part from the energy of the critical bubble, as promised. What we are left with is a term from the difference \[CapitalDelta]Vbar\[Congruent]Vbar(\[CapitalPhi]bar_fv)-Vbar(\[CapitalPhi]bar_tv)>0 between the two minima, as well as the surface term.*)
(**)
(*The contribution from the surface depends on the surface tension \[Sigma]bar, given by:*)
(**)
(*\[Sigma]bar\[TildeTilde]|\[Integral]_(\[CapitalPhi]bar_fv)^(\[CapitalPhi]bar_tv) d\[CapitalPhi]bar Sqrt[2 Vbar_T(\[CapitalPhi]bar)]| = |\[Integral]_0^1 d\[CapitalPhi]bar Sqrt[2 Vbar_T(\[CapitalPhi]bar)]|*)


(* ::Subsection:: *)
(*Surface tension \[Sigma]*)


(*the dimensionless surface tension; performing the integral gives 1/6*)
\[Sigma]=1/6;


(* ::Text:: *)
(*Its dimensionality is \[CapitalPhi]_b^2 M, as can be seen both from the fact that the dimensionality of E_c is \[CapitalPhi]_b^2 /M and that of R_c is 1/M, and from the fact that the dimensionality of V_T is M^2 \[CapitalPhi]_b^2 and that of \[CapitalPhi] is \[CapitalPhi]_b.*)


(* ::Subsection:: *)
(*Critical radius R_c*)


(* ::Text:: *)
(*Moving on, we can calculate the critical bubble radius in the twa by realizing that, if the bubble is an extremum of Ebar_c, then Rbar_c is such that \[PartialD]Ebar_c/\[PartialD]Rbar_c=0. The result is:*)


Rctwa[\[Lambda]bar_]=((-3+Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar)/(3 Abs[-1+\[Lambda]bar]))(*the critical radius*);


(* ::Subsection:: *)
(*Critical energy E_c*)


(* ::Text:: *)
(*The bubble's critical energy is then:*)


Ectwa[\[Lambda]bar_]=((2 \[Pi] (-3+Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar)^2)/(81 (-1+\[Lambda]bar)^2));


(* ::Text:: *)
(*The first and second derivatives are*)


dEctwad\[Lambda][\[Lambda]bar_]=-((8 \[Pi] (-3+Sqrt[9-8 \[Lambda]bar]+4 (3-2 \[Lambda]bar) \[Lambda]bar))/(81 Sqrt[9-8 \[Lambda]bar] (-1+\[Lambda]bar)^3));
d2Ectwad\[Lambda]2[\[Lambda]bar_]=-((8 \[Pi] (-5-9 Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar (-9+2 Sqrt[9-8 \[Lambda]bar]+2 (9-4 \[Lambda]bar) \[Lambda]bar)))/(27 (9-8 \[Lambda]bar)^(3/2) (-1+\[Lambda]bar)^4));


(* ::Subsection:: *)
(*Bubble profile \[CapitalPhi]_bub(r)*)


(* ::Text:: *)
(*It can be shown that in the twa*)
(**)
(*d \[CapitalPhi]bar/d rbar \[TildeTilde] sign(\[Lambda]bar-1)\[CenterDot]Sqrt[2Vbar_T(\[CapitalPhi]bar)],*)
(**)
(*(i.e. positive/negative derivatives for hot (supercritical)/cold (subcritical) PT). From here we can solve for rbar(\[CapitalPhi]bar):*)
(**)
(*rbar\[TildeTilde]|\[Integral]_(\[CapitalPhi]bar_fv)^(\[CapitalPhi]bar_tv) d\[CapitalPhi]bar'/Sqrt[2 Vbar_T(\[CapitalPhi]bar')]*)
(**)
(*and then invert the equation.*)


\[CapitalDelta]rtwa[\[CapitalPhi]lo_,\[CapitalPhi]hi_]=(Log[(\[CapitalPhi]hi (-1+\[CapitalPhi]lo))/((-1+\[CapitalPhi]hi) \[CapitalPhi]lo)])(*\[CapitalDelta]r change in r due to a change in \[CapitalPhi]*);

\[CapitalPhi]tvtwa[\[Lambda]bar_]=(E^(1/(-1+\[Lambda]bar))/(E^(1/(-1+\[Lambda]bar))+E^((Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar)/(3 (-1+\[Lambda]bar)))))(*final value of \[CapitalPhi] from PT (namely, \[CapitalPhi](r\[Rule]0)) for a given critical radius R_c(\[Lambda]bar)*);

\[CapitalPhi]twa[r_,\[Lambda]bar_]=(1/(1+E^((-3+Sqrt[9-8 \[Lambda]bar]+4 \[Lambda]bar-3 r Abs[-1+\[Lambda]bar])/(3 (-1+\[Lambda]bar)))))(*the bubble profile in the twa*);


(* ::Subsection:: *)
(*Wall thickness l_w*)


(* ::Text:: *)
(*We can define the wall thickness l_w in terms of the change in radius r upon a certain change \[CapitalDelta] \[CapitalPhi]bar in the scalar field, centered around half the field-distance \[CapitalPhi]_*=|(\[CapitalPhi]_tv-\[CapitalPhi]_fv)/2|:*)
(**)
(*l_w \[Congruent] \[CapitalDelta] rbar = \[Integral]_((1-\[CapitalDelta]\[CapitalPhi]bar)/2)^((1+\[CapitalDelta]\[CapitalPhi]bar)/2) d\[CapitalPhi]bar'/(d\[CapitalPhi]bar'/drbar) = \[Integral]_((1-\[CapitalDelta]\[CapitalPhi]bar)/2)^((1+\[CapitalDelta]\[CapitalPhi]bar)/2) d\[CapitalPhi]bar'/(Sqrt[2 Vbar_T(\[CapitalPhi]bar')])*)


thickness[\[CapitalDelta]\[CapitalPhi]_]=(Log[(1+\[CapitalDelta]\[CapitalPhi])^2/(-1+\[CapitalDelta]\[CapitalPhi])^2])(*wall thickness for arbitrary change \[CapitalDelta]\[CapitalPhi]bar in \[CapitalPhi]bar around 1/2*);


(* ::Text:: *)
(*Since what where does the wall start and ends is an arbitrary definition, we simply take its value to be the thickness for changes of \[CapitalDelta]\[CapitalPhi]bar=0.9 around 1/2:*)


lw=thickness[9/10](*bubble thickness*);


(* ::Section:: *)
(*2.3 Semi-analytical solution: thick wall approximation*)


(* ::Text:: *)
(*In the limit where the minimum nucleated inside the bubble is very deep we can use the thick wall approximation, where we keep only the most relevant terms in the potential. We then want to find Ebar_c.*)


(* ::Subsection:: *)
(*Numerical action*)


(* ::Text:: *)
(*The action can be massaged into a dimensionless expression Sbar_E, whose value is roughly 19.4:*)


SThickNum=19.404686768981865;


(* ::Subsection:: *)
(*Cold (subcritical) 1PT*)


(* ::Text:: *)
(*For subcritical, cold 1PT, Ebar_c is:*)


EcThick["cold"][\[Lambda]bar_]=310.47498830370984 \[Sqrt](1/(3+Sqrt[9-8 \[Lambda]bar]+Sqrt[2] Sqrt[-(9+3 Sqrt[9-8 \[Lambda]bar]-4 \[Lambda]bar) (-1+\[Lambda]bar)])^4) \[Lambda]bar^2;


(* ::Text:: *)
(*In addition, in the \[Lambda]bar->0 limit, it and its first and second derivatives are:*)


limEcThick["cold"][\[Lambda]bar_]=2.1560763076646516` \[Lambda]bar^2;
limdEcThick["cold"][\[Lambda]bar_]=4.312152615329304` \[Lambda]bar;
limd2EcThick["cold"][\[Lambda]bar_]=4.312152615329304`;


(* ::Subsection:: *)
(*Hot (supercritical) 1PT*)


(* ::Text:: *)
(*For supercritical, hot 1PT, Ebar_c is:*)


EcThick["hot"][\[Lambda]bar_]=(155.23749415185492` Sqrt[((1-Sqrt[1-Sqrt[9-8 \[Lambda]bar]]+Sqrt[9-8 \[Lambda]bar])^4 \[Lambda]bar)/(9+3 Sqrt[9-8 \[Lambda]bar]-8 \[Lambda]bar)])/(3 +Sqrt[9-8 \[Lambda]bar])^2;


(* ::Text:: *)
(*In addition, in the \[Lambda]bar->9/8 limit, it and its first and second derivatives are:*)


limEcThick["hot"][\[Lambda]bar_]=113.04978910678393` (9/8-\[Lambda]bar)^(3/4);
limdEcThick["hot"][\[Lambda]bar_]=-(84.78734183008798`/(9/8-\[Lambda]bar)^(1/4));
limd2EcThick["hot"][\[Lambda]bar_]=-(21.19683545752185`/(9/8-\[Lambda]bar)^(5/4));


(* ::Section:: *)
(*2.4 Numerical solution*)


(* ::Text:: *)
(*We now need to find the critical bubble profiles, i.e. the solutions to the critical bubble differential equation. Note that since \[CapitalPhi]bar=1(0) is a minimum (\[CapitalPhi]_b or \[CapitalPhi]_s respectively), then for both of these values V_T'(\[CapitalPhi])=0 and \[CapitalPhi]bar is constant in rbar: there is not bubble, just a constant value for all radii. Thus we need to start a bit off from these values, but not so much that we over/undershoot and get stuck in the maximum instead.*)
(**)
(*The whole point of the numerical approach is to perform a shooting algorithm to find the initial condition that allows us to interpolate between the required minima.*)


(* ::Subsection:: *)
(*Defining useful lists*)


(* ::Input:: *)
(*Clear[\[Lambda]barList,\[Lambda]barString,rList]*)


(* ::Text:: *)
(*Computing a list of \[Lambda]bar*)


\[Lambda]barList=Table[lb,{lb,0.01,0.99,0.005}];
\[Lambda]barList=\[Lambda]barList~Join~Table[lb,{lb,1.01,1.124,0.001}];


(* ::Text:: *)
(*String form of \[Lambda]bar*)


\[Lambda]barString[\[Lambda]bar_]:=Module[{decimals,lbStr},
decimals=IntegerString[Round[1000*(\[Lambda]bar-IntegerPart[\[Lambda]bar])],10,3];
lbStr=ToString[IntegerPart[\[Lambda]bar]]<>"."<>decimals;
lbStr
]


(* ::Text:: *)
(*A list of many values for radius:*)


rList={0.0}~Join~Table[r,{r,10^-2,100,(100-10^-2)/399}];


(* ::Subsection:: *)
(*Importing numerical results*)


(* ::Text:: *)
(*Importing numerical results previously computed*)


Clear[bubTab,bubFn,rmaxTab]

For[i=1,i<=(\[Lambda]barList//Length),i++,
\[Lambda]bar=\[Lambda]barList[[i]];
fileName="bubble_lb-"<>\[Lambda]barString[\[Lambda]bar]<>".csv";
bubTab[i]=Import[bubbleDir<>fileName];
bubFn[i]=Interpolation[bubTab[i],InterpolationOrder->2]
]

rmaxTab=Import[fnsLambdaDir<>"rmax.csv"];

Clear[i,\[Lambda]bar,fileName]


(* ::Section:: *)
(*2.5 Critical Energy (dimensionless): full value*)


(* ::Text:: *)
(*The energy of the critical bubble is:*)
(**)
(*E_T[\[CapitalPhi]_bub]=\[Integral]d^3 x  1/2 (\[Del]\[CapitalPhi]_bub)^2+ V_T(\[CapitalPhi]_bub) = (\[CapitalPhi]_b^2)/M 4\[Pi] \[Integral] drbar rbar^2 ( 1/2 (d\[CapitalPhi]bar_bub/drbar)^2 + Vbar_T(\[CapitalPhi]bar_bub) )*)
(**)
(*with \[CapitalPhi]_bub = \[CapitalPhi]_b \[CapitalPhi]bar_bub and Vbar_T(\[CapitalPhi]bar) = V_T(\[CapitalPhi]bar \[CapitalPhi]_b)/(\[CapitalPhi]_b^2 M^2)*)
(**)
(*As we saw, this expression is formally infinite. However, what the physics depends on is the (finite) difference between the energies of the initial constant-\[CapitalPhi] configuration (\[CapitalPhi]bar_fv) and the bubble \[CapitalPhi]bar_bub configuration. This is called the critical energy, and is given by:*)
(**)
(*E_c\[Congruent]E_T[\[CapitalPhi]_bub]-E_T[\[CapitalPhi]_fv].*)


(* ::Subsection:: *)
(*Ebar_c(\[Lambda]bar)*)


(* ::Subsubsection:: *)
(*Importing full Ebar_c(\[Lambda]bar) table*)


EcritTab=Import[fnsLambdaDir<>"Ecrit.csv"];
alllbarTab=#[[1]]&/@EcritTab;


(* ::Subsubsection:: *)
(*Defining Ebar_c(\[Lambda]bar)*)


(* ::Text:: *)
(*Separating the regions for \[Lambda]bar<1 and \[Lambda]bar>1.*)


(* ::Input:: *)
(*Clear[lolbarTab,hilbarTab,lolbarFn,hilbarFn,loL,loR,hiL,hiR,EcFn]*)


lolbarTab=Select[EcritTab,#[[1]]<1&];
lolbarTab={#[[1]]//Log10,#[[2]]//Log10}&/@lolbarTab;

hilbarTab=Select[EcritTab,#[[1]]>1&];
hilbarTab={#[[1]]//Log10,#[[2]]//Log10}&/@hilbarTab;


(* ::Text:: *)
(*Interpolating in log-space*)


lolbarFn=Interpolation[lolbarTab,InterpolationOrder->1,Method->"Spline"];
hilbarFn=Interpolation[hilbarTab,InterpolationOrder->1,Method->"Spline"];


(* ::Text:: *)
(*Edges of the patches*)


{loL,loR}={10^lolbarTab[[1,1]],10^lolbarTab[[-1,1]]};
{hiL,hiR}={10^hilbarTab[[1,1]],10^hilbarTab[[-1,1]]};


(* ::Text:: *)
(*Defining the full function*)


Clear[EcFn]

EcFn[\[Lambda]bar_]:=Which[0<=\[Lambda]bar<loL,limEcThick["cold"][\[Lambda]bar],loL<=\[Lambda]bar<loR,10^lolbarFn[Log10[\[Lambda]bar]],((loR<=\[Lambda]bar<1)||(1<\[Lambda]bar<=hiL)),Ectwa[\[Lambda]bar],hiL<\[Lambda]bar<=hiR,10^hilbarFn[Log10[\[Lambda]bar]],hiR<\[Lambda]bar<=9/8,limEcThick["hot"][\[Lambda]bar]]


(* ::Subsection:: *)
(*Ebar_c'(\[Lambda]bar)*)


(* ::Subsubsection:: *)
(*Importing the first derivative of Ebar_c(\[Lambda]bar)*)


DEcritTab=Import[fnsLambdaDir<>"DEcrit.csv"];


(* ::Subsubsection:: *)
(*Defining Ebar_c'(\[Lambda]bar)*)


(* ::Input:: *)
(*Clear[DlolbarTab,DhilbarTab,DlolbarFn,DhilbarFn,DloL,DloR,DhiL,DhiR,dEcFnd\[Lambda]]*)


(* ::Text:: *)
(*Separating the regions for \[Lambda]bar<1 and \[Lambda]bar>1, which have positive/negative values of the derivative.*)


DlolbarTab=Select[DEcritTab,#[[1]]<1&];
DlolbarTab={#[[1]]//Log10,#[[2]]//Log10}&/@DlolbarTab;

DhilbarTab=Select[DEcritTab,#[[1]]>1&];
DhilbarTab={#[[1]]//Log10,Abs[#[[2]]]//Log10}&/@DhilbarTab;


(* ::Text:: *)
(*Interpolating in log-space*)


DlolbarFn=Interpolation[DlolbarTab,InterpolationOrder->1,Method->"Spline"];
DhilbarFn=Interpolation[DhilbarTab,InterpolationOrder->1,Method->"Spline"];


(* ::Text:: *)
(*Edges of the patches*)


{DloL,DloR}={10^DlolbarTab[[1,1]],10^DlolbarTab[[-1,1]]};
{DhiL,DhiR}={10^DhilbarTab[[1,1]],10^DhilbarTab[[-1,1]]};


(* ::Text:: *)
(*Defining the full function*)


Clear[dEcFnd\[Lambda]]

dEcFnd\[Lambda][\[Lambda]bar_]:=Which[0<=\[Lambda]bar<DloL,limdEcThick["cold"][\[Lambda]bar],DloL<=\[Lambda]bar<DloR,10^DlolbarFn[Log10[\[Lambda]bar]],((DloR<=\[Lambda]bar<1)||(1<\[Lambda]bar<=DhiL)),dEctwad\[Lambda][\[Lambda]bar],DhiL<\[Lambda]bar<=DhiR,-10^DhilbarFn[Log10[\[Lambda]bar]],DhiR<\[Lambda]bar<=9/8,limdEcThick["hot"][\[Lambda]bar]]


(* ::Subsection:: *)
(*Ebar_c''(\[Lambda]bar)*)


(* ::Subsubsection:: *)
(*Importing the second derivative of Ebar_c(\[Lambda]bar)*)


D2EcritTab=Import[fnsLambdaDir<>"D2Ecrit.csv"];


(* ::Subsubsection:: *)
(*Defining Ebar_c''(\[Lambda]bar)*)


(* ::Input:: *)
(*Clear[D2ColdlbarTab,D2HotlbarTab,D2SpinlbarTab,D2ColdlbarFn,D2HotlbarFn,D2SpinlbarFn,D2ColdL,D2ColdR,D2HotL,D2HotR,D2SpinL,D2SpinR,d2EcFnd\[Lambda]2]*)


(* ::Text:: *)
(*Separating the regions according to \[Lambda]bar and the sign of the second derivative*)


D2ColdlbarTab=Select[D2EcritTab,#[[1]]<1&];
D2ColdlbarTab={#[[1]]//Log10,#[[2]]//Log10}&/@D2ColdlbarTab;

D2HotlbarTab=Select[D2EcritTab,((#[[1]]>1)&&(#[[2]]>0))&];
D2HotlbarTab={#[[1]]//Log10,#[[2]]//Log10}&/@D2HotlbarTab;

D2SpinlbarTab=Select[D2EcritTab,((#[[1]]>1)&&(#[[2]]<0))&];
D2SpinlbarTab={#[[1]]//Log10,Abs[#[[2]]]//Log10}&/@D2SpinlbarTab;


(* ::Text:: *)
(*Interpolating in log-space*)


D2ColdlbarFn=Interpolation[D2ColdlbarTab,InterpolationOrder->1,Method->"Spline"];
D2HotlbarFn=Interpolation[D2HotlbarTab,InterpolationOrder->1,Method->"Spline"];
D2SpinlbarFn=Interpolation[D2SpinlbarTab,InterpolationOrder->1,Method->"Spline"];


(* ::Text:: *)
(*Edges of the patches*)


{D2ColdL,D2ColdR}={10^D2ColdlbarTab[[1,1]],10^D2ColdlbarTab[[-1,1]]};
{D2HotL,D2HotR}={10^D2HotlbarTab[[1,1]],10^D2HotlbarTab[[-1,1]]};
{D2SpinL,D2SpinR}={10^D2SpinlbarTab[[1,1]],10^D2SpinlbarTab[[-1,1]]};


(* ::Text:: *)
(*Defining the full function*)


Clear[d2EcFnd\[Lambda]2]

d2EcFnd\[Lambda]2[\[Lambda]bar_]:=Which[0<=\[Lambda]bar<D2ColdL,limd2EcThick["cold"][\[Lambda]bar],D2ColdL<=\[Lambda]bar<D2ColdR,10^D2ColdlbarFn[Log10[\[Lambda]bar]],((D2ColdR<=\[Lambda]bar<1)||(1<\[Lambda]bar<=D2HotL)),d2Ectwad\[Lambda]2[\[Lambda]bar],D2HotL<\[Lambda]bar<=D2HotR,10^D2HotlbarFn[Log10[\[Lambda]bar]],D2HotR<\[Lambda]bar<=D2SpinL,10^D2HotlbarFn[Log10[D2HotR]]+((-10^D2SpinlbarFn[Log10[D2SpinL]]-10^D2HotlbarFn[Log10[D2HotR]])/(D2SpinL-D2HotR))(\[Lambda]bar-D2HotR),D2SpinL<\[Lambda]bar<=D2SpinR,-10^D2SpinlbarFn[Log10[\[Lambda]bar]],D2SpinR<\[Lambda]bar<=9/8,limd2EcThick["hot"][\[Lambda]bar]]


(* ::Subsection:: *)
(*Analytical Approximation*)


(* ::Text:: *)
(*We are interested in a simple, one-term, analytic expression for Ebar_c(\[Lambda]bar) that may approximately capture the behavior for all \[Lambda]bar.*)
(**)
(*From our study of the thin wall approximation we know Ebar_c ~ (1-\[Lambda]bar)^-2, whereas in the thick wall approximation*)
(**)
(*Ebar_c ~ \[Lambda]bar^2 (\[Lambda]bar<1)*)
(*Ebar_c ~ (9-8\[Lambda]bar)^(3/4) (\[Lambda]bar>1)*)
(**)
(*We can then just fit a non-linear model to both regimes, and find a simple expression that is good to within O(1):*)


EcAnCold[\[Lambda]bar_]=(1.9764934553839089` (1.1308763363462961` -\[Lambda]bar)^0.9143524346793435` \[Lambda]bar^2)/(1-\[Lambda]bar)^2;
EcAnHot[\[Lambda]bar_]=(1.6441933519976608*(9/8-\[Lambda]bar)^(3/4))/(1-\[Lambda]bar)^2;
EcAn[\[Lambda]bar_]:=Which[0<=\[Lambda]bar<1,EcAnCold[\[Lambda]bar],9/8>=\[Lambda]bar>1,EcAnHot[\[Lambda]bar],\[Lambda]bar==1,\[Infinity]]


(* ::Text:: *)
(*For the action, we multiply P_S by Ebar_c:*)


SAn[\[Lambda]bar_]:=Which[0<=\[Lambda]bar<1,((Sqrt[3] A (3+Sqrt[9-8 \[Lambda]bar])^2)/(2 \[Lambda]^(3/2) Sqrt[\[Lambda]bar]))*EcAnCold[\[Lambda]bar],9/8>=\[Lambda]bar>1,((Sqrt[3] A (3+Sqrt[9-8 \[Lambda]bar])^2)/(2 \[Lambda]^(3/2) Sqrt[\[Lambda]bar]))*EcAnHot[\[Lambda]bar],\[Lambda]bar==1,\[Infinity]]


(* ::Text:: *)
(*A simple power-law estimate for S(\[Lambda]bar), based on the inflection points:*)


\[Lambda]barInfCold=0.5229448768554983`;
\[Lambda]barInfHot=1.075660395061243`;


SAnColdPL[\[Lambda]bar_]:=A/\[Lambda]^(3/2) (E^(1.346607299235003`+ 4.854987779449222` \[Lambda]bar))
SAnHotPL[\[Lambda]bar_]:=A/\[Lambda]^(3/2) (E^(54.86051144998107`- 45.609033621086894` \[Lambda]bar))


(* ::Text:: *)
(*The inverse \[Lambda]bar(S):*)


\[Lambda]barColdS[S_]:=-0.27736574434545125`+ 0.20597374193873785`*Log[(S \[Lambda]^(3/2))/A]
\[Lambda]barHotS[S_]:=1.2028431013415912`- 0.02192548099808148` *Log[(S \[Lambda]^(3/2))/A]


(* ::Chapter:: *)
(*3. Important Physical Quantities*)


(* ::Section:: *)
(*3.1 Temperature function*)


(* ::Subsection:: *)
(*TLow & THigh*)


(* ::Text:: *)
(*These will be shorthand names for the lowest and highest temperatures allowed (close to the binodal and spinodal temperatures):*)


TLow=Tc Sqrt[(4 A^2-3 \[Lambda] \[Mu]^2)/(A^2/250-3 \[Lambda] \[Mu]^2)];
THigh=Tc Sqrt[(4 A^2-3 \[Lambda] \[Mu]^2)/((112499 A^2)/25000-3 \[Lambda] \[Mu]^2)];


(* ::Subsection:: *)
(*Potential Parameters*)


(* ::Text:: *)
(*We start by defining a "master list" whose i-th entry consists of the the following information about the i-th particle of the dark sector:*)
(**)
(*{type, N_i, g_i},*)
(**)
(*where type = "B"/"F" denotes boson/fermion, N_i is the degrees of freedom of the i-th particle, and g_i is its coupling to the higgs \[CapitalPhi].*)
(**)
(*\[Mu]^2/2=1/24  \[Sum]_i c_i N_i g_i^2,*)
(**)
(*A/3=1/(12\[Pi]) \[Sum]_B N_B g_B^3,*)
(**)
(*a = \[Pi]^2/30 g_* = \[Pi]^2/30 \[Sum]_(m_i << T) c_i' N_i,*)
(**)
(*where g_* is the usual number of relativistic d.o.f. in the dark sector plasma.*)


Clear[cType,cprimeType,\[Mu]FromList,AFromList,aFromList]

(*Boson/Fermion factor in V_T(T)*)
cType[type_String]:=Which[type=="B",1,type=="F",1/2]

(*Boson/Fermion factor in \[Rho]_rad(T)*)
cprimeType[type_String]:=Which[type=="B",1,type=="F",7/8]

(*\[Mu]-coefficient in thermal potential V_T*)
\[Mu]FromList[particleContent_List]:=Sqrt[1/12 Total[cType[#[[1]]]*#[[2]]*#[[3]]^2&/@particleContent]];

(*A-coefficient in thermal potential V_T*)
AFromList[particleContent_List]:=1/(4\[Pi]) Total[If[#[[1]]=="B",1,0]*cType[#[[1]]]*#[[2]]*#[[3]]^3&/@particleContent];

(*a-coefficient in \[Rho]_rad(T) (a=\[Pi]^2/30 g_* ). Note only massless particles contribute to it*)
aFromList[particleContent_List,scalarValue_]:=\[Pi]^2/30 Total[cprimeType[#[[1]]]*#[[2]]*HeavisideTheta[-Rationalize[(#[[3]]*scalarValue)]]&/@particleContent]/.{HeavisideTheta[0]->1};


(* ::Text:: *)
(*Nevertheless we will most likely not bother nailing down the exact particle content of the dark sector. As such, we will be concerned mostly with \[Mu], A, and a directly.*)


(* ::Subsection:: *)
(*Transition strength \[Alpha]_n, runaway threshold strength \[Alpha]_\[Infinity], and runaway condition*)


(* ::Text:: *)
(*From this, let us write some functions that will help us directly write down the phase transition strength in terms of the relevant parameters.*)
(**)
(*\[Alpha]_\[Epsilon]=-V_b0/\[Rho]_r (vacuum energy),*)
(**)
(*Subscript[\[Alpha], V]\[Congruent]\[CapitalDelta]V_T/\[Rho]_r  (potential energy),*)
(**)
(*Subscript[\[Alpha], L]\[Congruent]\[CapitalDelta]L/\[Rho]_r=\[CapitalDelta](V_T-T*dV_T/dT)/\[Rho]_r  (latent heat),*)
(**)
(*Subscript[\[Alpha], \[Theta]]\[Congruent]\[CapitalDelta]\[Theta]/\[Rho]_r=\[CapitalDelta](V_T-(T/4)*dV_T/dT)/\[Rho]_r (trace anomaly),*)
(**)
(*\[Alpha]_\[Infinity]\[TildeTilde](\[Mu]^2/2) T^2 \[CapitalPhi]_b^2/\[Rho]_r (=\[CapitalDelta]P_LO in Ellis et al. 1903.09642)*)
(**)
(*The runaway condition is*)
(**)
(*\[Kappa]_run>0,*)
(**)
(*where \[Kappa]_run \[Congruent] sign[1-\[Lambda]bar](\[Alpha]_n - \[Alpha]_\[Infinity])/\[Alpha]_n .*)


Clear[\[CapitalDelta]VT,\[CapitalDelta]Lheat,\[CapitalDelta]\[Theta]an]

\[CapitalDelta]VT[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T,\[Lambda]bar,sign,res},
(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs)/.T->Temp;

(*>0 if <\[CapitalPhi]>_b is the true vacuum (cPTs), <0 if it's the false vacuum (hPTs)*)
sign=Sign[1-\[Lambda]bar];

(*\[CapitalDelta]V_T: the potential energy difference \[CapitalDelta]V_T=(V_T+)-(V_T-)*)
res=(sign*(Vs-Vb)/.PotToCoeffs)/.T->Temp;

res
];



\[CapitalDelta]Lheat[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T,\[Lambda]bar,sign,sterm,VbT,bterm,res},
(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs)/.T->Temp;

(*>0 if <\[CapitalPhi]>_b is the true vacuum (cPTs), <0 if it's the false vacuum (hPTs)*)
sign=Sign[1-\[Lambda]bar];

(*symmetric phase term: V_T-T*dV_T/dT*)
sterm=(Vs-T*D[Vs,T]);

(*broken phase term: V_T-T*dV_T/dT*)
VbT=(Vb/.PotToCoeffs);
bterm=(VbT-T*D[VbT,T])/.T->Temp;

(*\[CapitalDelta]L: the difference in latent heat*)
res=sign*(sterm-bterm);

res
];



\[CapitalDelta]\[Theta]an[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T,\[Lambda]bar,sign,sterm,VbT,bterm,res},
(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs)/.T->Temp;

(*>0 if <\[CapitalPhi]>_b is the true vacuum (cPTs), <0 if it's the false vacuum (hPTs)*)
sign=Sign[1-\[Lambda]bar];

(*symmetric phase term: V_T-(T/4)*dV_T/dT*)
sterm=(Vs-T/4*D[Vs,T]);

(*broken phase term: V_T-(T/4)*dV_T/dT*)
VbT=(Vb/.PotToCoeffs);
bterm=(VbT-T/4*D[VbT,T])/.T->Temp;

(*\[CapitalDelta]\[Theta]: the difference in trace anomaly*)
res=sign*(sterm-bterm);

res
];


Clear[\[Alpha]qts,\[Alpha]\[Epsilon],\[Alpha]V,\[Alpha]L,\[Alpha]\[Theta],\[Alpha]n,\[Alpha]\[Infinity],\[Kappa]run]

\[Alpha]qts={"\[Epsilon]","V","L","\[Theta]"};

\[Alpha]\[Epsilon][gStar_,coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Epsilon]0,an,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*vacuum energy density (i.e. at T=0)*)
\[Epsilon]0=Abs[(Vb0/.PotToCoeffs0Temp)];

(*the coefficient in \[Rho]_rad(T)=a_N T^4 (a_N = \[Pi]^2/30 g_* )*)
an=\[Pi]^2/30 gStar;

(*\[Alpha]_\[Epsilon]*)
res=\[Epsilon]0/(an*T^4);

res
]



\[Alpha]V[gStar_,coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,DelV,an,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*potential energy difference*)
DelV=\[CapitalDelta]VT[coeffs,Tc,T];

(*the coefficient in \[Rho]_rad(T)=a_N T^4 (a_N = \[Pi]^2/30 g_* )*)
an=\[Pi]^2/30 gStar;

(*\[Alpha]_V*)
res=DelV/(an*T^4);

res
]



\[Alpha]L[gStar_,coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,DelL,an,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*latent heat difference*)
DelL=\[CapitalDelta]Lheat[coeffs,Tc,T];

(*the coefficient in \[Rho]_rad(T)=a_N T^4 (a_N = \[Pi]^2/30 g_* )*)
an=\[Pi]^2/30 gStar;

(*\[Alpha]_L*)
res=DelL/(an*T^4);

res
]



\[Alpha]\[Theta][gStar_,coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,Del\[Theta],an,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*trace anomaly difference*)
Del\[Theta]=\[CapitalDelta]\[Theta]an[coeffs,Tc,T];

(*the coefficient in \[Rho]_rad(T)=a_N T^4 (a_N = \[Pi]^2/30 g_* )*)
an=\[Pi]^2/30 gStar;

(*\[Alpha]_\[Theta]*)
res=Del\[Theta]/(an*T^4);

res
]



\[Alpha]n[gStar_,coeffs_List,Tcrit_,Temp_,quantity_String]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Alpha]Options={"\[Epsilon]","V","L","\[Theta]","vacuum energy","vacuum","potential energy","potential","latent heat","latent","trace anomaly","trace"},which,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*checking whether the quantity requested is among those we have foreseen*)
If[MemberQ[\[Alpha]Options,quantity],None,Message[\[Alpha]n::NoMethodFound, quantity,\[Alpha]Options]; Abort[]];

(*\[Alpha]_n*)
res=Which[MemberQ[{"\[Epsilon]","vacuum energy","vacuum"},quantity],\[Alpha]\[Epsilon][gStar,coeffs,Tc,T],MemberQ[{"V","potential energy","potential"},quantity],\[Alpha]V[gStar,coeffs,Tc,T],MemberQ[{"L","latent heat","latent"},quantity],\[Alpha]L[gStar,coeffs,Tc,T],MemberQ[{"\[Theta]","trace anomaly","trace"},quantity],\[Alpha]\[Theta][gStar,coeffs,Tc,T]];

res
]
\[Alpha]n::NoMethodFound = "There is no definition or method to compute the PT strength \[Alpha] that makes use of the quantity=`1` you requested. The only options are `2`."; 



\[Alpha]\[Infinity][gStar_,coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[CapitalPhi]VEV,aN,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*the <\[CapitalPhi]> VEV, i.e. its value in the broken phase*)
\[CapitalPhi]VEV=(Fb/.PotToCoeffs);

(*the coefficient in \[Rho]_rad(T)=a_N*T^4 ( a_N=\[Pi]^2/30 g_* )*)
aN=\[Pi]^2/30 gStar;

(*\[Alpha]_\[Infinity]*)
res=(\[Mu]^2/2*T^2*\[CapitalPhi]VEV^2)/(aN*T^4);

res
]



\[Kappa]run[gStar_,coeffs_List,Tcrit_,Temp_,qtyNum_String,qtyDen_String]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Lambda]bar,sign,alphnum,alphden,alphInf,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*the sign of the runaway condition*)
sign=Sign[1-\[Lambda]bar];

(*\[Alpha]_num*)
alphnum=\[Alpha]n[gStar,coeffs,Tc,T,qtyNum];

(*\[Alpha]_den*)
alphden=\[Alpha]n[gStar,coeffs,Tc,T,qtyDen];

(*\[Alpha]_\[Infinity]*)
alphInf=\[Alpha]\[Infinity][gStar,coeffs,Tc,T];

(*\[Kappa]_run*)
res=sign*((alphnum-alphInf)/alphden);

res
]


(* ::Subsection:: *)
(*Daisy contributions*)


(* ::Text:: *)
(*We approximate the daisy contributions as*)
(**)
(*daisy\[TildeTilde](1+(T/\[CapitalPhi]_b)^2)^(3/2) - 1*)


Clear[daisy]

daisy[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[CapitalPhi]VEV,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*the <\[CapitalPhi]> VEV, i.e. its value in the broken phase*)
\[CapitalPhi]VEV=(Fb/.PotToCoeffs);

(*daisy contributions*)
res=(1+T^2/\[CapitalPhi]VEV^2)^(3/2)-1;

res
]


(* ::Subsection:: *)
(*Critical energy E_c(T)*)


(* ::Text:: *)
(*We start by noting that E_c=\[CapitalPhi]_b^2/M Ebar_c. We can then define our critical energy function in terms of the potential coefficients {\[Mu],A,\[Lambda]} (in that order) and T_c and T:*)


Clear[EcTemp]

EcTemp[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Theta],\[Lambda]bar,dim,ec},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=T/Tc;

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*\[CapitalPhi]_b^2/(M*T_c): the dimensionality of E_c, in units of T_c*)
dim=(Fb^2/(M*Tc)/.PotToCoeffs);

(*E_c*)
ec=Tc*dim*EcFn[\[Lambda]bar];

ec
]


(* ::Text:: *)
(*Note that, depending on the potential coefficients, not all values of T will yield sensible values. In particular, T should be located between the binodal and spinodal temperatures.*)


(* ::Subsection:: *)
(*Action S_E(T)*)


(* ::Text:: *)
(*S_E = E_c/T = P_S \[CenterDot] Ebar_c*)


Clear[SETemp]

SETemp[coeffs_List,Tcrit_,Temp_]:=Block[{Tc=Tcrit,T=Temp,Ecrit},

(*E_c*)
Ecrit=EcTemp[coeffs,Tc,T];

(*S_E=E_c/T*)
Ecrit/T
]


(* ::Text:: *)
(*First and second derivatives w.r.t. \[Lambda]bar*)


Clear[dSd\[Lambda],d2Sd\[Lambda]2]

dSd\[Lambda][coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Theta],\[Lambda]bar,ds},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=T/Tc;

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*dS_E/d\[Lambda]bar*)
ds=dPSd\[Lambda]*EcFn[\[Lambda]bar]+PS*dEcFnd\[Lambda][\[Lambda]bar];

ds
]



d2Sd\[Lambda]2[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Theta],\[Lambda]bar,dds},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=T/Tc;

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*d^2S_E/d\[Lambda]bar^2*)
dds=d2PSd\[Lambda]2*EcFn[\[Lambda]bar]+2*dPSd\[Lambda]*dEcFnd\[Lambda][\[Lambda]bar]+PS*d2EcFnd\[Lambda]2[\[Lambda]bar];

dds
]


(* ::Text:: *)
(*First and second derivatives w.r.t. lnT*)


Clear[dSdlnT,d2SdlnT2]

dSdlnT[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Theta],\[Lambda]bar,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=T/Tc;

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*dS_E/dlnT*)
res=d\[Lambda]dlnT*dSd\[Lambda][coeffs,Tc,T];

res
]



d2SdlnT2[coeffs_List,Tcrit_,Temp_]:=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,T=Temp,\[Theta],\[Lambda]bar,res},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=T/Tc;

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*d^2S_E/dlnT^2*)
res=d2\[Lambda]dlnT2*dSd\[Lambda][coeffs,Tc,T]+d\[Lambda]dlnT^2*d2Sd\[Lambda]2[coeffs,Tc,T];

res
]


(* ::Subsection:: *)
(*Nucleation rate \[CapitalGamma]_nucl/\[ScriptCapitalV](T)*)


(* ::Text:: *)
(*The bubble nucleation rate (really, rate per unit volume \[ScriptCapitalV]) is given by*)
(**)
(*\[CapitalGamma]_nucl/\[ScriptCapitalV]\[TildeTilde]K\[CenterDot]T^4\[CenterDot](S_E/(2\[Pi]))^(3/2) e^(-S_E),*)
(**)
(*where S_E=E_c/T is the Euclidean action, and K some numerical coefficient.*)


Clear[\[CapitalGamma]nucl]

\[CapitalGamma]nucl[coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False,yieldLog_:False]:=Block[{\[Theta],action,pref,res},

(*\[Theta]\[Congruent]T/T_c*)
\[Theta]=Temp/Tcrit;

(*S_E*)
action=SETemp[coeffs,Tcrit,Temp];

(*the prefactor from the 0-modes in the path integral*)
pref=If[full,(action/(2\[Pi]))^(3/2),1];

(*\[CapitalGamma]_nucl/\[ScriptCapitalV] vs. log10(\[CapitalGamma]_nucl/\[ScriptCapitalV]). log10(x)=ln(x)/ln(10). It will be useful when computing T_PT.*)
res=If[yieldLog,(Log[Kfactor]+4*Log[Tcrit]+4*Log[\[Theta]]+Log[pref]-action)/Log[10],
Kfactor*(Tcrit^4)*(\[Theta]^4)*pref*Exp[-action]];

res
]


(* ::Subsection:: *)
(*Scale-invariant transition rate parameter \[Beta]bar(T) \[Congruent] d ln \[CapitalGamma]_nucl/d ln T*)


(* ::Text:: *)
(*We define \[Beta]bar_1 \[Congruent] d ln \[CapitalGamma]_nucl/d ln T and \[Beta]bar_2 \[Congruent] d^2 ln \[CapitalGamma]_nucl/d ln T^2:*)


Clear[\[Beta]bar1,\[Beta]bar2]

\[Beta]bar1[coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=Block[{SE,dSE,extra,betabar1},

(*S_E*)
SE=SETemp[coeffs,Tcrit,Temp];

(*dS_E/dlnT*)
dSE=dSdlnT[coeffs,Tcrit,Temp];

(*extra term coming from derivative of the prefactor from the 0-modes of the path integral*)
extra=(3/2)/SE Boole[full];

(*\[Beta]bar_1*)
betabar1=4+dSE*(-1+extra);

betabar1
]



\[Beta]bar2[coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=Block[{SE,dSE,ddSE,extra,betabar2},

(*S_E*)
SE=SETemp[coeffs,Tcrit,Temp];

(*dS_E/dlnT*)
dSE=dSdlnT[coeffs,Tcrit,Temp];

(*d^2S_E/dlnT^2*)
ddSE=d2SdlnT2[coeffs,Tcrit,Temp];

(*extra term coming from derivative of the prefactor from the 0-modes of the path integral*)
extra=(3/2)/SE Boole[full];

(*\[Beta]bar_2*)
betabar2=ddSE(-1+extra)-2/3 (extra*dSE)^2;

betabar2
]


(* ::Section:: *)
(*3.2 Time evolution*)


(* ::Subsection:: *)
(*Evolution of temperature T(t)*)


(* ::Subsubsection:: *)
(*Analytic*)


(* ::Text:: *)
(*In an analytic approximation, we parameterize the temperature evolution with a simple power law*)
(**)
(*T(t)=T_c((t-t_0)/(t_c-t_0))^\[Gamma].*)
(**)
(*From this:*)
(**)
(*dlnT/dt = \[Gamma]/(t-t_0)*)
(*d^2lnT/dt^2 = -\[Gamma]/(t-t_0)^2*)


Clear[tempEvolAn,tOfTempAn]

tempEvolAn[Tcrit_,tcrit_,gamma_,t0_,t_]:=Tcrit*((t-t0)/(tcrit-t0))^gamma

tOfTempAn[Tcrit_,tcrit_,gamma_,t0_,Temp_]:=(tcrit-t0)*(Temp/Tcrit)^(1/gamma)+t0


(* ::Subsubsection:: *)
(*Numeric*)


(* ::Text:: *)
(*However, we will also accommodate numerical (i.e. interpolated) functions.*)


(* ::Text:: *)
(*Below, we define functions to:*)
(**)
(*1. find the time and temperature at which the temperature reaches its maximum value,*)
(*2. find the time at which the temperature reaches a specific value, and*)
(*3. check whether a specified temperature is ever reached:*)


Clear[tTMax,tOfTempNum,noTempFn]

tTMax[Tfn_,argScale_:1]:=tTMax[Tfn,argScale]=Block[{dTdx,xA,xB,xData,prec,Tmx,xmx,result,eps=smallnum},
(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*derivative*)
dTdx[x_]=D[Tfn[x],x];

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tfn]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tfn]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*searching for the time at which we get the maximum temperature*)
{Tmx,xmx}=FindMaximum[Log10[SetPrecision[Tfn[10^Lx],prec]],{Lx,0,Log10[xA],Log10[xB]},MaxIterations->10^6,WorkingPrecision->prec,AccuracyGoal->6,PrecisionGoal->4];
Tmx=10^Tmx;
xmx=10^(Lx/.xmx);

result={xmx*argScale,Tmx}
]


tOfTempNum[Tfn_,Temp_,argScale_:1]:=tOfTempNum[Tfn,Temp,argScale]=Block[{xA,xB,xData,yData,prec,Tmx,xmx,xLSet,xRSet,yLSet,yRSet,xLGuess,xRGuess,xLe,xRi,result,eps=smallnum},
(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tfn]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tfn]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[Tfn]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];

(*searching for the time at which we get the maximum temperature*)
{xmx,Tmx}=tTMax[Tfn,1];

(*testing Temp is in the probed temperature interval*)
If[IntervalMemberQ[Interval[{Max[Tfn[xA],Tfn[xB]],Tmx}],Temp]==False,Message[tOfTempNum::NoTemp, Temp,{Max[Tfn[xA],Tfn[xB]],Tmx}];Abort[]];

(*the initial guesses for the crossing times*)
If[(Length@yData != Length@xData),
	xLGuess=GeometricMean[{xA,xmx}];
	xRGuess=GeometricMean[{xB,xmx}],
	
	xLSet=Select[xData,(#<xmx)&];
	yLSet=yData[[1;;Length@xLSet]];
	yLSet=Abs[yLSet-Temp];
	xLGuess=(Position[yLSet,Min[yLSet]]//Flatten)[[1]];
	xLGuess=xLSet[[xLGuess]];
	
	xRSet=Select[xData,(#>xmx)&];
	yRSet=yData[[(Length@yData-Length@xRSet)+1;;-1]];
	yRSet=Abs[yRSet-Temp];
	xRGuess=(Position[yRSet,Min[yRSet]]//Flatten)[[1]];
	xRGuess=xRSet[[xRGuess]];
	
	Clear[xLSet,xRSet,yLSet,yRSet,xData,yData];
	];

(*times at which the temperature is equal to some value. Since T(t) is concave, there are *two* such times. In increasing order: *)
xLe=(10^Logx)/.FindRoot[Log10[SetPrecision[Tfn[10^Logx],prec]]-Log10[SetPrecision[Temp,prec]],{Logx,Log10[xLGuess],Log10[xA],Log10[xmx*(1-eps)]},MaxIterations->10^6,WorkingPrecision->prec,AccuracyGoal->6,PrecisionGoal->4];
xRi=(10^Logx)/.FindRoot[Log10[SetPrecision[Tfn[10^Logx],prec]]-Log10[SetPrecision[Temp,prec]],{Logx,Log10[xRGuess],Log10[xmx*(1+eps)],Log10[xB]},MaxIterations->10^6,WorkingPrecision->prec,AccuracyGoal->6,PrecisionGoal->4];

(*the results*)
result=argScale*{xLe,xRi};

result
]
tOfTempNum::NoTemp="The temperature Temp=`1` is not in the interval `2` of temperatures probed by the Tfn function.";


noTempFn[Tfn_,Temp_,argScale_:1]:=Block[{xA,xB,xData,Tmx,xmx,Tmin,eps=smallnum},

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tfn]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tfn]//Flatten);

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*searching for the time at which we get the maximum temperature*)
{xmx,Tmx}=tTMax[Tfn,1];

(*lowest temperature, depending on direction*)
Tmin=Max[Tfn[xB],Tfn[xA]];

If[IntervalMemberQ[Interval[{Tmin,Tmx}],Temp]==False,Message[noTempFn::NoTemp, Temp,{Tmin,Tmx}];Abort[]]
]
noTempFn::NoTemp = "The temperature Temp=`1` is not in the interval `2` of temperatures probed by the Tfn function."; 


(* ::Subsection:: *)
(*Action rate change*)


(* ::Text:: *)
(*S_ 1\[Congruent]dS_E/dt = dlnT/dt dS_E/d lnT*)
(*S_ 2 \[Congruent]d^2S_E/dt^2 = d^2 lnT/dt^2 dS_E/d lnT + (d lnT/dt)^2 d^2S_E/d lnT^2*)


Clear[S1Rate,S2Rate]

(*general formula for dS_E/dt*)
S1Rate[dlnTdt_,coeffs_List,Tcrit_,Temp_]:=dlnTdt*dSdlnT[coeffs,Tcrit,Temp]

(*general formula for d^2S_E/dt^2*)
S2Rate[dlnTdt_,d2lnTdt2_,coeffs_List,Tcrit_,Temp_]:=d2lnTdt2*dSdlnT[coeffs,Tcrit,Temp]+dlnTdt^2*d2SdlnT2[coeffs,Tcrit,Temp]


(* ::Text:: *)
(*For the analytic, single-power law of the temperature, we have:*)
(**)
(*S_ 1(t)=(\[Gamma]/(t-t_ 0))((dS_E)/(d lnT))(T)*)
(*S_2(t) = -(\[Gamma]/(t-t_0)^2)((dS_E)/(d lnT)) + (\[Gamma]/(t-t_ 0))^2 ((d^2S_E)/(d lnT^2))*)


Clear[S1An,S2An,S1OfTempAn,S2OfTempAn]

(*analytic approximation*)
S1An[gamma_,t0_,time_,coeffs_List,Tcrit_,Temp_]:=S1Rate[gamma/(time-t0),coeffs,Tcrit,Temp]
S2An[gamma_,t0_,time_,coeffs_List,Tcrit_,Temp_]:=S2Rate[gamma/(time-t0),-(gamma/(time-t0)^2),coeffs,Tcrit,Temp]

(*analytic approximation, as a function of temperature and not time*)
S1OfTempAn[gamma_,t0_,tcrit_,coeffs_List,Tcrit_,Temp_]:=S1An[gamma,t0,tOfTempAn[Tcrit,tcrit,gamma,t0,Temp],coeffs,Tcrit,Temp]
S2OfTempAn[gamma_,t0_,tcrit_,coeffs_List,Tcrit_,Temp_]:=S2An[gamma,t0,tOfTempAn[Tcrit,tcrit,gamma,t0,Temp],coeffs,Tcrit,Temp]


(* ::Subsection:: *)
(*Transition rate parameter \[Beta](t)*)


(* ::Text:: *)
(*\[Beta]_1(t) \[Congruent] d ln \[CapitalGamma]_nucl/d t = d ln T/dt \[Beta]bar_1*)
(*\[Beta]_2(t)\[Congruent] d^2 ln \[CapitalGamma]_nucl/d t^2 = d^2 ln T/dt^2 \[Beta]bar_1 + (d ln T/dt)^2 \[Beta]bar_2*)


Clear[\[Beta]1Rate,\[Beta]2Rate]

(*general formula for \[Beta]_1*)
\[Beta]1Rate[dlnTdt_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=dlnTdt*\[Beta]bar1[coeffs,Tcrit,Temp,Kfactor,full]

(*general formula for \[Beta]_2*)
\[Beta]2Rate[dlnTdt_,d2lnTdt2_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=d2lnTdt2*\[Beta]bar1[coeffs,Tcrit,Temp,Kfactor,full]+dlnTdt^2*\[Beta]bar2[coeffs,Tcrit,Temp,Kfactor,full]


(* ::Text:: *)
(*For the analytic approximation we have:*)
(**)
(*\[Beta]_1(t) = \[Gamma]/(t-t_0) \[Beta]bar_1(T).*)
(*\[Beta]_2(t) = -(\[Gamma]/(t-t_0)^2) \[Beta]bar_1 + (\[Gamma]/(t-t_ 0))^2 \[Beta]bar_2*)


Clear[beta1An,beta2An,beta1OfTempAn,beta2OfTempAn]

(*analytic approximation*)
beta1An[gamma_,t0_,time_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=\[Beta]1Rate[gamma/(time-t0),coeffs,Tcrit,Temp,Kfactor,full]
beta2An[gamma_,t0_,time_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=\[Beta]2Rate[gamma/(time-t0),-(gamma/(time-t0)^2),coeffs,Tcrit,Temp,Kfactor,full]

(*analytic approximation, as a function of temperature and not time*)
beta1OfTempAn[gamma_,t0_,tcrit_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=beta1An[gamma,t0,tOfTempAn[Tcrit,tcrit,gamma,t0,Temp],coeffs,Tcrit,Temp,Kfactor,full]
beta2OfTempAn[gamma_,t0_,tcrit_,coeffs_List,Tcrit_,Temp_,Kfactor_:1,full_:False]:=beta2An[gamma,t0,tOfTempAn[Tcrit,tcrit,gamma,t0,Temp],coeffs,Tcrit,Temp,Kfactor,full]


(* ::Subsection:: *)
(*Fractional volume h(t)*)


(* ::Text:: *)
(*h(t)=exp[-\[Integral]_ (t_c)^t dt' 4\[Pi]/3 v_w^3 (t-t')^3  \[CapitalGamma]_nucl/\[ScriptCapitalV]].*)
(**)
(*For simplicity we use log10[-ln[h(log10(t))] instead.*)


Clear[Lmlnh]

Lmlnh[direction_,vw_,TempEvol_List,coeffs_List,Tcrit_,Kfactor_:1,full_:False,atau_:{1,1},Nlx_:250]:=Lmlnh[direction,vw,TempEvol,coeffs,Tcrit,Kfactor,full,atau,Nlx]=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,Tlo,Thi,Toffset,Tofx,tScale,xmax,Tmax,TmaxFlag,xA,xB,xData,prec,Trange,xrange,scaleFactor,comovingTime,aofx,tauofx,scale,lnscale,lnGammaPT,GammaPT,radius,integrand,OnlyHot,zrange,\[CapitalDelta]lz,lzTable,lxTable,Nmids,\[CapitalDelta]lxTable,lxMids,integral,table,resTable,non0Tab,reg,data,res,eps=smallnum},

(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*the phase transition is either cold (subcritical) or hot (supercritical):*)
noOption[direction,{"cold","hot"}];

(*For the hot PT case we will need to offset the leftmost value of x*)
OnlyHot=If[direction=="hot",1,0];

(*checking consistency of 'TempEvol' arguments.*)
If[(Length@TempEvol)!=2,Message[Lmlnh::TempEvolError];Abort[]];

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tofx]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tofx]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*time t at which T(t) reaches a maximum*)
{xmax,Tmax}=tTMax[Tofx,1];

(*checking whether T_c is in the interval of temperatures probed by T(t) function*)
noTempFn[Tofx,Tc,1];

(*lowest/highest temperatures probed by our pre-computed critical bubbles*)
Tlo=TLow;
Thi=THigh;
(*correcting Thi in case this corresponds to a non-existent temperature (because the spinodal temperature never disappears!)*)
Thi=If[(Im[Thi]!=0)||(Thi===ComplexInfinity),Tmax*2,Thi];

(*whether Tmax is in the interval of temperatures for which we computed the critical bubbles*)
TmaxFlag=IntervalMemberQ[Interval[{Tlo,Thi}],Tmax];

(*the temperature ranges for the hot (supercritical)/cold (subcritical) directions, depending on whether TmaxFlag is true ot no. If True, then extend the temperatures explored by the hot (supercritical) PT all the way past Tmax, down to the critical temperature back gain. If False, then only consider temperatures up to Thi.*)
Trange=If[TmaxFlag,
If[direction=="hot",{Tc,Tc},{Tc,Max[Tofx[xB],Tlo]}],
If[direction=="hot",{Tc,Thi},{Tc,Max[Tofx[xB],Tlo]}]
];

(* the corresponding range of times for the temperatures probed. Note that if TmaxFlag\[Equal]True then the latest temperature explored in the hot (supercritical) PT is the critical temperature (after passing by Tmax), which is located in the "downwards" trajectory of the concave temperature curve.*)
xrange=If[TmaxFlag,
If[direction=="hot",{tOfTempNum[Tofx,Trange[[1]],1][[1]],tOfTempNum[Tofx,Trange[[2]],1][[2]]}(*hot: {t_c1,t_c2}*),{tOfTempNum[Tofx,Trange[[1]],1][[2]],tOfTempNum[Tofx,Trange[[2]],1][[2]]}(*cold: {t_c2,t_lo/end}*)],
If[direction=="hot",{tOfTempNum[Tofx,Trange[[1]],1][[1]],tOfTempNum[Tofx,Trange[[2]],1][[1]]}(*hot: {t_c1,t_hi}*),{tOfTempNum[Tofx,Trange[[1]],1][[2]],tOfTempNum[Tofx,Trange[[2]],1][[2]]}(*cold: {t_c2,t_lo/end}*)]
];

(*updating xrange: offset from xcrit (when T=Tcrit)*)
xrange=If[TmaxFlag,
If[direction=="hot",{xrange[[1]]*(1+eps),xrange[[2]]*(1-eps)},{xrange[[1]]*(1+eps),xrange[[2]]}],
If[direction=="hot",{xrange[[1]]*(1+eps),xrange[[2]]},{xrange[[1]]*(1+eps),xrange[[2]]}]
];

(*scale factor and comoving time*)
{scaleFactor,comovingTime}=atau;

(*scale factor*)
aofx[xx_]:=If[scaleFactor===1,1,scaleFactor[xx]];

(*comoving time*)
tauofx[xx_]:=If[comovingTime===1,xx,comovingTime[xx]];

(*overall scale of integral: scale^4*)
scale=Tc*tScale;
(*lnS \[Congruent] ln(scale)*)
lnscale=Log[scale];

(*ln\[CapitalGamma] \[Congruent] ln(\[CapitalGamma]_nucl(x)/T_c^4)*)
lnGammaPT[xx_]:=\[CapitalGamma]nucl[coeffs,1,Tofx[xx]/Tc,Kfactor,full,True]*Log[10];

(*dimensionless (comoving) nucleation rate: a^4 \[CapitalGamma]_nucl(x)*tScale^4 = a^4 (\[CapitalGamma]_nucl(x)/T_c^4)*scale^4 = exp[ln\[CapitalGamma]+4*lnS] (a dimensionless quantity), regularized if it's too small. The reason for combining the nucleation rate and the overall scale of the integral is to make the exponent bigger*)
GammaPT[xx_]:=If[(lnGammaPT[xx]+4*lnscale)>-500,aofx[xx]^4*Exp[lnGammaPT[xx]+4*lnscale],0];

(*dimensionless (comoving) bubble radius: w/ or w/o scale factor*)
radius[x_,xp_]:=vw*(tauofx[x]-tauofx[xp])*HeavisideTheta[x-xp];

(*integrand, in ln-space*)
integrand[x_,xp_]:=xp/aofx[xp]*((4\[Pi])/3 (radius[x,xp])^3 * GammaPT[xp]);

(*defining z=x-xA: a time variable offset by xA: it helps to zoom-in to low-x*)
zrange=xrange-xA*OnlyHot;

(*sampling steps in ln(z) for the numerical integral, so that there are Nlx number of steps*)
\[CapitalDelta]lz=(Log[zrange[[2]]]-Log[zrange[[1]]])/(Nlx-1);

(*table of Nlx ln(z) values*)
lzTable=Log[zrange[[1]]]+Table[\[CapitalDelta]lz*(i-1),{i,Nlx}];

(*table of the Nlx corresponding ln(x) values*)
lxTable=Log[Exp[lzTable]+xA*OnlyHot];

(*table of mid-point widths, so that there are Nmids rectangles and midpoints*)
Nmids=Nlx;
\[CapitalDelta]lxTable=(Table[lx,{lx,lxTable}]-lxTable[[1]])/Nmids;

(*table of ln(x) mid-points*)
lxMids=Table[Table[(lxTable[[1]]+\[CapitalDelta]lxTable[[i]]/2)+\[CapitalDelta]lxTable[[i]]*(j-1),{j,Nmids}],{i,lxTable//Length}];

(*table of Riemann sum terms to integrate, already weighted by their widths. The rows (i) are x, columns (j) are xp.*)
table=Table[Table[\[CapitalDelta]lxTable[[i]]*integrand[Exp[lxTable[[i]]],Exp[lxMids[[i,j]]]],{j,(Dimensions[lxMids][[2]])}],{i,lxTable//Length}];

(*integrating along xp: a simple sum of columns, for each row!*)
resTable=Total[table,{2}];
Clear[lzTable,\[CapitalDelta]lxTable,lxMids,table];

(*selecting those that are not 0, to regularize*)
non0Tab=Select[resTable,#!=0&];
If[(Length@non0Tab)==0,Message[Lmlnh::NoNucleation,direction];Abort[]];

(*regularizing*)
reg=Min[non0Tab]*eps*eps;
resTable=If[#==0,reg,#]&/@resTable;

(*data to interpolate, in log10 (which is ln/ln(10))*)
data=Transpose[{lxTable/Log[10],Log10[resTable]}];
Clear[resTable];

(*interpolating the data*)
res=Interpolation[data];

res
]
Lmlnh::TempEvolError="'TempEvol' should be {Tofx, tScale}: a list with only two elements: the temperature as a function of a time-like parameter x, T(x), and the time-scale given by the ratio t/x.";
Lmlnh::NoNucleation="The nucleation rate is so small at all times ((\[CapitalGamma]/\[ScriptCapitalV] * tScale^4) < e^(-500)~7.e-218) that there are effectively no bubbles being produced, and the `1` phase transition does not take place.";


(* ::Subsection:: *)
(*Bubble number density n_b(t)*)


(* ::Text:: *)
(*n_b(t)=\[Integral]_ (t_c)^t dt' \[CapitalGamma]_nucl/\[ScriptCapitalV] (t') h(t').*)
(**)
(*For simplicity we use log10[n_b(log10(t))] instead.*)
(**)
(*Note that from here we can define R_*\[Congruent]n_b^(-1/3), the mean bubble separation.*)


Clear[Lnbubble]

Lnbubble[LmlnhLx_,TempEvol_List,coeffs_List,Tcrit_,xVal_:"all",Kfactor_:1,full_:False,atau_:{1,1}]:=Lnbubble[LmlnhLx,TempEvol,coeffs,Tcrit,xVal,Kfactor,full,atau]=Block[{Tc=Tcrit,Tofx,tScale,scaleFactor,comovingTime,aofx,tauofx,LxA,LxB,LxData,scale,lnscale,lnGammaPT,lnh,\[CapitalGamma]h,integrand,lxTable,lxMids,table,integral,Nmids,\[CapitalDelta]lx,\[CapitalDelta]lxTable,resTable,non0Tab,reg,Lnorm,data,res,eps=smallnum},

(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*checking consistency of 'TempEvol' arguments.*)
If[(Length@TempEvol)!=2,Message[Lnbubble::TempEvolError];Abort[]];

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*scale factor and comoving time*)
{scaleFactor,comovingTime}=atau;

(*scale factor*)
aofx[xx_]:=If[scaleFactor===1,1,scaleFactor[xx]];

(*comoving time*)
tauofx[xx_]:=If[comovingTime===1,xx,comovingTime[xx]];

(*anatomy of the interpolating function of the fractional metastable volume. NOTE: must have been computed using the same parameters!*)
{LxA,LxB}=InterpolatingFunctionDomain[LmlnhLx]//Flatten;
LxData=InterpolatingFunctionCoordinates[LmlnhLx]//Flatten;

(*overall scale of the result: T_c^4*tScale=(T_c*tScale)^4/tScale^3. NOTE: we will need to divide by tScale^3 in the end*)
scale=Tc*tScale;
(*lnS \[Congruent] ln(scale)*)
lnscale=Log[scale];

(*ln\[CapitalGamma] \[Congruent] ln(\[CapitalGamma]_nucl(x)/T_c^4)*)
lnGammaPT[xx_]:=\[CapitalGamma]nucl[coeffs,1,Tofx[xx]/Tc,Kfactor,full,True]*Log[10];

(*lnh \[Congruent] ln(h(x))*)
lnh[xx_]:=-10^LmlnhLx[Log10[xx]];

(*dimensionless (comoving) nucleation rate times fractional volume in the metastable phase: a^4*\[CapitalGamma]_nucl(x)*tScale^4*h(x)= a^4*(\[CapitalGamma]_nucl(x)/T_c^4)*scale^4*h(x) = a^4*exp[ln\[CapitalGamma]+4*lnS+lnh] (a dimensionless quantity), regularized if it's too small. The reason for combining the nucleation rate, the overall scale of the integral, and the fractional volume h(x) in the metastable phase, is to make the exponent bigger*)
\[CapitalGamma]h[xx_]:=If[(lnGammaPT[xx]+4*lnscale+lnh[xx])>-500,aofx[xx]^4*Exp[lnGammaPT[xx]+4*lnscale+lnh[xx]],0];

(*integrand, in ln-space*)
integrand[x_,xp_]:=aofx[x]^-3*xp/aofx[xp]*\[CapitalGamma]h[xp]*(HeavisideTheta[x-xp]*Sign[x-xp]);

(*calculating result in two cases: for all x-values, or for a specific value*)
If[xVal=="all",


(*A: no specific x-value: compute full function*)
(*table of the Nlx sampled ln(x) values*)
lxTable=(LxData*Log[10]);

(*table of mid-point widths, so that there are Nmids rectangles and midpoints*)
Nmids=(LxData//Length);
\[CapitalDelta]lxTable=(Table[lx,{lx,lxTable}]-lxTable[[1]])/Nmids;

(*table of ln(x) mid-points*)
lxMids=Table[Table[(lxTable[[1]]+\[CapitalDelta]lxTable[[i]]/2)+\[CapitalDelta]lxTable[[i]]*(j-1),{j,Nmids}],{i,lxTable//Length}];

(*table of Riemann sum terms to integrate, already weighted by their widths. The rows (i) are x, columns (j) are xp.*)
table=Table[Table[\[CapitalDelta]lxTable[[i]]*integrand[Exp[lxTable[[i]]],Exp[lxMids[[i,j]]]],{j,(Dimensions[lxMids][[2]])}],{i,lxTable//Length}];

(*integrating along xp: a simple sum of columns, for each row!*)
resTable=Total[table,{2}];
Clear[\[CapitalDelta]lxTable,lxMids,table];

(*selecting those that are not 0, to regularize*)
non0Tab=Select[resTable,#!=0&];
If[(Length@non0Tab)==0,Message[Lnbubble::NoNucleation];Abort[]];

(*regularizing*)
reg=Min[non0Tab]*eps*eps;
resTable=If[#==0,reg,#]&/@resTable;

(*normalization correction for the scale of the result: dividing by tScale^3: Subscript[log, 10]tScale^3 = 3 Subscript[log, 10]tScale*)
Lnorm=3*Log10[tScale];

(*data to interpolate, in Subscript[log, 10] (which is ln/ln(10))*)
data=Transpose[{lxTable/Log[10],Log10[resTable]-Lnorm}];
Clear[resTable];

(*interpolating the data*)
res=Interpolation[data],


(*B: specific x-value: compute result at that point*)
(*number of mid-points to use*)
Nmids=(LxData//Length);

(*size of steps*)
\[CapitalDelta]lx=((Log[xVal]-LxA*Log[10])/Nmids);

(*table of sampled ln(x) values*)
lxTable=(LxA*Log[10]+Table[\[CapitalDelta]lx*i,{i,0,Nmids}]);

(*table of ln(x) mid-points*)
lxMids=Table[(lxTable[[1]]+\[CapitalDelta]lx/2)+\[CapitalDelta]lx*(j-1),{j,Nmids}];

(*table of Riemann sum terms to integrate, already weighted by their widths.*)
table=Table[\[CapitalDelta]lx*integrand[xVal,Exp[lxMids[[j]]]],{j,lxMids//Length}];

(*integrating along xp: a simple sum of columns, for each row!*)
res=Total[table];
Clear[\[CapitalDelta]lx,lxMids,table];

(*normalization correction for the scale of the result: dividing by tScale^3: Subscript[log, 10]tScale^3 = 3 Subscript[log, 10]tScale*)
Lnorm=3*Log10[tScale];

(*final result*)
res=Log10[res]-Lnorm;
];

res
]

Lnbubble::TempEvolError="'TempEvol' should be {Tofx, tScale}: a list with only two elements: the temperature as a function of a time-like parameter x, T(x), and the time-scale given by the ratio t/x.";
Lnbubble::NoNucleation="The nucleation rate is so small at all times that there are effectively no bubbles being produced.";


(* ::Subsection:: *)
(*Mean bubble radius \[LeftAngleBracket]R_b(t)\[RightAngleBracket]*)


(* ::Text:: *)
(*The mean bubble radius:*)
(**)
(*\[LeftAngleBracket]R_b(t)\[RightAngleBracket]=[\[Integral]_ (t_c)^t dt' \[CapitalGamma]_nucl/\[ScriptCapitalV] (t') h(t') v_w(t-t')]/n_b(t).*)
(**)
(*For simplicity, we will use log10[\[LeftAngleBracket]R_b(log10(t))\[RightAngleBracket]] instead.*)


Clear[LRbubble]

LRbubble[LmlnhLx_,LnbLx_,vw_,TempEvol_List,coeffs_List,Tcrit_,xVal_:"all",Kfactor_:1,full_:False,atau_:{1,1}]:=LRbubble[LmlnhLx,LnbLx,vw,TempEvol,coeffs,Tcrit,xVal,Kfactor,full,atau]=Block[{Tc=Tcrit,Tofx,tScale,scaleFactor,comovingTime,aofx,tauofx,LxA,LxB,LxData,LnbData,scale,lnscale,lnGammaPT,lnh,\[CapitalGamma]h,radius,integrand,lxTable,Nmids,\[CapitalDelta]lx,\[CapitalDelta]lxTable,lxMids,table,resTable,non0Tab,reg,Lnorm,data,res,eps=smallnum},

(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*checking consistency of 'TempEvol' arguments.*)
If[(Length@TempEvol)!=2,Message[LRbubble::TempEvolError];Abort[]];

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*scale factor and comoving time*)
{scaleFactor,comovingTime}=atau;

(*scale factor*)
aofx[xx_]:=If[scaleFactor===1,1,scaleFactor[xx]];

(*comoving time*)
tauofx[xx_]:=If[comovingTime===1,xx,comovingTime[xx]];

(*anatomy of the interpolating function of fractional metastable volume. NOTE: must have been computed using the same parameters!*)
{LxA,LxB}=InterpolatingFunctionDomain[LmlnhLx]//Flatten;
LxData=InterpolatingFunctionCoordinates[LmlnhLx]//Flatten;

(*extracting data from bubble number density. NOTE: must have been computed using the same parameters!*)
LnbData=InterpolatingFunctionValuesOnGrid[LnbLx]//Flatten;

(*overall scale of the result: T_c^4*tScale^2=(T_c*tScale)^4/tScale^2. NOTE: we will need to divide by tScale^2 in the end*)
scale=Tc*tScale;
(*lnS \[Congruent] ln(scale)*)
lnscale=Log[scale];

(*ln\[CapitalGamma] \[Congruent] ln(\[CapitalGamma]_nucl(x)/T_c^4)*)
lnGammaPT[xx_]:=\[CapitalGamma]nucl[coeffs,1,Tofx[xx]/Tc,Kfactor,full,True]*Log[10];

(*lnh \[Congruent] ln(h(x))*)
lnh[xx_]:=-10^LmlnhLx[Log10[xx]];

(*dimensionless (comoving) nucleation rate times fractional volume in the metastable phase: a^4*\[CapitalGamma]_nucl(x)*tScale^4*h(x)= a^4*(\[CapitalGamma]_nucl(x)/T_c^4)*scale^4*h(x) = a^4*exp[ln\[CapitalGamma]+4*lnS+lnh] (a dimensionless quantity), regularized if it's too small. The reason for combining the nucleation rate, the overall scale of the integral, and the fractional volume h(x) in the metastable phase, is to make the exponent bigger*)
\[CapitalGamma]h[xx_]:=If[(lnGammaPT[xx]+4*lnscale+lnh[xx])>-500,aofx[xx]^4*Exp[lnGammaPT[xx]+4*lnscale+lnh[xx]],0];

(*dimensionless (comoving) bubble radius: w/ or w/o scale factor*)
radius[x_,xp_]:=vw*(tauofx[x]-tauofx[xp])*HeavisideTheta[x-xp];

(*integrand, in ln-space*)
integrand[x_,xp_]:=aofx[x]*xp/aofx[xp]*(\[CapitalGamma]h[xp]*radius[x,xp]);

(*calculating result in two cases: for all x-values, or for a specific value*)
If[xVal=="all",


(*A: no specific x-value: compute full function*)
(*table of the Nlx sampled ln(x) values*)
lxTable=(LxData*Log[10]);

(*table of mid-point widths, so that there are Nmids rectangles and midpoints*)
Nmids=(LxData//Length);
\[CapitalDelta]lxTable=(Table[lx,{lx,lxTable}]-lxTable[[1]])/Nmids;

(*table of ln(x) mid-points*)
lxMids=Table[Table[(lxTable[[1]]+\[CapitalDelta]lxTable[[i]]/2)+\[CapitalDelta]lxTable[[i]]*(j-1),{j,Nmids}],{i,lxTable//Length}];

(*table of Riemann sum terms to integrate, already weighted by their widths. The rows (i) are x, columns (j) are xp.*)
table=Table[Table[\[CapitalDelta]lxTable[[i]]*integrand[Exp[lxTable[[i]]],Exp[lxMids[[i,j]]]],{j,(Dimensions[lxMids][[2]])}],{i,lxTable//Length}];

(*integrating along xp: a simple sum of columns, for each row!*)
resTable=Total[table,{2}];
Clear[\[CapitalDelta]lxTable,lxMids,table];

(*selecting those that are not 0, to regularize*)
non0Tab=Select[resTable,#!=0&];
If[(Length@non0Tab)==0,Message[LRbubble::NoNucleation];Abort[]];

(*regularizing*)
reg=Min[non0Tab]*eps*eps;
resTable=If[#==0,reg,#]&/@resTable;

(*normalization factor tScale^2\[CenterDot]Subscript[n, b]\[CenterDot]a^3 (present due to dimensionality of the probability distribution) in Subscript[log, 10]: Subscript[log, 10](tScale^2Subscript[n, b]a^3)=2 Subscript[log, 10]tScale + Subscript[log, 10]Subscript[n, b] + 3 Subscript[log, 10]a*)
Lnorm=2*Log10[tScale]+LnbData+3Log10[aofx[Exp[lxTable]]];

(*data to interpolate, in Subscript[log, 10] (which is ln/ln(10))*)
data=Transpose[{lxTable/Log[10],Log10[resTable]-Lnorm}];
Clear[resTable,LnbData,Lnorm];

(*interpolating the data*)
res=Interpolation[data],


(*B: specific x-value: compute result at that point*)
(*number of mid-points to use*)
Nmids=(LxData//Length);

(*size of steps*)
\[CapitalDelta]lx=((Log[xVal]-LxA*Log[10])/Nmids);

(*table of sampled ln(x) values*)
lxTable=(LxA*Log[10]+Table[\[CapitalDelta]lx*i,{i,0,Nmids}]);

(*table of ln(x) mid-points*)
lxMids=Table[(lxTable[[1]]+\[CapitalDelta]lx/2)+\[CapitalDelta]lx*(j-1),{j,Nmids}];

(*table of Riemann sum terms to integrate, already weighted by their widths.*)
table=Table[\[CapitalDelta]lx*integrand[xVal,Exp[lxMids[[j]]]],{j,lxMids//Length}];

(*integrating along xp: a simple sum of columns, for each row!*)
res=Total[table];
Clear[\[CapitalDelta]lx,lxMids,table];

(*normalization factor tScale^2\[CenterDot]Subscript[n, b]\[CenterDot]a^3 (present due to dimensionality of the probability distribution) in Subscript[log, 10]: Subscript[log, 10](tScale^2Subscript[n, b]a^3)=2 Subscript[log, 10]tScale + Subscript[log, 10]Subscript[n, b] + 3 Subscript[log, 10]a*)
Lnorm=2*Log10[tScale]+LnbLx[Log10[xVal]]+3Log10[aofx[xVal]];

(*final result*)
res=Log10[res]-Lnorm;
];

res
]

LRbubble::TempEvolError="'TempEvol' should be {Tofx, tScale}: a list with only two elements: the temperature as a function of a time-like parameter x, T(x), and the time-scale given by the ratio t/x.";
LRbubble::NoNucleation="The nucleation rate is so small at all times that there are effectively no bubbles being produced.";


(* ::Subsection:: *)
(*Temperature of the Phase Transition T_PT*)


(* ::Text:: *)
(*This is obtained by demanding that there is roughly 1 bubble in a bubble-size volume (a sphere of radius v_w/\[Beta]) in a time 1/\[Beta], i.e. -ln h(t)=1*)
(**)
(*Using the saddle point approximation we can analytically compute -ln h(t) as:*)
(*(8\[Pi] v_w^3 \[CapitalGamma]_nucl(T_PT)/\[ScriptCapitalV])/\[Beta]^4(T_PT)=1.*)
(**)
(*However, since this is quickly varying (due to e^-S_E in \[CapitalGamma]_nucl/\[ScriptCapitalV]), it is more convenient to use the condition log10(ln h(t))=0 instead*)


Clear[condPT,LogCondPT,NoDLogCondPT]

(*condition for PT: must be equal to 1*)
condPT[vw_,GammaNucl_,betaRate_]:=(8\[Pi] vw^3 GammaNucl)/betaRate^4

(*log10 version of the above condition, useful for numerical solutions in order to avoid Exp[-LARGE NUMBER] in \[CapitalGamma]_nucl/\[ScriptCapitalV]*)
LogCondPT[vw_,LogGammaNucl_,betaRate_]:=Log10[8\[Pi]]+3Log10[vw]+LogGammaNucl-4Log10[Abs[betaRate]]

(*same as above, except the dimensions of \[CapitalGamma]_nucl/V and \[Beta] have been factored out. Here LogGammList={log10(\[CapitalGamma]_nucl/V/d_1^4), d_1 } and betaList={\[Beta]/d_2, d_2}, with d_1 and d_2 the typical dimensions of \[CapitalGamma]_nucl/V and \[Beta] respectively *)
NoDLogCondPT[vw_,LogGammList_List,betaList_List]:=Log10[8\[Pi]]+3Log10[vw]+LogGammList[[1]]-4Log10[Abs[betaList[[1]]]]+4Log10[LogGammList[[2]]/betaList[[2]]]


Clear[tTPT]

tTPT[direction_,vw_,TempEvol_List,coeffs_List,Tcrit_,method_:"analytic",Kfactor_:1,full_:False,atau_:{1,1},Nlx_:250]:=tTPT[direction,vw,TempEvol,coeffs,Tcrit,method,Kfactor,full,atau,Nlx]=Block[{gamma,t0,tcrit,Tofx,tScale,dlnTdt,xmax,Tmax,xA,xB,xData,yData,prec,\[Mu],A,\[Lambda],Tc=Tcrit,Tlo,Thi,TmaxFlag,PTCritFlag,Toffset,Tini,TLeft,TRight,x0,xlo,xhi,xRange,xLeft,xRight,GammaPT,LogGammaPT,betaPT,cond,LogCond,noDLogCond,LhLx,LnbLx,LRLx,data,xpt,tpt,Tpt,result,eps=smallnum},
(*NOTE: CRUCIAL FOR NUMERICAL METHOD: we assume T(t) is a concave function, and that therefore it has a maximum in its domain. True during reheating.*)

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*lowest/highest temperatures probed by our pre-computed critical bubbles*)
Tlo=TLow;
Thi=THigh;

(*the phase transition is either cold (subcritical) or hot (supercritical):*)
noOption[direction,{"cold","hot"}];

(*the 'method' argument is either analytic, semi-analytic, numeric, or alternative*)
noOption[method,{"analytic","semi","numeric","alt"}];

(*checking consistency between 'method' and 'TempEvol' arguments.*)
If[(method=="analytic")&&(Length@TempEvol)!=3,Message[tTPT::AnalyticTempEvolError];Abort[]];
If[((method=="semi")||(method=="numeric")||(method=="alt"))&&(Length@TempEvol)!=2,Message[tTPT::NumericTempEvolError];Abort[]];

(*temperature offset from T_c depending on direction*)
Toffset=Tc*(1+eps*If[direction=="cold",-1,+1]);

(*routine for analytic method*)
If[method=="analytic",

(*{\[Gamma], t_0, t_c}*)
{gamma,t0,tcrit}=TempEvol;

(*correcting Thi in case this corresponds to a non-existent temperature (because the spinodal temperature never disappears!)*)
Thi=If[(Im[Thi]!=0)||(Thi===ComplexInfinity),Tc*10,Thi];

(*the initial guess for the PT temperature is either below T_c for cold (subcritical) PT, or above T_c for hot (supercritical) PT.*)
Tini=If[direction=="cold",GeometricMean[{Tlo,Tc}],GeometricMean[{Thi,Tc}]];

(*the lower and upper edges of the numerical solver*)
TLeft=If[direction=="cold",Tlo,Toffset];
TRight=If[direction=="cold",Toffset,Thi];

(*\[CapitalGamma]_nucl(T_PT)*)
LogGammaPT[TT_]:=\[CapitalGamma]nucl[coeffs,Tc,TT,Kfactor,full,True];

(*\[Beta](T_PT)*)
betaPT[TT_]:=beta1OfTempAn[gamma,t0,tcrit,coeffs,Tc,TT,Kfactor,full];

(*log-condition for PT: must be equal to 0*)
LogCond[TT_?NumericQ]:=LogCondPT[vw,LogGammaPT[TT],betaPT[TT]];

(*checking whether the PT finished immediately after Tc*)
PTCritFlag = (LogCond[Toffset]>0);
If[(PTCritFlag == True),Message[tTPT::AnalyticCritPT,direction,Tc,Toffset,LogCond[Toffset]]];

(*checking whether the PT can complete at all*)
If[(PTCritFlag == False) && (LogCond[TLeft]*LogCond[TRight]>0),Message[tTPT::AnalyticUnfinishedPT,direction,TLeft,LogCond[TLeft],TRight,LogCond[TRight]];Abort[]];

(*solving for T_PT (using the pre-calculated log of the PT condition)*)
Tpt=If[(PTCritFlag == True),Toffset,
10^(LT/.FindRoot[LogCond[10^LT],{LT,Log10[Tini],Log10[TLeft],Log10[TRight]},MaxIterations->10^6,AccuracyGoal->6,PrecisionGoal->4]//Quiet)];

(*t_PT*)
tpt=tOfTempAn[Tc,tcrit,gamma,t0,Tpt];

If[IntervalMemberQ[Interval[{Tlo,Thi}],Tpt]==False,Message[tTPT::NoTpt, Tpt,{Tlo,Thi}];Abort[],result={tpt,Tpt}];
];

(*routine for semi-analytic method*)
If[method=="semi",
(*NOTE: the crucial object for the 'numeric' method is T(t), the temperature as a function of time. However, the only purpose of the time t here is to serve as a parameter or variable with which the temperature evolves. It does *not* matter what its absolute scale or dimensions are. This is useful, since in our reheating code we will use x=t*H_d as our variable. In other words, tScale = 1/H_d.*)

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*dlnTdt(t)*)
dlnTdt[x_]=1/tScale D[Log[Tofx[x]],x];

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tofx]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tofx]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the leftmost edge*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*checking whether T_c is in the interval of temperatures probed by the T(t) function along the stated direction*)
noTempFn[Tofx,Tc,1];

(*time t at which T(t) reaches a maximum*)
{xmax,Tmax}=tTMax[Tofx,1];

(*correcting Thi in case this corresponds to a non-existent temperature (because the spinodal temperature never disappears!)*)
Thi=If[(Im[Thi]!=0)||(Thi===ComplexInfinity),Tmax*2,Thi];

(*redefining Tlo and Thi based on what the interpolating function can accommodate*)
Tlo=If[direction=="cold",Max[Tofx[xB],Tlo],Max[Tofx[xA],Tlo,Toffset]];
Thi=If[direction=="cold",Min[Thi,Tmax,Toffset],Min[Thi,Tmax]];

(*finding the time range over which we will look for the PT time, for each direction*)
xlo=tOfTempNum[Tofx,Tlo,1];
xhi=tOfTempNum[Tofx,Thi,1];

(*the initial guess for the PT temperature is either below T_c for cold (subcritical) PT, or above T_c for hot (supercritical) PT.*)
Tini=If[direction=="cold",GeometricMean[{Tlo,Tc}],GeometricMean[{Thi,Tc}]];

(*the initial guess for the PT time*)
x0=tOfTempNum[Tofx,Tini,1];
x0=If[direction=="cold",x0[[2]],x0[[1]]];

xLeft=If[direction=="cold",xhi[[2]],xlo[[1]]];
xRight=If[direction=="cold",xlo[[2]],xhi[[1]]];

(*\[CapitalGamma]_nucl(T_PT) in units of Subsuperscript[T, c, 4]*)
LogGammaPT[xx_]:=\[CapitalGamma]nucl[coeffs,1,Tofx[xx]/Tc,Kfactor,full,True];

(*\[Beta](T_PT), in units of 1/tScale*)
betaPT[xx_]:=\[Beta]1Rate[tScale*dlnTdt[xx],coeffs,1,Tofx[xx]/Tc,Kfactor,full];

(*dimensionless log-condition for PT: must be equal to 0*)
noDLogCond[xx_?NumericQ]:=NoDLogCondPT[vw,{LogGammaPT[xx],Tc},{betaPT[xx],1/tScale}];

(*checking whether the PT finished immediately after T_c*)
PTCritFlag = (noDLogCond[xLeft]>0);
If[(PTCritFlag == True),Message[tTPT::SemiCritPT,direction,Tc,xLeft,xLeft*tScale,Tofx[xLeft],noDLogCond[xLeft]]];

(*checking whether the PT completes at all*)
If[(PTCritFlag == False) && (noDLogCond[xLeft]*noDLogCond[xRight]>0),Message[tTPT::SemiUnfinishedPT,direction,xLeft,xLeft*tScale,Tofx[xLeft],noDLogCond[xLeft],xRight,xRight*tScale,Tofx[xRight],noDLogCond[xRight]];Abort[]];

(*solving for t_PT (using the pre-calculated log of the PT condition)*)
xpt=If[(PTCritFlag == True),xLeft,10^(Lx/.FindRoot[noDLogCond[10^Lx],{Lx,Log10[x0],Log10[xLeft],Log10[xRight]},MaxIterations->10^6,AccuracyGoal->6,PrecisionGoal->4]//Quiet)];
tpt=xpt*tScale;

(*T_PT*)
Tpt=Tofx[xpt];

If[IntervalMemberQ[Interval[{Tlo,Thi}],Tpt]==False,Message[tTPT::NoTpt, Tpt,{Tlo,Thi}];Abort[],result={tpt,Tpt}];

];

(*routine for numeric full method*)
If[method=="numeric",
(*NOTE: the crucial object for the 'numeric' method is T(t), the temperature as a function of time. However, the only purpose of the time t here is to serve as a parameter or variable with which the temperature evolves. It does *not* matter what its absolute scale or dimensions are. This is useful, since in our reheating code we will use x=t*H_d as our variable. In other words, tScale = 1/H_d.*)

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tofx]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tofx]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the edges*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*time t at which T(t) reaches a maximum*)
{xmax,Tmax}=tTMax[Tofx,1];

(*correcting Thi in case this corresponds to a non-existent temperature (because the spinodal temperature never disappears!)*)
Thi=If[(Im[Thi]!=0)||(Thi===ComplexInfinity),Tmax*2,Thi];

(*whether Tmax is in the interval of temperatures for which we computed the critical bubbles*)
TmaxFlag=IntervalMemberQ[Interval[{Tlo,Thi}],Tmax];

(*computing log10(-ln h(log10x)), the PT condition; and the time range*)
LogCond=Lmlnh[direction,vw,TempEvol,coeffs,Tc,Kfactor,full,atau,Nlx];

(*the time range, slightly offset from the edges*)
xRange=10^(InterpolatingFunctionDomain[LogCond]//Flatten);
xLeft=xRange[[1]];
xRight=xRange[[2]];

(*checking whether the PT finished immediately after Tc*)
PTCritFlag = (LogCond[Log10[xLeft]]>0);
If[(PTCritFlag == True),Message[tTPT::NumericCritPT,direction,Tc,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]]]];

(*checking whether the PT completes at all*)
If[(PTCritFlag == False) && (direction == "cold") && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::NumericUnfinishedColdPT,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];
If[(PTCritFlag == False) && (direction == "hot") &&(TmaxFlag == True) && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::NumericUnfinishedHotPT1,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];
If[(PTCritFlag == False) && (direction == "hot") &&(TmaxFlag == False) && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::NumericUnfinishedHotPT2,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];

(*the initial guess for the PT time*)
If[(Length@yData != Length@xData),
x0=GeometricMean[xRange],

xData=(InterpolatingFunctionCoordinates[LogCond]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[LogCond]//Flatten);

yData=Abs[yData];
x0=(Position[yData,Min[yData]]//Flatten)[[1]];
x0=10^xData[[x0]];

Clear[xData,yData];
];

(*solving for t_PT (using the pre-calculated log of the PT condition)*)
xpt=If[(PTCritFlag == True),xLeft,10^(Lx/.FindRoot[LogCond[Lx],{Lx,Log10[x0],Log10[xLeft],Log10[xRight]},MaxIterations->10^6,WorkingPrecision->prec,AccuracyGoal->6,PrecisionGoal->4])];
tpt=xpt*tScale;

(*T_PT*)
Tpt=Tofx[xpt];

If[IntervalMemberQ[Interval[{Tlo,Thi}],Tpt]==False,Message[TPT::NoTpt, Tpt,{Tlo,Thi}];Abort[],result={tpt,Tpt}];
];

(*routine for alternative method*)
If[method=="alt",
(*NOTE: the crucial object for the 'numeric' method is T(t), the temperature as a function of time. However, the only purpose of the time t here is to serve as a parameter or variable with which the temperature evolves. It does *not* matter what its absolute scale or dimensions are. This is useful, since in our reheating code we will use x=t*H_d as our variable. In other words, tScale = 1/H_d.*)

(*T(x), t/x*)
{Tofx,tScale}=TempEvol;

(*anatomy of the interpolating function*)
{xA,xB}=(InterpolatingFunctionDomain[Tofx]//Flatten);
xData=(InterpolatingFunctionCoordinates[Tofx]//Flatten);
prec=Round[Precision[xData[[1]]]];

(*slightly away from the edges*)
xA=If[xA<=0,Min[xData[[2]],eps],Min[xData[[2]],xA*(1+eps)]];
Clear[xData];

(*time t at which T(t) reaches a maximum*)
{xmax,Tmax}=tTMax[Tofx,1];

(*correcting Thi in case this corresponds to a non-existent temperature (because the spinodal temperature never disappears!)*)
Thi=If[(Im[Thi]!=0)||(Thi===ComplexInfinity),Tmax*2,Thi];

(*whether Tmax is in the interval of temperatures for which we computed the critical bubbles*)
TmaxFlag=IntervalMemberQ[Interval[{Tlo,Thi}],Tmax];

(*computing log10(-ln h(log10 x))*)
LhLx=Lmlnh[direction,vw,TempEvol,coeffs,Tc,Kfactor,full,atau,Nlx];

(*computing log10(n_b(log10 x))*)
LnbLx=Lnbubble[LhLx,TempEvol,coeffs,Tc,"all",Kfactor,full,atau];

(*computing log10(\[LeftAngleBracket]R(log10 x)\[RightAngleBracket])*)
LRLx=LRbubble[LhLx,LnbLx,vw,TempEvol,coeffs,Tc,"all",Kfactor,full,atau];

(*computing the PT condition: log10 R_b - log10\[LeftAngleBracket]R\[RightAngleBracket], where R_b\[Congruent]n_b^(-1/3)*)
xData=(InterpolatingFunctionCoordinates[LnbLx]//Flatten);
yData=(-(1/3)(InterpolatingFunctionValuesOnGrid[LnbLx]//Flatten))-(InterpolatingFunctionValuesOnGrid[LRLx]//Flatten);
data=Transpose[{xData,yData}];

LogCond=Interpolation[data];
Clear[LhLx,LnbLx,LRLx,xData,yData,data];

(*the time range, slightly offset from the edges*)
xRange=10^(InterpolatingFunctionDomain[LogCond]//Flatten);
xLeft=xRange[[1]];
xRight=xRange[[2]];

(*checking whether the PT finished immediately after Tc*)
PTCritFlag = (LogCond[Log10[xLeft]]<0);
If[(PTCritFlag == True),Message[tTPT::AltCritPT,direction,Tc,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]]]];

(*checking whether the PT completes at all*)
If[(PTCritFlag == False) && (direction == "cold") && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::AltUnfinishedColdPT,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];
If[(PTCritFlag == False) && (direction == "hot") &&(TmaxFlag == True) && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::AltUnfinishedHotPT1,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];
If[(PTCritFlag == False) && (direction == "hot") &&(TmaxFlag == False) && (LogCond[Log10[xLeft]]*LogCond[Log10[xRight]]>0),Message[tTPT::AltUnfinishedHotPT2,direction,xLeft,xLeft*tScale,Tofx[xLeft],LogCond[Log10[xLeft]],xRight,xRight*tScale,Tofx[xRight],LogCond[Log10[xRight]]];Abort[]];

(*the initial guess for the PT time*)
xData=(InterpolatingFunctionCoordinates[LogCond]//Flatten);
yData=(InterpolatingFunctionValuesOnGrid[LogCond]//Flatten);

yData=Abs[yData];
x0=(Position[yData,Min[yData]]//Flatten)[[1]];
x0=10^xData[[x0]];

Clear[xData,yData];

(*solving for t_PT (using the pre-calculated log of the PT condition)*)
xpt=If[(PTCritFlag == True),xLeft,10^(Lx/.FindRoot[LogCond[Lx],{Lx,Log10[x0],Log10[xLeft],Log10[xRight]},MaxIterations->10^6,WorkingPrecision->prec,AccuracyGoal->6,PrecisionGoal->4])];
tpt=xpt*tScale;

(*T_PT*)
Tpt=Tofx[xpt];

If[IntervalMemberQ[Interval[{Tlo,Thi}],Tpt]==False,Message[TPT::NoTpt, Tpt,{Tlo,Thi}];Abort[],result={tpt,Tpt}];
];

result
]

tTPT::AnalyticTempEvolError="If you pass 'method'=='analytic', then argument 'TempEvol' should be {gamma, t0, tcrit}: a list with three elements: the power-law behavior of the temperature T as a function of time t, some reference time \!\(\*SubscriptBox[\(t\), \(0\)]\) at which the temperature vanishes, and the critical time.";
tTPT::NumericTempEvolError="If you pass 'method'=='semi'/'numeric'/'alt', then argument 'TempEvol' should be {Tofx, tScale}: a list with only two elements: the temperature as a function of a time-like parameter x, T(x), and the time-scale given by the ratio t/x.";

tTPT::AnalyticCritPT="The condition for the completion of the `1` Phase Transition was satisfied immediately after the critical temperature Tc=`2`: Toffset=`3`, \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`4`. This is a very fast PT and quantities such as \[Beta] should not be trusted. We will nevertheless proceed as if this was when the PT finished.";
tTPT::AnalyticUnfinishedPT="The condition for the completion of the `1` Phase Transition cannot be satisfied: at T=`2` the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`3`, while at T=`4` the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`: the function never crosses 0.";

tTPT::SemiCritPT="The condition for the completion of the `1` Phase Transition was satisfied immediately after the critical temperature Tc=`2`: at xLeft=`3` (tLeft=`4`) with T=`5`, \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`6`. This is a very fast PT and quantities such as \[Beta] should not be trusted. We will nevertheless proceed as if this was when the PT finished.";
tTPT::SemiUnfinishedPT="The condition for the completion of the `1` Phase Transition cannot be satisfied: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0.";

tTPT::NumericCritPT="The condition for the completion of the `1` Phase Transition was satisfied immediately after the critical temperature Tc=`2`: at xLeft=`3` (tLeft=`4`) with T=`5`, \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`6`. This is a very fast PT and quantities such as \[Beta] should not be trusted. We will nevertheless proceed as if this was when the PT finished.";
tTPT::NumericUnfinishedColdPT="The condition for the completion of the `1` Phase Transition cannot be satisfied: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0.";
tTPT::NumericUnfinishedHotPT1="The condition for the completion of the `1` Phase Transition cannot be satisfied between the two times at which the critical temperature is reached: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0. In other words, the PT does not finish by the time the temperature drops below its critical value again.";
tTPT::NumericUnfinishedHotPT2="The condition for the completion of the `1` Phase Transition cannot be satisfied between the times at which the critical and the spinodal temperatures are reached: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0. In other words, the PT does not finish by the time the spinodal temperature is reached, and the broken minimum disappears, leading to a rolling transition.";

tTPT::AltCritPT="The condition for the completion of the `1` Phase Transition was satisfied immediately after the critical temperature Tc=`2`: at xLeft=`3` (tLeft=`4`) with T=`5`, \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`6`. This is a very fast PT and quantities such as \[Beta] should not be trusted. We will nevertheless proceed as if this was when the PT finished.";
tTPT::AltUnfinishedColdPT="The condition for the completion of the `1` Phase Transition cannot be satisfied: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0.";
tTPT::AltUnfinishedHotPT1="The condition for the completion of the `1` Phase Transition cannot be satisfied between the two times at which the critical temperature is reached: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0. In other words, the PT does not finish by the time the temperature drops below its critical value again.";
tTPT::AltUnfinishedHotPT2="The condition for the completion of the `1` Phase Transition cannot be satisfied between the times at which the critical and the spinodal temperatures are reached: at x=`2` (t=`3`, T=`4`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`5`, while at x=`6` (t=`7`, T=`8`) the \!\(\*SubscriptBox[\(log\), \(10\)]\)(cond.)=`9`: the function, which is monotonic in x, never crosses 0. In other words, the PT does not finish by the time the spinodal temperature is reached, and the broken minimum disappears, leading to a rolling transition.";

tTPT::NoTpt="Tpt=`1` is outside the interval `2`. In other words, the PT could not finish before the radiation cooled-down/heated-up beyond this interval.";


(* ::Chapter:: *)
(*4. Full Physics Routine*)


(* ::Text:: *)
(*The full physics routine. The only function that matters in the end.*)
(**)
(*Input: g_*, "direction", v_w, {Temperature Evolution}, t_c, {\[Mu],A,\[Lambda]}, T_c, (method="analytic"), (K=1), (0-modes from path integral: False), (atau={1,1}), (Nlx=300)*)
(**)
(*If method="analytic"\[Implies]{Temperature Evolution}={\[Gamma], t_0, t_c}*)
(*If method="semi"\[Implies]{Temperature Evolution}={T(x), t_scale}*)
(*If method="numeric"\[Implies]{Temperature Evolution}={T(x), t_scale}*)
(*If method="alt"\[Implies]{Temperature Evolution}={T(x), t_scale}*)
(**)
(*Recall that:*)
(**)
(*1. g_* is the # d.o.f. in the radiation (i.e. light particles) outside the bubble,*)
(*2. the direction is one of the strings "hot" or "cold"*)
(*3. v_w is the bubble wall speed*)
(*4. For 'Temperature Evolution': \[Gamma] is the power-law behavior of T(t), t_0 is a reference time at which T(t_0)=0, t_c is the time at which the critical temperature is reached, T(x) is an interpolated function of the temperature as a function of a time-like variable x, and t_scale is the time scale: x=t/t_scale. *)
(*5. {\[Mu], A, \[Lambda]} are the dimensionless coefficients in the thermal potential (see their definition in previous section),*)
(*6. T_c the critical temperature of the phase transition.*)
(**)
(*The last five arguments (the method to compute the PT temperature, the numerical coefficient in front of \[CapitalGamma]_nucl/\[ScriptCapitalV], whether we include T-dependent contributions from the 0-modes of the path integral, whether we consider the impact of the scale factor on bubble nucleation, and the number of log steps in time for the full numerical method) are optional parameters.*)
(**)
(*Output: T_PT(t_PT), t_PT, \[Lambda]bar(t_PT), \[LeftAngleBracket]\[CapitalPhi]_b(t_PT)\[RightAngleBracket], \[CapitalDelta]V(t_PT), E_c(t_PT), S_E(t_PT), S_E'(t_PT), S_E''(t_PT), \[CapitalGamma]_nucl(t_PT)/\[ScriptCapitalV], \[Beta]_1(t_PT), \[Beta]_2(t_PT), n_b(t_PT), R_b(t_PT), \[LeftAngleBracket]R(t_PT)\[RightAngleBracket], \[Beta]_eff(t_PT), \[Alpha]_\[Infinity](t_PT), {\[Alpha]_n(t_PT) for \[Epsilon]0, V_T, L, & \[Theta]}, {\[Kappa]_run \[Congruent] sign[1-\[Lambda]bar] (\[Alpha]_\[Epsilon]-\[Alpha]_\[Infinity])/\[Alpha]_n}.*)


Clear[ptParams]

ptParams[gStar_,direction_,vw_,TempEvol_List,coeffs_List,Tcrit_,method_:"analytic",Kfactor_:1,full_:False,atau_:{1,1},Nlx_:250]:=ptParams[gStar,direction,vw,TempEvol,coeffs,Tcrit,method,Kfactor,full,atau,Nlx]=Block[{\[Mu],A,\[Lambda],Tc=Tcrit,gamma,t0,tcrit,Tfn,Toft,dlnTdt,d2lnTdt2,tt,tScale,Tpt,tpt,T,\[Lambda]bar,\[CapitalPhi]b,VT,Ec,SE,SE1,SE2,GammaNucl,beta1Rate,beta2Rate,LmlnhLx,LnbLx,LRbLx,nbPT,RPT,Ravg,betaEff,alphInf,alphn,effRun},

(*distributing the coefficients*)
{\[Mu],A,\[Lambda]}=coeffs;

(*checking whether there can be a first order phase transition*)
noCritTemp[coeffs];

(*{t_PT, T_PT}*)
{tpt,Tpt}=tTPT[direction,vw,TempEvol,coeffs,Tc,method,Kfactor,full,atau,Nlx];
T=Tpt;

(*allocating some parameters*)
If[method=="analytic",{gamma,t0,tcrit}=TempEvol,
{Tfn,tScale}=TempEvol;
Toft[tt_]=Tfn[tt/tScale];
dlnTdt[t_]=D[Log[Toft[t]],t];
d2lnTdt2[t_]=D[Log[Toft[t]],{t,2}]];

(*finding t_c*)
If[method!="analytic",
tcrit=tOfTempNum[Tfn,Tc,tScale];
tcrit=If[direction=="cold",tcrit[[2]],tcrit[[1]]];
];

(*\[Lambda]bar*)
\[Lambda]bar=(\[Lambda]bar/.Mc\[Lambda]barToCoeffs);

(*\[CapitalPhi]_b*)
\[CapitalPhi]b=(Fb/.PotToCoeffs);

(*\[CapitalDelta]V_T (|\[CapitalDelta]V|)*)
VT=(\[CapitalDelta]V/.Mc\[Lambda]barToCoeffs);

(*E_c*)
Ec=EcTemp[coeffs,Tc,Tpt];

(*S_E*)
SE=SETemp[coeffs,Tc,Tpt];

(*S_E'*)
SE1=If[method=="analytic",S1OfTempAn[gamma,t0,tcrit,coeffs,Tc,Tpt],S1Rate[dlnTdt[tpt],coeffs,Tc,Tpt]];

(*S_E''*)
SE2=If[method=="analytic",S2OfTempAn[gamma,t0,tcrit,coeffs,Tc,Tpt],S2Rate[dlnTdt[tpt],d2lnTdt2[tpt],coeffs,Tc,Tpt]];

(*\[CapitalGamma]_nucl/\[ScriptCapitalV]*)
GammaNucl=\[CapitalGamma]nucl[coeffs,Tc,Tpt,Kfactor,full];

(*\[Beta]_1*)
beta1Rate=If[method=="analytic",beta1OfTempAn[gamma,t0,tcrit,coeffs,Tc,Tpt,Kfactor,full],\[Beta]1Rate[dlnTdt[tpt],coeffs,Tc,Tpt,Kfactor,full]];

(*\[Beta]_2*)
beta2Rate=If[method=="analytic",beta2OfTempAn[gamma,t0,tcrit,coeffs,Tc,Tpt,Kfactor,full],\[Beta]2Rate[dlnTdt[tpt],d2lnTdt2[tpt],coeffs,Tc,Tpt,Kfactor,full]];

(*n_b, R_b, \[LeftAngleBracket]R\[RightAngleBracket], \[Beta]_eff*)
Which[MemberQ[{"analytic","semi"},method],
nbPT=(beta1Rate^3/(8\[Pi]*vw^3));
RPT=nbPT^(-1/3);
Ravg=vw*(tpt-tcrit);
betaEff=(8\[Pi]*vw^3*nbPT)^(1/3),

method=="numeric",
LmlnhLx=Lmlnh[direction,vw,TempEvol,coeffs,Tc,Kfactor,full,atau,Nlx];
LnbLx=Lnbubble[LmlnhLx,TempEvol,coeffs,Tc,tpt/tScale,Kfactor,full,atau];
nbPT=10^(LnbLx);
RPT=nbPT^(-1/3);
Ravg=vw*(tpt-tcrit);
betaEff=(8\[Pi]*vw^3*nbPT)^(1/3),

method=="alt",
LmlnhLx=Lmlnh[direction,vw,TempEvol,coeffs,Tc,Kfactor,full,atau,Nlx];
LnbLx=Lnbubble[LmlnhLx,TempEvol,coeffs,Tc,tpt/tScale,Kfactor,full,atau];
LRbLx=LRbubble[LmlnhLx,LnbLx,vw,TempEvol,coeffs,Tc,tpt/tScale,Kfactor,full,atau];
nbPT=10^(LnbLx);
RPT=nbPT^(-1/3);
Ravg=10^(LRbLx);
betaEff=(8\[Pi]*vw^3*nbPT)^(1/3)
];

(*\[Alpha]_\[Infinity]*)
alphInf=\[Alpha]\[Infinity][gStar,coeffs,Tc,Tpt];

(*\[Alpha]_n*)
alphn=\[Alpha]n[gStar,coeffs,Tc,Tpt,#]&/@\[Alpha]qts;

(*\[Kappa]_run: the "efficiency" for runaway bubbles, if the runaway condition is satisfied (\[Kappa]_run>0). This is the fraction of energy that actually goes into accelerating the bubble wall when we have runaway. For runaway bubbles we will take this to be the efficiency corresponding to the energy going into the envelope. For non-runaway bubbles (when this number is negative) we should NOT use this number.*)
effRun=\[Kappa]run[gStar,coeffs,Tc,Tpt,"\[Epsilon]",#]&/@\[Alpha]qts;

(*final result, 19 parameters: {T_PT, t_PT, \[Lambda]bar, \[CapitalPhi]_b, \[CapitalDelta]V_T, E_c, S_E, S_E', S_E'', \[CapitalGamma]_nucl/\[ScriptCapitalV], \[Beta]_1, \[Beta]_2, n_b, R_b, <R>, \[Beta]_eff, \[Alpha]_\[Infinity], {\[Alpha]_n}, {\[Kappa]_run}}*)
{Tpt,tpt,\[Lambda]bar,\[CapitalPhi]b,VT,Ec,SE,SE1,SE2,GammaNucl,beta1Rate,beta2Rate,nbPT,RPT,Ravg,betaEff,alphInf,alphn,effRun}

]


(* ::Chapter:: *)
(*End*)


End[];
EndPackage[];
