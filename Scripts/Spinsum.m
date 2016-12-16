(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified July 2016
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FeynArtsAdd`
<< FormCalc`
<< FormCalcAdd`
ClearProcess[]
<<"!rm *.frm"
<<"!rm *.wdx"
<<"!rm *.F"
<<"!rm *.f"

time1 = SessionTime[]


(*You can now load the script with the command $ MathKernel -script spinsum.m "ubar" "u" "ubar" "u"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
	(p[1] = ToString[$CommandLine[[4]]];
	 p[2] = ToString[$CommandLine[[5]]];
	 p[3] = ToString[$CommandLine[[6]]];
	 p[4] = ToString[$CommandLine[[7]]];),
	(*Else*)
	(p[1] = "ubar";
	 p[2] = "u";
	 p[3] = "gam";
	 p[4] = "g";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4];
name = CalcProcess;
Print[CalcProcess]

GluonLegs = {};
For[i=1, i<5, i++,
If[p[i] === "qu", P[i] = F[3],
If[p[i] === "qubar", P[i] = -F[3],
If[p[i] === "qd", P[i] = F[4],
If[p[i] === "qdbar", P[i] = -F[4],
If[p[i] === "nI", P[i] = F[11],
If[p[i] === "nJ", P[i] = F[11],
If[p[i] === "xI-", P[i] = F[12],
If[p[i] === "xI+", P[i] = -F[12],
If[p[i] === "xJ-", P[i] = F[12],
If[p[i] === "xJ+", P[i] = -F[12],

If[p[i] === "g", (GluonLegs = Join[GluonLegs, {i}]; P[i] = V[5]),
If[p[i] === "gam", P[i] = V[1],
If[p[i] === "Z", P[i] = V[2],
If[p[i] === "W+", P[i] = V[3],
If[p[i] === "W-", P[i] = -V[3],

If[p[i] === "u", P[i] = F[3,{1}],
If[p[i] === "ubar", P[i] = -F[3,{1}],
If[p[i] === "c", P[i] = F[3,{2}],
If[p[i] === "cbar", P[i] = -F[3,{2}],
If[p[i] === "t", P[i] = F[3,{3}],
If[p[i] === "tbar", P[i] = -F[3,{3}],

If[p[i] === "d", P[i] = F[4,{1}],
If[p[i] === "dbar", P[i] = -F[4,{1}],
If[p[i] === "s", P[i] = F[4,{2}],
If[p[i] === "sbar", P[i] = -F[4,{2}],
If[p[i] === "b", P[i] = F[4,{3}],
If[p[i] === "bbar", P[i] = -F[4,{3}],

If[p[i] === "n1", P[i] = F[11,{1}],
If[p[i] === "n2", P[i] = F[11,{2}],
If[p[i] === "n3", P[i] = F[11,{3}],
If[p[i] === "n4", P[i] = F[11,{4}],

If[p[i] === "x1-", P[i] = F[12,{1}],
If[p[i] === "x1+", P[i] = -F[12,{1}],
If[p[i] === "x2-", P[i] = F[12,{2}],
If[p[i] === "x2+", P[i] = -F[12,{2}]
]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
]

process = {P[1], P[2]} -> {P[3], P[4]};
Print[process]


(*Neglect Masses (URL)*)
Neglect[ME] = Neglect[ME2] = 0;
(*Neglect[MQU] = Neglect[MQD] = 0;*)
Neglect[MU] = Neglect[MU2] = 0;
Neglect[MC] = Neglect[MC2] = 0;
(*Neglect[MT] = Neglect[MT2] = 0;*)
Neglect[MD] = Neglect[MD2] = 0;
Neglect[MS] = Neglect[MS2] = 0;
(*Neglect[MB] = Neglect[MB2] = 0;*)

(*Diagonale CKM Matrix*)
CKM = IndexDelta;
CKMC = IndexDelta;


(*Options*)
SetOptions[InsertFields, Model -> "SMQCD",
           (*No Fermion-Higgs coupling*)
           Restrictions -> {NoLightFHCoupling},
           (*Exclude Top, Higgs, Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
		   ExcludeParticles -> {S[1|2|3|4|5|6|11|12], F[1|2]},
		   (*no internal Weakinos*)
		   LastSelections -> {!F[11],!F[12]}];

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];

(*Reduce tensor to scalar integrals and choose regularization scheme*)
(*D = dimensional regularization (default),*)
(*4 = constrained differential renormalization,*)
(*0 = keeps the whole amplitude D-dimensional*)
SetOptions[CalcFeynAmp,Dimension->D];

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)]


Print["Born"]

tops = CreateTopologies[0, 2 -> 2];
ins = InsertFields[tops, process];
(*ins = DiagramExtract[ins,2];*)
DoPaint[ins, "born"];

amp = CreateFeynAmp[ins];
Print["amp = "];
Print[amp//InputForm];

ampB = CalcFeynAmp[amp, FermionChains -> Chiral];
(*insert the partice widths*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
ampB = ampB/.{Den[x_,y_]:>Den[x,y/.widths]};
Print["ampB = "];
Print[ampB//InputForm];

(*execute the polarizations sums over all legs except for external gluons*)
Legs = {1, 2, 3, 4};
LegsToSum = Complement[Legs, GluonLegs];
Print["Summing over legs "<>ToString[LegsToSum]];
born = PolarizationSum2[ampB, SumLegs -> LegsToSum, GaugeTerms -> False];
Print["born = "];
Print[born];
full = PolarizationSum2[born, SumLegs -> GluonLegs, GaugeTerms -> False, RetainFile -> False];
Print["full = "];
Print[full];

(*carry out the spin correlated sum and store the result in variables called spinsum`i'*)
Do[
  Pair[eta[i], eta[i]] = 0;
  spinsum[i] = SpinCorrelatedSum[born, SumLegs -> {i}, GaugeTerms -> False, RetainFile -> False];
  Print["spinsum["<>ToString[i]<>"] = "];
  Print[spinsum[i]];,
  {i, GluonLegs}
]


(*Write files*)
Print["Writing files..."]
amps = {born, full};
{born, full} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];
col = ColourME[All, born];
abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];
subexprc = ConjugateRule[subexpr];
rules = Join[abbr,subexpr,subexprc];

(*Write spin correlated amplitude and only necessary rules*)
Do[
  (*optimize the rules and write out*)
  optimizedRules[i] = LoopRemove[spinsum[i],rules];,
  {i, GluonLegs}
]


(*Preferences*)
(*Mandelstams and masses are real*)
$Assumptions=Element[S,Reals]&&Element[S34,Reals]&&Element[T,Reals]&&
             Element[T14,Reals]&&Element[U,Reals]&&Element[T24,Reals]&&
             Element[MNeu[_],Reals]&&Element[MSf[___],Reals]&&Element[WSf[___],Reals]&&
             Element[MU,Reals]&&Element[MW,Reals]&&Element[SB,Reals]&&
             Element[SW,Reals]&&Element[CW,Reals];
(*general code substitutions*)
indices={Sfe6->2,Sfe6c->2};
functions={Pair->DotP,k[1]->k1,k[2]->k2,k[3]->k3,k[4]->k4,k[5]->k5, IndexDelta->Kronecker,Eps->Epsilon,Conjugate[WSf[i_,j_,k_]]:>WSf[i,j,k],Conjugate[WZ]->WZ, I->ii,-I->-ii};


WriteSpinCorrelatedMatrixElement["bmunu_"<>name,spinsum[4],optimizedRules[4],indices,functions,4,4]


Print["time used: ", SessionTime[] - time1]
Exit[];
