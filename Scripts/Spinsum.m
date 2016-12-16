(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified July 2016
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FormCalc`
<< SpinCorr`
ClearProcess[]
<<"!rm *.frm"
<<"!rm *.wdx"
<<"!rm *.F"
<<"!rm *.f"

time1 = SessionTime[]


(*functions*)
(*break line automatically*)
WriteStringn[strm_,str_]:=WriteString[strm,str<>"\n"]
(*Generate unique variable names (even for functions)*)
UniqueName[var_Symbol]:=Unique[var]
UniqueName[var_[arg__]]:=Unique[var][arg]
(*assign values to a list of names*)
AssignValues[ulist_List,val_List]:=MapThread[Set[#1,#2]&,{ulist,val}]//Quiet;
(*Extract only the elements of list with given arguments*)
CheckArgs[expr_]:=False
CheckArgs[expr_,arg__]:=Map[!FreeQ[GetArgument[expr],#]&,Permutations[{arg}]]/.List->Or
FreeArgs[expr_,arg__]:=Map[!CheckArgs[expr,#/.List->Sequence]&,Subsets[{arg}]]/.List->And
ExprWithArgs[expr_,arg__]:=Sequence[]
ExprWithArgs[expr_,arg__]:=expr/;CheckArgs[expr,arg]
ExprWithoutArgs[expr_,arg__]:=Sequence[]
ExprWithoutArgs[expr_,arg__]:=expr/;FreeArgs[expr,arg]
GetElementsWithArgs[list_List,arg__]:=Map[ExprWithArgs[#,arg]&,list]
GetElementsWithoutArgs[list_List,arg__]:=Map[ExprWithoutArgs[#,arg]&,list]
(*Subset of n elements with overhang*)
Subsetn[list_List,n_Integer]:=Block[{subs},Join[subs=Partition[list,n],{Complement[list,Flatten[subs]]}]]/.{}->Sequence[]


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


(*generate fortran code*)
strm = OpenFortran["bornmunu_"<>name<>".mf"];
WriteStringn[strm, "subroutine bornmunu_"<>name<>"(p,bmunu)"];
WriteStringn[strm, "implicit none"]
WriteStringn[strm, "#include \"PhysPars.h\"\n#include \"pwhg_math.h\""]
WriteStringn[strm, "#include \"nlegborn.h\""]
WriteStringn[strm, "double precision p(0:3,nlegborn)"]
WriteStringn[strm, "double precision al(0:3), be(0:3)"]
WriteStringn[strm, "double precision k1(0:3), k2(0:3), k3(0:3), k4(0:3), k5(0:3)"]
WriteStringn[strm, "double precision bmunu(0:3,0:3,nlegborn)"]
WriteStringn[strm, "double precision S, T, U, S34, T14, T24"]
WriteStringn[strm, "integer Sfe6, Sfe6c"]
WriteStringn[strm, "integer alind, beind"]

(*define functions*)
WriteStringn[strm, ""]
WriteStringn[strm, "double precision Epsilon, DotP, Den"]
WriteStringn[strm, "double precision momsq, momsum2sq, momsum3sq"]
WriteStringn[strm, "external Epsilon, DotP, Den"]
WriteStringn[strm, "external momsq, momsum2sq, momsum3sq"]

(*define local variables*)
WriteStringn[strm, ""]
defvars=Map[Join[#,{TAG}]&,Subsetn[GetVariables[optimizedRules[GluonLegs[[1]]]]/.indices,4]];
For[i=1,i<=Length[defvars],i++,
defvarsout[i]=defvars[[i]];
WriteStringn[strm, "double precision <* defvarsout["<>ToString[i]<>"] *>"];
]

(*reset*)
WriteStringn[strm, ""]
WriteStringn[strm, "bmunu(:,:,:) = 0D0"]

(*Momenta and Mandelstams*)
WriteStringn[strm, ""]
WriteStringn[strm, "k1(:) = p(:,1)"]
WriteStringn[strm, "k2(:) = p(:,2)"]
WriteStringn[strm, "k3(:) = p(:,3)"]
WriteStringn[strm, "k4(:) = p(:,4)"]
WriteStringn[strm, "S   = momsum2sq(k1(:), k2(:))"]
WriteStringn[strm, "T   = momsum2sq(k1(:),-k3(:))"]
WriteStringn[strm, "U   = momsum2sq(k2(:),-k3(:))"]

(*loop over al and be*)
WriteStringn[strm, ""]
WriteStringn[strm, "do alind=0,3"]
WriteStringn[strm, "do beind=0,3"]
WriteStringn[strm, ""]
WriteStringn[strm, "al(:) = 0D0"]
WriteStringn[strm, "al(alind) = 1D0"]
WriteStringn[strm, "be(:) = 0D0"]
WriteStringn[strm, "be(beind) = 1D0"]

(*calculate abbreviations*)
WriteStringn[strm, ""]
vars=GetVariables[optimizedRules[GluonLegs[[1]]]];
ulist=Map[UniqueName,vars];
varsSfe6=GetElementsWithArgs[vars,Sfe6];
ulistSfe6=GetElementsWithArgs[ulist,Sfe6];
varsSfe6c=GetElementsWithArgs[vars,Sfe6c];
ulistSfe6c=GetElementsWithArgs[ulist,Sfe6c];
varsSfe6Sfe6c=GetElementsWithArgs[vars,Sfe6,Sfe6c];
ulistSfe6Sfe6c=GetElementsWithArgs[ulist,Sfe6,Sfe6c];
vars0=GetElementsWithoutArgs[vars,Sfe6c,Sfe6];
ulist0=GetElementsWithoutArgs[ulist,Sfe6c,Sfe6];
MapThread[WriteStringn[strm, "      <* "<>ToString[#1]<>" *> = <* "<>ToString[#2]<>" *>" ]&,{vars0,ulist0}];

If[Length[varsSfe6]!=0,
  WriteStringn[strm, ""];
  WriteStringn[strm, "do Sfe6=1,2"];
  MapThread[WriteStringn[strm, "      <* "<>ToString[#1]<>" *> = <* "<>ToString[#2]<>" *>" ]&,{varsSfe6,ulistSfe6}];
  WriteStringn[strm, "enddo"];
]

If[Length[varsSfe6c]!=0,
  WriteStringn[strm, ""];
  WriteStringn[strm, "do Sfe6c=1,2"];
  MapThread[WriteStringn[strm, "      <* "<>ToString[#1]<>" *> = <* "<>ToString[#2]<>" *>" ]&,{varsSfe6c,ulistSfe6c}];
  WriteStringn[strm, "enddo"];
]

If[Length[varsSfe6Sfe6c]!=0,
  WriteStringn[strm, ""];
  WriteStringn[strm, "do Sfe6=1,2"];
  WriteStringn[strm, "do Sfe6c=1,2"];
  MapThread[WriteStringn[strm, "      <* "<>ToString[#1]<>" *> = <* "<>ToString[#2]<>" *>" ]&,{varsSfe6Sfe6c,ulistSfe6Sfe6c}];
  WriteStringn[strm, "enddo"];
  WriteStringn[strm, "enddo"];
]

(*write Matrix element*)
WriteStringn[strm, ""]
WriteStringn[strm, "bmunu(alind,beind,"<>ToString[GluonLegs[[1]]]<>") = <* spinsumout *>"];

(*end loop over al and be*)
WriteStringn[strm, ""]
WriteStringn[strm, "enddo"]
WriteStringn[strm, "enddo"]

WriteStringn[strm, ""]
WriteStringn[strm, "end"];
Close[strm];

(*substitute wildcards*)
AssignValues[ulist,GetValues[optimizedRules[GluonLegs[[1]]]]/.functions];
spinsumout=spinsum[GluonLegs[[1]]]/.functions;
Splice["bornmunu_"<>name<>".mf", PageWidth -> 72];
(*FileTemplateApply[FileTemplate["bornmunu_"<>name<>".mf"],"bornmunu_"<>name<>".f"]*)


(*finalize the output, remove the Function "List(...)" that is wrapped around the variables*)
strm = OpenFortran["finalize.sh"];
WriteStringn[strm, "#!/bin/bash"];
WriteStringn[strm, "gsed -i -e \"s/List\(//g\" "<>"bornmunu_"<>name<>".f"];
WriteStringn[strm, "gsed -i -e \"s/,TAG\)//g\" "<>"bornmunu_"<>name<>".f"];
WriteStringn[strm, "gsed -i -e 's/\\t/      /g' "<>"bornmunu_"<>name<>".f"];
WriteStringn[strm, "mv bornmunu_"<>name<>".f " <>"bornmunu_"<>name<>".F"];
Close[strm];
<<"!chmod +x finalize.sh"
<<"!./finalize.sh"
<<"!rm *.f-e"


Print["time used: ", SessionTime[] - time1]
Exit[];
