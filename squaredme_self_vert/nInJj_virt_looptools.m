(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified February 2016
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FeynArtsAdd`
<< FormCalc`
<< FormCalcAdd`
ClearProcess[]
<<"!rm *.frm"

time1 = SessionTime[]


(*You can now load the script with the command $ MathKernel -script nInJj_virt.m "qd" "qdbar" "nI" "nJ" "g"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
	(p[1] = ToString[$CommandLine[[4]]];
	 p[2] = ToString[$CommandLine[[5]]];
	 p[3] = ToString[$CommandLine[[6]]];
	 p[4] = ToString[$CommandLine[[7]]];
	 p[5] = ToString[$CommandLine[[8]]];),
	(*Else*)
	(p[1] = "dbar";
	 p[2] = "d";
	 p[3] = "n1";
	 p[4] = "n2";
	 p[5] = "g";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5];
name = CalcProcess;
Print[CalcProcess]

For[i=1, i<=5, i++,
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

If[p[i] === "g", P[i] = V[5],
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

process = {P[1], P[2]} -> {P[3], P[4], P[5]};
Print[process]


(*Neglect Masses (URL)*)
Neglect[ME] = Neglect[ME2] = 0;
Neglect[MU] = Neglect[MU2] = 0;
Neglect[MC] = Neglect[MC2] = 0;
Neglect[MD] = Neglect[MD2] = 0;
Neglect[MS] = Neglect[MS2] = 0;
(*Neglect[MB] = Neglect[MB2] = 0;
Neglect[MT] = Neglect[MT2] = 0;*)

(*Diagonale CKM Matrix*)
CKM = IndexDelta;
CKMC = IndexDelta;

(*SUSY restoring CT*)
dZe1y = 0;
dZgs1y = 0;


(*Options*)
SetOptions[InsertFields, Model -> "MSSMCTPOWHEG", InsertionLevel->{Classes},
           (*No Fermion-Higgs coupling*)
           Restrictions -> {NoLightFHCoupling},
           (*Exclude Top, Higgs, Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
		   (*ExcludeParticles -> {S[1|2|3|4|5|6|11|12], F[1|2]},*)
		   (*Exclude Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
		   ExcludeParticles -> {S[11|12], F[1|2]},
		   (*no internal Weakinos*)
		   LastSelections -> {!F[11],!F[12]}];

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];

(*Reduce tensor to scalar integrals and choose regularisazation scheme*)
(*D = dimensional regularization (default),*)
(*4 = constrained differential renormalization,*)
(*0 = keeps the whole amplitude D-dimensional*)
SetOptions[CalcFeynAmp,Dimension->D];
(*Note: There is currently a bug in FormCalc which does not allow to compile*)
(*the generated code with PaVeReduce\[Rule]True set.*)
(*One has to replace "Derivative(1)(IGram)(MS2)" with "(-1/(MS2**2))"*)

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)]


Print["Born"]

tops = CreateTopologies[0, 2 -> 3];
ins = InsertFields[tops, process];
(*exclude fermion higgs couplings. top pdfs = 0 \[Rule] no external tops \[Rule] no internal tops \[Rule] no fermion higgs coupling*)
(*TODO: check for chargino pair production and chargino neutralino production*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

DoPaint[ins, "born"];

born = CalcFeynAmp[CreateFeynAmp[ins]];
born = born//.{Alfa2->0};


Print["Counter Terms"]

top = CreateCTTopologies[1, 2 -> 3, ExcludeTopologies -> {TadpoleCTs, WFCorrectionCTs}]; (*Exclude Tadpole- and Self-Energy CT on external legs*)
ins = InsertFields[top, process];

(*exclude fermion higgs couplings*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

(*Self Energy Counter Terms*)
insSelf = DiagramSelect[ins, (FieldMemberQ[FieldPoints[##],FieldPoint[_][_, _]]) &];
DoPaint[insSelf, "counterSelf"];
counterSelf = CreateFeynAmp[insSelf];

(*Vertex Correction Counter Terms*)
insVert = DiagramSelect[ins, (FieldMemberQ[FieldPoints[##],FieldPoint[1][_, _, _]]) &];
DoPaint[insVert, "counterVert"];
counterVert = CreateFeynAmp[insVert];


Print["Self Energies"]

top = CreateTopologies[1, 2 -> 3, SelfEnergiesOnly];
ins = InsertFields[top, process];

(*exclude fermion higgs couplings*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##], _. F[15,_]] || FieldMemberQ[LoopFields[##], _. S[13|14,_]] || FieldMemberQ[LoopFields[##], V[5,_]]) &]; (*Loop contains g, sguark, q, gluino*)
ins = DiagramSelect[ins,(FreeQ[LoopFields[##], V[1|2|3]] && FreeQ[LoopFields[##], S[n_/;n<=6]]) &]; (*Loop does not contain Z, W, gam, Higgs/Goldstone*)

(*delete diagrams with two ew particles that couple to the loop*)
insDel = DiagramSelect[ins,(FieldMemberQ[LoopFields[##],_. S[13|14,_]])& ]; (*only squark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3|5,___]]])& ]; (*vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakino*)
ins = DiagramComplement[ins,insDel];

ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3],V[1|2|3|5,___]]]) &]; (*Diagram does not contain V-V-squark-squark coupling*)

DoPaint[ins, "self"];
self = CalcFeynAmp[CreateFeynAmp[ins], counterSelf];
self = self//.{Alfa2->0};

Print["self = "];
Print[self];


Print["Vertices"]

top = CreateTopologies[1, 2 -> 3, TrianglesOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##], _. F[15,_]] || FieldMemberQ[LoopFields[##], _. S[13|14,_]] || FieldMemberQ[LoopFields[##], V[5,_]] || FieldMemberQ[LoopFields[##], _. F[3|4,_]]) &]; (*Loop contains g, sguark, q, gluino*)
ins = DiagramSelect[ins,(FreeQ[LoopFields[##], V[1|2|3]] && FreeQ[LoopFields[##], S[n_/;n<=6]]) &]; (*Loop does not contain Z, W, gam, Higgs/Goldstone*)

(*delete diagrams with two ew particles that couple to the loop*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_],_. F[3|4,_],V[1|2|3]]])& ]; (* ew vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakinos*)
(*insDel = DiagramSelect[insDel, NotSChannelQ[V[5,_]]]; (*no s-channel gluons in the diagrams that we want to delete*)*)
insDel = DiagramSelect[insDel, FreeQ[#, Field[n_/;n>5] -> V[5,_]]& ];  (*no internal gluon lines in the diagrams that we want to delete*)
ins = DiagramComplement[ins,insDel];

(*same for ew scalars*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][S[n_/;n<=6],_. S[13|14,_],_. S[13|14,_]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_],_. F[3|4,_],S[n_/;n<=6]]])& ]; (* ew vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakinos*)
insDel = DiagramSelect[insDel, FreeQ[#, Field[n_/;n>5] -> V[5,_]]& ];  (*no internal gluon lines in the diagrams that we want to delete*)
ins = DiagramComplement[ins,insDel];

(*Diagram does not contain V-V-squark-squark coupling*)
insDel = DiagramSelect[ins,(MemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3],V[1|2|3|5,___]]]) &];
(*but allow diagrams with gluon-Z-squark-squark coupling*)
insDel = DiagramSelect[insDel,(Not[MemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],V[2]]] && FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_],_. F[3|4,_],V[1|2|3]]]]) &];
ins = DiagramComplement[ins,insDel];

(*Diagram does not contain higgs-higgs-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[1|2|3|4|5|6],S[1|2|3|4|5|6],_. S[13|14,_],_. S[13|14,_]]]) &];

(*delete diagrams with ew particle couplings in the loop and ew vector in propagator*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. F[3|4,_],_. F[3|4,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. S[13|14,_],_. S[13|14,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_,_,V[1|2|3|5,___]]])&]; (*diagram contains ew vector or has zero color trace*)
ins = DiagramComplement[ins,insDel];

(*delete higgs to external fermion coupling (no external tops)*)
insDel = DiagramSelect[ins,(FieldMemberQ[FieldPoints[##],FieldPoint[_][S[n_/;n<=6],_,_]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_,_,S[n_/;n<=6]]])&];
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}]])&]; (*no pure quark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}]])&]; (*no pure squark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_]}]])&]; (*no pure squark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. F[15,_],_. S[13|14,_],_. S[13|14,_]}]])&]; (*no gluino squark squark loop*)
ins = DiagramComplement[ins,insDel];

DoPaint[ins, "vert"];

vert = CalcFeynAmp[CreateFeynAmp[ins], counterVert];
vert = vert//.{Alfa2->0};

Print["vert = "];
Print[vert];


(*Print["Boxes"]

top = CreateTopologies[1, 2 -> 3, BoxesOnly];
ins = InsertFields[top, process];

(*exclude fermion higgs couplings*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##], _. F[15,_]] || FieldMemberQ[LoopFields[##], _. S[13|14,_]] || FieldMemberQ[LoopFields[##], V[5,_]] || FieldMemberQ[LoopFields[##], _. F[3|4,_]]) &]; (*Loop contains g, sguark, q, gluino*)
ins = DiagramSelect[ins,(FreeQ[LoopFields[##], V[1|2|3]] && FreeQ[LoopFields[##], S[n_/;n<=6]]) &]; (*Loop does not contain Z, W, gam, Higgs/Goldstone*)

(*delete diagrams which contains V-V-squark-squark coupling, but do not delete diagram with gluino squark squark loop*)
insDel = DiagramSelect[ins,(Not[FieldMatchQ[LoopFields[##],{_. F[15,_],_. S[13|14,_],_. S[13|14,_]}]])&];
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3],V[1|2|3|5,___]]]) &];
ins = DiagramComplement[ins,insDel];

(*delete diagrams with s channel ew vector*)
(*insDel = DiagramSelect[ins, SChannelQ[V[1|2|3]]];*)
insDel = DiagramSelect[ins, MemberQ[#, Field[n_/;n>5] -> V[1|2|3]]& ];  (*internal ew vector line*)
insDel = DiagramSelect[insDel,(FieldMemberQ[LoopFields[##], _. S[13|14,_]] && FieldMemberQ[LoopFields[##], _. F[3|4,_]])&];
insDel = DiagramSelect[insDel,(FreeQ[LoopFields[##], V[1|2|3|5,___]] && FreeQ[LoopFields[##], S[n_/;n<=6]])&];
ins = DiagramComplement[ins,insDel];

DoPaint[ins, "box"];

box = CalcFeynAmp[CreateFeynAmp[ins]];
box = box//.{Alfa2->0};

Print["box = "];
Print[box];*)


(*Print["Pentagons"]

top = CreateTopologies[1, 2 -> 3, PentagonsOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(MemberQ[LoopFields[##], V[5,{_}]] || MemberQ[LoopFields[##], _. F[15,{_}]]) &];(*Loop contains a g or gluino*)
DoPaint[ins, "pent"];

pent = CalcFeynAmp[CreateFeynAmp[ins]];
pent = pent//.{Alfa2->0};

Print["pent = "];
Print[pent];*)


(* Write files *)
amps = {born,self,vert,box,pent};
{born,self,vert,box,pent} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All,born];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]]//.{Alfa2->0};

Print["abbr = "];
Print[abbr];

Print["subexpr = "];
Print[subexpr];

(*fortran can't handle arrays with dimensionality greater than 7*)
(*apply back the subexpressions with number of arguments greater than 6*)
subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
born = born//.subexpr6;
self = self//.subexpr6;
vert = vert//.subexpr6;
box  = box//.subexpr6;
pent = pent//.subexpr6;
abbr = abbr//.subexpr6;
amps = {born,self,vert,box,pent};

(*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
subexpr = subexpr//.subexpr6;

dir = SetupCodeDir[name <> "_virt", Drivers -> name <> "_drivers"];
WriteSquaredME[born, {self,vert(*,box,pent*)}, col, abbr, subexpr, dir];


(*Calculate RenConsts*)
InsertFieldsHook[tops_,f1_->f2_]:=InsertFields[tops,f1->f2,ExcludeFieldPoints -> {
    FieldPoint[_][S[n_/;n<=6], _, _],(*Exclude Higgs*)
    FieldPoint[_][S[n_/;n<=6], _, _, _],
    FieldPoint[_][V[n_/;n<5], _, _],(*Exclude Photon, Z, W*)
    FieldPoint[_][V[n_/;n<5], _, _, _],
    FieldPoint[_][F[11|12], _, _],(*Exclude Neutralino+Chargino*)
    FieldPoint[_][F[11|12], _, _, _]
}];

ren = CalcRenConst[amps];
(*the renormalization constant for the 3-gluon vertex must be calculated externally*)
dZgg3s = Import["dZgg3_MSSM.wdx"];
Print["dZgg3s"];
Print[dZgg3s];

renlist = Join[{dZgg3->dZgg3s},Level[ren,1]];
ren = RenConstList[RenConst][renlist//.{List->Sequence}];

ren = ren/.{RenConst[0]->0};
ren = ren//.{Alfa->0, Alfa2->0}; (*consider only strong corrections*)

Print["renConsts = "];
Print[ren];

WriteRenConst[ren,dir];


Print["time used: ", SessionTime[] - time1]
Exit[];
