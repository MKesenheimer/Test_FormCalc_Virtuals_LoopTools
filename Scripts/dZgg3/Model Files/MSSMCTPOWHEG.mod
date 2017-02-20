(*
	MSSMCTPOWHEG.mod
                model file for MSSM based on MSSMCT.mod
                by Julien Baglio.
                Includes SUSY-restoring counterterms for
                Yukawa couplings in MS_bar.
                quark-squark-gluino:
                gsy = gs*(1+AlfaS/(3*Pi)) == gs*(1+dZgs1y) (see: hep-ph/9308222)
                For quark-squark-neutralino:
                ely = el*(1-AlfaS/(6*Pi)) == el*(1+dZe1y).
                The values of dZgs1y and dZe1y must be set manually
                in the generated fortran code.
                implementation of Apr 2015		
                last modified 11 Nov 16 by jb
*)

LoadModel["MSSMCT"]

(* modification of the couplings *)

SusyCTCoup[(lhs:C[F[11, _], -F[3, _], S[13, _]]) == rhs_] := lhs == (rhs/.{dZe1 -> dZe1+dZe1y})
SusyCTCoup[(lhs:C[F[11, _], -F[4, _], S[14, _]]) == rhs_] := lhs == (rhs/.{dZe1 -> dZe1+dZe1y})
SusyCTCoup[(lhs:C[F[3, _], F[11, _], -S[13, _]]) == rhs_] := lhs == (rhs/.{dZe1 -> dZe1+dZe1y}) 
SusyCTCoup[(lhs:C[F[4, _], F[11, _], -S[14, _]]) == rhs_] := lhs == (rhs/.{dZe1 -> dZe1+dZe1y})
SusyCTCoup[(lhs:C[F[15, _], -F[3, _], S[13, _]]) == rhs_] := lhs == (rhs/.{dZgs1 -> dZgs1+dZgs1y})
SusyCTCoup[(lhs:C[F[15, _], -F[4, _], S[14, _]]) == rhs_] := lhs == (rhs/.{dZgs1 -> dZgs1+dZgs1y})
SusyCTCoup[(lhs:C[F[15, _], F[3, _], -S[13, _]]) == rhs_] := lhs == (rhs/.{dZgs1 -> dZgs1+dZgs1y})
SusyCTCoup[(lhs:C[F[15, _], F[4, _], -S[14, _]]) == rhs_] := lhs == (rhs/.{dZgs1 -> dZgs1+dZgs1y})
SusyCTCoup[other_] = other

M$CouplingMatrices = M$CouplingMatrices /. {Alfa -> 0}
M$CouplingMatrices = SusyCTCoup /@ M$CouplingMatrices

(* remove pure  EW counterterms *)
dZW1 := 0;
dZAA1 := 0;
dZAZ1 := 0;
dZZA1 := 0;
dZZZ1 := 0;
dMWsq1 := 0;
dMZsq1 := 0;

dSW1 := 0;
dZe1 := 0;

dZG01 := 0;
dZGp1 := 0;
dUZZ1 := 0;
dUZA1 := 0;
dUAZ1 := 0;
dUAA1 := 0;
dUW1  := 0;

dMUE1 := 0;
dMUEdr := 0;
 
dTB1 := 0;
dSB1 := 0;
dCB1 := 0;

dZHiggs1[_, _] := 0;
dMHiggs1[_, _] := 0;

dZH1 := 0;
dZH2 := 0;

dMChaOS1[_] := 0;
dMNeuOS1[_] := 0;
dMCha1[_] := 0;
dMNeu1[_] := 0;

dZNeu1[_] := 0;
dMNeu1[_,_] := 0;

dMino11 := 0;
dMino21 := 0;

dZfL1[1|2|11|12, _, _] := 0;
dZfR1[1|2|11|12, _, _] := 0;
dZbarfL1[1|2|11|12, _, _] := 0;
dZbarfR1[1|2|11|12, _, _] := 0;

dAf1[_,_,_] := 0;

RenConst[0] := 0;
