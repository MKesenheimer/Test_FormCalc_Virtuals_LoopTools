#if 0
* vert.F
* form factors for {-F[4, {1, Col1}], F[4, {1, Col2}]} -> {F[11, {1}], F[11, {2}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 16-Feb-2017 13:39
#endif

#include "vars.h"
#include "inline.h"
#include "contains.h"


	subroutine vert
	implicit none

#include "vars.h"
#include "inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa/(36.D0*CW2)*(GS*
     &        ((Alfas*Sub10217 - 4*Sub1198)/(MD2 - Sub9988) + 
     &          (Alfas*Sub10225 + 4*Sub1203)/(MD2 - Sub9989)))/
     &      (MZ2 - S34)

#if DEBUG >= 4
	DEB("vert:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "contains.h"

	end

