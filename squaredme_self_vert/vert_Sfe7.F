#if 0
* vert_Sfe7.F
* form factors for {-F[4, {1, Col1}], F[4, {1, Col2}]} -> {F[11, {1}], F[11, {2}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 16-Feb-2017 14:11
#endif

#include "vars.h"
#include "inline.h"
#include "contains.h"


	subroutine vert_Sfe7
	implicit none

#include "vars.h"
#include "inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    (Alfa*Pi)/(CW2*MW2*SW2)*
     &     (GS*(2/9.D0*(Abb1*Sub1537(HelAll(Sfe7))*
     &              (F4*Sub46(Sfe7) + F7*Sub47(Sfe7)))/
     &            (T24 - MSf2(Sfe7,4,1)) - 
     &          2/9.D0*(Abb2*Sub1538(HelAll(Sfe7))*
     &              (F41*Sub62(Sfe7) + F44*Sub63(Sfe7)))/
     &            (U - MSf2(Sfe7,4,1))))/CB2

#if DEBUG >= 4
	DEB("vert_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "contains.h"

	end

