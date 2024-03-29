#if 0
* self_Sfe6_Sfe7.F
* form factors for {-F[4, {1, Col1}], F[4, {1, Col2}]} -> {F[11, {1}], F[11, {2}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 16-Feb-2017 14:11
#endif

#include "vars.h"
#include "inline.h"
#include "contains.h"


	subroutine self_Sfe6_Sfe7
	implicit none

#include "vars.h"
#include "inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/(CW2*MW2*SW2)*
     &     (GS*(2/27.D0*(Abb1*Sub1263(HelAll(Sfe6),Sfe7))/
     &            ((T - MSf2(Sfe7,4,1))*(T24 - MSf2(Sfe7,4,1))) - 
     &          1/27.D0*Sub1271(HelAll(Sfe6),Sfe7)/(MD2 - Sub1233) + 
     &          4/27.D0*(Alfas*Sub1278(HelAll(Sfe6),Sfe7)) + 
     &          1/27.D0*(-((Sub1268(HelAll(Sfe6),Sfe7)*
     &                  (F37*Sub15(Sfe6) + F39*Sub16(Sfe6)))/
     &                (T14 - MSf2(Sfe6,4,1))) - 
     &             (Sub1262(HelAll(Sfe6),Sfe7)*
     &                (F27*Sub2(Sfe6) + F11*Sub3(Sfe6)))/
     &              (T - MSf2(Sfe6,4,1))) - 
     &          (2*Pi)/9.D0*(Abb2*
     &              (F41*Sub10(Sfe6) + F44*Sub13(Sfe6))*
     &              (Sub33(Sfe6,Sfe7) - U*Sub34(Sfe6,Sfe7))*
     &              (F37*Sub70(Sfe7) + F39*Sub71(Sfe7)))/
     &            ((U - MSf2(Sfe6,4,1))*(T14 - MSf2(Sfe7,4,1))*
     &              (U - MSf2(Sfe7,4,1)))))/CB2

#if DEBUG >= 4
	DEB("self_Sfe6_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "contains.h"

	end

