      subroutine bmunu_ubaru_gamg(p,ampmunu)
      implicit none
#include "PhysPars.h"
#include "pwhg_math.h"
      double precision p(0:3,4)
      double precision al(0:3), be(0:3)
      integer alind, beind
      double precision ampmunu(0:3,0:3,4)
      double precision k1(0:3)
      double precision k2(0:3)
      double precision k3(0:3)
      double precision k4(0:3)
      double precision S, T, U
      integer Sfe6,Sfe6c

      double precision Epsilon, DotP, Den
      double precision momsq, momsum2sq, momsum3sq
      external Epsilon, DotP, Den
      external momsq, momsum2sq, momsum3sq

      double precision Pair18,Pair16,Pair17,Pair14
      double precision Pair15,Abb60,Abb61,Sub2
      double precision Sub3

      ampmunu(:,:,:) = 0D0

      k1(:) = p(:,1)
      k2(:) = p(:,2)
      k3(:) = p(:,3)
      k4(:) = p(:,4)
      S   = momsum2sq(k1(:), k2(:))
      T   = momsum2sq(k1(:),-k3(:))
      U   = momsum2sq(k2(:),-k3(:))

      do alind=0,3
      do beind=0,3

      al(:) = 0D0
      al(alind) = 1D0
      be(:) = 0D0
      be(beind) = 1D0

      Pair18 = DotP(al,be)
      Pair16 = DotP(k1,al)
      Pair17 = DotP(k1,be)
      Pair14 = DotP(k2,al)
      Pair15 = DotP(k2,be)
      Abb60 = -(Pair14*Pair15) + Pair16*Pair17
      Abb61 = Pair16*Pair17*S + Abb60*U
      Sub2 = 4*Pair14*Pair15*T - Pair18*T**2
      Sub3 = 4*Pair16*Pair17*U - Pair18*U**2

      ampmunu(alind,beind,4) =         (-128*Alfa*Alfas*Pi**2*
     -    (Sub2*Den(T,MU2)**2 + 
     -      (4*Abb61 + Pair18*S**2)*Den(T,MU2)*Den(U,MU2) + 
     -      Sub3*Den(U,MU2)**2))/9.

      enddo
      enddo

      end
