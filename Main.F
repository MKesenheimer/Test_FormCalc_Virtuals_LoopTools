c############### Main.F ################################################
      program main
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegborn)
        double precision xborn(ndiminteg-3)
        integer flav(nlegborn), i
        double precision amp(2), ampUV(2)
        
        ! generate random number
        call srand(1234)
        
        ! process parameters
        flav(1) = -1
        flav(2) = 1
        flav(3) = 1000022
        flav(4) = 1000023
        flav(5) = 0
        
        call init_couplings
        call ltini
        
        do i=1,15
          ! choose a point
          xborn(1) = rand() ! x1
          xborn(2) = rand() ! x2 
          xborn(3) = rand() ! theta
          xborn(4) = rand()
          xborn(5) = rand()
          xborn(6) = rand()
        
          call born_phsp(xborn,flav,p)
        
#ifdef DEBUG
          print*
          print*,"p(:,:) = "
          print*,p(:,1)
          print*,p(:,2)
          print*,p(:,3)
          print*,p(:,4)
          print*,p(:,5)
          print*,"pt(5) = ",dsqrt(p(1,5)**2+p(2,5)**2)
#endif
          call formCalcAmp(p,flav,amp, ampUV)
          print*,"born, virt   = ", amp
          print*,"born, virtUV = ", ampUV
        enddo  
      end
c############### end Main ##############################################

c############### subroutine formCalcVecSet #############################
c calculate unit vectors and set the vectors for FormCalc
      subroutine formCalcVecSet(nlegs,p,flav)
        implicit none
        integer nlegs
        double precision p(0:3,nlegs)
        integer  flav(nlegs)
        double precision mass(nlegs)
        double precision ex, ey, ez, pNorm
        integer i
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq, dotp
        external momsum2sq, momsum3sq, dtop
        ! load process dependent masses
        call set_process(nlegs,flav(1:nlegs),mass(1:nlegs))
        do i=1,nlegs
          pNorm=dsqrt(p(1,i)**2+p(2,i)**2+p(3,i)**2)
          ex=p(1,i)/pNorm
          ey=p(2,i)/pNorm
          ez=p(3,i)/pNorm
          call VecSet(i, mass(i), pNorm, ex,ey,ez)
        enddo
      end
c############### end subroutine formCalcVecSet #########################

c############### subroutine getAmps ####################################
      subroutine formCalcAmp(p,flav,amp, ampUV)
        implicit none
#include "nlegborn.h"
#include "PhysPars.h"
        double precision p(0:3,nlegborn)
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        integer  flav(nlegborn)
        ! formcalc results
        double precision fc_result(2), amp(2), ampUV(2), symfac
        double precision st_muren2
        double precision momsum2sq
        external momsum2sq

        ! reset
        fc_result(:) = 0D0
        amp = 0D0
        
        call set_ebe_couplings
        st_muren2 = momsum2sq(p(:,1),p(:,2))
        
        ! process parameters
        ! unpolarized fermion: B01010  FC8Guid.pdf p.49
        ! unpolarized gluon: B01110
        helicities = 10824014
        flags = 6
        symfac = 36D0
        ! set the formCalc vectors
        call formCalcVecSet(nlegborn,p,flav)
        
        call clearcache
        call setlambda(0d0) ! Only finite part
        call setuvdiv(0d0)  ! no 1/eps_UV terms
        call setdelta(0d0)  ! set divergences to zero
        call setmudim(st_muren2) ! renormalization scale
        call setDivergence()
        
        call RenConst
        call squaredME(fc_result,helicities,flags)
        amp(:) = fc_result(:)/symfac
        
        ! test for finiteness
        call clearcache
        call setuvdiv(1d0)
        call setdelta(1D7)
        call setDivergence()
        
        call RenConst
        call squaredME(fc_result,helicities,flags)
        ampUV(:) = fc_result(:)/symfac
      end
c############### subroutine getAmps ####################################

c############### subroutine setDivergence ##############################
      subroutine setDivergence()
        implicit none
#include "types.h"
#include "const.h"
        double precision getdelta
        external detdelta
        Divergence = getdelta()
      end
c############### subroutine setDivergence ##############################