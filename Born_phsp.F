c############### Born.f ################################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### subroutine born_phsp ##################################
c fills the four-momenta of the Born-process particles,
c both in the laboratory and in the center-of-mass frame,
c the Bjorken x of the two incoming partons,
c the partonic Mandelstam variable s,
c the masses for Born and real emission,
c and the Jacobi factor for the phase space volume
      subroutine born_phsp(xborn,flav,p_lab)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_math.h"
        ! input:
        double precision xborn(ndiminteg-3)
        integer  flav(nlegborn)
        double precision p_lab(0:3,nlegborn)
        double precision sbeams
        ! output, local variables:
        double precision vec(3),beta
        ! jacobian
        double precision jac
        double precision p12(0:3), pij(0:3)
        integer ind,nu
        ! final state arrangement
        integer i,j,k
        ! check 4-momentum conservation
        logical lresult
        ! jacobian
        double precision jacborn
        ! phase space borders
        double precision sijmin
        ! mandelstams
        double precision sborn
        double precision mass(nlegborn), minmass, p_cms(0:3,nlegborn)
        double precision x1, x2
        double precision p_beams(0:3,2)
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
        ! pseudorapidity and pt
        double precision etajet, ptjet

        ! reset the jacobians
        jac = 1D0
        jacborn = 1d0

        sbeams = (14D3)**2
        ! center of mass momenta
        p_beams(0,1) =  dsqrt(sbeams)/2d0
        p_beams(1,1) =  0d0
        p_beams(2,1) =  0d0
        p_beams(3,1) =  p_beams(0,1)
        
        p_beams(0,2) =  p_beams(0,1)
        p_beams(1,2) =  0d0
        p_beams(2,2) =  0d0
        p_beams(3,2) = -p_beams(0,1)
        
        ! set masses for Born and real emission
        call set_process(nlegborn, flav, mass)

        ! lower bound on the mass of the final state 2->3 process
        minmass = mass(3) + mass(4)
        
        ! calculate the parton momentum fractions x1, x2
        ! the squared CM energy of the Born process sborn
        ! and the jacobi factor of the PS volume
        call x1x2phspace(1,sbeams,minmass,xborn(1:2),x1,x2,sborn,jac)
        jacborn = jacborn*jac
        
        ! center of mass momenta
        p_cms(0,1) =  dsqrt(sborn)/2d0
        p_cms(1,1) =  0d0
        p_cms(2,1) =  0d0
        p_cms(3,1) =  p_cms(0,1)
        
        p_cms(0,2) =  p_cms(0,1)
        p_cms(1,2) =  0d0
        p_cms(2,2) =  0d0
        p_cms(3,2) = -p_cms(0,1)

        p12(:) = p_cms(:,1) + p_cms(:,2)
        sijmin = (mass(i)+mass(j))**2
        
        ! generate four-momenta in the cms frame
        ! for the particles produced in the Born process,
        ! calculates the Jacobi factor for the phase space volume
        ! we don't want to integrate over phi, so we provide 0 for xphi
        ! (see phi1_2.f)
        call R2phsp(xborn(3),0D0,mass(3),mass(4),p12,p_cms(:,3),
     &              p_cms(:,4),jac)
        jacborn = jacborn*jac

        ! physical phase space jacobian ((2*Pi)**(3n-4), n=2)
        jacborn = jacborn/(2D0*pi)**2
        
        ! velocity of subsystem in laboratory frame
        beta = (x1-x2)/(x1+x2)

        ! set up boost vector
        vec(1) = 0D0
        vec(2) = 0D0
        vec(3) = 1D0

        ! calculate momenta p_lab in the laboratory frame
        ! boost p_cms (CM) -> p_lab (lab. frame)
        call mboost(nlegborn-2,vec,beta,p_cms(0,3),p_lab(0,3))

        do nu=0,3
           p_lab(nu,1) = x1*p_beams(nu,1)
           p_lab(nu,2) = x2*p_beams(nu,2)
        enddo

        ! check four momentum conservation
        call check_4conservation(p_lab,nlegborn,2,lresult)
        if(.not.lresult) jacborn = 0D0
      end
c############### end subroutine born_phsp ##############################
