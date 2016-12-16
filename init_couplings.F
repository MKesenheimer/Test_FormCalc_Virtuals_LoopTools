c############### init_couplings.f ######################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### init_couplings subroutine #############################
c setup SM and MSSM parameters in PhysPars.h
c requires encoded process ID in lprup(1)
      subroutine init_couplings
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
        ! final state IDs
        integer id_fin1, id_fin2
        logical verbose
        parameter (verbose=.true.)

        ! set PhysPars parameters
        call set_phys_params

        ! set remaining FormCalc parameters
        call set_formcalc_params
      end
c############### end subroutine init_couplings #########################

c############### subroutine set_ebe_couplings ##########################
c set the madgraph and formcalc couplings event by event
      subroutine set_ebe_couplings
        implicit none
#include "PhysPars.h"
#include "pwhg_math.h"
        ! FormCalc
        ! QCD coupling
        AlfaS  = SMInputs_AlfasMZ
        AlfaS2 = AlfaS**2
        GS = dsqrt(AlfaS*4d0*pi)
        ! e.m. coupling
        Alfa = par_alpha
        Alfa2   = Alfa**2
        EL = dsqrt(Alfa*4d0*pi)
      end
c############### end subroutine set_ebe_couplings ######################

c############### set_phys_params subroutine ############################
c extracts mass and mixing matrices from slhadata
c assumes slhadata to be filled already
c sets the PhysPars.h parameters
      subroutine set_phys_params
        implicit none
#include "PhysPars.h"
        integer i,j,k   ! loops
        ! decay widths
        integer particle, channel
        ! for reading parameters from SHLA file
        integer error
        character*100 slhafilename
        parameter (slhafilename="input_orig.slha")
        ! final state IDs
        integer id_fin1, id_fin2, id_fin3
        ! weinberg angle
        double precision sinW
        
        par_alpha    = invalid
        par_Zmass    = invalid
        par_Wmass    = invalid
        par_Zmass2   = invalid
        par_Wmass2   = invalid
        par_Zwidth   = invalid
        par_Wwidth   = invalid
        par_Fin1mass = invalid
        par_Fin2mass = invalid
        par_Fin3mass = invalid

        call SLHARead(error, slhadata, slhafilename, 1)
        if (error.ne.0)  stop "error reading parameters from SLHA file"

        ! read all particle widths from SLHA-file
        nchannels = SLHADecayTable(slhadata, 0, width, partid, 7, 1024)

#ifdef DEBUGQ
        do channel = 1, nchannels
          nparticles = partid(0,channel)
          if(nparticles .eq. 1) then
            print*,"==> total width(",partid(1,channel),") = "
     &            ,width(channel)
          else
            print*, "--> partial width(",partid(1,channel),"to"
            do particle = 2, nparticles
              print*,partid(particle,channel)
            enddo
            print*, "            ) = ", width(channel)
          endif
          print*
        enddo
        !stop
#endif

        ! SM parameters from SLHA-file
        print*, "note: ignoring alpha_s value given in SLHA file"
        par_Zmass   = dreal(SMInputs_MZ)
        par_Wmass   = dreal(Mass_MW)
        par_gf      = dreal(SMInputs_GF)
        par_alphaMZ = 1d0/dreal(SMInputs_invAlfaMZ)
#define SLHA_ALPHA
#ifdef SLHA_ALPHA
        par_alpha   = par_alphaMZ
#else
        sinW        = dsqrt(1d0-par_Wmass**2/par_Zmass**2)
        par_alpha   = dsqrt(2d0)*par_Wmass**2*sinW**2*par_gf/pi
#endif
        !print*,1/par_alpha
        !stop
        par_Zmass2  = par_Zmass**2
        par_Wmass2  = par_Wmass**2

        ! particle widths from SLHA-file
        do channel = 1, nchannels
          nparticles = partid(0,channel)
          if(nparticles .eq. 1) then
            if(partid(1,channel).eq.23) then
              par_Zwidth = width(channel)
            else if(partid(1,channel).eq.24) then
              par_Wwidth = width(channel)
            endif
          endif
        enddo

        if( par_Wwidth .eq. invalid) then
          print*, "warning: W-width not found in SLHA file."
          print*, "         Use zero width."
          par_Wwidth = 0D0
        endif

        if( par_Zwidth .eq. invalid) then
          print*, "warning: Z-width not found in SLHA file."
          print*, "         Use zero width."
          par_Zwidth = 0D0
        endif
        
        ! Sfermion masses
        do i=1,2     ! left, right
          do j=1,4   ! type (nu,e,u,d)
            do k=1,3 ! generation
              par_MSf(i,j,k) = dreal(Mass_MSf(i,j,k))
            enddo
          enddo
        enddo
        
        ! Neutralino masses
        do i=1,4
          par_MNeu(i)  = dabs(dreal(Mass_MNeu(i)))
          !par_MNeu(i)  = dreal(Mass_MNeu(i))
        enddo
        
        ! Chargino masses
        do i=1,2
          par_MCha(i)  = dreal(Mass_MCha(i))
        enddo
        
        ! gluino mass
        par_MGl = dreal(Mass_MGl)

c small mass to regularize LoopTools integrals
c#define EPSMASS 1D-30
#ifdef EPSMASS
        print*,"note (PhysPars): neglecting u,d,c,s-quark masses"
        par_MU = EPSMASS ! note: setting this exactly to zero can cause 
        par_MC = EPSMASS ! problems in calculating propagators, since the
        par_MD = EPSMASS ! formcalc code already neglects all light 
        par_MS = EPSMASS ! masses analytically but leaves some masses in propagators
#else
        par_MU = 0D0
        par_MC = 0D0
        par_MD = 0D0
        par_MS = 0D0
#endif

c b quark mass
#ifdef NEGLECTBMASS
        print*,"note (PhysPars): neglecting b-quark mass"
#ifdef EPSMASS
        par_MB = EPSMASS
#else
        par_MB = 0D0
#endif
#else
        ! read mass paramater from slha file
        par_MB  = dreal(Mass_Mf(4,3)) 
#endif

        ! top mass
        !par_MT  = dreal(Mass_Mf(3,3))
        par_MT = dreal(SMInputs_Mt)

c particle widths from SLHA-file
c NOTE: FormCalc does not use squark widths in the current software 
c version.
        do channel = 1, nchannels
          nparticles = partid(0,channel)
          if(nparticles .eq. 1) then
            select case(partid(1,channel))
              case (1000021) ! gluino
                par_WGl = width(channel)
#ifndef NEGLECT_SWIDTH
              print*, "warning: FormCalc does not use squark-widths."  
              case (1000001) ! sdown_L
                par_WSf(1,4,1) = width(channel)
              case (2000001) ! sdown_R
                par_WSf(2,4,1) = width(channel)
              case (1000002) ! sup_L
                par_WSf(1,3,1) = width(channel)
              case (2000002) ! sup_R
                par_WSf(2,3,1) = width(channel)
              case (1000003) ! sstrange_L
                par_WSf(1,4,2) = width(channel)
              case (2000003) ! sstrange_R
                par_WSf(2,4,2) = width(channel)
              case (1000004) ! scharm_L
                par_WSf(1,3,2) = width(channel)
              case (2000004) ! scharm_R
                par_WSf(2,3,2) = width(channel)
              case (1000005) ! sbottom_1
                par_WSf(1,4,3) = width(channel)
              case (2000005) ! sbottom_2
                par_WSf(2,4,3) = width(channel)
              case (1000006) ! stop_1
                par_WSf(1,3,3) = width(channel)
              case (2000006) ! stop_2
                par_WSf(2,3,3) = width(channel)
#endif
            endselect
          endif
        enddo
#ifdef NEGLECT_SWIDTH
        do i=1,2 ! sfermion index s
          do j=3,4 ! type (u or d)
            do k=1,3 ! generation
              WSf(i,j,k) = 0d0
            enddo
          enddo
        enddo
#endif
        
#ifdef DEBUGQ
        print*, "sd_L width =", par_WSf(1,4,1)
        print*, "sd_R width =", par_WSf(2,4,1)
        print*, "sc_L width =", par_WSf(1,3,2)
        print*, "sc_R width =", par_WSf(2,3,2)
        print*, "go width   =", par_WGl
        stop
#endif
      end
c############### end subroutine set_phys_params ########################

c############### subroutine set_formcalc_params ########################
c extracts mass and mixing matrices from slhadata
c assumes slhadata to be filled already
c sets the FormCalc parameters
      subroutine set_formcalc_params
        implicit none
#include "PhysPars.h"
#include "pwhg_math.h"
        double precision powheginput, kronecker
        external powheginput, kronecker
        integer i,j,k,l ! loops
        ! Avoid multiple calls to this subroutine.
        logical called
        data called/.false./
        save called
        if(called) then
          return
        else
          called=.true.
        endif

c CKM-Matrix
c if you change this, you have to change something in
c MadGraphs couplings.f as well.
#define NO_CKM_MIXING
#ifdef NO_CKM_MIXING
        print*, "note (FormCalc): no CKM mixing"
        do i=1,3
          do j=1,3
            CKM(i,j) = kronecker(i,j)
          enddo
        enddo
#else
        CKM(1,1) = 0.97427d0
        CKM(1,2) = 0.22534d0
        CKM(1,3) = 0.00351d0
        CKM(2,1) = 0.22520d0
        CKM(2,2) = 0.97344d0
        CKM(2,3) = 0.04120d0
        CKM(3,1) = 0.00867d0
        CKM(3,2) = 0.04040d0
        CKM(3,3) = 0.99915d0
#endif

        ! Slepton/Squark masses and widths
        do i=1,2 ! sfermion index s
          do j=1,4 ! type (nu,e,u,d)
            do k=1,3 ! generation
              WSf(i,j,k)  = par_WSf(i,j,k)
              MSf(i,j,k)  = par_MSf(i,j,k)
              MSf2(i,j,k) = MSf(i,j,k)**2
            enddo
          enddo
        enddo
       
        ! Neutralino masses
        do i=1,4 !neutralino index s
          MNeu(i)  = par_MNeu(i)
          MNeu2(i) = MNeu(i)**2
        enddo
        
        ! Chargino masses
        do i=1,2 !chargino index c
          MCha(i)  = par_MCha(i)
          MCha2(i) = MCha(i)**2
        enddo
        
        ! Z/W mass
        MZ  = par_Zmass
        MZ2 = MZ**2
        MW  = par_Wmass
        MW2 = MW**2

        ! Quark masses
        MU   = par_MU
        MU2  = MU**2
        MC   = par_MC
        MC2  = MC**2
        MD   = par_MD
        MD2  = MD**2
        MS   = par_MS
        MS2  = MS**2
        MB   = par_MB
        MB2  = MB**2
        MT   = par_MT
        MT2  = MT**2
        
        Mf(1,1) = 0D0
        Mf(2,1) = 0D0
        Mf(3,1) = MU
        Mf(4,1) = MD
        Mf(1,2) = 0D0
        Mf(2,2) = 0D0
        Mf(3,2) = MC
        Mf(4,2) = MS
        Mf(1,3) = 0D0
        Mf(2,3) = 0D0
        Mf(3,3) = MT
        Mf(4,3) = MB

        Mf2(1,1) = 0D0
        Mf2(2,1) = 0D0
        Mf2(3,1) = MU2
        Mf2(4,1) = MD2
        Mf2(1,2) = 0D0
        Mf2(2,2) = 0D0
        Mf2(3,2) = MC2
        Mf2(4,2) = MS2
        Mf2(1,3) = 0D0
        Mf2(2,3) = 0D0
        Mf2(3,3) = MT2
        Mf2(4,3) = MB2
        
        ! Gluino mass and width
        MGl  = par_Mgl
        MGl2 = MGl**2
        WGl  = par_WGl

        ! Z/W Widths (added by hand to formcalc)
        WZ  = par_Zwidth
        WW  = par_Wwidth

        ! Neutralino mixing matrix
        ! SLHA name: NMix_ZNeu(n1,n2)
        do i=1,4 ! neutralino index n1
          do j=1,4 ! neutralino index n2
            if(dreal(Mass_MNeu(i)).lt.0D0) then ! check if we need a complex mixing matrix
              ZNeu(i,j) = ii*dcmplx(NMix_ZNeu(i,j))
            else
              ZNeu(i,j) = dcmplx(NMix_ZNeu(i,j))
            endif  
          enddo
        enddo
        
        ! Chargino mixing matrix
        do i=1,2 ! chargino index c1
          do j=1,2 ! chargino index c2
            VCha(i,j) =  dcmplx(VMix_VCha(i,j))
            UCha(i,j) =  dcmplx(UMix_UCha(i,j))
          enddo
        enddo

        ! Squark mixing matrices (u, d, s, c - sfermions)
        do i=1,2 ! sfermion index s1 (left, right)
          do j=1,2 ! sfermion index s2
            do k=2,4 ! type index t (u- or d-type)
              do l=1,2 ! generation index g (note: generation 3 is in line 448)
                if(i.eq.j)  USf(i,j,k,l) = (1D0,0D0)
                if(i.ne.j)  USf(i,j,k,l) = (0D0,0D0)
              enddo
            enddo
          enddo
        enddo
        do i=1,2 ! sfermion index s1
          do j=1,2 ! sfermion index s2
            ! Mixing between t-sfermions (mixing)
            USf(i,j,3,3) = dcmplx(StopMix_USf(i,j))
            ! Mixing between b-sfermions (mixing)
            USf(i,j,4,3) = dcmplx(SbotMix_USf(i,j))
          enddo
        enddo
        
        ! mixing angles
        CW  = MW/MZ
        CW2 = CW**2
        SW2 = 1 - CW2
        SW  = dsqrt(SW2)
        
        ! sin(beta), cos(beta), tan(beta)
        ! note: the matrix elements do not depend on TB
        !TB = HMix_TB
        TB = MinPar_TB
        CB = dabs(1.D0/dsqrt(1+TB**2))
        SB = dabs(TB*CB)
        SB2 = SB**2
        CB2 = CB**2
        TB2 = TB**2
        S2B = 2*TB*CB2
        C2B = (CB - SB)*(CB + SB)
        
        ! Gaugino mass parameter
        Mino3 = dcmplx(Mgl)
        
        ! trilinear couplings
        ! note: the matrix elements do not depend on Af
        do i=2,4 ! type 
          do j=1,3 ! generation
            do k=1,3 ! generation
              Af(i,j,k) = dcmplx(Af_Af(j,k,i))
            enddo
          enddo
        enddo
        
        ! mass term mixing the 2 Higgs doublets
        ! note: the matrix elements do not depend on MUE
        MUE  = dcmplx(HMix_MUE)

        ! couplings
        ! set Formcalc couplings equal to the ones provided by POWHEG
        Alfa    = par_alpha
        Alfa2   = Alfa**2
        AlfaS   = SMInputs_AlfasMZ ! dummy value, gets overwritten by set_ebe_couplings
        AlfaS2  = AlfaS**2
        AlfaMZ  = par_alphaMZ
        AlfaSMZ = SMInputs_AlfasMZ
        EL = dsqrt(4*pi*Alfa)
        GS = dsqrt(4*pi*AlfaS)
      end
c############### end subroutine set_formcalc_params ####################