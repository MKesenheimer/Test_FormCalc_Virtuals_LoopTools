c############### funcbasic.f ###########################################
c some simple auxiliary functions for matrix elements
c last modified by MK, 18.12.2015
c adapted from dislepton and disquark, plus additional helpful functions

c############### functions #############################################
c Kronecker delta
      double precision function kronecker(i,j)
        implicit none
        integer i,j
        if (i.eq.j) then
          kronecker = 1d0
        else
          kronecker = 0d0
        endif
      end

c swap two integers
      subroutine swapi(i1,i2)
        implicit none
        integer i1,i2,itmp
        itmp=i1
        i1=i2
        i2=itmp
      end

c scalar product
      function dotp(p1,p2)
      implicit none
      real * 8 dotp,p1(0:3),p2(0:3)
      dotp = (p1(0)*p2(0) - p1(3)*p2(3)) - p1(1)*p2(1) - p1(2)*p2(2)
      end
      
c squared of a four vector
      double precision function momsq(p)
        implicit none
        double precision p(0:3), dotp
        external dotp
        momsq = dotp(p,p)
      end

c check odd or even integer
      logical function iseven(n)
        implicit none
        integer n
        iseven = .true.
        if(mod(abs(n),2).eq.1) iseven = .false.
      end
      logical function isodd(n)
        implicit none
        integer n
        isodd = .false.
        if(mod(abs(n),2).eq.1) isodd = .true.
      end 

c squared of sum of 2 four vectors
      double precision function momsum2sq(p1,p2)
      implicit none
        double precision p1(0:3),p2(0:3),psum(0:3)
        double precision dotp
        external dotp
        integer j
        do j=0,3
          psum(j) = p1(j) + p2(j)
        enddo
        momsum2sq = dotp(psum,psum)
      end

c squared of sum of 3 four vectors
      double precision function momsum3sq(p1,p2,p3)
      implicit none
        double precision p1(0:3),p2(0:3),p3(0:3),psum(0:3)
        double precision dotp
        external dotp
        integer j
        do j=0,3
          psum(j) = p1(j) + p2(j) + p3(j)
        enddo
        momsum3sq = dotp(psum,psum)
      end
      
c levi-civita symbol
      integer function levi_civita(i,j,k)
      implicit none
        integer i,j,k
        ! unit vectors
        integer x(3,3)
        if( (i .le. 0 .or. i .ge. 4) .or. 
     &      (j .le. 0 .or. j .ge. 4) .or.
     &      (k .le. 0 .or. k .ge. 4)) then
          print*, "error in function levi_civita"
          print*, "i,j,k = ", i,j,k
          stop
        endif
        x(1,1) = 1
        x(2,1) = 0
        x(3,1) = 0
        x(1,2) = 0
        x(2,2) = 1
        x(3,2) = 0
        x(1,3) = 0
        x(2,3) = 0
        x(3,3) = 1
        ! calculate levi_civita as determinant of (x1,x2,x3)
        levi_civita = - x(i,3)*x(j,2)*x(k,1) + x(i,2)*x(j,3)*x(k,1)
     &                + x(i,3)*x(j,1)*x(k,2) - x(i,1)*x(j,3)*x(k,2)
     &                - x(i,2)*x(j,1)*x(k,3) + x(i,1)*x(j,2)*x(k,3)
      end

c denominator function needed by FormCalc
      double precision function Den(x,y)
        implicit none
        double precision x,y
        Den = 1/(x-y)
      end
      
c the epsilon tensor fully contracted with four-momenta k1..k4
      double precision function Epsilon(k1,k2,k3,k4)
        implicit none
        double precision k1(0:3),k2(0:3),k3(0:3),k4(0:3)
        Epsilon = k1(3)*k2(2)*k3(1)*k4(0) - k1(2)*k2(3)*k3(1)*k4(0) - 
     &            k1(3)*k2(1)*k3(2)*k4(0) + k1(1)*k2(3)*k3(2)*k4(0) + 
     &            k1(2)*k2(1)*k3(3)*k4(0) - k1(1)*k2(2)*k3(3)*k4(0) - 
     &            k1(3)*k2(2)*k3(0)*k4(1) + k1(2)*k2(3)*k3(0)*k4(1) + 
     &            k1(3)*k2(0)*k3(2)*k4(1) - k1(0)*k2(3)*k3(2)*k4(1) - 
     &            k1(2)*k2(0)*k3(3)*k4(1) + k1(0)*k2(2)*k3(3)*k4(1) + 
     &            k1(3)*k2(1)*k3(0)*k4(2) - k1(1)*k2(3)*k3(0)*k4(2) - 
     &            k1(3)*k2(0)*k3(1)*k4(2) + k1(0)*k2(3)*k3(1)*k4(2) + 
     &            k1(1)*k2(0)*k3(3)*k4(2) - k1(0)*k2(1)*k3(3)*k4(2) - 
     &            k1(2)*k2(1)*k3(0)*k4(3) + k1(1)*k2(2)*k3(0)*k4(3) + 
     &            k1(2)*k2(0)*k3(1)*k4(3) - k1(0)*k2(2)*k3(1)*k4(3) - 
     &            k1(1)*k2(0)*k3(2)*k4(3) + k1(0)*k2(1)*k3(2)*k4(3)
      end
      
c takes an array with n entries, if entry is negative: 
c mult. with -1 and save sign in the 2nd array
        subroutine transfersign(arr,signarr,n)
          implicit none
          integer n
          real*8 arr(n)
          integer signarr(n)
          integer i
          do i=1,n
            if(arr(i).lt.0) then
              arr(i) = -arr(i)
              signarr(i) = -1
            else
              signarr(i) = 1
            endif
          enddo
        end

c error-determination (taken from btilde-routines)
      double precision function calc_error(tot,etot2,n)
        implicit none
        double precision tot,etot2
        integer n
        calc_error = dsqrt((etot2/n-(tot/n)**2)/n)
      end
      
c factorial
      integer function factorial(n)
        implicit none
        integer n,m,i
        factorial=1
        m=1
        do i=1,n
          m=m*i
        enddo
        factorial=m
      end

c gibt die i-te Permutation der 4-Impulsvektoren p1, p2, ... p_DIMEN
c zurück. DIMEN und FAKUL müssen in Abhänbgigkeit der zu permutierenden
c Impulse von Hand gesetzt werden.
c Im common Block /ind/ ist außerdem die aktuelle Permutation zu
c Statuszwecken zu finden.
#define DIMEN 5
#define FAKUL 120
      subroutine permute(p,i)
        integer n,i,j,k,a
        logical nextp
        double precision r(0:3,1:FAKUL,1:DIMEN)
        integer indices(1:DIMEN),pindices(1:FAKUL,1:DIMEN)  
        double precision p(0:3,1:DIMEN)
        double precision q(0:3,1:DIMEN)  
        external nextp
        common /ind/ indices  
        parameter(n=DIMEN)
        dimension a(1:n)
        if(i.gt.FAKUL) then
          print*,"error: i > DIMEN"    
          stop
        endif    
        do j=1,n
          a(j)=j  
        enddo
        k = 1  
        q(:,:) = p(:,:)
   10   do j=1,n
          pindices(k,j) = a(j)  
          r(:,k,j) = q(:,a(j))
        enddo 
        k = k+1  
        if(nextp(n,a)) go to 10
        p(:,:) = r(:,i,:)
        indices(:) = pindices(i,:)  
        !print*,indices(i,:)
      end subroutine permute
 
      function nextp(n,a)
        integer n,a,i,j,k,t
        logical nextp
        dimension a(n)
        i=n-1
   10   if(a(i).lt.a(i+1)) go to 20
        i=i-1
        if(i.eq.0) go to 20
        go to 10
   20   j=i+1
        k=n
   30   t=a(j)
        a(j)=a(k)
        a(k)=t
        j=j+1
        k=k-1
        if(j.lt.k) go to 30
        j=i
        if(j.ne.0) go to 40
        nextp=.false.
        return
   40   j=j+1
        if(a(j).lt.a(i)) go to 40
        t=a(i)
        a(i)=a(j)
        a(j)=t
        nextp=.true.
      end

c berechnet die Größenordnung einer Zahl
      integer function magnitude(x)
        implicit none
        double precision x,z,p
        integer i
        i = 0
        z = x
        if(dabs(x).eq.0D0) go to 20
        if(dabs(x).lt.1d0) p = 1D1
        if(dabs(x).gt.1d0) p = 1D-1
   10   z = z*p
        if(dabs(x).gt.1d0.and.dabs(z).ge.1d0) then
          i = i+1
          go to 10
        endif
        if(dabs(x).lt.1d0.and.dabs(z).le.1d0) then
          i = i-1
          go to 10
        endif
   20   magnitude = i
      end 
      
c übersetzt eine dezimalzahl in ein anderes Zahlsystem zur 
c Basis "BASE" und gibt die Zahl an der Stelle k zurück
#define LENGTH 9
#define BASE 3
      integer function cdec(d,k)
        implicit none
        integer d,di,df,r,i,k
        integer b(0:LENGTH)
        do i=0,LENGTH
        b(i) = 0
        enddo
        di = d  
        i = LENGTH
   10   df = int(di/(BASE**(i)))
        r = mod(di,BASE**(i))
        b(i) = df
        di = r    
        i = i - 1    
        if(r.ne.0) go to 10
        cdec = b(k)  
      end

c gibt das Vorzeichen einer reellen Zahl zurück (+-1)
      double precision function signum(r)
        implicit none
        double precision r
        !signum = r/dabs(r)
        if(r.ge.0D0) then
          signum = 1D0
        else
          signum = -1D0
        endif
      end

c transforms a lower case character string to an upper case
      subroutine to_uppercase(str1,upper1)
        implicit none
        character*100 str1, upper1
        integer j
#ifdef DEBUGQ
        print*,str1
#endif
        do j=1,len(trim(str1))
          ! convert both strings to uppercase
          if(str1(j:j) .ge. "a" .and. str1(j:j) .le. "z") then
            upper1(j:j) = achar(iachar(str1(j:j)) - 32)
          else
            upper1(j:j) = str1(j:j)
          endif
        enddo
#ifdef DEBUGQ
        print*,upper1
#endif
#ifdef DEBUGQ
        upper1 = str1
#endif
      end

c sorts the entries of an integer list in decreasing order
      subroutine sorti(list,lgth)
        implicit none
        integer i,j,n,t,lgth
        parameter (n=100)
        integer b(n), list(n)
        if(lgth.gt.n) then
          print*,"error in sorti: increase n = ",n
          print*,"lgth = ",lgth
          stop
        endif  
        do i=1,lgth
          b(i) = list(i)
        enddo
        do i=1,lgth
          do j=i,lgth
            if(b(i)<b(j)) then
              t = b(j)
              b(j) = b(i)
              b(i) = t
            endif
          enddo
        enddo
        do i=1,lgth
          list(i) = b(i)
        enddo
      end 
      
c sorts the entries of an integer list in decreasing order and keep track
c of sorting in ilist
c bevor sorting:
c list(1) = 5
c list(2) = 6
c list(3) = 4
c after sorting:
c list(1) = 6, ilist(1) = 2
c list(2) = 5, ilist(2) = 1
c list(3) = 4, ilist(3) = 3
      subroutine sortit(list,ilist,lgth)
        implicit none
        integer i,j,n,t,it,lgth
        parameter (n=100)
        integer b(n), list(n),ilist(n)
        if(lgth.gt.n) then
          print*,"error in sorti: increase n = ",n
          print*,"lgth = ",lgth
          stop
        endif  
        do i=1,lgth
          b(i) = list(i)
          ilist(i) = i
        enddo
        do i=1,lgth
          do j=i,lgth
            if(b(i)<b(j)) then
              t = b(j)
              it = ilist(j)
              b(j) = b(i)
              ilist(j) = ilist(i)
              b(i) = t
              ilist(i) = it
            endif
          enddo
        enddo
        do i=1,lgth
          list(i) = b(i)
        enddo
      end

c stops the program if gets called for n times (useful for debugging)
      subroutine nstop(n)
        integer n,i
        save i
        i = i + 1
        if(i.ge.n) then
          i = 0
          stop
        endif
      end
      
c calculates the kaellen function and the sqrt of kaellen function
      double precision function kaellen(x, y, z)
        implicit none
        double precision x, y, z
        kaellen = x**2+y**2+z**2-2*(x*y+x*z+y*z)
      end

      double precision function kaellenSqrt(x, y, z)
        implicit none
        double precision x, y, z
        kaellenSqrt = dsqrt(dabs(x**2+y**2+z**2-2*(x*y+x*z+y*z)))
      end
c############### end functions #########################################
