      subroutine fkfun(x,f,ier)

      use mlookup
      use mchains
      use mparameters
      use mncells
      use mkai
      use mporesystem
      use mvariables
      use mparameters_chain
      use mparameters_monomer
      use mprotein

      implicit none
 
      include 'mpif.h' ! MPI libraries
      include 'MPI.h' ! MPI libraries

      double precision Factorcurv

      integer ntot
      real*8 x((2+N_poorsol)*ncells)
      real*8 f((2+N_poorsol)*ncells)

      real*8  protemp, protemp1, avePnorm, xave, smoothstep

      integer i,j, k, ix, iy, iC, ii, aR, temp, aZ, jj, im, imc, iic

      real*8 temp2
      real*8 Fpair_ref, Rpair_ref, Fpair_tot_ref, hdistance,hfactor

      real*8 psi(ncells+2) ! psi se define asi para evitar problemas al construir las fs
      real*8 xtotal(N_poorsol, ncells+2) ! psi se define asi para evitar problemas al construir las fs

      real*8 xh(ncells) ! psi se define asi para evitar problemas al construir las fs
 
! Kinsol

      integer*8 neq
      integer*4 ier

! MPI

      integer spp ! sitios por procesador

      integer cuantas_p(2)

! solving auxiliary variables

      real*8 avpol_temp(N_monomer, N_chains, ncells)
      real*8 avpol_tosend(N_monomer, N_chains, ncells)

      real*8 xpot(N_monomer,ncells)
       
     
! bit operations

      integer*1 displ_one(0:7)
      integer*4 pC, pZ, pR

      integer*1 displacement(maxlong+1,2)
      integer*1 binary(int(maxlong/2))

C-----------------------------------------------------

! Jefe

      if(rank.eq.0) then ! llama a subordinados y pasa vector x
       flagsolver = 1

       CALL MPI_BCAST(flagsolver, 1, MPI_INTEGER, 0, MPI_COMM_WORLD,err)

       CALL MPI_BCAST(x, (2+N_poorsol)*ncells , MPI_DOUBLE_PRECISION,0
     &  , MPI_COMM_WORLD,err)

      endif


! Recupera xh y psi

      ntot = ncells

            do iC = 1,ntot

            xh(iC)=x(iC)
            psi(iC)=x(iC+ntot)

            do i=1, N_poorsol
            xtotal(i,iC)=x(iC+(1+i)*ntot) ! v-frac of the good solvent polymers
            enddo

            enddo
  
      qtot_amp = 0.0      

! Condiciones de borde psi 
 
! This implictly considers SIGMAQ = 0
! 


! bulk
      psi(ncells+1) = 0.0
      psi(ncells+2) = 0.0 

! las condiciones en la pared y condicion de simetria esta en el lookuptable

! Fracciones de volumen inicial y fdis

           avpol=0
           avpol_tosend = 0.0
           fdis = 0.0
           fdisP = 0.0

           do iC = 1, ncells
     
           xpos(iC) = expmupos*(xh(iC)**vsalt)
     &     *dexp(-psi(iC)*zpos) ! ion plus volume fraction

           xneg(iC) = expmuneg*(xh(iC)**vsalt)
     &     *dexp(-psi(iC)*zneg) ! ion neg volume fraction
     
           xHplus(iC) = expmuHplus*(xh(iC))
     &     *dexp(-psi(iC))           ! H+ volume fraction
     
           xOHmin(iC) = expmuOHmin*(xh(iC))
     &     *dexp(+psi(iC))           ! OH-  volume fraction



         if (prot_q < 0) then
               fdisP(iC) =
     &              1.0 /(1.0 + xHplus(iC)/(K0P*xh(iC)) )
         else ! base
               fdisP(iC) =
     &              1.0 /(1.0 + xOHmin(iC)/(K0P*xh(iC)) )
         endif

 
         do im =1,N_monomer
            if (zpol(im).eq.1) then !BASE
               fdis(im,iC) =
     &              1.0 /(1.0 + xOHmin(iC)/(K0(im)*xh(iC)) )
            else if (zpol(im).eq.-1) then !ACID
               fdis(im,iC) =
     &              1.0 /(1.0 + xHplus(iC)/(K0(im)*xh(iC)) )
            endif
         enddo

            enddo


! Calculo de xtotal para poor solvent

          do i = 1, N_poorsol
          xtotal(i, ncells+1) = 0.0
          xtotal(i, ncells+2) = 0.0
          enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculo de xpot

      do im =1, N_monomer
      do iC = 1, ntot

      select case (hguess)

      case (1) !wall-center

      hdistance = (min(dfloat(indexa(iC,1))*delta,
     & abs(dfloat(indexa(iC,1))*delta-CdimR*delta+hside)))**2
     &  +(oval*(dfloat(indexa(iC,2))-0.5*dimZ)*delta)**2

      case (2) !wall

      hdistance = (dfloat(indexa(iC,1))*delta-(CdimR*delta-hside))**2
     &  +(oval*(dfloat(indexa(iC,2))-0.5*dimZ)*delta)**2

      case (3) !center

      hdistance = (dfloat(indexa(iC,1))*delta)**2
     &  +(oval*((dfloat(indexa(iC,2))-0.5*dimZ)*delta-hcenter))**2

      case (4) !Ghavami

      hdistance = (dfloat(indexa(iC,1))*delta-CdimR*delta+10)**2
     &  +((dfloat(indexa(iC,2))-0.5*dimZ-hcenter/delta)*delta)**2

      case (5) !wall-upcenter

      hdistance = min((dfloat(indexa(iC,1))*delta)**2
     & +(dfloat(indexa(iC,2))*delta-0.5*dimZ*delta-hcenter)**2,
     & (dfloat(indexa(iC,1))*delta-CdimR*delta+hside)**2
     & +((dfloat(indexa(iC,2))-0.5*dimZ)*delta)**2)

      endselect

      hfactor = dexp(-(kp**2)*hdistance)

      xpot(im,iC) = dlog(xh(iC))*vpol

      if(zpol(im).ne.0) then
      xpot(im,iC) = xpot(im,iC)
     & -psi(iC)*zpol(im)
     & -dlog(fdis(im, iC))
      endif

      if(hydroph(im).ne.0) then 

               xpot(im,iC) = xpot(im,iC)+henergy(hydroph(im))
     & *proteinhC(iC)*hst

               do ii = 1, N_poorsol ! loop over different poor sv types
               do jj = 1, nXu(ii, iC) ! loop over kai neighbors

                        xpot(im,iC) = xpot(im,iC) +
     &                       (st_matrix(hydroph(im),ii) ! st_matrix(x, y) : interaction of hydrophobic segments of type x with those of type y ( should be diagonal)
     &                       *hfactor*st/(vsol*vpol)*           ! st in kT/monomer          
     &                       Xulist_value(2, iC, jj)*
!     &                       Xulist_value(ii, iC, jj)*
     &                       xtotal(ii, Xulist_cell(ii, iC, jj)))

               enddo ! jj
               enddo ! ii

               if(hydroph(im).eq.1) then

                 aveP(iC) = 0.0
!                 aveC(iC) = 0.0
                 avePnorm = 0.0

                 do jj = 1, nXu(1, iC) ! loop over kai neighbors
                 avePnorm = avePnorm + Xulist_value(1, iC, jj)
     &                      *(dfloat(indexa(iC,1))-0.5)
                 aveP(iC) = aveP(iC) +
     &                      (Xulist_value(1, iC, jj)*
     &                      (dfloat(indexa(iC,1))-0.5)*
     &                      (xtotal(1, Xulist_cell(1, iC, jj))))
!     &                      decouple*xtotal(2, Xulist_cell(2, iC, jj))))
!                 aveC(iC) = aveC(iC) +
!     &                      (Xulist_value(1, iC, jj)*
!     &                      qtot_amp(Xulist_cell(1, iC, jj)))
                 enddo
                 aveP(iC) = aveP(iC)/avePnorm
!                 aveC(iC) = aveC(iC)/avePnorm

!                 aveP(iC) = xtotal(1,iC)  ! +decouple*xtotal(2,iC)

                 if(aveP(iC).gt.0) then
                 Spair(iC) = dlog(pairsize*aveP(iC))
                 Fpair_ref = 0.5*(0.0+Spair(iC))
                 Rpair_ref = (0.5*(sqrt(dexp(-2.0*Fpair_ref)+4.0)
     &               -dexp(-Fpair_ref)))**2
                 Fpair_tot_ref = -Rpair_ref*dlog(Rpair_ref)
     &               -(1-Rpair_ref)*dlog(1-Rpair_ref)
     &               +Rpair_ref*Fpair_ref
     &               +0.5*Rpair_ref*(dlog(Rpair_ref)-1)+Rpair_ref
                 hdistance =sqrt(dfloat(indexa(iC,1))**2
     &               +(dfloat(indexa(iC,2))-0.5*dimZ-20)**2)
                 Fpair(iC) = 0.5*(pairst*hfactor
     &               +Spair(iC))
                 Rpair(iC) = (0.5*(sqrt(dexp(-2*Fpair(iC))+4.0)
     &               -dexp(-Fpair(iC))))**2
                 Fpair_tot(iC) = -Rpair(iC)*dlog(Rpair(iC))
     &               -(1-Rpair(iC))*dlog(1-Rpair(iC))
     &               +Rpair(iC)*Fpair(iC)
     &               +0.5*Rpair(iC)*(dlog(Rpair(iC))-1)
     &               -Fpair_tot_ref+Rpair(iC)
                 xpot(im,iC)=xpot(im,iC)+Fpair_tot(iC)
                 endif
               endif

!               if(hydroph(im).eq.2) then

!                 aveP(iC) = 0.0
!!                 aveC(iC) = 0.0
!                 avePnorm = 0.0
!
!                 do jj = 1, nXu(2, iC) ! loop over kai neighbors
!                 avePnorm = avePnorm + Xulist_value(2, iC, jj)
!     &                      *(dfloat(indexa(iC,1))-0.5)
!                 aveP(iC) = aveP(iC) +
!     &                      (Xulist_value(2, iC, jj)*
!     &                      (dfloat(indexa(iC,1))-0.5)*
!     &                      (xtotal(2, Xulist_cell(2, iC, jj))+
!     &                      decouple*xtotal(1, Xulist_cell(1, iC, jj))))
!!                 aveC(iC) = aveC(iC) +
!!     &                      (Xulist_value(1, iC, jj)*
!!     &                      qtot_amp(Xulist_cell(1, iC, jj)))
!                 enddo
!                 aveP(iC) = aveP(iC)/avePnorm
!!                 aveC(iC) = aveC(iC)/avePnorm
!
!!                 aveP(iC) = xtotal(1,iC)  ! +decouple*xtotal(2,iC)
!
!                 if(aveP(iC).gt.0) then
!                 Spair(iC) = dlog(pairsize*aveP(iC))
!                 Fpair_ref = 0.5*(0.0+Spair(iC))
!                 Rpair_ref = (0.5*(sqrt(dexp(-2.0*Fpair_ref)+4.0)
!     &               -dexp(-Fpair_ref)))**2
!                 Fpair_tot_ref = -Rpair_ref*dlog(Rpair_ref)
!     &               -(1-Rpair_ref)*dlog(1-Rpair_ref)
!     &               +Rpair_ref*Fpair_ref
!     &               +0.5*Rpair_ref*(dlog(Rpair_ref)-1)
!                 hdistance =sqrt(dfloat(indexa(iC,1))**2
!     &               +(dfloat(indexa(iC,2))-0.5*dimZ-20)**2)
!                 Fpair(iC) = 0.5*(pairst*hfactor
!     &               +Spair(iC))
!                 Rpair(iC) = (0.5*(sqrt(dexp(-2*Fpair(iC))+4.0)
!     &               -dexp(-Fpair(iC))))**2
!                 Fpair_tot(iC) = -Rpair(iC)*dlog(Rpair(iC))
!     &               -(1-Rpair(iC))*dlog(1-Rpair(iC))
!     &               +Rpair(iC)*Fpair(iC)
!     &               +0.5*Rpair(iC)*(dlog(Rpair(iC))-1)
!     &               -Fpair_tot_ref
!                 xpot(im,iC)=xpot(im,iC)+Fpair_tot(iC)
!                 endif
!               endif


      endif ! hydrophob
      enddo ! iC
      enddo ! im
      ii = rank+1

      avpol_temp = 0.0

      q = 0.0
      sumprolnpro = 0.0
      endtoend_av = 0.0

      do i=1,newcuantas(ii)

         lnpro=0.0


!! DECODER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         pC = fs(i)                                        !
         pR = indexa(pC,1)                                 !
         pZ = indexa(pC,2)                                 !
         binary(:) = displ(i,:)                            !
         call decode(displacement, binary, long(ii))                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         do j=1,long(ii)

         pR = pR + displacement(j,1) 
         pZ = pZ + displacement(j,2) 
         pC = matriz(pR, pZ)

         lnpro = lnpro + xpot(segtype(ii, j),pC) !+ dlog(pbias(i))

         enddo ! j
            
            pro = dexp(lnpro)*pbias(ii,i)
           ! lnpro = dlog(pro)

            q=q+pro
            sumprolnpro = sumprolnpro + pro*lnpro

            endtoend_av = endtoend_av + pro*endtoend(i)

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         pC = fs(i)                                     !
         pR = indexa(pC,1)                              !
         pZ = indexa(pC,2)                              !
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         do j=1,long(ii)

         pR = pR + displacement(j,1)
         pZ = pZ + displacement(j,2)
         pC = matriz(pR, pZ)

         avpol_temp(segtype(ii,j), ii, pC) 
     & = avpol_temp(segtype(ii,j), ii, pC)
     & + pro*chainsperdelta(ii)*vsol
     & *vpol*Factorcurv(pR)

         enddo ! j
         enddo !i

          avpol_temp=avpol_temp/q
          endtoend_av = endtoend_av/q
          avpol_tosend = avpol_temp

          avpol_temp = 0
        
c------------------ MPI ----------------------------------------------


         call MPI_REDUCE(avpol_tosend, avpol_temp
     & , ncells*N_monomer*N_chains,
     &   MPI_DOUBLE_PRECISION, MPI_SUM,0, MPI_COMM_WORLD, err)

      CALL MPI_BARRIER(MPI_COMM_WORLD,err)

      do ii = 1, N_chains
      do iC = 1,ntot
      do im = 1, N_monomer
      avpol(im,ii,iC) = avpol_temp(im,ii,iC)
      enddo ! im
      enddo ! iC
      enddo ! ii

      if(rank.ne.0)goto 3333
!!!!!!!!!!! IMPORTANT, SLAVES STOP HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C----------------------------------------------------------------------------------------------
C   Construct f vector
C----------------------------------------------------------------------------------------------

! Qtot

      do iC=1,ntot

         qtot(iC) = 
     &   (zpos*xpos(iC)+zneg*xneg(iC))/vsalt 
     &   + xHplus(iC)-xOHmin(iC)

         qtot_amp(iC) =
     &   (abs(zpos*xpos(iC))+abs(zneg*xneg(iC)))/vsalt
     &   + abs(xHplus(iC))+abs(xOHmin(iC))

         if(weakP.eq.1) then
         qtot(iC) = qtot(iC)  + proteinqC(iC)*vsol*fdisP(iC) 
         qtot_amp(iC)=qtot_amp(iC)+abs(proteinqC(iC))*vsol
     &   *fdisP(iC)
         else
         qtot(iC) = qtot(iC)  + proteinqC(iC)*vsol
         qtot_amp(iC)=qtot_amp(iC)+abs(proteinqC(iC))*vsol
         endif

         do im = 1, N_monomer
         do ii = 1, N_chains

         qtot(iC) = qtot(iC) + avpol(im, ii, iC)
     &   *fdis(im, iC)*zpol(im)/vpol
         qtot_amp(iC) = qtot_amp(iC) + avpol(im, ii, iC)
     &   *fdis(im, iC)*abs(zpol(im))/vpol

         enddo ! ii
         enddo ! im
 
      enddo



! Volume fraction

      do iC=1, ntot
         
               f(iC)=
     &   xh(iC) + xneg(iC) 
     &  + xpos(iC) + xHplus(iC) + xOHmin(iC)
     &  + proteinC(iC) - 1.0d0
   
      do im = 1, N_monomer
      do ii = 1, N_chains

      f(iC) = f(iC) + avpol(im, ii, iC)

      enddo ! ii
      enddo ! im

      enddo

! Poisson eq.

            do iC=1,ntot

! Cilindro (derivada al centro), ver notas     
     
               f(iC+ntot)=
     & psi(rp(iC)) -2*psi(iC) + psi(rm(iC)) +
     & (0.5/(dfloat(indexa(iC,1))-0.5))*(psi(rp(iC))-psi(rm(iC))) + ! termino de curvatura
     & psi(zp(iC)) -2*psi(iC) + psi(zm(iC)) + ! derivada en z
     & qtot(iC)*constq
     
               f(iC+ntot)=f(iC+ntot)/(-2.0) ! mejora kinsol...
      enddo

! poor solvent

      do ii = 1, N_poorsol
      do iC = 1,ntot

      f(iC+(1+ii)*ntot) = xtotal(ii,iC)

      do im = 1, N_monomer
      if(hydroph(im).eq.ii) then
      do jj = 1, N_chains
        f(iC+(1+ii)*ntot) = f(iC+(1+ii)*ntot) - avpol(im,jj,iC)
      enddo ! jj
      endif
      enddo ! im
      enddo ! iC
      enddo ! ii

      iter = iter + 1

      norma = 0.0

      do i = 1, (2+N_poorsol)*ntot
      norma = norma +(f(i))**2    
      enddo
      
      print*, iter, norma, q
      open(unit=6660, file='iter.dat', access='append')
      write(6660,*)iter, norma, q
      close (6660)
      ier = 0.0

! saves infile

      if(mod(iter, save_every).eq.0) then
      print*, 'Save resume file'
      open(unit=45, file = 'out.temp.dat')
      do i = 1, (2+N_poorsol)*ncells
      write(45, *)x(i)
      enddo
      close(45)
      endif

 3333 ier = 0.0

      return
      end
