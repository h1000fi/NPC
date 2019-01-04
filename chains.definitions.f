         subroutine chains_definitions

         use mparameters
         use mparameters_chain
         implicit none
         include 'mpif.h'
         include 'MPI.h'
         real*8 z_center,a,b,c
         integer i,j,io,ii,jj,kk,nnup,nc,run,l
         integer, dimension(1000) :: ncpu
         character(len=50) :: filename = 'NUPS.txt'
         character*100 filenameCG

         N_chains = 0
         nnup = 0
         ncpu = 0

         if(rank.eq.0)print*, 'Reading Nups parameters from ', filename

         open (unit=10,file=filename,
     &        status='old', action='read', form="formatted")
         do
            read (10,*,IOSTAT=io) nc
            if (io>0) then
             if(rank.eq.0)print*, 'error with NUPS.txt'
            else if (io<0) then
             if(rank.eq.0)print*, 'number of chains', N_chains
             exit
            else
             nnup = nnup + 1
             ncpu(nnup) = nc
             N_chains = N_chains + ncpu(nnup)
            endif
         end do
         close(10)

         ALLOCATE (long(N_chains))
         ALLOCATE (unilong(N_chains))
         ALLOCATE (chainsperdelta(N_chains))
         ALLOCATE (zposition(N_chains))
         ALLOCATE (rposition(N_chains))
         ALLOCATE (segtype(N_chains, maxlong+3))
         ALLOCATE (aatype(N_chains, maxlong+3)) 

         open (unit=11,file=filename,
     &        status='old', action='read', form="formatted")
         run = 0
         do i=1,nnup,1
          read(11,*,end=998) nc,a,l,b,c
          do j=1,ncpu(i),1
           chainsperdelta(run+j) = a
           long(run+j) = l
           zposition(run+j) = b
           rposition(run+j) = c
          end do
          run = run + ncpu(i)
         end do
998      continue
         close(11)

         z_center   = delta*dimZ/2.0  ! NPC center z[nm]

         do i=1,N_chains,1
          zposition(i) = zposition(i)+z_center
          call read_seq(i)
         enddo

!        if (i.ne.N_chains) then 
!        if(rank.eq.0)print*, 'BAD NUMBER OF CHAINS for seq_type 13'
!        stop
!        end if                                  ! MK END ADDED

        unilong(:) = 0
        unilong(1) = long(1)
        kk = 2
        do ii = 1, N_chains
           jj = 1
           do while((jj.lt.kk).and.(long(ii).ne.unilong(jj)))
              jj = jj + 1
              if(jj.eq.kk) then
              unilong(kk) = long(ii)
              kk = kk + 1
              endif
           end do
        enddo
        if(rank.eq.0)print*, 'lengths of Nups:', unilong(:)

!        do ii = 1, kk-1
!          write(filenameCG,'(A15,I4.4,A4)')
!     &    'polymer_coarse_',unilong(ii),'.txt'
!
!          open(unit=3110+ii,file=filenameCG)
!          write(3110+ii, *), unilong(ii)
!          do jj=1,unilong(ii)
!              write(3110+ii, *), segtype(ii,jj) !segtype output bug
!          enddo
!
!          close(3110+ii)
!        end do

        end

C*************************************************************
      subroutine read_seq(i)

      use mparameters_chain

      implicit none
      include 'mpif.h'
      include 'MPI.h'

      character*100 filename1, filename2
      integer i, nseq, j
      character(1) aacode

      write(filename1,'(A15,I4.4, A4)') 'polymer_aaCode_',long(i),'.txt'
      write(filename2,'(A5,I4.4,A6,I4.4,A4)')
     &    'chain',i,'length',long(i),'.txt'

      open(unit=2110+i,file=filename1)
      open(unit=1110+i,file=filename2)
          
          !print*, filename, i, 'opened'
          
          read(2110+i, *), nseq
          write(1110+i, *), nseq
c          print*, 'nseq', nseq

          long(i)=nseq          
          do j=1,long(i)
            read(2110+i, *), aacode
            aatype(i,j) = aacode
            if(aacode .eq. 'f') then
              if(j.lt.3) then
                segtype(i,j) = 1
              elseif(segtype(i,j-2).eq.1) then
                segtype(i,j) = 5
                segtype(i,j-2) = 3
              elseif(segtype(i,j-1).eq.5) then
                segtype(i,j) = 2
              elseif(segtype(i,j-1).eq.1) then
                segtype(i,j) = 2
                segtype(i,j-1) = 5
              else
                segtype(i,j) = 1
              endif
            elseif((aacode.eq.'q').or.
     &      (aacode.eq.'n').or.(aacode.eq.'t')) then
              segtype(i,j) = 4
            elseif((aacode.eq.'a').or.(aacode.eq.'l').or.
     &      (aacode.eq.'w').or.(aacode.eq.'i').or.(aacode.eq.'y')) then
              segtype(i,j) = 5
              if((j.gt.1).and.(segtype(i,j-1).eq.1)) then
                segtype(i,j-1) = 2
              endif
            elseif((aacode.eq.'m').or.(aacode.eq.'s').or.
     &      (aacode.eq.'p').or.(aacode.eq.'v') .or.(aacode.eq.'g')) then
              segtype(i,j) = 6
            elseif((aacode.eq.'k').or.(aacode.eq.'r')) then
              segtype(i,j) = 7
            elseif((aacode.eq.'d').or.(aacode.eq.'e')) then
              segtype(i,j) = 8
            elseif(aacode.eq.'c') then
              segtype(i,j) = 9
            elseif(aacode.eq.'h') then
              segtype(i,j) = 10
            else
              print*, 'Error in sequence file', 2110+i
              call MPI_FINALIZE(ierr) ! finaliza MPI
              stop
            endif
          enddo

          close(2110+i) 

!          write(filename2,'(A15,I4.4,A5,I2.2,A4)')
!     &    'polymer_coarse_',long(i),'chain',i,'.txt'

!          open(unit=1110+i,file=filename2)
!          write(1110+i, *), nseq
          do j=1,long(i)
              write(1110+i, *), segtype(i,j)
          enddo

          close(1110+i)

          return 
      end



