         subroutine chains_definitions

         use mparameters
         use mparameters_chain
         implicit none
         include 'mpif.h'
         include 'MPI.h'
         real*8 z_center,a,b,c
         integer i,j,io,ii,jj,kk,nnup,nc,run,l
         integer, dimension(100) :: ncpu
         character(len=50) :: filename = 'NUPS.txt'
         character*100 filename2

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

        do i = 1, kk-1
          write(filename2,'(A15,I4.4,A4)')
     &    'polymer_coarse_',unilong(i),'.txt'

          open(unit=3110+i,file=filename2)
          write(3110+i, *), unilong(i)
          do j=1,unilong(i)
              write(3110+i, *), segtype(i,j)
          enddo

          close(3110+i)
        end do

        end

C*************************************************************
      subroutine read_seq(i)

      use mparameters_chain

      implicit none
      include 'mpif.h'
      include 'MPI.h'

      character*100 filename1!,filename2
      integer i, nseq, j
      character(1) aacode

      write(filename1,'(A15,I4.4, A4)') 'polymer_aaCode_',long(i),'.txt'

      open(unit=2110+i,file=filename1)
!      open(unit=3110+i,file=filename2)
          
          !print*, filename, i, 'opened'
          
          read(2110+i, *), nseq
!          write(3110+i, *), nseq
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
                segtype(i,j-2) = 1
              else
                segtype(i,j) = 1
              endif
            elseif((aacode.eq.'q').or.
     &      (aacode.eq.'n').or.(aacode.eq.'t')) then
              segtype(i,j) = 4
            elseif((aacode.eq.'a').or.(aacode.eq.'l').or.
     &      (aacode.eq.'w').or.(aacode.eq.'i').or.(aacode.eq.'y')) then
              segtype(i,j) = 5
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

          do j=1,long(i)
            if(((j+3).le.long(i)).and.((aatype(i,j).eq.'g'))
     &      .and.(aatype(i,j+1).eq.'f')) then
              if((aatype(i,j+2).eq.'l').and.(aatype(i,j+3).eq.'g')) then
                segtype(i,j+1) = 2
              elseif(aatype(i,j+3).eq.'f') then
                segtype(i,j+1) = 3
              endif              
            endif
            if(((j+3).le.long(i)).and.((aatype(i,j).eq.'f'))
     &      .and.(aatype(i,j+2).eq.'f').and.(aatype(i,j+3).eq.'g')) then
                segtype(i,j) = 3
            endif
          enddo

!          write(filename2,'(A15,I4.4,A5,I2.2,A4)')
!     &    'polymer_coarse_',long(i),'chain',i,'.txt'

!          open(unit=3110+i,file=filename2)
!          write(3110+i, *), nseq
!          do j=1,long(i)
!              write(3110+i, *), segtype(i,j)
!          enddo

!          close(3110+i)

          return 
      end



