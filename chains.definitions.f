         subroutine chains_definitions

         use mparameters
         use mparameters_chain
         implicit none
         include 'mpif.h'
         include 'MPI.h'
         real*8 z_center
         integer i

         if(rank.eq.0) then
         print*, 'Using chain definitions for the NPC'
         print*, '20 different chains in the system'
         endif

         N_chains = 20
         ALLOCATE (long(N_chains))
         ALLOCATE (chainsperdelta(N_chains))
         ALLOCATE (zposition(N_chains))
         ALLOCATE (rposition(N_chains))
         ALLOCATE (segtype(N_chains, maxlong)) 
         N_chains = 0

         z_center   = delta*dimZ/2.0  ! NPC center z[nm]

         i=0
         chainsperdelta(:) = polyCov !8.0

         if(nups(1).eq.1) then
         i=i+1
         ! Nup42 #1
         long(i)=382
         call read_seq(1) 
!         zposition(i)      = 13 + z_center   ! in nm                    
         zposition(i) = znups(1)
         rposition(i) = rnups(1)
         N_chains = N_chains+1
         end if

         if(nups(2).eq.1) then
         ! Nup159 #2
         i=i+1
         long(i)=685
         call read_seq(2) 
!         zposition(i)      = 16 + z_center     
         zposition(i) = znups(2)
         rposition(i) = rnups(2)
         N_chains = N_chains+1
         end if

         if(nups(3).eq.1) then
         ! Nup116 #3
         i=i+1
         long(i)=789
         call read_seq(3) 
!         zposition(i)      = 11 + z_center
         zposition(i) = znups(3)
         rposition(i) = rnups(3)
         N_chains = N_chains+1
         end if

         if(nups(4).eq.1) then
         ! Nsp1 #4
         i=i+1
         long(i)=617
         call read_seq(4) 
!         zposition(i)      = 12 + z_center
         zposition(i) = znups(4)
         rposition(i) = rnups(4)
         N_chains = N_chains+1
         end if

         if(nups(5).eq.1) then                           
         ! Nup100 #5
         i=i+1
         long(i)=800
         call read_seq(5) 
!         zposition(i)      = 9 + z_center
         zposition(i) = znups(5)
         rposition(i) = rnups(5)
         N_chains = N_chains+1
         end if              
   
         if(nups(6).eq.1) then
         ! Nup59 #6
         i=i+1
         long(i)=206
         call read_seq(6) 
!         zposition(i)      =  8 + z_center
         zposition(i) = znups(6)
         rposition(i) = rnups(6)
         N_chains = N_chains+1
         end if         

         if(nups(7).eq.1) then
         ! Nup53 #7
         i=i+1
         long(i)=227
         call read_seq(7) 
!         zposition(i)      =  7 + z_center
         zposition(i) = znups(7)
         rposition(i) = rnups(7)
         N_chains = N_chains+1
         end if               
   
         if(nups(8).eq.1) then       
         ! Nsp1 #6 #8
         i=i+1
         long(i)=617
         call read_seq(8) 
!         zposition(i)      =  7 + z_center
         zposition(i) = znups(8)
         rposition(i) = rnups(8)
         N_chains = N_chains+1
         end if        

         if(nups(9).eq.1) then
         ! Nup49 #7 #9      
         i=i+1
         long(i)=251
         call read_seq(9) 
!         zposition(i)      =  6 + z_center
         zposition(i) = znups(9)
         rposition(i) = rnups(9)
         N_chains = N_chains+1
         end if 

         if(nups(10).eq.1) then
         ! Nup57 #8 #10     
         i=i+1
         long(i)=255
         call read_seq(10) 
!         zposition(i)      =  4 + z_center 
         zposition(i) = znups(10)
         rposition(i) = rnups(10)
         N_chains = N_chains+1
         end if               
    
         if(nups(11).eq.1) then        
         ! Nup49 #9 #11
         i=i+1
         long(i)=251
         call read_seq(11) 
!         zposition(i)      = -6 + z_center
         zposition(i) = znups(11)
         rposition(i) = rnups(11)
         N_chains = N_chains+1
         end if               
   
         if(nups(12).eq.1) then         
         ! Nup57 #10 #12
         i=i+1 
         long(i)=255
         call read_seq(12) 
!         zposition(i)      = -4 + z_center
         zposition(i) = znups(12)
         rposition(i) = rnups(12)
         N_chains = N_chains+1
         end if               
    
         if(nups(13).eq.1) then      
         ! Nup145 #11 #13   
         i=i+1
         long(i)=433
         call read_seq(13) 
!         zposition(i)      =  -5 + z_center
         zposition(i) = znups(13)
         rposition(i) = rnups(13)
         N_chains = N_chains+1
         end if               
    
         if(nups(14).eq.1) then        
         ! Nup53 #14
         i=i+1 
         long(i)=227
         call read_seq(14) 
!         zposition(i)      = -7 + z_center
         zposition(i) = znups(14)
         rposition(i) = rnups(14)
         N_chains = N_chains+1 
         end if               
    
         if(nups(15).eq.1) then        
         ! Nsp1 #12 #15
         i=i+1
         long(i)=617
         call read_seq(15) 
!         zposition(i)      = -7 + z_center
         zposition(i) = znups(15)
         rposition(i) = rnups(15)
         N_chains = N_chains+1
         end if               
    
         if(nups(16).eq.1) then        
         ! Nup59 #16
         i=i+1
         long(i)=206
         call read_seq(16) 
!         zposition(i)      = -8 + z_center
         zposition(i) = znups(16)
         rposition(i) = rnups(16)
         N_chains = N_chains+1
         end if         

         if(nups(17).eq.1) then
         ! Nsp1 #13 #17
         i=i+1
         long(i)=617
         call read_seq(17) 
!         zposition(i)      = -12 + z_center
         zposition(i) = znups(17)
         rposition(i) = rnups(17)
         N_chains = N_chains+1
         end if               
    
         if(nups(18).eq.1) then        
         ! Nup1 #14 #18
         i=i+1
         long(i)=857
         call read_seq(18) 
!         zposition(i)      = -12 + z_center
         zposition(i) = znups(18)
         rposition(i) = rnups(18)
         N_chains = N_chains+1
         end if               
    
         if(nups(19).eq.1) then        
        ! Nup60 #15 #19
         i=i+1
         long(i)=151
         call read_seq(19) 
!         zposition(i)      = -14 + z_center
         zposition(i) = znups(19)
         rposition(i) = rnups(19)
         N_chains = N_chains+1
         end if

         if(nups(20).eq.1) then
         ! Nup145N #16 #20
         i=i+1
         long(i)=433
         call read_seq(20) 
!         zposition(i)      = -14 + z_center
         zposition(i) = znups(20)
         rposition(i) = rnups(20)
         N_chains = N_chains+1
         end if

!        if (i.ne.N_chains) then 
!        if(rank.eq.0)print*, 'BAD NUMBER OF CHAINS for seq_type 13'
!        stop
!        end if                                  ! MK END ADDED

        end

C*************************************************************
      subroutine read_seq(i)

      use mparameters_chain

      implicit none
      include 'mpif.h'
      include 'MPI.h'

      character*100 filename,filename2
      integer i, nseq, j
      character(1) aacode

      write(filename,'(A15, I3.3, A4)') 'polymer_aaCode_',i,'.txt'  
      write(filename2,'(A15, I3.3, A4)') 'polymer_coarse_',i,'.txt'

      open(unit=2110+i,file=filename)
      open(unit=3110+i,file=filename2)
          
          !print*, filename, i, 'opened'
          
          read(2110+i, *), nseq
          write(3110+i, *), nseq
c          print*, 'nseq', nseq

          long(i)=nseq          
          do j=1,long(i)
            read(2110+i, *), aacode
            if(aacode .eq. 'f') then
              if(j.lt.3) then
                segtype(i,j) = 1
                write(3110+i, *), segtype(i,j)
              elseif(segtype(i,j-2).eq.1) then
                segtype(i,j) = 3
                write(3110+i, *), segtype(i,j)
              else
                segtype(i,j) = 1
                write(3110+i, *), segtype(i,j)
              endif
            elseif((aacode.eq.'g').or.(aacode.eq.'q').or.
     &      (aacode.eq.'n').or.(aacode.eq.'t')) then
              segtype(i,j) = 2
              write(3110+i, *), segtype(i,j)
            elseif((aacode.eq.'a').or.(aacode.eq.'l').or.
     &      (aacode.eq.'w').or.(aacode.eq.'i').or.(aacode.eq.'y')) then
              segtype(i,j) = 3
              write(3110+i, *), segtype(i,j)
            elseif((aacode.eq.'m').or.(aacode.eq.'s').or.
     &      (aacode.eq.'p').or.(aacode.eq.'v')) then
              segtype(i,j) = 4
              write(3110+i, *), segtype(i,j)
            elseif((aacode.eq.'k').or.(aacode.eq.'r')) then
              segtype(i,j) = 5
              write(3110+i, *), segtype(i,j)
            elseif((aacode.eq.'d').or.(aacode.eq.'e')) then
              segtype(i,j) = 6
              write(3110+i, *), segtype(i,j)
            elseif(aacode.eq.'c') then
              segtype(i,j) = 7
              write(3110+i, *), segtype(i,j)
            elseif(aacode.eq.'h') then
              segtype(i,j) = 8
              write(3110+i, *), segtype(i,j)
            else
              print*, 'Error in sequence file', 2110+i
              call MPI_FINALIZE(ierr) ! finaliza MPI
              stop
            endif
          enddo

          close(2110+i) 
          close(3110+i)
          
          return 
      end



