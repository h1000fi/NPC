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
         ALLOCATE (aatype(N_chains, maxlong)) 
         N_chains = 0

         z_center   = delta*dimZ/2.0  ! NPC center z[nm]

         i=0
         chainsperdelta(:) = polyCov !8.0

         if(nups(1).gt.0) then
         i=i+1
         ! Nup42 #1
         long(i)=382
         call read_seq(1) 
!         zposition(i)      = 13 + z_center   ! in nm                    
         zposition(i) = znups(1)
         rposition(i) = rnups(1)
         chainsperdelta(i) = nups(1)
         N_chains = N_chains+1
         end if

         if(nups(2).gt.0) then
         ! Nup159 #2
         i=i+1
         long(i)=685
         call read_seq(2) 
!         zposition(i)      = 16 + z_center     
         zposition(i) = znups(2)
         rposition(i) = rnups(2)
         chainsperdelta(i) = nups(2)
         N_chains = N_chains+1
         end if

         if(nups(3).gt.0) then
         ! Nup116 #3
         i=i+1
         long(i)=789
         call read_seq(3) 
!         zposition(i)      = 11 + z_center
         zposition(i) = znups(3)
         rposition(i) = rnups(3)
         chainsperdelta(i) = nups(3)
         N_chains = N_chains+1
         end if

         if(nups(4).gt.0) then
         ! Nsp1 #4
         i=i+1
         long(i)=617
         call read_seq(4) 
!         zposition(i)      = 12 + z_center
         zposition(i) = znups(4)
         rposition(i) = rnups(4)
         chainsperdelta(i) = nups(4)
         N_chains = N_chains+1
         end if

         if(nups(5).gt.0) then                           
         ! Nup100 #5
         i=i+1
         long(i)=800
         call read_seq(5) 
!         zposition(i)      = 9 + z_center
         zposition(i) = znups(5)
         rposition(i) = rnups(5)
         chainsperdelta(i) = nups(5)
         N_chains = N_chains+1
         end if              
   
         if(nups(6).gt.0) then
         ! Nup59 #6
         i=i+1
         long(i)=206
         call read_seq(6) 
!         zposition(i)      =  8 + z_center
         zposition(i) = znups(6)
         rposition(i) = rnups(6)
         chainsperdelta(i) = nups(6)
         N_chains = N_chains+1
         end if         

         if(nups(7).gt.0) then
         ! Nup53 #7
         i=i+1
         long(i)=227
         call read_seq(7) 
!         zposition(i)      =  7 + z_center
         zposition(i) = znups(7)
         rposition(i) = rnups(7)
         chainsperdelta(i) = nups(7)
         N_chains = N_chains+1
         end if               
   
         if(nups(8).gt.0) then       
         ! Nsp1 #6 #8
         i=i+1
         long(i)=617
         call read_seq(8) 
!         zposition(i)      =  7 + z_center
         zposition(i) = znups(8)
         rposition(i) = rnups(8)
         chainsperdelta(i) = nups(8)
         N_chains = N_chains+1
         end if        

         if(nups(9).gt.0) then
         ! Nup49 #7 #9      
         i=i+1
         long(i)=251
         call read_seq(9) 
!         zposition(i)      =  6 + z_center
         zposition(i) = znups(9)
         rposition(i) = rnups(9)
         chainsperdelta(i) = nups(9)
         N_chains = N_chains+1
         end if 

         if(nups(10).gt.0) then
         ! Nup57 #8 #10     
         i=i+1
         long(i)=255
         call read_seq(10) 
!         zposition(i)      =  4 + z_center 
         zposition(i) = znups(10)
         rposition(i) = rnups(10)
         chainsperdelta(i) = nups(10)
         N_chains = N_chains+1
         end if               
    
         if(nups(11).gt.0) then        
         ! Nup49 #9 #11
         i=i+1
         long(i)=251
         call read_seq(11) 
!         zposition(i)      = -6 + z_center
         zposition(i) = znups(11)
         rposition(i) = rnups(11)
         chainsperdelta(i) = nups(11)
         N_chains = N_chains+1
         end if               
   
         if(nups(12).gt.0) then         
         ! Nup57 #10 #12
         i=i+1 
         long(i)=255
         call read_seq(12) 
!         zposition(i)      = -4 + z_center
         zposition(i) = znups(12)
         rposition(i) = rnups(12)
         chainsperdelta(i) = nups(12)
         N_chains = N_chains+1
         end if               
    
         if(nups(13).gt.0) then      
         ! Nup145 #11 #13   
         i=i+1
         long(i)=433
         call read_seq(13) 
!         zposition(i)      =  -5 + z_center
         zposition(i) = znups(13)
         rposition(i) = rnups(13)
         chainsperdelta(i) = nups(13)
         N_chains = N_chains+1
         end if               
    
         if(nups(14).gt.0) then        
         ! Nup53 #14
         i=i+1 
         long(i)=227
         call read_seq(14) 
!         zposition(i)      = -7 + z_center
         zposition(i) = znups(14)
         rposition(i) = rnups(14)
         chainsperdelta(i) = nups(14)
         N_chains = N_chains+1 
         end if               
    
         if(nups(15).gt.0) then        
         ! Nsp1 #12 #15
         i=i+1
         long(i)=617
         call read_seq(15) 
!         zposition(i)      = -7 + z_center
         zposition(i) = znups(15)
         rposition(i) = rnups(15)
         chainsperdelta(i) = nups(15)
         N_chains = N_chains+1
         end if               
    
         if(nups(16).gt.0) then        
         ! Nup59 #16
         i=i+1
         long(i)=206
         call read_seq(16) 
!         zposition(i)      = -8 + z_center
         zposition(i) = znups(16)
         rposition(i) = rnups(16)
         chainsperdelta(i) = nups(16)
         N_chains = N_chains+1
         end if         

         if(nups(17).gt.0) then
         ! Nsp1 #13 #17
         i=i+1
         long(i)=617
         call read_seq(17) 
!         zposition(i)      = -12 + z_center
         zposition(i) = znups(17)
         rposition(i) = rnups(17)
         chainsperdelta(i) = nups(17)
         N_chains = N_chains+1
         end if               
    
         if(nups(18).gt.0) then        
         ! Nup1 #14 #18
         i=i+1
         long(i)=857
         call read_seq(18) 
!         zposition(i)      = -12 + z_center
         zposition(i) = znups(18)
         rposition(i) = rnups(18)
         chainsperdelta(i) = nups(18)
         N_chains = N_chains+1
         end if               
    
         if(nups(19).gt.0) then        
        ! Nup60 #15 #19
         i=i+1
         long(i)=151
         call read_seq(19) 
!         zposition(i)      = -14 + z_center
         zposition(i) = znups(19)
         rposition(i) = rnups(19)
         chainsperdelta(i) = nups(19)
         N_chains = N_chains+1
         end if

         if(nups(20).gt.0) then
         ! Nup145N #16 #20
         i=i+1
         long(i)=433
         call read_seq(20) 
!         zposition(i)      = -14 + z_center
         zposition(i) = znups(20)
         rposition(i) = rnups(20)
         chainsperdelta(i) = nups(20)
         N_chains = N_chains+1
         end if

         if(nups(21).gt.0) then
         ! Nup116 #3
         i=i+1
         long(i)=789
         call read_seq(21)
!         zposition(i)      = 11 + z_center
         zposition(i) = znups(21)
         rposition(i) = rnups(21)
         chainsperdelta(i) = nups(21)
         N_chains = N_chains+1
         end if

         if(nups(22).gt.0) then
         ! Nup100 #5
         i=i+1
         long(i)=800
         call read_seq(22)
!         zposition(i)      = 9 + z_center
         zposition(i) = znups(22)
         rposition(i) = rnups(22)
         chainsperdelta(i) = nups(22)
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
            elseif((aacode.eq.'g').or.(aacode.eq.'q').or.
     &      (aacode.eq.'n').or.(aacode.eq.'t')) then
              segtype(i,j) = 4
            elseif((aacode.eq.'a').or.(aacode.eq.'l').or.
     &      (aacode.eq.'w').or.(aacode.eq.'i').or.(aacode.eq.'y')) then
              segtype(i,j) = 5
            elseif((aacode.eq.'m').or.(aacode.eq.'s').or.
     &      (aacode.eq.'p').or.(aacode.eq.'v')) then
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
            if(((j+3).lt.long(i)).and.((aatype(i,j).eq.'g'))
     &      .and.(aatype(i,j+1).eq.'f')) then
              if((aatype(i,j+2).eq.'l').and.(aatype(i,j+3).eq.'g')) then
                segtype(i,j+1) = 2
              elseif(aatype(i,j+3).eq.'f') then
                segtype(i,j+1) = 3
              endif              
            endif
            if(((j+3).lt.long(i)).and.((aatype(i,j).eq.'f'))
     &      .and.(aatype(i,j+2).eq.'f').and.(aatype(i,j+3).eq.'g')) then
                segtype(i,j+1) = 3
            endif
          enddo

          do j=1,long(i)
              write(3110+i, *), segtype(i,j)
          enddo

          close(3110+i)
          
          return 
      end



