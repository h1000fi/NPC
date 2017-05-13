      subroutine monomer_definitions

      use mparameters_monomer
      use mparameters   

      implicit none

      include 'mpif.h'
      include 'MPI.h'
      integer i,j

      N_poorsol = 7 ! number of different kais
      N_monomer = 10 

      ALLOCATE (st_matrix(N_poorsol, N_poorsol)) ! interaction between monomer types in fraction of st, scaled by st-scale during running....
      ALLOCATE (zpol(N_monomer))    ! charge of monomer segment: 1: base, -1: acid, 0:neutral
      ALLOCATE (hydroph(N_monomer)) ! 0: hydrophilic, 1 < x < N_poorsol, type of poor solvent
      ALLOCATE (pKa(N_monomer), Ka(N_monomer), K0(N_monomer))
      ALLOCATE (henergy(N_poorsol))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      do i = 1, N_poorsol
        do j = 1, N_poorsol
          st_matrix(i,j) = 0.0
        enddo
      enddo

      st_matrix(:,:)= 0.0

      st_matrix(1,1)= 0.0
      st_matrix(2,2)= 0.0
      st_matrix(3,3)= 0.0
      st_matrix(4,4)= 0.0
      st_matrix(5,5)= kaiHO
      st_matrix(6,6)= kaiC
      st_matrix(7,7)= kaiHI

      st_matrix(1,4)= kaiHI
      st_matrix(4,1)= kaiHI
      st_matrix(2,4)= kaiHI
      st_matrix(4,2)= kaiHI
      st_matrix(3,4)= kaiHI
      st_matrix(4,3)= kaiHI

      st_matrix(1,5)= kaiHO 
      st_matrix(5,1)= kaiHO
      st_matrix(2,5)= kaiHO
      st_matrix(5,2)= kaiHO
      st_matrix(3,5)= kaiHO
      st_matrix(5,3)= kaiHO 
      st_matrix(1,7)= kaiHI
      st_matrix(7,1)= kaiHI
      st_matrix(2,7)= kaiHI
      st_matrix(7,2)= kaiHI
      st_matrix(3,7)= kaiHI
      st_matrix(7,3)= kaiHI
      st_matrix(4,7)= kaiHI
      st_matrix(7,4)= kaiHI
      st_matrix(5,7)= kaiHI
      st_matrix(7,5)= kaiHI

      henergy(1) =  pairst !pairst !st_matrix(1,1)
      henergy(2) =  0
      henergy(3) =  kaiHO !kaiHI !st_matrix(1,2)
      henergy(4) =  0
      henergy(5) =  0

! set any number if zpol = 0....

! F 1: single F, 2: FxF, 3: HF (H is hydrophobic)

      zpol(1) = 0
      hydroph(1) = 1
      pKa(1) = 7

      zpol(2) = 0
      hydroph(2) = 2
      pKa(2) = 7

      zpol(3) = 0
      hydroph(3) = 3
      pKa(3) = 7


! hydrophilic beta spacers: G,Q,N,T

      zpol(4) = 0
      hydroph(4) = 4
      pKa(4) = 7

! non-F hydrophobic spacers: A,L,W,I,Y

      zpol(5) = 0
      hydroph(5) = 5
      pKa(5) = 7

! neutral hydrophilic non-beta spacers: M,S,P,V

      zpol(6) = 0
      hydroph(6) = 7
      pKa(6) = 7


! base: K, R

      zpol(7) = 1
      hydroph(7) = 6
      pKa(7) = 11

! acid: D, E

      zpol(8) = -1
      hydroph(8) = 6
      pKa(8) = 4

! weak acid: C

      zpol(9) = -1
      hydroph(9) = 7
      pKa(9) = 8.3 

! weak base: H

      zpol(10) = 1
      hydroph(10) = 7
      pKa(10) = 6.08

      end

