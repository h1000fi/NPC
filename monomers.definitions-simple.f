      subroutine monomer_definitions

      use mparameters_monomer
      use mparameters   

      implicit none

      include 'mpif.h'
      include 'MPI.h'
      integer i,j

      N_poorsol = 3 ! number of different kais
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

!      st_matrix(:,:)= 0.0

      st_matrix(1,1)= 0.0
      st_matrix(2,2)= kaibeta
      st_matrix(3,3)= kaiHO
!      st_matrix(4,4)= kaiHI

      st_matrix(1,2)= 0 !kaiHI
      st_matrix(2,1)= 0 !kaiHI
      st_matrix(1,3)= kaiHO 
      st_matrix(3,1)= kaiHO
      st_matrix(2,3)= 0 !kaiHI
      st_matrix(3,2)= 0 !kaiHI

!      st_matrix(1,4)= kaiHI
!      st_matrix(4,1)= kaiHI
!      st_matrix(2,4)= kaiHI
!      st_matrix(4,2)= kaiHI
!      st_matrix(3,4)= kaiHI
!      st_matrix(4,3)= kaiHI

      henergy(1) =  0 !pairst !pairst !st_matrix(1,1)
      henergy(2) =  0
      henergy(3) =  0 !kaiHO !kaiHI !st_matrix(1,2)
!      henergy(4) =  0

! set any number if zpol = 0....

! F 1: single F, 2: FxF, 3: HF (H is hydrophobic)

      zpol(1) = 0
      hydroph(1) = 1
      pKa(1) = 7

      zpol(2) = 0
      hydroph(2) = 1
      pKa(2) = 7

      zpol(3) = 0
      hydroph(3) = 1
      pKa(3) = 7


! hydrophilic beta spacers: Q,N,T

      zpol(4) = 0
      hydroph(4) = 2
      pKa(4) = 7

! non-F hydrophobic spacers: A,L,W,I,Y

      zpol(5) = 0
      hydroph(5) = 3
      pKa(5) = 7

! neutral hydrophilic non-beta spacers: G,M,S,P,V

      zpol(6) = 0
      hydroph(6) = 0 !4
      pKa(6) = 7


! base: K, R

      zpol(7) = 1
      hydroph(7) = 0 !4
      pKa(7) = 11

! acid: D, E

      zpol(8) = -1
      hydroph(8) = 0 !4
      pKa(8) = 4

! weak acid: C

      zpol(9) = -1
      hydroph(9) = 0 !4
      pKa(9) = 8.3 

! weak base: H

      zpol(10) = 1
      hydroph(10) = 0 !4
      pKa(10) = 6.08

      end

