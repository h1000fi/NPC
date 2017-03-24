      subroutine monomer_definitions

      use mparameters_monomer
      use mparameters   

      implicit none

      include 'mpif.h'
      include 'MPI.h'

      N_poorsol = 4 ! number of different kais
      N_monomer = 8 

      ALLOCATE (st_matrix(N_poorsol, N_poorsol)) ! interaction between monomer types in fraction of st, scaled by st-scale during running....
      ALLOCATE (zpol(N_monomer))    ! charge of monomer segment: 1: base, -1: acid, 0:neutral
      ALLOCATE (hydroph(N_monomer)) ! 0: hydrophilic, 1 < x < N_poorsol, type of poor solvent
      ALLOCATE (pKa(N_monomer), Ka(N_monomer), K0(N_monomer))
      ALLOCATE (henergy(N_poorsol))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      st_matrix(:,:)= 0.0

      st_matrix(1,1)= 0.0 
      st_matrix(2,2)= 0.0
      st_matrix(3,3)= kaiHO
      st_matrix(4,4)= kaiHI

      st_matrix(1,2)= 0
      st_matrix(2,1)= 0
      st_matrix(1,3)= kaiHO 
      st_matrix(3,1)= kaiHO 
      st_matrix(2,3)= kaiHO 
      st_matrix(3,2)= kaiHO
      st_matrix(1,4)= 0
      st_matrix(4,1)= 0
      st_matrix(2,4)= 0
      st_matrix(4,2)= 0
      st_matrix(3,4)= 0
      st_matrix(4,3)= 0

      henergy(1) =  pairst !pairst !st_matrix(1,1)
      henergy(2) =  pairst
      henergy(3) =  kaiHO !kaiHI !st_matrix(1,2)
      henergy(4) =  kaiHI

! Segment type 1 for NPC, positive base, hydrophilic

      zpol(1) = 1
      hydroph(1) = 4
      pKa(1) = 11.0

! Segment type 2 for NPC, negative , hydrophilic

      zpol(2) = -1
      hydroph(2) = 4
      pKa(2) = 5.0

! Segment type 3 for NPC, neutral , hydrophilic

      zpol(3) = 0
      hydroph(3) = 3
      pKa(3) = 1 ! set any number if zpol = 0...

! Segment type 4 for NPC , neutral, hydrophobic, 1

      zpol(4) = 0
      hydroph(4) = 1
      pKa(4) = 1 ! set any number if zpol = 0....


! Segment type 5 for NPC = Histidine

      zpol(5) = 1
      hydroph(5) = 4
      pKa(5) = 6.08 ! set any number if zpol = 0....

! Segment type 6 for NPC = Cysteamine

      zpol(6) = -1
      hydroph(6) = 4
      pKa(6) = 8.3 ! set any number if zpol = 0....

! Segment type 7 for NPC = dummyFG

      zpol(7) = 0
      hydroph(7) = 3
      pKa(7) = 8.3 ! set any number if zpol = 0....

! Segment type 8 for NPC = FG2, nsp1

      zpol(8) = 0
      hydroph(8) = 2
      pKa(8) = 8.3 ! set any number if zpol = 0....

      end

