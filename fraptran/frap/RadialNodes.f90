MODULE RadialNodes_fraptran
    !>@brief
    !> This module contains the subroutines used to determine the heat and relative weight in the radial nodes_fraptran.
    !> Subroutines include radheatsource and weights
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    USE Kinds_fraptran
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE Radheatsource (Nmesh, Nfmesh, Ivoid, Kaxs, CladdingPower, AxNodElevat, Zvoid1, Zvoid2, &
      &                       Fdratio, Radii, VolumeWeightL, VolumeWeightR, Radsrco, Radsrc)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : nradialnodes, naxialnodes
    IMPLICIT NONE
    !>@brief
    !> Subroutine for calculating radial distribution of heat source in pellet and cladding
    !>@author
    !> LO Jernkvist
    !>@date
    !> 2014-10-24
    !
    ! Input:
    !
    ! Nmesh         ! Number of radial nodes in pellet and cladding   [-]
    ! Nfmesh        ! Number of radial nodes in pellet                [-]
    ! Ivoid         ! Flag for central void (annulus) in fuel pellet  [-]
                                                ! =0 No central void
                                                ! =1 Gas filled central void
                                                ! =2 Central void filled with instrument
    ! Kaxs          ! Axial segment under consideration               [-]
    ! CladdingPower    ! Relative power in the cladding                  [-]
    ! AxNodElevat      ! Axial elevation of axial segment               [ft]
    ! Zvoid1           ! Lower end position of central void             [ft]
    ! Zvoid2           ! Upper end position of central void             [ft]
    ! Fdratio          ! Fuel pellet density in deformed state           [-]
                                                ! relative to as-fabricated state
    ! Radii         - Radial node positions                          [ft]
    ! VolumeWeightL - Left volume weight                          [ft**2]
    ! VolumeWeightR - Right volume weight                         [ft**2]
    ! Radsrco       - Radial power distribution                       [-]
    !
    ! Output
    !
    ! Radsrc        - Volume source weight for all nodes          [ft**2]
    !
    INTEGER(ipk), INTENT(IN) :: Nmesh, Nfmesh, Ivoid, Kaxs
    INTEGER(ipk) :: I
    REAL(r8k), INTENT(IN) :: CladdingPower, AxNodElevat, Zvoid1, Zvoid2, fdratio
    REAL(r8k) :: dA, dAsum, Powsum, Srcold, Srcnew, Volwtl, Volwtr, Volsrc 
    REAL(r8k), DIMENSION(:), INTENT(IN) :: Radii
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: VolumeWeightL, VolumeWeightR, Radsrco
    REAL(r8k), DIMENSION(:,:), INTENT(OUT) :: Radsrc
    REAL(r8k), DIMENSION(nradialnodes) :: Srctemp
    
    ! Initialize:
    dAsum  = 0.0_r8k
    Powsum = 0.0_r8k
    Srcold = 0.0_r8k
    Radsrc = 0.0_r8k
    Srctemp = 0.0_r8k

    ! Take average of neighbouring nodes in fuel pellet.
    ! Distribution in cladding is uniform (CladdingPower), but the radial deformation
    ! of the fuel pellet has to be compensated for. This is done by the fuel density
    ! ratio in deformed/undeformed state:
    DO I = 1, (Nmesh - 1)
        
        Srctemp(I) = 0.5_r8k * (Radsrco(Kaxs,I) + Radsrco(Kaxs,I+1))
        
        IF (I > Nfmesh) Srctemp(I) = CladdingPower / Fdratio
         
    ENDDO    
    
    ! Normalize fuel pellet distribution:
    DO I = 1, (Nfmesh - 1)

        dA = Radii(I+1) ** 2 - Radii(I) ** 2
        dAsum = dAsum + dA
        Powsum = Powsum + dA * Srctemp(I)
        
    ENDDO

    Srctemp(1:(Nfmesh-1)) = Srctemp(1:(Nfmesh-1)) * dAsum / Powsum

    DO I = 1, (Nmesh - 1)
        
        Srcnew = Srctemp(I)
        Volwtl = VolumeWeightL(I,Kaxs)
        Volwtr = VolumeWeightR(I,Kaxs)
        
        Volsrc = Srcold * Volwtl + Srcnew * Volwtr
        
        ! LOJ: Corrections
        ! IF (I >= Nfmesh ) Volsrc = Srcnew * (Volwtl + Volwtr)
        IF (I == Nfmesh) Volsrc = Srcold * Volwtl
        IF (I == Nfmesh+1) Volsrc = Srcnew * Volwtr
        IF (I >  Nfmesh+1) Volsrc = Srcnew * (Volwtl + Volwtr)
        ! LOJ: End
        
        Srcold = Srcnew
        
        ! Radial source array for use in subroutines HT1SST and HT1TDP_fraptran:
        Radsrc(Kaxs,I) = Volsrc

    END DO
    
    ! Outermost cladding node:
    ! LOJ: Correction
    ! Radsrc(Kaxs,Nmesh) = Srcold * VolumeWeightL(Nmesh-1,Kaxs)
    Radsrc(Kaxs,Nmesh) = Srcold * VolumeWeightL(Nmesh,Kaxs)
    ! LOJ: End
    
    ! Modify source array, if a central void exists:
    IF ((Ivoid > 0) .AND. (AxNodElevat >= Zvoid1) .AND. (AxNodElevat <= Zvoid2)) THEN
        
        Radsrc(Kaxs,1) = 0.0_r8k
        Radsrc(Kaxs,2) = Srctemp(2) * VolumeWeightR(2,Kaxs)
        
    END IF
    
    END SUBROUTINE Radheatsource
    !
    !
    !
    SUBROUTINE Weights (Nmesh, Gapmin, Radii, VolumeWeightL,  VolumeWeightR, AreaWeight)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : nradialnodes
    USE conversions_fraptran, ONLY : pi
    IMPLICIT NONE
    !>@brief
    !> Subroutine for calculating volume and area weights used in thermal analyses.
    !>@author
    !> LO Jernkvist
    !>@date
    !> 2014-10-23
    !
    ! Input
    !
    ! Nmesh         - Number of radial nodes in pellet and cladding [-]
    ! Gapmin        - Minimum pellet-clad gap size [ft]
    ! Radii         - Radial node positions [ft]
    !
    ! Output:
    !
    ! VolumeWeightL - Left volume weight [ft**2]
    ! VolumeWeightR - Right volume weight [ft**2]
    ! AreaWeight    - Area weight [-]
    !
    INTEGER(ipk), INTENT(IN) :: Nmesh
    INTEGER(ipk) :: I
    REAL(r8k), INTENT(IN) :: Gapmin
    REAL(r8k) :: dR, dum1, dum2, dum3
    REAL(r8k), DIMENSION(:), INTENT(IN) :: Radii
    REAL(r8k), DIMENSION(:), INTENT(OUT) :: VolumeWeightL, VolumeWeightR, AreaWeight

    VolumeWeightL = 0.0_r8k
    VolumeWeightR = 0.0_r8k
    AreaWeight = 0.0_r8k

    DO I = 1, (Nmesh - 1)

        dR = MAX( Gapmin, Radii(I+1) - Radii(I) )

        dum1 = dR * 0.25_r8k
        dum2 = dum1 + Radii(I)
        dum3 = Pi * dR

        ! Right volume weight:
        VolumeWeightR(I) = dum3 * dum2
        dum2 = dum2 + dum1

        ! Surface area weight:
        AreaWeight(I) = 2.0_r8k * Pi * dum2/dR
        dum2 = dum2 + dum1
     
        ! Left volume weight:
        ! LOJ: This was a serious error in the original code
        ! VolumeWeightL(I) = dum3 * dum2
        VolumeWeightL(I+1) = dum3 * dum2
        ! LOJ: End

    END DO
    
    END SUBROUTINE Weights
    !
END MODULE RadialNodes_fraptran













