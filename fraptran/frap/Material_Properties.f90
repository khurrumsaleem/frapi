MODULE Material_Properties
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran, ONLY : CladType, imox, ounit, GasFraction
    USE uraniumdioxide_fraptran
    USE mox_fraptran
    USE zirconium_fraptran_fraptran
    USE zirconium_fraptran_fraptranDioxide
    USE NCGases
    USE Crud_Data
    USE plenumspring_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module calls thermal and mechanical properties for the following materials:
    !> 1) Fuels (imox) : UO2, MOX
    !> 2) Gas (idxgas): He, Ar, Kr, Xe, H2, N2, Air, H2O
    !> 3) Cladding (CladType): Zirc-2, Zirc-4, M5, ZIRLO, Optimized ZIRLO
    !> 4) Oxide (CladType): ZrO2
    !> 5) Crud
    !> 6) Spring
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 12/23/2015
    !
    PRIVATE
    PUBLIC :: MatProperty
    
    TYPE (UraniumDiox) :: UO2
    Type (MixedOxide) :: UPuO2
    TYPE (ZircaloyDioxide) :: ZrO2
    TYPE (Zircaloy) :: Zirc
    TYPE (NonCondGases) :: NCGas
    TYPE (CrudProperties) :: Crud
    TYPE (SpringProperties) :: Spring
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProperty (Material, Property, Temperature, Burnup, OMRatio, Fraction_TD, Gadolinia, Pu, Fraction_Melt, &
      &                             Tmelt, HeatofFusion, Flux, Fluence, ColdWork, OxygenConcen, Oxide, Pressure, &
      &                             Width, GasComposition) RESULT (mat_prop)
    USE Kinds
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calls all material properties based on the material being
    !> modeled and the property needed
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/18/2016
    !
    ! Input
    !
    ! Material       - Material type. Currently modeled: 'FUEL', 'GAS', 'CLAD', 'OXIDE', 'CRUD', 'SPRING'
    ! imox           - Fuel Type
    ! CladType       - Clad Type
    ! Property       - Material property to be calculated
    ! Temperature    - Temperature of node
    !
    ! Optional Input
    ! (Fuel)
    ! Burnup         - Burnup to end of timestep (End Of Step)
    ! OMRatio        - As-fabricated Oxygen to Metal ratio
    ! Fraction_TD    - Fraction of theoretical density
    ! Gadolinia      - Gadolinia concentration
    ! Pu             - Weight % PuO2
    ! Fraction_Melt  - Fraction of molten fuel
    ! Tmelt          - Melting temperature
    ! HeatofFusion   -
    ! (Clad)
    ! Flux           - Flux
    ! Fluence        - Fluence
    ! ColdWork       - Cold work
    ! OxygenConcen   - 
    ! Oxide          - Oxide thickness
    ! (Gas)
    ! Pressure       - Rod internal pressure
    ! Width          - Gap width
    ! GasComposition - Rod gas composition (molar ratios)
    !
    ! Output
    !
    ! mat_prop       - Calculated material property value
    !
    INTEGER(ipk) :: idxgas
    REAL(r8k), INTENT(IN) :: Temperature
    ! Fuel input options
    REAL(r8k), INTENT(IN), OPTIONAL :: Burnup, OMRatio, Fraction_TD, Gadolinia, Pu, Fraction_Melt, Tmelt, HeatofFusion
    ! Cladding input options
    REAL(r8k), INTENT(IN), OPTIONAL :: Flux, Fluence, ColdWork, OxygenConcen, Oxide
    ! Gas input options
    REAL(r8k), INTENT(IN), OPTIONAL :: Pressure, Width
    REAL(r8k), DIMENSION(:), INTENT(IN), OPTIONAL :: GasComposition
    CHARACTER(LEN=*), INTENT(IN) :: Material, Property
    !
    SELECT CASE (Material)
    CASE ('FUEL')
        ! ******************************
        ! *   Fuel Properties Called   *
        ! ******************************
        SELECT CASE (imox)
        CASE (0) ! UO2 Fuel
            UO2%Temp = Temperature
            IF (PRESENT(Fraction_TD)) UO2%fraDen = Fraction_TD
            IF (PRESENT(OMRatio)) UO2%OMRatio = OMRatio
            IF (PRESENT(Burnup)) UO2%Burnup = Burnup
            IF (PRESENT(Gadolinia)) UO2%Gadoln = Gadolinia
            IF (PRESENT(Fraction_Melt)) UO2%facmot = Fraction_Melt
            IF (PRESENT(HeatofFusion)) UO2%hefus = HeatofFusion
            IF (PRESENT(Tmelt)) UO2%Tmelt = Tmelt
            
            ! Get property
            mat_prop = UO2%MatProp (property)
        CASE (1, 2) ! MOX Fuel
            UPuO2%Temp = Temperature
            IF (PRESENT(Fraction_TD)) UPuO2%fraDen = Fraction_TD
            IF (PRESENT(OMRatio)) UPuO2%OMRatio = OMRatio
            IF (PRESENT(Burnup)) UPuO2%Burnup = Burnup
            IF (PRESENT(Gadolinia)) UPuO2%Gadoln = Gadolinia
            IF (PRESENT(Pu)) UPuO2%wtPu = Pu
            IF (PRESENT(Fraction_Melt)) UPuO2%facmot = Fraction_Melt
            IF (PRESENT(HeatofFusion)) UPuO2%hefus = HeatofFusion
            IF (PRESENT(Tmelt)) UPuO2%Tmelt = Tmelt
            
            ! Get property
            mat_prop = UPuO2%MatProp (property)
        CASE DEFAULT
            WRITE (0, 100) imox
            WRITE (ounit, 100) imox
100         FORMAT ('Error: Wrong fuel type selected. imox = ',i3,'. Code execution terminated in Subroutine: MatProperty.')
            ERROR STOP 'Wrong fuel properties called into Subroutine: MatProperty'
        END SELECT
        !
    CASE ('GAS')
        ! ******************************
        ! * Gas-Gap Properties Called  *
        ! ******************************
        ! Note: FRAPTRAN has no user-defined gas-type indicator flag.
        ! Hardwired idxgas = 1 as long as only the default gases are used (gases 1 to 7)
        
        ! Load values
        NCGas%Temp = Temperature
        IF (PRESENT(GasComposition)) THEN
            NCGas%Composition(1:ngases) = GasComposition
        ELSE
            NCGas%Composition(1:ngases) = GasFraction
        END IF
        IF (PRESENT(Pressure)) NCGas%Press = Pressure
        IF (PRESENT(Width)) NCGas%Width = Width
        
        IF (ABS(1.0_r8k - SUM(GasFraction(1:ngases))) < 0.001_r8k) THEN
            idxgas = 1
        ELSE
            ! Error or user supplied gas. If user supplied gas, create own CASE for your gas type
            ! Otherwise, gas will be reset to Helium
            GasFraction(1) = 1.0_r8k
            GasFraction(2:ngases) = 0.0_r8k
            idxgas = 1
        END IF
        SELECT CASE (idxgas)
        CASE (1, 2, 3, 4, 5, 6)
            mat_prop = NCGas%MatProp (property)
        CASE DEFAULT
            WRITE (0, 101) idxgas
            WRITE (ounit, 101) idxgas
101         FORMAT ('Warning: Wrong Gas type selected. idxgas = ',i3,'. Code execution terminated in Subroutine: MatProperty.')
            ERROR STOP 'Wrong gas properties called into Subroutine: MatProperty'
        END SELECT
        !
    CASE ('CLAD')
        ! ******************************
        ! * Cladding Properties Called *
        ! ******************************
        SELECT CASE (CladType)
            ! Selection based on cladding type modeled
        CASE (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
            ! 1  - Generic Zircaloy properties
            ! 2  - Zircaloy-2
            ! 3  - Optimized ZIRLO(TM)
            ! 4  - Zircaloy-4
            ! 5  - ZIRLO(TM)
            ! 6  - Zr-1%Nb; RRC-KI Zr-1%Nb properties
            ! 7  - M5
            ! 8  - E-110; RRC-KI Zr-1%Nb properties
            ! 9  - Low-tin Zry
            ! 10 - Cold-worked annealed SRA Zry
            ! 11 - Fully annealed Zry
            Zirc%Temp = Temperature
            IF (PRESENT(Flux)) Zirc%Flux = Flux
            IF (PRESENT(ColdWork)) Zirc%cwkf = ColdWork
            IF (PRESENT(OxygenConcen)) Zirc%deloxy = OxygenConcen
            IF (PRESENT(Oxide)) Zirc%Oxide = Oxide
            IF (PRESENT(Fluence)) Zirc%fnck = Fluence
            
            ! Get property
            mat_prop = Zirc%MatProp (property)
        CASE DEFAULT
            WRITE (0, 102) CladType
            WRITE (ounit, 102) CladType
102         FORMAT ('Error: Wrong Cladding Type Selected. CladType = ',i3,'. Code execution terminated in Subroutine: MatProperty.')
            ERROR STOP 'Wrong cladding properties called into Subroutine: MatProperty'
        END SELECT
    CASE ('OXIDE')
        ! ****************************
        ! * Oxide Properties Called  *
        ! ****************************
        SELECT CASE (CladType)
        CASE (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
            ! ZrO2
            ZrO2%Temp = Temperature
            mat_prop = ZrO2%MatPropZrO2 (Property)
        CASE DEFAULT
            WRITE (0, 103) CladType
            WRITE (ounit, 103) CladType
103         FORMAT ('Error: Wrong Oxide Type Selected. CladType = ',i3,'. Code execution terminated in Subroutine: MatProperty.')
            ERROR STOP 'Wrong oxide properties called into Subroutine: MatProperty'
        END SELECT
    CASE ('CRUD')
        Crud%Temp = Temperature
        ! **************************
        ! * Crud Properties Called *
        ! **************************
        mat_prop = Crud%MatProp (Property)
    CASE ('SPRING')
        Spring%Temp = Temperature
        ! ****************************
        ! * Spring Properties Called *
        ! ****************************
        mat_prop = Spring%MatProp (Property)
    CASE DEFAULT
        ! Wrong material selected
        WRITE (0, 104) Material
        WRITE (ounit, 104) Material
104     FORMAT ('Error: Wrong Material called in Subroutine: MatProperty.',/,'Material = ',a20,/, &
          &     'Program execution stopped.')
        ERROR STOP 'Program execution stopped due to invalid material ID sent to MatProperty'
    END SELECT
    
    END FUNCTION MatProperty
    !
END MODULE Material_Properties












