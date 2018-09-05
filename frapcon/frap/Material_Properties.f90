MODULE Material_Properties
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : icm, imox, ounit, idxgas
    USE UraniumDioxide
    USE MOX
    USE Zirconium
    USE ZirconiumDioxide
    USE Gas
    USE Crud
    USE PlenumSpring
    IMPLICIT NONE
    !>@brief
    !> This module calls thermal and mechanical properties for the following materials:
    !> 1) Fuels (imox) : UO2, MOX
    !> 2) Gas (idxgas): He, Ar, Kr, Xe, H2, N2, Air, H2O
    !> 3) Cladding (icm): Zirc-2, Zirc-4, M5, ZIRLO, Optimized ZIRLO
    !> 4) Oxide (icm): ZrO2
    !> 5) Crud
    !> 6) Spring
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2013
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (Material, Property, Temperature, Burnup, Burnup_prev, Power) RESULT (mat_prop)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calls all material properties based on the material being
    !> modeled and the property needed
    !>@author
    !> Ian Porter, NRC
    !
    ! Input
    !
    ! matTyp      - Material type. Currently modeled: 'FUEL', 'GAS', 'CLAD', 'OXIDE', 'CRUD', 'SPRING'
    ! imox        - Fuel Type
    ! icm         - Clad Type
    ! Property    - Material property to be calculated
    ! Temperature - Material temperature
    !
    ! Optional
    !
    ! Burnup      - burnup to end of timestep (End Of Step)
    ! Burnup_prev - burnup to end of last timestep (Beginning Of Step)
    ! Power       - power
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    REAL(r8k) :: burn, burn_prev, porosity, powr
    REAL(r8k), INTENT(IN) :: Temperature
    REAL(r8k), INTENT(IN), OPTIONAL :: Burnup, Burnup_prev, Power
    CHARACTER(LEN=*), INTENT(IN) :: Material, Property

    ! Only assign values if at least one of the optional arguments are present (used for fuel parameters)
    IF (PRESENT(Burnup)) THEN
        burn = Burnup
    ELSE
        burn = 0.0_r8k
    END IF
    IF (PRESENT(Burnup_prev)) THEN
        burn_prev = Burnup_prev
    ELSE
        burn_prev = 0.0_r8k
    END IF        
    IF (PRESENT(Power)) THEN
        powr = Power
    ELSE
        powr = 0.0_r8k
    END IF
    !
    SELECT CASE (Material)
    CASE ('FUEL')
        ! ******************************
        ! *   Fuel Properties Called   *
        ! ******************************
        SELECT CASE (imox)
        CASE (0)
            ! UO2 Fuel
            mat_prop = MatPropUO2 (Property, Temperature, burn, burn_prev, powr)
        CASE (1, 2)
            ! MOX Fuel
            mat_prop = MatPropMOX (Property, Temperature, burn, burn_prev, powr)
        CASE DEFAULT
            WRITE (0, 100) imox
            WRITE (ounit, 100) imox
100         FORMAT ('Warning: Wrong fuel type selected. imox = ',i2,'. Will default to UO2 values.')
            imox = 0
            mat_prop = MatPropUO2 (Property, Temperature, burn)
        END SELECT
        !
    CASE ('GAS')
        ! ******************************
        ! * Gas-Gap Properties Called  *
        ! ******************************
        SELECT CASE (idxgas)
        CASE (1, 2, 3, 4, 5, 6)
            mat_prop = MatPropGas (Property, Temperature)
        CASE DEFAULT
            WRITE (0, 101) idxgas
            WRITE (ounit, 101) idxgas
101         FORMAT ('Warning: Wrong Gas type selected. idxgas = ',i2,'. Program will default to He Properties.')
            idxgas = 1
            mat_prop = MatPropGas (Property, Temperature)
        END SELECT
        !
    CASE ('CLAD')
        ! ******************************
        ! * Cladding Properties Called *
        ! ******************************
        SELECT CASE (icm)
            ! Selection based on cladding type modeled
        CASE (2, 4, 5, 6, 7)
            ! 2 - Zircaloy-2
            ! 4 - Zircaloy-4
            ! 5 - M5
            ! 6 - ZIRLO(TM)
            ! 7 - Optimized ZIRLO(TM)
            mat_prop = MatPropZirc (Property, Temperature)
        CASE DEFAULT
            WRITE (0, 102) icm
            WRITE (ounit, 102) icm
102         FORMAT ('Warning: Wrong Cladding Type Selected. icm = ',i2,'. Program will default to Zirc-4 Properties.')
            icm = 4
            mat_prop = MatPropZirc (Property, Temperature)
        END SELECT
    CASE ('OXIDE')
        ! ****************************
        ! * Oxide Properties Called  *
        ! ****************************
        SELECT CASE (icm)
        CASE (2, 4, 5, 6, 7)
            ! ZrO2
            mat_prop = MatPropZrO2 (Property, Temperature)
        CASE DEFAULT
            WRITE (0, 103) icm
            WRITE (ounit, 103) icm
103         FORMAT ('Warning: Wrong Oxide Type Selected. icm = ',i2,'. Program will default to Zirc-4 Properties.')
            icm = 4
            ! Defaults to ZrO2
            mat_prop = MatPropZrO2 (Property, Temperature)
        END SELECT
    CASE ('CRUD')
        ! **************************
        ! * Crud Properties Called *
        ! **************************
        mat_prop = MatPropCrud (Property, Temperature)
    CASE ('SPRING')
        ! ****************************
        ! * Spring Properties Called *
        ! ****************************
        mat_prop = MatPropSpring (Property, Temperature)
    CASE DEFAULT
        ! Wrong material selected
        WRITE (0, 104) Material
        WRITE (ounit, 104) Material
104     FORMAT ('Error: Wrong Material called in Subroutine: MatProp.',/,'Material = ',a20,/, &
          &     'Program execution stopped.')
        ERROR STOP 'Error: Wrong material called in Subroutine: MatProp'
    END SELECT
    !
    END FUNCTION MatProp
    !
END MODULE Material_Properties

