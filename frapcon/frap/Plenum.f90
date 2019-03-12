MODULE Plenum_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Gas_frapcon
    USE Material_Properties_frapcon
    USE CladDeformation_frapcon
    USE CoolantData_frapcon
    USE CorrosionData_frapcon
    USE Temperature_frapcon
    USE Refabrication_frapcon, ONLY : jmax, irefab
    use variables_frapcon, only : firstcall
    IMPLICIT NONE
    !>@brief
    !> This module contains the calculations needed to estimate the plenum temperature_frapcon
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/28/2015
    !
    ! Flag to identify if the first call to subroutine plnt2
    !LOGICAL, PRIVATE :: FirstCall = .TRUE.
    ! Flag for printing a warning message for prandtl calculation
    !LOGICAL, PRIVATE :: PrandtlWarning = .TRUE.
    LOGICAL, PRIVATE :: PrandtlWarning = .FALSE.   ! Supressing warning messages
    !
    CONTAINS
    !
    SUBROUTINE plnt
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : qmpy, deltaz, CladIrradGrowStrn, BulkCoolantTemp, na, eps, crdtt, dci, &
      &                   dco, PelAveTemp, CladAveTemp, p2, qend, qaxnorm, de, HotNodLength, &
      &                   tpa, visc, hpv, go, it, totl, tplen, amfair, amffg, coldwk, cpl, &
      &                   fa, hpl, hfll, hflmdb, hflmp, Nu, press, j, nt, m, jmin, jmax, Spring
    USE Material_Properties_frapcon, ONLY : MatProp
    USE Gas_frapcon
    IMPLICIT NONE
    !>@brief
    !> Subroutine plnt is called from frpcon and computes the plenum temperature and hot state plenum volume
    !>@author
    !> plnt was coded by g a berna
    !> Updated by Ian Porter, NRC
    !>@date
    !> January, 1978 (g a berna)
    !> June, 2015 (IP)
    !
    ! Input
    !
    ! CladIrradGrowStrn - axial strain due to cladding irradiation growth
    ! coldwk            - cladding cold work
    ! cpl               - cold plenum length (in)
    ! crdtt             - crud thickness (mils)
    ! deltaz            - axial node lengths (ft)
    ! dci(nt)           - cold state cladding inside diameter (in) As-Fabricated
    ! dco(nt)           - cold state cladding outside diameter (in) As-Fabricated
    ! eps               - cladding strains (m/m)
    ! fa                - Peak-to-average power ratio (fa = 1 if iq = 0)
    ! go                - coolant mass flux
    ! hfll              - maximum hot fuel stack height (in)
    ! it                - power time step index
    ! nt                - Node at top of fuel stack
    ! na                - # of axial nodes
    ! press             - rod internal gas pressure (psia)
    ! pi                - the constant 3.1415926
    ! p2                - system pressure (psia)
    ! qaxnorm(nt,m)     - normalized axial node power
    ! qend(m)           - normalized heat flux at top of fuel stack
    ! qmpy              - rod average heat flux for power-time step (btu/hr-ft**2)
    ! CladAveTemp(nt)   - average cladding temperature of top axial node (F)
    ! BulkCoolantTemp   - bulk coolant temperature at the top of fuel stack, F
    ! totl              - fuel stack height (ft)
    ! PelAveTemp(nt)    - average temperature of the top pellet (F)
    ! Spring%Vcold      - cold volume of spring (in**3)
    !
    ! Output
    !
    ! hflmdb            - dittus-boelter film coefficient (btu/hr-ft**2-F)
    ! hflmp             - forced convection film coefficient (btu/hr-ft**2-F)
    ! hpl               - hot plenum length (in)
    ! hpv               - hot plenum volume (cu.in.)
    ! nu                - nusselt number of plenum gas mixture (unitless)
    ! tpa               - Pellet average temperature, F
    ! Tplen             - plenum temperature (F)
    !
    ! Internal
    !
    ! CladHotIDm - Cladding hot inner diameter, m
    ! Cladthcon  - Cladding thermal conductivity
    ! CladThermR - Cladding thermal strain in radial direction (unitless)
    ! CladThermA - Cladding thermal strain in axial direction (unitless)
    ! CrudOD     - Crud outer diameter, ft
    ! Crudthcon  - Crud thermal conductivity
    ! FuelLenH   - Fuel hot length, m
    ! GasCond    - Gas thermal conductivity (W/m^2*K)
    ! Gr         - Grashof number of plenum gas mixture (unitless)
    ! hciplen    - natural convection heat transfer coefficient from cladding inner surface to the plenum, Btu/(hr*ft^2*F)
    ! HotPlenLen - Hot plenum length, m
    ! hp         - natural convection heat transfer coefficient from fuel to the plenum, Btu/(hr*ft^2*F)
    ! HydroDiam  - hydraulic diameter, ft
    ! OxideOD    - Oxide outer diameter, ft
    ! Oxidethcon - Oxide thermal conductivity    
    ! Pr         - Prandtl number (unitless)
    ! TcladK     - Cladding Temperature, K
    ! Tcool      - Bulk coolant Temperature, F
    ! TcoolK     - Bulk coolant temperature, K
    ! TfuelK     - Pellet average temperature, K
    ! qp         - Heat flow from top of fuel stack (btu/hr)
    ! qspring    - heat generated in spring (btu/hr)
    !
    ! References
    ! (Natural convection HTCs, effective conductivity and plenum temperature)
    ! McAams, William H. Heat Transmission. McGraw-Hill, NY, 1954
    !
    ! Notes:
    ! Subroutine is calculated using British units
    !
    INTEGER(ipk) :: i
    REAL(r8k) :: CladFuelLen, CladPlenLen, TcladK, CladThermR, Elast, Elasts, TplenK, Elmod, &
      &          GasCond, hp, Tcool, TcoolK, Cladthcon, Uc, dhic, Gr, Pr, CladThermA, TFuelK, &
      &          qspring, qp, HydroDiam, VoD2, hciplen, Length, hcifilm, FuelTherm, hfpv, hsv, &
      &          Crudthcon, Springthexp, dhoc, OxideOD, CrudOD, Oxidethcon
    !
    Tcool = BulkCoolantTemp(jmax)
    HydroDiam = de(jmax) * intoft
    ! Calculate fuel temperature at top of fuel stack
    ! Heat Flux * Power Ratio * Normalized Axial Profile * (Pellet Temp - Coolant Temp) + Coolant Temp
    tpa = qend(m) * fa * qaxnorm(jmax-1,m) * (PelAveTemp(jmax-1) - Tcool) + Tcool
    ! On first pass, set Tplen = BulkCoolantTemp(na)
    IF (FirstCall) THEN
        Tplen = Tcool
        FirstCall = .FALSE.
    END IF
    ! Call to plenum occurs before new hot fuel length is set in subroutine gspres
    hfll = SUM(HotNodLength(jmin-1:jmax-1))
    ! Unit conversions
    TcladK = tfk(CladAveTemp(jmax-1)) ! Cladding temperature at plenum not yet calculated, so using temperature below plenum
    TplenK = tfk(Tplen)
    TfuelK = tfk(tpa)
    TcoolK = tfk(Tcool)
    ! Cladding Radial & Axial thermal strain (unitless)
    CladThermR = MatProp ('CLAD', 'THEXP_RADIAL', TcladK)
    CladThermA = MatProp ('CLAD', 'THEXP_AXIAL', TcladK)
    ! Cladding axial growth in plenum (CladIrradGrowStrn(jmax))
    ! Fluence in plenum is not known or calculated.  
    ! Assumption is that it is equal to the values at the node directly under the plenum.
    FastFlux(jmax) = FastFlux(jmax-1)
    FastFluence(jmax) = FastFluence(jmax-1)
    CALL cldgro
    ! Cladding hot length at plenum (from as-fabricated conditions)
    CladPlenLen = cpl * (1.0_r8k + CladThermA + CladIrradGrowStrn(jmax))
    ! Cladding hot length at fueled regions
    CladFuelLen = 0.0_r8k
    DO i = jmin, jmax
        CladFuelLen = CladFuelLen + deltaz(i-1) * fttoin * (1.0_r8k + eps(i-1,2) + CladIrradGrowStrn(i-1))
    END DO
    ! Hot plenum length = Hot Cladding Length at plenum + Hot cladding length in fueled region - Hot fuel length
    hpl = CladPlenLen + CladFuelLen - hfll
    ! Cladding elastic modulus
    Elmod = MatProp ('CLAD', 'YOUNG_MOD', TcladK) * PatoPSI
    ! Elastic cladding radial deformation
    IF (hpl > (2.0_r8k * dci(jmax))) THEN
        ! Hoop stress / Elastic Modulus
        Elasts = ((press * dci(jmax) - p2(it) * dco(jmax)) / (dco(jmax) - dci(jmax))) / Elmod
    ELSE
        Elasts = 0.0_r8k
    END IF
    !
    ! Calculate cladding dimensions
    !
    ! Calculation of plenum heat generated in the spring by gamma radiation, qsp (Btu/hr)
    ! qsp = 3.76 (W/m3 for every W/m2 of rod avg heat flux) * heat flux (btuh/ft2) * spring volume (ft3)
    qspring = (3.76_r8k / mtoft) * qmpy(it) * (Spring%Vcold * in3toft3)
    ! Convert to surface heat flux, qc(j-1) (Btu/hr*ft^2)
    qc(j-1) = qspring / (pi * dco(jmax) * hpl * in2toft2)
    ! Calculate the oxide thickness, crud thickness and cladding temperature drop at the plenum
    ! Note: Subroutine frpcon loops over the axial nodes relating to active fuel only so the plenum
    !       changes must be calculated here, outside of the axial node loop
    CALL Coolant
    CALL Corrosion
    CALL cladrp
    ! Calculate new dimensions of cladding, oxide and crud
    ! Update cladding temperature with new calculation of plenum cladding temperature
    TcladK = tfk(CladAveTemp(jmax))
    ! Cladding Radial thermal strain (unitless) with newly calculated temperature
    CladThermR = MatProp ('CLAD', 'THEXP_RADIAL', TcladK)
    ! Clad Hot ID, As Fabricated diameter * (1.0 + thermal strain + elastic strain)
    dhic = dci(jmax) * intoft * (1.0_r8k + CladThermR + Elasts)
    ! Clad Hot OD
    dhoc = dco(jmax) * intoft * (1.0_r8k + CladThermR + Elasts)
    ! Oxide OD
    OxideOD = dhoc + EOSZrO2Thk(j-1)
    ! Crud OD
    !CrudOD = OxideOD + crdtt(nt) * miltoft
    CrudOD = OxideOD + crdtt(jmax-1) * miltoft
    !
    ! Calculation of hot plenum volume (hpv)
    !
    ! Hot free plenum volume, no spring (hfpv) - right cylinder equation: Pi * radius^2 * Length
    hfpv = hpl * pi * (dhic * fttoin / 2.0_r8k ) ** 2
    ! Hot spring volume = cold spring volume * (1 + 3 * linear expansion)
    Springthexp = MatProp ('SPRING', 'THEXP', TplenK)
    Spring%Vhot = Spring%VCold * (1.0_r8k + 3.0_r8k * Springthexp)
    ! Hot plenum volume = Hot free plenum volume - Hot spring volume
    hpv = hfpv - Spring%Vhot
    ! Calculation of natural convection heat transfer coefficient (hp) at top of fuel stack
    IF (hpv <= 0.0_r8k) THEN ! No available plenum volume
        Tplen = tpa
        hpv = 0.0_r8k
    ELSE ! Availale plenum volume
        ! Gas Thermal Conductivity
        GasCond = MatProp ('GAS', 'THERMCOND', TplenK)
        !
        ! HTC: Natural convection from top of fuel stack to plenum (hp)
        !
        ! Length = 0.9 * diameter (McAdams, 1954)
        Length = 0.9_r8k * dhic * ftom
        ! Calculate Grashof number
        Gr = Grashof (Length, TplenK, TfuelK)
        ! Calculate Prandtl number
        Pr = Prandtl (TplenK)
        ! Calculate Nusselt number and natural convection heat transfer coefficient to the plenum, Btu/(hr*ft^2*F)
        IF (tpa > Tplen) THEN
            ! Fuel is hotter than plenum
            Nu = Nusselt (Gr, Pr, 'HORIZONTAL', 'PLATE')
            hp = MAX(Nu * (GasCond * WmKtoBhftF) / dhic, 1.0E-10_r8k)
        ELSE
            ! Plenum is hotter than fuel. Plenum will lose heat to the fuel 
            ! (although this is not truly fed back into solution due to no axial conduction)
            Nu = Nusselt (Gr, Pr, 'HORIZONTAL', 'PLATE', 'COOLING')
            hp = Min(-Nu * (GasCond * WmKtoBhftF) / dhic, -1.0E-10_r8k)
        END IF
        !
        ! HTC: Natural convection from cladding inner surface to plenum
        !
        Length = hpl * intom
        ! Calculate Grashof number
        Gr = Grashof (Length, TplenK, TcladK)
        ! Calculate Prandtl number
        Pr = Prandtl (TplenK)
        ! Calculate Nusselt number
        Nu = Nusselt (Gr, Pr, 'VERTICAL', 'CYLINDER')
        ! Natural convection heat transfer coefficient to the plenum, Btu/(hr*ft^2*F)
        hciplen = Nu * (GasCond * WmKtoBhftF) / (hpl * intoft)
        hciplen = MAX(hciplen, 1.0E-10_r8k)
        ! Clad outside HTC @ plenum region
        IF (go(it) > 0.0_r8k) THEN
            hflmdb = DittusBoelterHTC (Tcool, go(it), p2(it), HydroDiam)
        ELSE ! User specified the outside cladding temperature. Set artifically high HTC.
            hflmdb = 50000.0_r8k
        END IF
        ! Calculate thermal conductivities. Temperature is assumed equal to that of the clad (due to minimal heat flux)
        ! Crud thermal conductivity
        Crudthcon = MatProp ('CRUD', 'THERMCOND', TcladK)
        ! Oxide thermal conductivity
        Oxidethcon = MatProp ('OXIDE', 'THERMCOND', TcladK)
        ! Cladding thermal conductivity
        Cladthcon = MatProp ('CLAD', 'THERMCOND', TcladK)
        ! ***** NEW MODEL *****
        ! Effective conductivity from plenum to coolant takes into account:
        ! Natural Convection: Plenum Gas & Clad ID
        ! Conduction: Cladding, Oxide, Crud
        ! Convection: To Coolant (Dittus-Boelter film drop coefficient)
        Uc = 1.0_r8k / (1.0_r8k / (dhic * hciplen) + LOG(dhoc / dhic) / (2.0_r8k * Cladthcon) + &
          &  LOG(OxideOD / dhoc) / (2.0_r8k * Oxidethcon) + LOG(CrudOD / OxideOD) / (2.0_r8k * CrudCond) + &
          &  1.0_r8k / (CrudOD * hflmdb))
        ! Calculate Plenum volume / hot clad ID
        VoD2 = hpv * in3toft3 / ((dhic / 2.0_r8k) ** 2)
        ! Calculate new plenum temperature, F
        Tplen = (qspring + Uc * VoD2 * Tcool + tpa * hp * pi * (dhic / 2.0_r8k) ** 2) &
          &   / (Uc * VoD2 + (hp * pi * (dhic / 2.0_r8k) ** 2))
        ! Calculate heat flow from top of fuel stack (Not currently returned)
        qp = hp * 0.785_r8k * dhic ** 2 * (tpa - Tplen)
    END IF
    !
    END SUBROUTINE plnt
    !
    !
    !
    REAL(r8k) FUNCTION Grashof (Length, Tplen, Tfuel)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function calculates the Grashof Number
    !>@author
    !> Ian Porter,NRC
    !>@date
    !> 4/28/2015
    !
    ! Input
    !
    ! Length    - Characteristic length (m)
    ! Tfuel     - Average fuel temperature, K
    ! Tplen     - Plenum temperature, K
    !
    ! Internal
    !
    ! Beta      - Volumetric thermal expansion coefficient of the gas (1/K)
    ! GravitySI - Gravity (9.81 m/s^2)
    ! Rho       - Gas density, kg/m^3
    ! Visc      - Gas viscosity
    !
    ! Output
    !
    ! Grashof   - Grashof number (unitless)
    !
    ! Reference:
    !
    ! Bird, Stewart and Lightfoot, "Transport Phenomena," New York: John Wiley & Sons, Inc., 1960.
    ! W.H. McAdams, "Heat Transmissions," 34th Edition, New York: McGraw-Hill Book Company, Inc., 1954.
    !
    ! Grashoff number calculated using the following formula: (Bird, Stewart and Lightfoot, Transport Phenomena, 1960)
    ! Gr = (Density ^ 2 * Beta * Gravity * Length ^ 3 * (Tfuel - Tplenum)) / (Viscosity ^ 2)
    ! Where:
    ! Beta = 1 / Tplenum (Assuming ideal gas behavior (Bird, Stewart and Lightfoot, Transport Phenomena, 1960)
    ! Length = 0.9 * Cladding Inner Diameter (W.H. McAdams, Heat Transmissions, 1954)
    !
    REAL(r8k), INTENT(IN) :: Length, Tplen, Tfuel
    REAL(r8k) :: Beta, Rho, Visc
    !
    ! Beta (Volumetric thermal expansion coefficient)
    Beta = 1.0_r8k / Tplen
    !
    ! Gas Density
    Rho = MatProp ('GAS', 'DENSITY', Tplen)
    !
    ! Gas Viscosity
    Visc = MatProp ('GAS', 'VISCOSITY', Tplen)
    !
    ! Grashof Number
    Grashof = (Length ** 3) * (Rho ** 2) * Gravity_SI * Beta * ABS(Tfuel - Tplen) / (Visc ** 2)
    !
    ! Check for errors
    IF (Grashof < 0.0_r8k) THEN
        WRITE (0,90) Beta, Rho, Visc, Grashof
        WRITE (ounit,90) Beta, Rho, Visc, Grashof
90      FORMAT ('Error in Grashof calculation. Beta = ',es12.4,/,30x,'Rho = ',es12.4,/,30x, &
          &     'Visc = ',es12.4,/,30x,'Grashof = ',es12.4,'. Execution Stopped')
        ERROR STOP 'Error in Grashof Calculation. Subroutine: Grashof'
    END IF
    !
    END FUNCTION Grashof
    !
    !
    !
    REAL(r8k) FUNCTION Prandtl (Tplen)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function calculates the Prandtl Number
    !>@author
    !> Ian Porter,NRC
    !>@date
    !> 4/28/2015
    !
    ! Input
    !
    ! Tplen     - Plenum temperature, K
    !
    ! Internal
    !
    ! SpecHeat  - Gas specific heat, kg/m^3
    ! Visc      - Gas viscosity
    ! GasCond   - Gas thermal conductivity, W/m*K
    !
    ! Output
    !
    ! Prandtl  - Prandtl number (unitless)
    !
    ! Prandtl number calculated using the following formula:
    ! Pr = (specific heat) * (viscosity) / (thermal conductivity)
    !
    REAL(r8k), INTENT(IN) :: Tplen
    REAL(r8k) :: SpecHeat, Visc, GasCond
    !
    ! Gas specific heat, J/kg*K
    SpecHeat = MatProp ('GAS', 'SPECHEAT', Tplen)
    !
    ! Gas Viscosity, kg/m*s
    Visc = MatProp ('GAS', 'VISCOSITY', Tplen)
    !
    ! Gas Thermal Conductivity, W/m*K
    GasCond = MatProp ('GAS', 'THERMCOND', Tplen)
    !
    ! Prandtl Number
    Prandtl = SpecHeat * Visc / GasCond
    !
    ! Check for errors
    IF (Prandtl <= 0.0_r8k) THEN
        WRITE (0,90) SpecHeat, Visc, GasCond, Prandtl
        WRITE (ounit,90) SpecHeat, Visc, GasCond, Prandtl
90      FORMAT ('Error in Prandtl calculation. SpecHeat = ',es12.4,/,30x,'Visc = ',es12.4,/,30x, &
          &     'GasCond = ',es12.4,/,30x,'Prandtl = ',es12.4,'. Execution Stopped')
        ERROR STOP 'Error in Prandtl Calculation. Subroutine: Prandtl'
    END IF
    !
    END FUNCTION Prandtl
    !
    !
    !
    REAL(r8k) FUNCTION Nusselt (Gr, Pr, Orientation, Geometry, HeatType)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function calculates the Nusselt Number based on the following:
    !> 1) Orientation - vertical or horizontal
    !> 2) Geometry - currently allowed are flat plates and cylinders
    !> 3) HeatType (Optional) - specifies whether the surface is heated or cooled. 
    !>                          By default, the surface is heated
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/23/2015
    !
    ! Input
    !
    ! Pr    - Prandtl number (unitless)
    ! Gr    - Grashof number (unitless)
    ! Orientation        - vertical ('VERTICAL') or horizontal ('HORIZONTAL') surface
    ! Geometry           - Flat plate ('PLATE') or cylinder ('CYLINDER')
    ! HeatType(Optional) - Specifies whether surface is cooled or heated.
    !                       By default (if left blank) surface is heated
    !
    ! Internal
    !
    ! X     - Grashof # multiplied by Prandtl #
    ! C     - fit coefficient
    ! m     - fit coefficient
    ! NuMin - minimum allowable value for Nusselt
    !
    ! Output
    !
    ! Nu    - Nusselt Number (unitless ratio)
    !
    ! Nusselt number calculated using the following formula:
    ! Nu = C * X ^ m
    ! Where X = Gr * Pr
    !
    REAL(r8k), INTENT(IN) :: Gr, Pr
    REAL(r8k) :: C, m, Nu, X
    REAL(r8k), PARAMETER :: onethird = 1.0_r8k / 3.0_r8k
    REAL(r8k), PARAMETER :: NuMin = 0.4_r8k
    CHARACTER(LEN=6) :: Surface
    CHARACTER(LEN=*), INTENT(IN) :: Orientation, Geometry
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: HeatType
    LOGICAL :: OutsideBounds
    !
    OutsideBounds = .FALSE.
    ! Determine whether surface is heated or cooled
    IF (PRESENT(HeatType)) THEN
        IF (HeatType == 'COOLING') THEN
            Surface = 'COOLED'
        ELSE IF (HeatType == 'HEATING') THEN
            Surface = 'HEATED'
        ELSE
            WRITE (0,90) HeatType
            WRITE (ounit,90) HeatType
90          FORMAT ('Error: Wrong HeatType input to function Prandtl.',/,' HeatType = ',a10,/, &
              &     'Execution Stopped')
            STOP
        END IF
    ELSE
        Surface = 'HEATED'
    END IF
    ! Calculate X (where X = Gr * Pr)
    X = Gr * Pr
    ! Check for errors in Gr or Pr
    IF (X < 0.0_r8k) THEN
        WRITE (0,91) Gr, Pr
        WRITE (ounit,91) Gr, Pr
91      FORMAT ('Error in function Nusselt. Bad input for Grashof or Prandtl Numbers.',/, &
          &     'Gr = ',es12.4,' Pr = ',es12.4)
        STOP
    END IF
    ! Check Orientation and Geometry
    IF ((Orientation /= 'VERTICAL' .AND. Orientation /= 'HORIZONTAL') &
      & .OR. (Geometry /= 'PLATE' .AND. Geometry /= 'CYLINDER')) THEN
        WRITE (0,95) Orientation, Geometry
        WRITE (ounit,95) Orientation, Geometry
95      FORMAT ('Error in function Nusselt. Bad input for Orientation or Geometry',/, &
          &     'Orientation = ',a20,/,'Geometry = ',a20)
        STOP
    END IF
    ! Determine C and m coefficients based on geometry, orientation, surface condition and X
    SELECT CASE (Orientation)
    CASE ('VERTICAL')
        SELECT CASE (Geometry)
        CASE ('PLATE', 'CYLINDER')
            ! McAdams Model
            IF (X > 1.0E9_r8k) THEN ! Turbulent
                C = 0.13_r8k
                m = onethird
                IF (X > 1.0E12_r8k) OutsideBounds = .TRUE.
            ELSE ! Laminar
                C = 0.59_r8k
                m = 0.25_r8k
                IF (X < 1.0E4_r8k) OutsideBounds = .TRUE.
            END IF
            ! FRAPTRAN Model
            IF (X > 3.02337E9_r8k) THEN ! Turbulent
                C = 0.021_r8k
                m = 0.4_r8k
            ELSE ! Laminar
                C = 0.555_r8k
                m = 0.25_r8k
            END IF
        END SELECT
    CASE ('HORIZONTAL')
        SELECT CASE (Geometry)
        CASE ('PLATE')
            SELECT CASE (Surface)
            CASE ('HEATED')
                ! McAdams, Heat Transmission, pg 180 (Data from Fishenden and Saunders)
                IF (X > 2.0e7_r8k) THEN ! Turbulent
                    C = 0.14_r8k
                    m = onethird
                    IF (X > 3.0E10_r8k) OutsideBounds = .TRUE.
                ELSE ! Laminar
                    C = 0.54_r8k
                    m = 0.25_r8k
                    IF (X < 1.0E5_r8k) OutsideBounds = .TRUE.
                END IF
            CASE ('COOLED') ! Laminar
                C = 0.27_r8k
                m = 0.25_r8k
            END SELECT
        CASE ('CYLINDER')
            ! McAdams, Heat Transmission, pg 177 (Data from Eberle, Wamsler, Rice, Koch and Ackermann)
            C = 0.53_r8k
            m = 0.25_r8k
            IF (X < 1.0E3_r8k .OR. X > 1.0E9_r8k) OutsideBounds = .TRUE.
        END SELECT
    END SELECT
    !
    Nu = C * (X ** m)
    !
    ! Ensure Nusselt number is within lower bounds
    Nusselt = MAX(Nu, NuMin)
    !
    IF (OutsideBounds .AND. PrandtlWarning) THEN
        WRITE (0,100) Surface, Orientation, Geometry, X
        WRITE (ounit,100) Surface, Orientation, Geometry, X
100     FORMAT ('Warning: outside of bounds in Prandtl calculation of natural convection on a: ', &
          &     /,a6,1x,a10,1x,a10,/,' Gr * Pr = ',es12.4)
        PrandtlWarning = .FALSE.
    END IF
    !
    END FUNCTION Nusselt
    !
END MODULE Plenum_frapcon



