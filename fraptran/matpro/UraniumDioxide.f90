MODULE uraniumdioxide_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    USE functions_fraptran
    USE Uncertainty_Vals_fraptran
    IMPLICIT NONE
    !>@brief
    !> This Module contains all UO2 Properties_fraptran
    !>@author
    !> Ian Porter, NRC
    !
    PRIVATE
    PUBLIC :: UraniumDiox
    ! Theoretical Density of PuO2 (g/cm^3)
    REAL(r8k), PARAMETER, PRIVATE :: PuO2TD = 11.46_r8k
    ! Theoretical Density of UO2 (g/cm^3)
    REAL(r8k), PARAMETER, PRIVATE :: UO2TD = 10.96_r8k
    ! Molecular Weight of Uranium (g/mol)
    REAL(r8k), PARAMETER, PRIVATE :: MWtU = 238.0_r8k
    ! Molecular Weight of Uranium Dioxide (g/mol)
    REAL(r8k), PARAMETER, PRIVATE :: MWtUO2 = 270.0_r8k
    !
    TYPE UraniumDiox
        ! Temperature
        REAL(r8k) :: Temp
        ! Burnup
        REAL(r8k) :: Burnup
        ! Oxygen-to-Metal Ratio
        REAL(r8k) :: OMRatio
        ! Fraction of theoretical density
        REAL(r8k) :: fraDen
        ! Melting point
        REAL(r8k) :: Tmelt
        ! Heat of fusion
        REAL(r8k) :: hefus
        ! Theoretical Density
        REAL(r8k) :: TD = UO2TD
        ! Gadolinia Concentration
        REAL(r8k) :: Gadoln
        ! Fraction of melting
        REAL(r8k) :: facmot
        ! Thermal conductivity
        REAL(r8k) :: Thcon
    CONTAINS
        PROCEDURE :: MatProp
        PROCEDURE, PRIVATE :: fthcon
        PROCEDURE, PRIVATE :: UPuO2Density
        PROCEDURE, PRIVATE :: UO2PhysProp
        PROCEDURE, PRIVATE :: fenthl
        PROCEDURE, PRIVATE :: fcp
        PROCEDURE, PRIVATE :: femiss
        PROCEDURE, PRIVATE :: fthexp
    END TYPE UraniumDiox
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (UO2, property) RESULT (mat_prop)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calls all of the UO2 Material Properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    ! Inputs
    !
    ! property    - material property to be calculated
    ! temp        - material temperature
    ! burnup      - burnup to end of timestep (End Of Step)
    ! burnup_prev - burnup to end of last timestep (Beginning Of Step)
    ! power       - power
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    
    CLASS (UraniumDiox), INTENT(INOUT) :: UO2
    CHARACTER(LEN=*), INTENT(IN) :: property
    
    ! Call material property
    SELECT CASE (property)
    CASE ('THERMCOND')  ! Thermal Conductivity
        mat_prop = UO2%fthcon ()
    CASE ('TDENSITY') ! Theoretical Density, (g/cm^3)
        mat_prop = UO2TD
!    CASE ('ASFABDENSITY') ! As-Fabricated Density, (g/in^3)
!        mat_prop = UPuO2Density()
    CASE ('MWT_U')   ! Molecular Weight of Uranium (g/mol)
        mat_prop = MWtU
    CASE ('MWT')   ! Molecular Weight of Uranium Dioxide (g/mol)
        mat_prop = MWtUO2
    CASE ('MWT_U_FRACTION') ! Weight Fraction Uranium
        mat_prop = MWtU / MWtUO2
!    CASE ('TMELT') ! Melting temperatures
!        CALL UO2PhysProp
!        mat_prop = ftmelt
    CASE ('SPECHEAT')  ! Specific Heat (J/kg*K)
        mat_prop = UO2%fcp ()
    CASE ('THEXP')  ! Thermal Expansion
        mat_prop = UO2%fthexp ()
    CASE ('EMISS')  ! Emissivity
        mat_prop = UO2%femiss ()
    CASE ('ENTHALPY')  ! Enthalpy
        mat_prop = UO2%fenthl ()
!    CASE ('THEXP_COEF') ! Thermal expansion coefficient
!        mat_prop = 1.12e-5_r8k
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module uraniumdioxide_fraptran. Invalid material property ID. Material Property ID = ',a20)
        ERROR STOP 'Error in Module uraniumdioxide_fraptran. Invalid material property called'
    END SELECT
    !
    END FUNCTION MatProp
    !
    !
    !
    FUNCTION fthcon (UO2) RESULT (con)
    USE Kinds_fraptran
    USE Uncertainty_Vals_fraptran
    USE variables_fraptran, ONLY : idx
    IMPLICIT NONE
    !>@brief
    !> FTHCON calculates the UO2 fuel thermal conductivity as a function of temperature, density, composition and burnup.
    !> The equation used in this Subroutine is that proposed by staff at NFI, Japan, at the May 1997 ANS Topical Meeting on
    !> Light Water Reactor Fuel Performance in Portland, Oregon.
    !>@author
    !> Modifid by PNNL, January 2002. The NFI formula was modified to raise low-burnup thermal conductivity at low temperature 
    !> and lower thermal conductivity at very high temperature.
    !
    ! Input
    !
    ! UO2%Burnup  - Current local burnup (MWd/MTU)
    ! UO2%Temp    - Current fuel ring temperature (K)
    ! UO2%fraDen  - Fuel density (ratio of actual to theoretical)
    ! UO2%OMRatio - Oxygen to metal ratio of fuel (atoms oxygen/atoms metal)
    ! UO2%Gadoln  - Weight fraction of gadolinia in the fuel
    !
    ! Output
    !
    ! UO2%Thcon   - Fuel thermal conductivity (W/(m*K))
    !
    ! Reference:
    !
    ! (1) Ohira and Itakaki, 1997. "Thermal Conductivity Measurements of High Burnup UO2 Pellet and a Benchmark
    !     Calculation of Fuel Center Temperature," proceedings pg. 541-549.
    ! The PNNL reference is:
    ! (2) Lanning and Beyer. 2002. "Revised UO2 Thermal Conductivity for FRAPCON-3 NRC Fuel Performance 
    !     Codes," in Proceedings of ANS Annual Meeting, June 9-13, 2002, Hollywood, Florida.
    !
    ! Note:
    !
    ! This is the same formulation used in FRAPCON-3.2 (February 2003), minus the formulations for MOX fuel.
    !
    CLASS (UraniumDiox) :: UO2
    REAL(r8k) :: bug, h, rphonon, elect, base, fm, x, ax, cx, con
    ! Convert burnup to GWd/MTU
    bug = UO2%Burnup / 1.0e3_r8k
    IF (bug < 1.0e-10_r8k) bug = 0.001_r8k
    !
    ! Use modified NFI model for UO2_fraptran
    ! Temperature dependence of annealing of irradiation effects
    h = 1.0_r8k / (1.0_r8k + 396.0_r8k * EXP(-6380.0_r8k / UO2%Temp))
    ! Phonon term
    rphonon = 1.0_r8k / (0.04520_r8k + 0.000246_r8k * UO2%Temp + 0.00187_r8k * bug + 1.1599_r8k * &
        &       UO2%Gadoln + (1.0_r8k - 0.9_r8k * EXP(-0.04_r8k * bug)) * (0.038_r8k * bug ** 0.28_r8k) * h)
    ! electronic term
    elect = (3.5e9_r8k / UO2%Temp ** 2) * EXP(-16361.0_r8k / UO2%Temp)
    ! base is conductivity before apply porosity correction
    base = rphonon + elect
    ! apply lucuta porosity correction factor (applied to 100% TD fuel)
    fm = UO2%fraDen / (1.0_r8k + 0.5_r8k * (1.0_r8k - UO2%fraDen))
    ! NFI base equation is for 95% TD fuel, so multiply by 1.079 to raise to 100% TD fuel conductivity, Then multiply by fm
    UO2%Thcon = base * fm * 1.079_r8k
    ! Multiply by Uncertainty coefficient
    UO2%Thcon = UO2%Thcon * sigfuelthermcond
    con = UO2%Thcon
    !
    END FUNCTION fthcon
    !
    !
    !
    REAL(r8k) FUNCTION UPuO2Density (UO2)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> Fuction calculates the as-fabricated density of U/PuO2 in (g/in^3)
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 8/13/2015
    !
    ! Input
    !
    ! den     - As-fabricated apparent fuel density (%)
    ! deng    - As-fabricated open porosity fraction (%)
    ! comp    - Weight percent plutonia in fuel (%)
    ! UO2TD   - Theoretical density of UO2 (g/cm3)
    ! PuO2TD  - Theoretical density of PuO2 (g/cm3)
    !
    ! Output
    !
    ! Density - Density of U/PuO2 fuel (g/in^3)
    !
    CLASS (UraniumDiox) :: UO2
    REAL(r8k) :: UO2Density, PuO2Density
    ERROR STOP 'Function: UPuO2Density in Module: UO2 has not yet been implemented'
    ! UO2 (g/cm^3)
!    UO2Density = ((den - deng) * 0.01_r8k) * UO2TD
    ! PuO2 (g/cm^3)
!    PuO2Density = ((den - deng) * 0.01_r8k) * PuO2TD
    ! U/PuO2 (g/cm^3)
!    UPuO2Density = UO2Density * (1.0_r8k - comp(j-1) / 100.0_r8k) + PuO2Density * (comp(j-1) / 100.0_r8k)
    ! Convert g/cm^3 to g/in^3
!    UPuO2Density = UPuO2Density / cm3toin3
    !
    END FUNCTION UPuO2Density
    !
    !
    !
    SUBROUTINE UO2PhysProp (UO2)
    USE Kinds_fraptran
    USE conversions_fraptran
    USE phypro_h_fraptran
    IMPLICIT NONE
    !>@brief
    !> Physical Properties of UO2. (Tmelt, Uranium content, TD, density)
    !> Note that this subroutine does not return any parameters directly but rather stores into the Module variables_fraptran
    !>@author
    !> Ian Porter, NRC, April 2014
    !
    ! Input
    !
    ! bup        - burnup
    ! UO2%Gadoln - Weight fraction of gadolinia in the fuel
    !
    ! Output
    !
    ! fhefus     - fuel heat of fusion
    ! ftmelt     - fuel melt temperature
    ! fdelta     - transition temperature
    !
    CLASS (UraniumDiox) :: UO2
    REAL(r8k) :: fbu
    !
    !fbu = bup / 86.4_r8k
    ! Melting and transition temperatures
!    IF (comp(j-1) > 0.0_r8k) THEN ! MOX Fuel
!        ftmelt = sldus(comp(j-1)) + 273.15_r8k - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * UO2%Gadoln(j-1) * 100.0_r8k
!        fdelta = liqdus(comp(j-1)) - sldus(comp(j-1)) - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * gadoln(j-1) * 100.0_r8k
!    ELSE ! UO2 Fuel
!        ftmelt = 3113.15_r8k - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * UO2%Gadoln(j-1) * 100.0_r8k
!        fdelta = 1.0e-10_r8k
!    END IF
    ! Heat of fusion
    fhefus = 27.4E4_r8k
    UO2%hefus = fhefus
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION sldus (PuConc)
        USE Kinds_fraptran
        USE conversions_fraptran
        IMPLICIT NONE
        !
        ! Input
        !
        ! PuConc - weight percent PuO2
        !
        REAL(r8k), INTENT(IN) :: PuConc
        !
        sldus = 2840.0_r8k - 5.41395_r8k * PuConc + 7.468390e-3_r8k * (PuConc ** 2)
        !
        END FUNCTION sldus
        !
        !
        !
        REAL(r8k) FUNCTION liqdus (PuConc)
        USE Kinds_fraptran
        USE conversions_fraptran
        IMPLICIT NONE
        !
        ! Input
        !
        ! PuConc - weight percent PuO2
        !
        REAL(r8k), INTENT(IN) :: PuConc
        !
        liqdus = 2840.0_r8k - 3.21860_r8k * PuConc - 1.448518e-2_r8k * (PuConc ** 2)
        !
        END FUNCTION liqdus
    !
    END SUBROUTINE UO2PhysProp
    !
    !
    !
    FUNCTION fcp (UO2)
    USE phypro_h_fraptran
    USE Kinds_fraptran
    USE Uncertainty_Vals_fraptran
    USE variables_fraptran, ONLY : idx
    IMPLICIT NONE
    !>@brief
    !> The function fcp is used to calculate the specific heat capacity of uo2, puo2, and (U/Pu)O2 fuels
    !> as a function of temperature, fraction of fuel which is molten, puo2 content, and oxygen-to-metal ratio.
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT_fraptran
    !>@author
    !> fcp was originally coded by v.f.baston in march 1974.
    !> last modified by g.a.reymann in may 1978.
    !> Modified by PNNL, January 1997, to clean up coding and delete licensing analysis and sensitivity uncertainty coding
    !
    ! Input
    !
    ! UO2%Temp    - Temperature (k)
    ! facmot      - Fraction molten (unitless) (0.0 = all solid, 1.0 = all molten)
    ! UO2%OMRatio - Oxygen to metal ratio (unitless) 
    !               (atoms oxygen) / (atoms metal) If not known, enter 2.0
    ! ufcp        - estimated standard error for uo2 (j/(kg*k))   (not currently Returned)
    ! pufcp       - estimated standard error for puo2 (j/(kg*k))  (not currently Returned)
    ! ftmelt      - fuel melting temperature (k)
    ! fdelta      - liquid-solid coexistence temperature (k)
    !
    ! Output
    !
    ! fcp         - Fuel specific heat capacity (j/(kg*k))
    !
    ! Reference:
    !
    ! (1) j.kerrisk and d.clIfton, nuclear technology,16 (1972).
    ! (2) o.kruger and h.savage, journal of chemical physics,45 (1968).
    ! The effect of oxygen-to-metal ratio was estimated from equations published by:
    ! (3) c.affortit and j.marcon, revue internationale des hautes temperatures et des refractaires,7 (1970).
    ! The specific heat capacity of molten uo2 was taken from:
    ! (4) l.leibowitz et al, journal of nuclear materials,39 (1970)
    !
    CLASS (UraniumDiox) :: UO2
    REAL(r8k) :: fcp, t, r, tm, ufcp, pufcp
    REAL(r8k), PARAMETER :: c1u = 296.7_r8k
    REAL(r8k), PARAMETER :: c2u = 2.43e-02_r8k
    REAL(r8k), PARAMETER :: c3u = 8.745e07_r8k
    REAL(r8k), PARAMETER :: thu = 535.285_r8k
    REAL(r8k), PARAMETER :: edu = 1.577e05_r8k
    REAL(r8k), PARAMETER :: c1pu = 347.4_r8k
    REAL(r8k), PARAMETER :: c2pu = 3.95e-04_r8k
    REAL(r8k), PARAMETER :: c3pu = 3.860e07_r8k
    REAL(r8k), PARAMETER :: thpu = 571.0_r8k
    REAL(r8k), PARAMETER :: edpu = 1.967e05_r8k
    REAL(r8k), PARAMETER :: fcpmol = 503.0_r8k
    !
    tm = ftmelt
    ! fcpmol = specific heat capacity of molten fuel (j/(kg*k))
    IF (UO2%Temp <= (tm + fdelta)) THEN
        !
        fcp = cp(c1u ,c2u ,c3u ,thu ,edu ,UO2%Temp)
        IF (UO2%Temp >= (tm - 0.1_r8k)) fcp = (1.0_r8k - UO2%facmot) * fcp + UO2%facmot * fcpmol
    ELSE
        fcp = fcpmol
        ufcp = 2.0_r8k
        pufcp = 5.6_r8k
    ENDIF
    ! Multiply by Uncertainty coefficient
    fcp = fcp * sigfuelheatcapa
    !
    CONTAINS
        !
        PURE REAL(r8k) FUNCTION cp (c1, c2, c3, th, ed, t)
        IMPLICIT NONE
        !
        REAL(r8k), INTENT(IN) :: c1, c2, c3, th, ed, t
        !
        cp = c1 * (th ** 2) * EXP(th / t) / ((t ** 2) * ((EXP(th / t) - 1.0_r8k) ** 2)) + c2 * t + &
          &  (UO2%OMRatio / 2.0_r8k) * (c3 * ed / (8.314_r8k * (t ** 2))) * EXP(-ed / (8.314_r8k * t))
        !
        END FUNCTION cp
        !
    END FUNCTION fcp
    !
    !
    !
    FUNCTION fthexp (UO2)
    USE Kinds_fraptran
    USE phypro_h_fraptran
    USE Uncertainty_Vals_fraptran
    USE variables_fraptran, ONLY : idx
    IMPLICIT NONE
    !>@brief
    !> Calculate the strain of the fuel caused by thermal expansion for UO2, PuO2, or (U,Pu)O2 as a function of
    !> temperature, puo2 content, and fraction of the fuel which is molten. The correlations were derived with the
    !> assumption that the thermal expansion strain is zero at 300k.
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT_fraptran
    !>@author
    !> fthexp was originally coded by v.f.baston in march 1974
    !> modified by c.s.olsen in feb. 1975
    !> last modified by g.a.reymann in july 1978
    !> Modified by PNNL, January 1997, to clean up coding and delete licensing analysis and sensitivity uncertainty coding
    !> c1u, c2u, c3u, and edu parameters updated by W.G. Luscher, PNNL 10-17-07
    !
    ! Input
    !
    ! UO2%Temp - Fuel temperature (k)
    ! facmot   - Fuel fraction which is molten (unitless)
    !      1.0 = fuel is all molten
    !      0.0 = fuel is all solid
    ! ftmelt   - fuel melting temperature (k)
    ! fdelta   - liquid-solid coexistence temperature range (k)
    !
    ! Output
    !
    ! fthexp   - Fuel strain due to thermal expansion (unitless)
    !
    ! Reference:
    !
    ! (1) p.j.baldock et al, journal of nuclear materials, 18 (1966)
    ! (2) n.h.brett and l.e.russel, proceedings of the 2nd international conference on plutonium metallurgy,
    !     Grenoble, France (1960) pp 397-410.
    ! (3) m.d.burdock and h.s.parker, journal of the american ceramic society,39 (1956) pp 181-187.
    ! (4) j.a.christensen, journal of the american ceramic society, 46 (1963) pp 607-608.
    ! (5) j.b.conway et al, transactions of the american nuclear society,6 (1963).
    ! (6) f.gronvold, journal of inorganic and nuclear chemistry,1 (1955) pp 357-370.
    ! (7) m.hoch and a.c.momin, high temperatures-high pressures,1 (1969) pp 401-407.
    ! (8) c.p.kempter and r.o.elliott, the journal of chemical physics,30 (1958) pp 1524-1526.
    ! (9) w.a.lambertson and j.h.handwerk, anl-5053 (1956).
    ! (10) m.tokar et al, nuclear technology, 17 (1973) pp 147-152.
    !
    CLASS (UraniumDiox) :: UO2
    REAL(r8k) :: fthexp, t, fcomp, fthexm
    REAL(r8k), PARAMETER :: c1u = 9.8e-06_r8k
    REAL(r8k), PARAMETER :: c2u = 2.61e-03_r8k
    REAL(r8k), PARAMETER :: c3u = 3.16e-01_r8k
    REAL(r8k), PARAMETER :: edu = 1.32e-19_r8k
    REAL(r8k), PARAMETER :: c1pu = 9.0e-06_r8k
    REAL(r8k), PARAMETER :: c2pu = 2.7e-03_r8k
    REAL(r8k), PARAMETER :: c3pu = 7.0e-02_r8k
    REAL(r8k), PARAMETER :: edpu = 7.0e-20_r8k
    REAL(r8k), PARAMETER :: bk = 1.38e-23_r8k ! bk is Boltzmann's constant (J/k)
    
    !
    IF (UO2%Temp <= ftmelt) THEN
        fthexp = ftx(c1u ,c2u ,c3u ,edu ,bk, UO2%Temp)
    ELSE
        fthexm = ftx(c1u ,c2u ,c3u ,edu ,bk,ftmelt)
        !
        IF (UO2%Temp <= (ftmelt + fdelta)) THEN
            fthexp = fthexm + 0.043_r8k * UO2%facmot
        ELSE
            fthexp = fthexm + 0.043_r8k + 3.6e-05_r8k * (UO2%Temp - (ftmelt + fdelta))
        ENDIF
    ENDIF
    ! Multiply by uncertainty coefficient
    fthexp = fthexp * sigfuelthermexp
    !
    CONTAINS
        !
        PURE REAL(r8k) FUNCTION ftx (c1, c2, c3, ed, bk, t)
        IMPLICIT NONE
        !
        REAL(r8k), INTENT(IN) :: c1, c2, c3, ed, bk, t
        !
        ftx = c1 * t - c2 + c3 * EXP(-ed / (bk * t))
        END FUNCTION ftx
    !
    END FUNCTION fthexp
    !
    !
    !
    PURE REAL(r8k) FUNCTION femiss (UO2)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> femiss calculates fuel emissivity as a function of temperature. 
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT_fraptran
    !>@author
    !> femiss was coded by r. e. mason in october 1978.
    !> Modified by PNNL, January 1997, to clean up coding and delete licensing analysis and sensitivity uncertainty coding
    !
    ! Input
    !
    ! UO2%Temp - Fuel temperature (k)
    !
    ! Output
    !
    ! femiss   - Fuel emissivity (unitless)
    !
    ! Reference:
    !
    ! (1) held and wilder, journ. amer. ceram. soc. vol. 52, (1969)
    ! (2) cabannes, et at, c. r. acad. sci., paris, ser. b (1967)
    !
    CLASS (UraniumDiox), INTENT(IN) :: UO2
    REAL(r8k) :: femisu, femisl
    !
    femiss = 0.78557_r8k + 1.5263e-05_r8k * UO2%Temp
    ! The following calculations provide the upper and lower bounds.
    ! The upper and lower bounds are not an output unless the user desires to modify the subcode appropriately.
    ! femisu is the upper bound
    femisu = femiss * (1.0_r8k + 0.06796_r8k)
    ! femisl is the lower bound
    femisl = femiss * (1.0_r8k - 0.06796_r8k)
    !
    END FUNCTION femiss
    !
    !
    !
    REAL(r8k) FUNCTION fenthl (UO2)
    USE Kinds_fraptran
    USE Uncertainty_Vals_fraptran
    USE variables_fraptran, ONLY : idx
    IMPLICIT NONE
    !>@brief
    !> This function is called by Subroutine energy and computes the enthalpy of fuel at a point relative to 0 Kelvin
    !
    ! Input
    !
    ! UO2%facmot  - Fraction of molten fuel
    ! UO2%hefus   - Heat of fussion of the fuel (J/kg)
    ! UO2%OMRatio - Fuel oxygen to metal ratio
    ! UO2%Tmelt   - Fuel melting temperature (K)
    ! UO2%Temp    - Local temperature (K)
    !
    ! Output
    !
    ! fenthl      - Local fuel enthalpy relative to zero degrees-K (J/kg)
    !
    ! Internal
    !
    ! fcpmol      - Specific heat capacity of molten fuel (j/(kg*K))
    !
    CLASS (UraniumDiox), INTENT(INOUT) :: UO2
    REAL(r8k) :: c1, th, c2, otm, ed, t, c3, tx
    REAL(r8k), PARAMETER :: fcpmol = 503.0_r8k
    ! The following statements contain constants from matpro-11
    REAL(r8k), PARAMETER :: c1u = 296.7_r8k
    REAL(r8k), PARAMETER :: c2u = 2.43e-02_r8k
    REAL(r8k), PARAMETER :: c3u = 8.745e07_r8k
    REAL(r8k), PARAMETER :: thu = 535.285_r8k
    REAL(r8k), PARAMETER :: edu = 1.577e05_r8k
    REAL(r8k), PARAMETER :: c1pu = 347.4_r8k
    REAL(r8k), PARAMETER :: c2pu = 3.95e-04_r8k
    REAL(r8k), PARAMETER :: c3pu = 3.860e07_r8k
    REAL(r8k), PARAMETER :: thpu = 571.0_r8k
    REAL(r8k), PARAMETER :: edpu = 1.967e05_r8k
    
    ! Limit temperature to Tmelt
    tx = MIN(UO2%Temp, UO2%Tmelt)
    
    ! Enthalpy
    fenthl = cpdt(c1u, thu, c2u, UO2%OMRatio, edu, tx, c3u)
    
    !
    IF (UO2%Temp > UO2%Tmelt - 2.0_r8k) THEN
        fenthl = fenthl + UO2%hefus * UO2%facmot
        IF (UO2%Temp > (UO2%Tmelt + 2.0_r8k)) fenthl = fenthl + (UO2%Temp - UO2%Tmelt) * fcpmol
    ENDIF
    
    ! Multiply by Uncertainty coefficient
    fenthl = fenthl * sigfuelheatcapa
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION cpdt (c1, th, c2, otm, ed, t, c3)
        !>@brief
        !> Integral of the fuel specific heat with respect to temperature
        REAL(r8k), INTENT(IN) :: c1, th, c2, otm, ed, t, c3
        !
        cpdt = c1 * th * (1.0_r8k / (EXP(th / t) - 1.0_r8k)) + c2 * t ** 2 / 2.0_r8k + &
          &    c3 * otm * EXP(-ed / (t * 8.314_r8k)) / 2.0_r8k
        !
        END FUNCTION cpdt
    !
    END FUNCTION fenthl
    !
END MODULE uraniumdioxide_fraptran













