MODULE UraniumDioxide
    USE Kinds
    USE conversions_frapcon
    USE Functions
    USE variables_frapcon, ONLY : fden, den, deng, comp, facmot, fotmtl, tsint, rsntr, prvden, ounit, &
      &                   sigftc, sigftex, sigswell, j, RelocModel, RinterfacPress, gadoln, &
      &                   ftmelt, fhefus, fdelta, bup, afdn
    IMPLICIT NONE
    !>@brief
    !> This Module contains all UO2 Properties
    !>@author
    !> Ian Porter, NRC
    !
    ! Theoretical Density of PuO2 (g/cm^3)
    REAL(r8k), PARAMETER, PRIVATE :: PuO2TD = 11.46_r8k
    ! Theoretical Density of UO2 (g/cm^3)
    REAL(r8k), PARAMETER, PRIVATE :: UO2TD = 10.96_r8k
    ! Molecular Weight of Uranium (g/mol)
    REAL(r8k), PARAMETER, PRIVATE :: MWtU = 238.0_r8k
    ! Molecular Weight of Uranium Dioxide (g/mol)
    REAL(r8k), PARAMETER, PRIVATE :: MWtUO2 = 270.0_r8k
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatPropUO2 (property, temp, burnup, burnup_prev, power) RESULT (mat_prop)
    USE Kinds
    USE conversions_frapcon
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
    REAL(r8k), INTENT(IN) :: temp
    REAL(r8k), INTENT(IN), OPTIONAL :: burnup, burnup_prev, power
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THERMCOND')  ! Thermal Conductivity
        mat_prop = fthcon (temp, burnup)
    CASE ('TDENSITY') ! Theoretical Density, (g/cm^3)
        mat_prop = UO2TD
    CASE ('ASFABDENSITY') ! As-Fabricated Density, (g/in^3)
        mat_prop = UPuO2Density()
    CASE ('MWT_U')   ! Molecular Weight of Uranium (g/mol)
        mat_prop = MWtU
    CASE ('MWT')   ! Molecular Weight of Uranium Dioxide (g/mol)
        mat_prop = MWtUO2
    CASE ('MWT_U_FRACTION') ! Weight Fraction Uranium
        mat_prop = MWtU / MWtUO2
    CASE ('TMELT') ! Melting temperatures
        CALL UO2PhysProp
        mat_prop = ftmelt
    CASE ('SPECHEAT')  ! Specific Heat (J/kg*K)
        mat_prop = fcp (temp)
    CASE ('SWELLING')  ! Swelling
        mat_prop = fswell(burnup, burnup_prev, temp)
    CASE ('THEXP')  ! Thermal Expansion
        mat_prop = fthexp (temp)
    CASE ('EMISS')  ! Emissivity
        mat_prop = femiss (temp)
    CASE ('DENSIFICATION')  ! Densification
        mat_prop = fudens (temp, burnup) * afdn / 100.0_r8k
    CASE ('ENTHALPY')  ! Enthalpy
        mat_prop = fenthl (temp)
    CASE ('RELOCATION')  ! Relocation
        mat_prop = UO2gtrloc (burnup, power)
    CASE ('POIS_RATIO') ! Poisson's Ratio
        mat_prop = fpoir (temp)
    CASE ('YOUNG_MOD') ! Young's Modulus
        mat_prop = UO2YoungMod (temp)
    CASE ('YIELD_STRESS') ! Yield stress
        mat_prop = 171.0_r8k
    CASE ('THEXP_COEF') ! Thermal expansion coefficient
        mat_prop = 1.12e-5_r8k
    CASE ('CREEP') ! Creep
        mat_prop = UO2Creep()
    CASE ('AXIAL_GROW') !Axial Growth
        mat_prop = 0.0_r8k
    CASE ('SHEAR_MOD') ! Shear Modulus
        mat_prop = 0.0_r8k
    CASE ('MEYER_HARD') ! Meyer's Hardness
        mat_prop = 0.0_r8k
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module UraniumDioxide. Invalid material property ID. Material Property ID = ',a20)
        STOP
    END SELECT
    !
    END FUNCTION MatPropUO2
    !
    !
    !
    REAL(r8k) FUNCTION fthcon (ftemp, burnup) RESULT (con)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> fthcon calculates the fuel thermal conductivity and its derivative with respect to temperature as a function of
    !> temperature, density, composition and burnup for UO2 fuel
    !
    ! Input
    !
    ! burnup - current local burnup (MWd/MTU)
    ! ftemp  - current fuel ring temperature (K)
    ! comp   - PuO2 content of fuel (percent puo2 in total fuel weight)
    ! fden   - Current fuel density (ratio of actual density to theoretical density)
    ! fotmtl - oxygen to metal ratio of fuel (atoms oxygen/atoms metal)
    ! gadoln - weight fraction of gadolinia in the fuel
    !
    ! Output
    !
    ! con    - output fuel thermal conductivity (W/(m*K))
    !
    ! Reference:
    !
    ! (1) Proposed by staff at NFI, Japan, at the May 1997 ANS Topical Meeting on LWR Fuel Performance
    !     in Portland, OR: (Ohira, K., and N.Itagaki, 1997. "Thermal Conductivity Measurements of High
    !     Burnup UO2 Pellet and a Benchmark Calculation of Fuel Center Temperature", proceedings pp. 541-549.
    !
    REAL(r8k) :: bug, h, rphonon, elect, base, ax, cx, fm, x, tc, tco, ucon, po, buguo2, frac
    REAL(r8k), INTENT(IN) :: ftemp, burnup
    ! Burnup in GWd/MTU
    bug = burnup / 1000.0_r8k
    !
    ! NFI formula (Ohira & Itagaki, ANS LWR Fuel perf. Topical mtg. 1997). 
    ! MODIFIED in January 2002 to raise low-burnup thermal conductivity at low temperature
    ! and to lower thermal conductivity at very high temp.
    h = 1.0_r8k / (1.0_r8k + 396.0_r8k * EXP(-6380.0_r8k / ftemp))
    rphonon = 1.0_r8k / (0.0452_r8k + 0.000246_r8k * ftemp + 1.0_r8k * 0.00187_r8k * bug + 1.1599_r8k * gadoln(j-1) + &
      &      (1.0_r8k - 0.9_r8k * EXP(-0.04_r8k * bug)) * 0.038_r8k * (bug ** 0.28_r8k) * h)
    elect = (3.50e9_r8k / ftemp ** 2) * EXP(-16361.0_r8k / ftemp)
    base = rphonon + elect
    ! fm is the Lucuta porosity correction factor(applied to 100% TD fuel)
    fm  = fden(j-1) / (1.0_r8k + 0.5_r8k * (1.0_r8k - fden(j-1)))
    ! NFI base equation is for 95% TD fuel, so multiply by 1.079 to raise to 100% TD fuel conductivity, then multiply by fm
    con = base * fm * 1.079_r8k
    ! Find uncertainty
    con = con * (1.0_r8k + ABS(sigftc * 0.088_r8k))
    !
    IF (ftemp < ftmelt) THEN
        ucon = 0.2_r8k * (1.0_r8k + ABS(2.0_r8k - fotmtl) * 10.0_r8k)
    ELSE
        ucon = con / 2.0_r8k
    END IF
    !
    END FUNCTION fthcon
    !
    !
    !
    REAL(r8k) FUNCTION UPuO2Density ()
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : den, deng, comp, j
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
    REAL(r8k) :: UO2Density, PuO2Density
    ! UO2 (g/cm^3)
    UO2Density = ((den - deng) * 0.01_r8k) * UO2TD
    ! PuO2 (g/cm^3)
    PuO2Density = ((den - deng) * 0.01_r8k) * PuO2TD
    ! U/PuO2 (g/cm^3)
    UPuO2Density = UO2Density * (1.0_r8k - comp(j-1) / 100.0_r8k) + PuO2Density * (comp(j-1) / 100.0_r8k)
    ! Convert g/cm^3 to g/in^3
    UPuO2Density = UPuO2Density / cm3toin3
    !
    END FUNCTION UPuO2Density
    !
    !
    !
    SUBROUTINE UO2PhysProp
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Physical Properties of UO2. (Tmelt, Uranium content, TD, density)
    !> Note that this subroutine does not return any parameters directly but rather stores into the Module variables_frapcon
    !>@author
    !> Ian Porter, NRC, April 2014
    !
    ! Input
    !
    ! bup    - burnup
    ! gadoln - Weight fraction of gadolinia in the fuel
    !
    ! Output
    !
    ! fhefus - fuel heat of fusion
    ! ftmelt - fuel melt temperature
    ! fdelta - transition temperature
    !
    REAL(r8k) :: fbu
    !
    fbu = bup / 86.4_r8k
    ! Melting and transition temperatures
    IF (comp(j-1) > 0.0_r8k) THEN ! MOX Fuel
        ftmelt = sldus(comp(j-1)) + 273.15_r8k - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * gadoln(j-1) * 100.0_r8k
        fdelta = liqdus(comp(j-1)) - sldus(comp(j-1)) - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * gadoln(j-1) * 100.0_r8k
    ELSE ! UO2 Fuel
        ftmelt = 3113.15_r8k - 5.0_r8k * fbu / 10000.0_r8k - 4.8_r8k * gadoln(j-1) * 100.0_r8k
        fdelta = 1.0e-10_r8k
    END IF
    ! Heat of fusion
    fhefus = 27.4E4_r8k
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION sldus (PuConc)
        USE Kinds
        USE conversions_frapcon
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
        USE Kinds
        USE conversions_frapcon
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
    REAL(r8k) FUNCTION fcp (ftemp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> The function fcp is used to calculate the specific heat capacity of uo2, puo2, and (u,pu)o2 fuels as a function
    !> of temperature, fraction of fuel which is molten, (P,U)O2 content, and oxygen-to-metal (O/M) ratio.
    !>@author
    !> fcp was originally coded by v.f.baston in march 1974.
    !> last modified by g.a.reymann in may 1978.
    !> Modification for gadolinia additions were made by DD Lanning in 1996.
    !
    ! Input
    !
    ! ftemp  - Fuel meshpoint temperature (K)
    ! facmot - Fuel fraction molten (unitless)
    !        = 1.0 - fuel is all molten
    !        = 0.0 - fuel is all solid
    ! fotmtl - Oxygen to metal (O/M) ratio ((atoms oxygen)/(atoms metal), unitless). If unknown, enter 2.0.
    ! gadoln - Weight fraction of gadolinia in the fuel
    !
    ! Output
    !
    ! fcp    - Fuel specific heat capacity (j/(kg*K))
    ! ufcp   - estimated standard error for uo2 (j/(kg*K))  (not currently returned)
    ! pufcp  - estimated standard error for puo2 (j/(kg*K)) (not currently returned)
    !
    ! Reference:
    !
    ! (1) j.kerrisk and d.clifton, nuclear technology,16 (1972).
    ! (2) o.kruger and h.savage, journal of chemical physics,45 (1968).
    !
    ! The effect of oxygen-to-metal ratio was estimated from equations published by:
    ! (3) c.affortit and j.marcon, revue internationale des hautes temperatures et des refractaires, (1970).
    !
    ! The specific heat capacity of molten uo2 was taken from:
    ! (4) l.leibowitz et al, journal of nuclear materials,39 (1970)
    !
    REAL(r8k) :: t, ufcp, pufcp
    REAL(r8k), INTENT(IN) :: ftemp
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
    REAL(r8k), PARAMETER :: c1gd = 315.86_r8k
    REAL(r8k), PARAMETER :: c2gd = 4.044e-02_r8k
    REAL(r8k), PARAMETER :: c3gd = 0.0_r8k
    REAL(r8k), PARAMETER :: thgd = 348.0_r8k
    REAL(r8k), PARAMETER :: edgd = 0.0_r8k
    REAL(r8k), PARAMETER :: fcpmol = 503.0_r8k ! Specific heat capacity of molten fuel (j/(kg*K))
    !
    IF (ftemp > (ftmelt + fdelta)) THEN
        fcp = fcpmol
        ufcp = 2.0_r8k
        pufcp = 5.6_r8k
    ELSE
        fcp = cp(c1u,c2u,c3u,thu,edu,ftemp,fotmtl) * (1.0_r8k - (comp(j-1) * 0.01_r8k)) + &
          &   cp(c1pu,c2pu,c3pu,thpu,edpu,ftemp,fotmtl) * (comp(j-1) * 0.01_r8k)
        fcp = fcp * (1.0_r8k - gadoln(j-1)) + gadoln(j-1) * cp(c1gd,c2gd,c3gd,thgd,edgd,ftemp,fotmtl)
        IF (ftemp >= (ftmelt - 0.1_r8k)) fcp = (1.0_r8k - facmot) * fcp + facmot * fcpmol
    END IF
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION cp (c1, c2, c3, th, ed, t, fotmtl)
        USE Kinds
        USE conversions_frapcon
        IMPLICIT NONE
        !>@brief
        !> Specific heat formula used for calculating specific heat of UO2
        !
        REAL(r8k), INTENT(IN) :: c1, c2, c3, th, ed, t, fotmtl
        !
        cp = c1 * (th ** 2) * EXP(th / t) / ((t ** 2) * ((EXP(th  / t) - 1.0_r8k) ** 2)) + &
          &  c2 * t + (fotmtl / 2.0_r8k) * (c3 * ed / (R_JmolK * (t ** 2))) * EXP(-ed / (R_JmolK * t))
        !
        END FUNCTION cp
    !
    END FUNCTION fcp
    !
    !
    !
    REAL(r8k) FUNCTION fswell (bu, bul, ftemp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Subroutine fswell calculates the fuel swelling as a function of burnup
    !>@author
    !> fswell was developed and programmed by r. e. mason - june 1978.
    !> fswell was revised July 1995 by G. A. Berna per D. D.  Lanning recommendations in ltr to L. Siefken, May 4, 1995
    !> fswell was revised October 2008 by K.J. Geelhood
    !
    ! Input
    !
    ! comp   - Plutonia content (weight percent)
    ! fdens  - Initial density of the fuel (kg/m**3)
    ! ftemp  - Temperature of the fuel ring (k)
    ! bu     - Burnup to end of time step (mw-s/kg-u)
    ! bul    - Burnup to end of last time step (mw-s/kg-u)
    !
    ! Output
    !
    ! fswell - Fuel swelling due to solid fission products (fraction)
    !
    ! Internal
    !
    ! bus    - burnup step
    !
    REAL(r8k), INTENT(IN) :: bu, bul, ftemp
    REAL(r8k) :: bus, fdens
    INTEGER(ipk) :: Regime
    !
    fdens = UPuO2Density() * 1000.0_r8k / in3tocm3
    !
    IF (bu < 5.184e5_r8k) THEN ! Burnup < 6,000 MWd/MTU
        Regime = 1
    ELSE IF (bu < 6.912e6_r8k) THEN ! Burnup > 6,000 MWd/MTU and < 80,000 MWd/MTU
        Regime = 2
    ELSE ! Burnup > 80,000 MWd/MTU
        Regime = 3
    END IF
    ! Gad Fuel
    IF (gadoln(j-1) > 0.0_r8k) Regime = 4
    !
    ! Calculate the burnup step increment
    !
    bus = fdens * 2.974e10_r8k * (bu - bul)
    !
    ! Calculate the swelling strain
    !
    SELECT CASE (Regime)
    CASE (1) ! No swelling is assumed to occur.
        fswell = 0.0_r8k
    CASE (2) ! 0.062% per GWd/MTU
        fswell = bus * (2.315e-23_r8k + sigswell * 2.987e-24_r8k)
    CASE (3) ! 0.086% per GWd/MTU
        fswell = bus * (3.211e-23_r8k + sigswell * 5.974e-24_r8k)
    CASE (4) ! Gadolinia doped fuel 0.05% per GWd/MTU        
        fswell = bus * (1.867e-23_r8k + sigswell * 2.987e-24_r8k)
    END SELECT
    !
    END FUNCTION fswell
    !
    !
    !
    REAL(r8K) FUNCTION fthexp (ftemp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function fthexp is used to calculate the strain of the fuel caused by thermal expansion.
    !> Thermal expansion strain is calculated for uo2,pu02, or (u,pu)02 as a function of temperature, puo2 content,
    !> and fraction of the fuel which is molten. The correlations were derived with the assumption that the
    !> thermal expansion strain is zero at 300k.
    !>@author
    !> fthexp was originally coded by v.f.baston in march 1974
    !> modified by c.s.olsen in feb. 1975
    !> last modified by g.a.reymann in july 1978
    !> W.G. Luscher 10/17/07
    !
    ! Input
    !
    ! ftemp  - Fuel temperature (k)
    ! facmot - Fuel fraction which is molten (unitless)
    !        = 1.0 - fuel is all molten
    !        = 0.0 - fuel is all solid
    !
    ! Output
    !
    ! fthexp - Fuel strain due to thermal expansion (unitless)
    ! ufthex - estimated standard error of fthexp (per cent) (not currently returned).
    !
    ! Reference:
    !
    ! (1)  p.j.baldock et al, journal of nuclear materials, 18 (1966)
    ! (2)  n.h.brett and l.e.russel, proceedings of the 2nd international conference on plutonium metallurgy, 
    !      grenoble, france (1960) pp 397-410.
    ! (3)  m.d.burdock and h.s.parker, journal of the american ceramic society,39 (1956) pp 181-187.
    ! (4)  j.a.christensen, journal of the american ceramic society, 46 (1963) pp 607-608.
    ! (5)  j.b.conway et al, transactions of the american nuclear society,6 (1963).
    ! (6)  f.gronvold, journal of inorganic and nuclear chemistry,1 (1955) pp 357-370.
    ! (7)  m.hoch and a.c.momin, high temperatures-high pressures,1 (1969) pp 401-407.
    ! (8)  c.p.kempter and r.o.elliott, the journal of chemical physics,30 (1958) pp 1524-1526.
    ! (9)  w.a.lambertson and j.h.handwerk, anl-5053 (1956).
    ! (10) m.tokar et al, nuclear technology, 17 (1973) pp 147-152.
    !
    REAL(r8k) :: mult
    REAL(r8k) :: ftemp, fcomp, burn, tempc, gasswell, ufthex, fthexm
    REAL(r8k), PARAMETER :: c1u = 9.8e-06_r8k
    REAL(r8k), PARAMETER :: c2u = 2.61e-03_r8k
    REAL(r8k), PARAMETER :: c3u = 3.16e-01_r8k
    REAL(r8k), PARAMETER :: edu = 1.32e-19_r8k
    REAL(r8k), PARAMETER :: c1pu = 9.0e-06_r8k
    REAL(r8k), PARAMETER :: c2pu = 2.7e-03_r8k
    REAL(r8k), PARAMETER :: c3pu = 7.0e-02_r8k
    REAL(r8k), PARAMETER :: edpu = 7.0e-20_r8k
    !
    fcomp = comp(j-1) / 100.0_r8k
    burn = bup * sectoday
    tempc = tkc(ftemp)
    IF (ftemp > ftmelt) THEN
        fthexm = ftx(c1u,c2u,c3u,edu,ftmelt) * (1.0_r8k - fcomp) + ftx(c1pu,c2pu,c3pu,edpu,ftmelt) * (fcomp)
        IF (ftemp >= (ftmelt + fdelta)) THEN
            fthexp = fthexm + 0.043_r8k + 3.6e-05_r8k * (ftemp - (ftmelt + fdelta))
        ELSE
            fthexp = fthexm + 0.043_r8k * facmot
        END IF
    ELSE
        fthexp = ftx(c1u,c2u,c3u,edu,ftemp) * (1.0_r8k - fcomp) + ftx(c1pu,c2pu,c3pu,edpu,ftemp) * fcomp
        IF (burn >= 40.0_r8k) THEN
            mult = MIN(1.0_r8k, (burn - 40.0_r8k) / 10.0_r8k)
            IF (tempc < 960.0_r8k) THEN
                gasswell = 0.0_r8k
            ELSE IF (tempc < 1370.0_r8k) THEN
                gasswell = 4.55e-5_r8k * tempc - 4.37e-2_r8k
            ELSE IF (tempc < 1832.0_r8k) THEN
                gasswell = -4.04e-5_r8k * tempc + 7.40e-2_r8k
            ELSE
                gasswell = 0.0_r8k
            END IF
        ELSE
            mult = 1.0_r8k
            gasswell = 0.0_r8k
        END IF
        fthexp = fthexp + gasswell * mult
    END IF
    ! Add on uncertainty
    fthexp = fthexp * (1.0_r8k + ABS(sigftex * 0.103_r8k))
    ufthex = 10.0_r8k
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION ftx (c1, c2, c3, ed, t)
        USE Kinds
        USE conversions_frapcon
        IMPLICIT NONE
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: c1, c2, c3, ed, t
        !
        ftx = c1 * t - c2 + c3 * EXP(-ed / (Boltzmann * t))
        !
        END FUNCTION ftx
    !
    END FUNCTION fthexp
    !
    !
    !
    REAL(r8k) FUNCTION femiss (ftemp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function femiss calculates fuel emissivity as a function of temperature.
    !>@author
    !> femiss was coded by r. e. mason in october 1978.
    !
    ! Input
    !
    ! ftemp  - Fuel temperature (k)
    !
    ! Output
    !
    ! femiss - Fuel emissivity (unitless)
    !
    ! Reference:
    !
    ! (1) Held and wilder, Journal American Ceramic Society, Vol. 52, (1969)
    ! (2) Cabannes, et al, c. r. acad. sci., paris, ser. b (1967)
    !
    REAL(r8k) :: ftemp, femisu, femisl
    !
    femiss = 0.78557_r8k + 1.5263e-05_r8k * ftemp
    ! the following calculations provide the upper and lower bounds.
    ! the upper and lower bounds are not an output unless the user desires to modify the subcode appropriately.
    ! femisu is the upper bound
    femisu = femiss * (1.0_r8k + 0.06796_r8k)
    ! femisl is the lower bound
    femisl = femiss * (1.0_r8k - 0.06796_r8k)
    !
    END FUNCTION femiss
    !
    !
    !
    REAL(r8k) FUNCTION fudens (ftemp, bu)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> fudens calculates irradiation-induced densification.
    !>@author
    !> fudens was developed and programmed by c. s. olsen (january 1975).
    !> Updated and corrected by b. w. burnham (october 1975).
    !> fudens was modified by r. e. mason (november 1978).
    !
    ! Input
    !
    ! ftemp  - Fuel temperature (k)
    ! bu     - Burnup (mw-s/kg-u)
    ! fdens  - Fuel density (kg/m**3)
    ! rsntr  - Maximum density change determined by a resintering test of 1973 k for 24 hours (kg/m**3)
    ! tsint  - Fuel sintering temperature (k)
    ! comp   - Plutonia content (weight percent)
    ! prvden - Total densification from previous time step (%)
    !
    ! Output
    !
    ! fudens - fuel dimensional change due to densification(%)
    !
    INTEGER(ipk) :: i, n
    REAL(r8k) :: dummy, dlen1, de, roth, fdens, x3, x4, al1, al3, al4, y1, y2, &
      &          alen, abu, fuden, al2, err, fbu, ts, x, x1, bp
    REAL(r8k), INTENT(IN) :: bu, ftemp
    LOGICAL :: errPass, RootFound
    REAL(r8k), DIMENSION(2), PARAMETER :: c = (/ UO2TD, PuO2TD /)
    REAL(r8k), DIMENSION(5), PARAMETER :: b = (/ 3.0_r8k, 1.0_r8k, 3.0_r8k, 2.0_r8k, 35.0_r8k /)
    !
    bp = bu / 86.4_r8k
    ! Calculate densification if burnup < 10,000 MWd/MTU
    IF (bp <= 10000.0_r8k) THEN
        ! Calculate fuel density
        fdens = UPuO2Density() * 1000.0_r8k / in3tocm3
        ! Conversions
        fbu = bu * 1.157e-05_r8k
        ! Sintering Temperature (C)
        IF (tsint <= 0.0_r8k) THEN
            ts = 1600.0_r8k ! Default value is 1600C
        ELSE
            ts = tkc(tsint)
        END IF
        !
        roth = c(1) * c(2) / (0.01_r8k * comp(j-1) * c(1) + (1.0_r8k - 0.01_r8k * comp(j-1)) * c(2))
        de = fdens / (roth * 10.0_r8k)
        IF (rsntr > 0.0_r8k) THEN
            dlen1 = 100.0_r8k * rsntr / (3.0_r8k * fdens)
        ELSE
            IF (ftemp >= 1000.0_r8k) THEN
                dlen1 = 66.6_r8k * (100.0_r8k - de) / (ts - 1180.0_r8k)
            ELSE
                dlen1 = 22.2_r8k * (100.0_r8k - de) / (ts - 1180.0_r8k)
            END IF
        END IF
        x3 = 0.0_r8k
        x4 = 1.0_r8k
        al1 = dlen1
        al3 = 3.0_r8k - al1
        al4 = 0.0_r8k
        RootFound = .FALSE.
        IF (al3 > 4.27e-3_r8k) THEN
            DO i = 1, 6
                y2 = dlen2(al3,x4,al4)
                y1 = dlen2(al3,x3,al4)
                IF (y1 * y2 <= 0.0_r8k) THEN
                    RootFound = .TRUE.
                    EXIT
                END IF
                x3 = x4
                x4 = x4 + 1.0_r8k
            END DO
            IF (RootFound) THEN
                errPass = .FALSE.
                x1 = x3
                DO n = 1, 50
                    x = x1 - dlen2(al3,x1,al4) / dlen3(x1)
                    err = ABS((x - x1) * 100.0_r8k / x)
                    IF (err <= 2.0e-04_r8k) THEN
                        errPass = .TRUE.
                        EXIT
                    END IF
                    x1 = x
                END DO
            END IF
        END IF
        IF (RootFound) THEN
            IF (errPass) THEN
                al2 = x
            ELSE
                al3 = 2.996_r8k
                al2 = 5.384_r8k
            END IF
            fuden = dlen2(al3,fbu,al2)
            IF (bu < 1728.0_r8k) fuden = 0.0_r8k
        ELSE
            WRITE (0,180)
            WRITE (ounit,180)
180         FORMAT (1x,/' no roots found between 0 and 6000 mws/mt uo2')
            fuden = 0.0_r8k
        END IF
        IF (ABS(fuden) <= ABS(prvden)) fudens = 0.0_r8k
        IF (ABS(fuden) > ABS(prvden)) fudens = fuden - prvden
    ELSE
        ! The burnup is greater than 10,000(MWd/mtU), no additional densification is assumed
        fudens = 0.0_r8k
    END IF
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION dlen2 (alen, bu, abu)
        USE Kinds
        USE conversions_frapcon
        IMPLICIT NONE
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/5/2015
        REAL(r8k), INTENT(IN) :: alen, bu, abu
        !
        dlen2 = -b(1) + alen + b(2) * EXP(-b(3) * (bu + abu)) + b(4) * EXP(-b(5) * (bu + abu))
        !
        END FUNCTION dlen2
        !
        !
        !
        REAL(r8k) FUNCTION dlen3 (bu)
        USE Kinds
        USE conversions_frapcon
        IMPLICIT NONE
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/5/2015
        REAL(r8k), INTENT(IN) :: bu
        !
        dlen3 = -b(2) * b(3) * EXP(-b(3) * bu) - b(4) * b(5) * EXP(-b(5) * bu)
        !
        END FUNCTION dlen3
    !
    END FUNCTION fudens
    !
    !
    !
    REAL(r8k) FUNCTION fenthl (temp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This function is called by subroutine energy and computes the enthalpy of fuel
    !> at a point relative to zero degrees absolute temperature.
    !
    ! Input
    !
    ! comp   - PUO2 fraction of the fuel
    ! facmot - Fraction of molten fuel
    ! fcpmol - Specific heat capacity of molten fuel (j/(kg*K))
    ! fhefus - Heat of fussion of the fuel (j/kg)
    ! fotmtl - Fuel oxygen to metal ratio
    ! ftmelt - Fuel melting temperature (K)
    ! gadoln - Weight fraction of gadolinia in the fuel
    ! temp   - Local temperature (K)
    !
    ! Output
    !
    ! fenthl - Local fuel enthalpy relative to zero degrees-K (j/kg)
    !
    REAL(r8k), PARAMETER :: fcpmol = 503.0_r8k
    ! The following data statements contain constants from matpro-11 fcp
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
    REAL(r8k), PARAMETER :: c1gd = 315.86_r8k
    REAL(r8k), PARAMETER :: c2gd = 4.044e-2_r8k
    REAL(r8k), PARAMETER :: c3gd = 0.0_r8k
    REAL(r8k), PARAMETER :: thgd = 348.0_r8k
    REAL(r8k), PARAMETER :: edgd = 0.0_r8k
    !
    REAL(r8k) :: tx, temp, facmot
    !
    If (temp > ftmelt) THEN
        facmot = 1.0_r8k   ! Molten fuel
    ELSE
        facmot = 0.0_r8k   ! Fuel has not melted
    END IF
    !
    tx = MIN(temp, ftmelt)
    fenthl = cpdt(c1u,thu,c2u,fotmtl,edu,tx,c3u) * (1.0_r8k - (comp(j-1) / 100.0_r8k))
    fenthl = fenthl + cpdt(c1pu,thpu,c2pu,fotmtl,edpu,tx,c3pu) * (comp(j-1) / 100.0_r8k)
    fenthl = fenthl * (1.0_r8k - gadoln(j-1)) + gadoln(j-1) * cpdt(c1gd,thgd,c2gd,fotmtl,edgd,tx,c3gd)
    IF (temp > (ftmelt - 2.0_r8k)) THEN
        fenthl = fenthl + fhefus * facmot
        IF (temp > (ftmelt + 2.0_r8k)) fenthl = fenthl + (temp - ftmelt) * fcpmol
    END IF
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION cpdt (c1, th, c2, otm, ed, t, c3)
        USE Kinds
        USE conversions_frapcon
        IMPLICIT NONE
        !>@brief
        !> The following equation is the integral of the fuel specific heat with respect to temperature
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/5/2015
        !
        REAL(r8k), INTENT(IN) :: c1, th, c2, otm, ed, t, c3
        !
        cpdt = c1 * th * (1.0_r8k / (EXP(th / t) - 1.0_r8k)) + c2 * t * t / 2.0_r8k + &
          &    c3 * otm * EXP(-ed / (t * R_JmolK)) / 2.0_r8k
        !
        END FUNCTION cpdt
    !
    END FUNCTION fenthl
    !
    !
    !
    REAL(r8k) FUNCTION UO2gtrloc (burnup, p)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : dp, cdg
    IMPLICIT NONE
    !>@brief
    !> UO2gtrloc computes the radial outward relocation of the fuel pellet.
    !> This Subroutine is based on comparisons to BOL measured centerline temperatures, and is presented as 
    !> the "revised best-estimate" model in GAPCON-THERMAL-2 Rev.2 .  It should be used in conjunction with
    !> uncracked fuel thermal conductivity only.
    !>@author
    !> This vervsion was coded by DD Lanning and K Geelhood May 1995
    !> Modified by K Geelhood, PNNL, 2014
    !> Modified by Ian Porter, NRC, June 2015
    !
    ! Input
    !
    ! burnup - Nodal burnup (MWd/mtU)
    ! gap    - Cold state radial gap size (in)
    ! p      - Local power (kW/ft)
    ! rp     - Fuel pellet cold state radius (in)
    !
    ! Output
    !
    ! gtrloc - Relocation strain (unitless)
    !
    ! Reference:
    !
    ! (1) C.E. Beyer and M.E. Cunningham, 1984. "GT2R2: An Updated Version of GAPCON-THERMAL-2", PNL-5178, NUREG/CR-3907.
    !
    REAL(r8k) :: pk, p, rp, gap, burnup, fbu, delgd, pf, reloc
    !
    pk = p * kWfttokWm ! Convert to kW/m
    rp = dp(j-1) / 2.0_r8k
    gap = cdg(j-1) / 2.0E3_r8k
    !
    SELECT CASE (RelocModel)
    CASE ('FRAPCON-3.3')
        IF (burnup > 0.0_r8k) THEN
            delgd = 0.45_r8k * gap
        ELSE
            delgd = 0.30_r8k * gap
        ENDIF
    CASE ('FRAPCON-3.4', 'FRAPCON-3.4A')
        fbu = burnup / 5000.0_r8k
        pf = (pk - 20.0_r8k) * 5.0_r8k / 2000.0_r8k
        IF (burnup >= 5000.0_r8k) fbu = 1.0_r8k
        delgd = gap * (0.3_r8k + 0.10_r8k * fbu)
        IF (pk > 20.0_r8k) delgd = gap * (0.28_r8k + pf + (pf + 0.12_r8k) * fbu)
        IF (pk > 40.0_r8k) delgd = gap * (0.32_r8k + 0.18_r8k * fbu)
    CASE ('FRAPCON-3.5')
        ! Set the power dependence on fuel relocation
        IF (pk <= 20.0_r8k) THEN
            reloc = 0.345_r8k
        ELSE IF (pk <= 40.0_r8k) THEN
            reloc = 0.345_r8k + (pk - 20.0_r8k) / 200.0_r8k
        ELSE
            reloc = 0.445_r8k
        END IF
        ! Set the burnup dependence on fuel relocation
        IF (burnup > 93.7_r8k) THEN
            delgd = gap * (0.055_r8k + MIN(reloc, reloc * (0.5795_r8k + 0.2447_r8k * LOG(burnup / 1000.0_r8k))))
        ELSE
            delgd = gap * 0.055_r8k
        END IF
    END SELECT
    !
    UO2gtrloc = delgd / rp
    !
    END FUNCTION UO2gtrloc
    !
    !
    !
    REAL(r8k) FUNCTION UO2YoungMod (temp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This function calculates the Youngs Modulus of UO2
    !
    ! Input
    !
    ! temp        - fuel temperature (K)
    ! porosity    - fuel porosity fraction (fraction, i.e. 0.05)
    !
    ! Output
    !
    ! UO2YoungMod - Young's modulus (GPa)
    !
    REAL(r8k) :: temp, porosity
    !
    porosity = (100.0_r8k - den) / 100.0_r8k
    UO2YoungMod = 229.0_r8k - 0.021_r8k * temp - 587.0_r8k * porosity
    !
    END FUNCTION UO2YoungMod
    !
    !
    !
    REAL(r8k) FUNCTION fpoir (ftemp)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> fpoir returns poisson's ratio for uo2 and mixed oxides.
    !>@author
    !> Originally coded by v.f. baston in march 1974.
    !> Modified by c.s. olsen in feb 1975.
    !
    ! Input
    !
    ! ftemp - Fuel temperature (k)
    ! comp  - Plutonia content (weight percent)
    !
    ! Output
    !
    ! fpoir - Poisson's ratio for UO2 (unitless)
    !
    ! Reference:
    !
    ! UO2
    ! (1) Wachtman et al., j. nuc. mat. 16 (1965) pp 39-41
    ! MOX
    ! (1) Nutt and allen, j. amer. ceram. soc., 53 (1970) p 205
    !
    REAL(r8k), INTENT(IN) :: ftemp
    !
    IF (comp(j-1) > 0.0_r8k) THEN ! MOX Fuel
        fpoir = 0.276_r8k
    ELSE ! UO2
        fpoir = 0.316_r8k
    END IF
    !
    END FUNCTION fpoir
    !
    !
    !
    REAL(r8k) FUNCTION UO2Creep()
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ProblemTime, RinterfacPress, it, j
    IMPLICIT NONE
    !>@brief
    !> This function calculates creep of UO2
    !
    ! Input
    !
    ! ProblemTime(it)   - ProblemTime
    ! it                - Timestep iteration
    ! RinterfacPress    - Interfacial Pressure
    !
    ! Output
    !
    ! UO2Creep          - Fuel Creep strain for Uranium Dioxide (unitless)
    !
    REAL(r8k) :: time
    !
    ! Get the timestep size, in seconds
    time = ProblemTime(it) - ProblemTime(it-1)
    !
    ! Simplistic UO2 Creep model
    ! Assuming temperatures not over 1500K
    ! Fission density of 8.4E18
    UO2Creep = 1.57e-11_r8k * (RinterfacPress(j-1) * PSItoMPa) - 1.69e-11_r8k
    !
    UO2Creep = UO2Creep * time
    !
    END FUNCTION UO2Creep
    !
    !
    !
    REAL(r8k) FUNCTION felmod (ftemp, fraden, fotmtl)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : comp
    IMPLICIT NONE
    !>@brief
    !> felmod calculates fuel elastic modulus as a function of temperature, fractional density, 
    !> oxygen to metal (O/M) ratio and (P/U)O2 content
    !>@author
    !> felmod coded by v. f. baston march 1974
    !> modified by c. s. olsen in february and july 1975
    !> modified by d. l. hagrman, e. t. laats, and m. a. morgan, june 1977
    !
    ! Input
    !
    ! ftemp  - Fuel temperature (k)
    ! fraden - Fractional fuel density (ratio of actual density to theoretical density)
    ! fotmtl - Oxygen to metal ratio of fuel (atoms oxygen/atoms metal)
    ! fcomp  - PuO2 content of fuel (weight fraction)
    !
    ! Output
    !
    ! felmod - fuel young's modulus (n/m**2)
    ! ufelmd - standard error expected in felmod (n/m**2) (not currently returned)
    !
    ! Reference:
    !
    ! (1) a. padel and c. de novion, j. nucl. mat. 33 (1969) pp 40 - 51
    ! (2) j. belle and b. lustman, fuel elements conference, tid-7546 (1958) pp 480-481
    ! (3) a. r. hall, j. nucl. mat. 37 (1970) pp 314 - 323
    !
    REAL(r8k) :: ys, ufelmd, unstoc, b, y
    REAL(r8k), INTENT(IN) :: ftemp, fraden, fotmtl
    !
    ys = 23.34e10_r8k * (1.0_r8k - (2.752_r8k * (1.0_r8k - fraden))) * (1.0_r8k - 1.0915e-04_r8k * ftemp)
    ufelmd = 6.0e9_r8k
    !
    IF (ftemp >= 1.6e3_r8k) THEN
        ufelmd = ufelmd + ys * (ftemp - 1.6e3_r8k) / 6.0526e3_r8k
        IF (ftemp >= 3113.15_r8k) THEN
            ys = 1.e6_r8k
            ufelmd = 0.0_r8k
            felmod = ys
            RETURN
        END IF
    END IF
    unstoc = ABS(fotmtl - 2.0_r8k)
    IF (unstoc <= 1.0e-03_r8k) THEN
        IF ((comp(j-1) * 0.01_r8k) < 1.0e-03_r8k) THEN
            felmod = ys
            RETURN
        END IF
    END IF
    b = 1.34_r8k
    IF (fotmtl < 2.0_r8k) b = 1.75_r8k
    y = ys * EXP(-b * unstoc) * (1.0_r8k + 0.15_r8k * (comp(j-1) * 0.01_r8k))
    ufelmd = SQRT(ufelmd ** 2 + (y - ys) ** 2)
    ys = y
    felmod = ys
    !
    END FUNCTION felmod
    !
END MODULE UraniumDioxide

