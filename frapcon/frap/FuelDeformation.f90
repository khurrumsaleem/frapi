MODULE FuelDeformation_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to calculate fuel deformation _frapcon
    !> due to thermal expansion, irradiation swelling, and relocation.
    !> Subroutines include fexpan, gtrloc, swell
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE gtrloc
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon
    USE variables_frapcon, ONLY : RelocModel, fuelreloc, j, rlcstrn, gasflg, rlcstrnold, &
      &                   hsolid, Relocation, dp, Power, EOSNodeBurnup, cdg
    IMPLICIT NONE
    !>@brief
    !> gtrloc is called from Subroutine frpcon and computes the radial outward relocation of the fuel pellet.
    !> This Subroutine is based on comparisons to BOL measured centerline temperatures, and is presented as 
    !> the"revised best-estimate" model in GAPCON-THERMAL-2 Rev.2 .  It should be used in conjunction with
    !> uncracked fuel thermal conductivity only.
    !> Reference is: C.E. Beyer and M.E. Cunningham, 1984. "GT2R2: An Updated Version of GAPCON-THERMAL-2", PNL-5178, NUREG/CR-3907.
    !>@author
    !> This vervsion was coded by DD Lanning and K Geelhood May 1995
    !> Modified by Ian Porter, NRC, April 2014
    !
    ! Input
    !
    ! burnup - nodal burnup (MWd/mtU)
    ! cdg    - cold state radial gap size (in)
    ! Power  - local power (kW/ft)
    ! dp     - fuel pellet cold state diameter (in)
    !
    ! Output
    !
    ! gtrloc - relocation strain (unitless)
    !
    REAL(r8k) :: pk, delgd, poisson, youngs, con, exp, thstr, porosity, fbu, temp
    !
    temp = 0.0_r8k
    !
    SELECT CASE (RelocModel)
    CASE ('OFF')
        rlcstrn(j-1) = 0.0_r8k
    CASE ('USER')
        fbu = MIN(EOSNodeBurnup(j-1) / 10000.0_r8k, 1.0_r8k)
        delgd = cdg(j-1) / 2000.0_r8k * fuelreloc * fbu
        rlcstrn(j-1) = delgd / (dp(j-1) / 2.0_r8k)
    CASE DEFAULT
        rlcstrn(j-1) = MatProp (Material='FUEL', Property='RELOCATION', Temperature=temp, &
          &                     Burnup=EOSNodeBurnup(j-1), Power=Power(j-1))
    END SELECT
    !
    IF (rlcstrnold(j-1) > rlcstrn(j-1)) rlcstrn(j-1) = rlcstrnold(j-1)
    IF (hsolid > 0.0_r8k .AND. rlcstrn(j-1) > rlcstrnold(j-1)) rlcstrn(j-1) = rlcstrnold(j-1)
    Relocation(j-1) = rlcstrn(j-1) * (dp(j-1) / 2.0_r8k)
    IF (gasflg) rlcstrnold(j-1) = rlcstrn(j-1)
    !
    END SUBROUTINE gtrloc
    !
    !
    !
    SUBROUTINE fexpan
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon
    USE variables_frapcon, ONLY : facmot, tfuelr, tfring, na, nr, uo2exp, crad, ftmelt, j, afal, sumexp
    IMPLICIT NONE
    !>@brief
    !> fexpan is called from frpcon and computes the thermal expansion of the fuel.
    !>@author
    !> This Subroutine was coded by g a berna in march 1978.
    !
    ! Input
    !
    ! afal   - additional thermal expansion factor
    ! ftmelt - fuel melt temperature (K)
    ! j      - axial node index
    ! nr     - maximum number of radial nodes
    ! na     - number of axial nodes plus one
    ! crad   - cold state radii of fuel radial nodes (in)
    ! tfring - fuel ring temperatures (F)
    ! tfuelr - radial node temperatures (F)
    !
    ! Output
    !
    ! dph    - thermally expanded pellet diameter (in)
    ! sumexp - total fuel surface displacement due to thermal expan.(in)
    ! uo2exp - thermal expansion (in/in)
    !
    !
    INTEGER(ipk) :: l
    REAL(r8k) :: tfringk, dph
    !
    sumexp = 0.0_r8k
    DO l = 1, (nr - 1)
        tfring(l) = 0.5_r8k * (tfuelr(l) + tfuelr(l + 1))
        tfringk = tfk(tfring(l))
        ! Thermal Expansion Calculation
        facmot = 0.0_r8k
        IF (tfringk > ftmelt) facmot = 1.0_r8k
        uo2exp(l,j-1) = MatProp ('FUEL', 'THEXP', tfringk) * afal
        sumexp = sumexp + (crad(l,j-1) - crad(l + 1, j - 1)) * (1.0_r8k + uo2exp(l,j-1))
    END DO
    sumexp = sumexp + crad(nr,j-1) * (1.0_r8k + uo2exp(nr-1,j-1))
    dph = sumexp * 2.0_r8k
    !
    END SUBROUTINE fexpan
    !
    !
    !
    SUBROUTINE swell
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon
    USE variables_frapcon, ONLY : comp, nr, ProblemTime, porosold, porosnew, addswell, slim, gaprecov, tfuelr, &
      &                   hrad, uo2exp, dpw, dpwpp, dpw2, dpwpp2, densf, densp, crad, rlcstrn, rc, j, &
      &                   it, tsint, rsntr, dphf, afdn, prvden, BOSNodeBurnup, EOSNodeBurnup
    IMPLICIT NONE
    !>@brief
    !> swell is called from Subroutine frpcon. This routine computes the fuel ring displacements 
    !> which include thermal expansion, irradiation swelling, and densification
    !
    ! Input
    !
    ! EOSNodeBurnup - burnup to end of power step (MWd/mtU)
    ! BOSNodeBurnup - burnup to beginning of power step (MWd/mtU)
    ! comp          - puo2 content of the fuel (weight %)
    ! densp         - storage of previous values of densification (m/m)
    ! dpwpp         - storage of previous values of outward swelling (m/m)
    ! intprs        - interfacial pressure (psi)
    ! it            - power step index
    ! j             - axial increment index
    ! nr            - maximum number of radial nodes
    ! porosold      - porosity left at beginning of power step (m/m)
    ! crad          - cold state fuel ring radii (in)
    ! rc            - radius of the fuel pelet central annulus (in)
    ! rsntr         - maximum density change determined by a resintering
    ! slim          - user controlled swelling limit test of 1973 K for 24 hours (kg/m**3)
    ! tfuelr        - fuel ring boundary temperature (F)
    ! tsint         - sintering temperature of the fuel (K)
    ! uo2exp        - thermal expansion of fuel ring (in/in)
    ! rlcstrn       - local current relocation strain (1/2 is added to the radial strain as permanent outward strain)
    !
    ! Output
    !
    ! dphf          - fuel diameter including expansion, swelling, and densification (in)
    ! dpw           - outward swelling (m/m)
    ! densf         - densification (m/m)
    ! porosnew      - porosity left after power step (m/m)
    ! hrad          - hot state fuel ring radii (in)
    ! coldrad       - total radial displacement (cold)
    !
    INTEGER(ipk) :: l, i, ii, ij, ik
    REAL(r8k) :: delden, bu, bupp, dd, delpor, delr, ftemp, gaswl, ftempavg, soldsw
    REAL(r8k), PARAMETER :: a = 3.0_r8k
    REAL(r8k), PARAMETER :: b = 0.1_r8k
    ! Convert burnup from MWd/t to MWsec/kg
    dd = 0.0_r8k
    bu = EOSNodeBurnup(j-1) * 86.4_r8k
    bupp = BOSNodeburnup(j-1) * 86.4_r8k
    hrad(nr, j-1) = rc(j-1)
    DO l = 1, (nr - 1)
        i = nr - l
        prvden = densp(i, j-1) * 100.0_r8k
        ftempavg = (tfuelr(i) + tfuelr(i + 1)) / 2.0_r8k
        ! Convert from F to K
        ftemp = tfk(ftempavg)
        ! The following call to fswell calculates additional swelling due to solid fission products for this time step (+m**3/m**3)
        IF (bu < 5.184e5_r8k) gaswl = 0.0_r8k
        ! Call swelling value
        soldsw = MatProp ('FUEL', 'SWELLING', ftemp, bu, bupp)
        !If the user specifies additional swelling, add it as volumetric swelling
        IF (it > 1) soldsw = soldsw + addswell(it) - addswell(it-1)
        dpw(i,j-1) = soldsw / 3.0_r8k + dpwpp(i, j-1)
        dpw(i,j-1) = MIN(dpw(i, j-1), slim / 3.0_r8k)
        ! The following wil calculate additional densification for this time step  (-m/m)
        delden = MatProp ('FUEL', 'DENSIFICATION', ftemp, bu)
        delpor = delden
        porosnew(i, j-1) = porosold(i, j-1) + delpor
        ! Porosity is not allowed to go negative
        IF (porosnew(i, j-1) <= 0.0_r8k) THEN
            porosnew(i, j-1) = 0.0_r8k
            delden = -porosold(i, j-1)
        END IF
        densf(i, j-1) = densp(i, j-1) + delden
        delr = crad(i, j-1) - crad(i+1, j-1)
        IF (l == 1) THEN
            hrad(i, j-1) = delr * (1.0_r8k + uo2exp(i, j-1) + dpw(i, j-1) + densf(i, j-1) + &
              &            rlcstrn(j-1) * (1.0_r8k - gaprecov)) + rc(j-1) * (1.0_r8k + uo2exp(i, j-1))
        ELSE
            hrad(i, j-1) = delr * (1.0_r8k + uo2exp(i, j-1) + dpw(i, j-1) + densf(i, j-1) + &
              &            rlcstrn(j-1) * (1.0_r8k - gaprecov)) + hrad(i+1, j-1)
        END IF
    END DO
    !
    dphf = 2.0_r8k * hrad(1, j-1)
    !
    END SUBROUTINE swell
    !
END MODULE FuelDeformation_frapcon



