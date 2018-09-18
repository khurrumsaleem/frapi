MODULE Burn_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE GadRadPower_frapcon
    USE Material_Properties_frapcon
    USE Comde_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used for burnup calculations_frapcon.
    !> Subroutines include burnup, tubrnp, turbin and turbo
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/17/2014
    !
    CONTAINS
    !
    SUBROUTINE burnup
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : modheat, ProblemTime, qmpy, HeatFlux, BOSNodeburnup, EOSNodeburnup, &
      &                   ounit, qaxnorm, na, im, buin, bup, it, j, totl, itt, cfv, fa, m, rhofuel, bu, &
      &                   buold, dcoBOL, delbu, delh, delbp, jmin
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and computes the burnup for node j during the power-time step.
    !>@author
    !> burnup was coded by g a berna november 1977.
    !
    ! Input
    !
    ! buin          - problem initial rod average burnup (MWd/mtU)
    ! BOSNodeburnup - begining of step average burnup for node j (MWd/mtU)
    ! buold         - rod average burnup to beginning of power-time step (MWd/mtU
    ! cfv           - cold state fuel volume (cu.in.)
    ! dcoBOL        - as fabricated cladding outside diameter (in)
    ! delh          - time step size (hours)
    ! fa            - ratio of peak to average power
    ! heatflux      - total coolant channel heat flux, assuming all of the coolant heat is from fission 
    !                 (thermal heating & gamma heating),(btu/hr-ft**2)
    ! im            - total number of power-time steps
    ! it            - power-time step index
    ! itt           - first time through index
    ! j             - axial node index
    ! modheat       - moderator heating coefficient
    ! m             - axial power shape index
    ! mwt           - molecular weight fraction of uranium
    ! na            - maximum number of axial nodes
    ! pi            - the constant 3.1415926
    ! qaxnorm       - normalized axial node power
    ! qbar          - average power over timestep
    ! qmpy          - rod average heat flux for power-time step (btu/hr-ft**2)
    ! rhofuel       - fuel density (gm/in**3)
    ! ProblemTime   - time step array (sec)
    ! totl          - fuel stack height (ft)
    !
    ! Output
    !
    ! bu            - rod average burnup to end of power-time step (MWd/mtU)
    ! bup           - rod average burnup for node j (MWs/kgU)
    ! EOSNodeburnup - end of step rod average burnup for node j (MWd/mtU)
    ! delbp         - average burnup during the power-time step for node j (MWd/mtU)
    ! delbu         - rod average burnup during the power-time step (MWd/mtU)
    !
    INTEGER(ipk) :: i
    REAL(r8k) :: mwt, amwdt, amwdk, qbar, dummy
    ! Calculation of rod average heat flux and burnup
    mwt = MatProp ('FUEL', 'MWT_U_FRACTION', dummy)
    IF (itt <= 1) THEN
        itt = 2
        HeatFlux(1) = qmpy(1)
        ! Note: (IP) This was not divided by (1-modheat) because this is for the initial timestep_frapcon, ProblemTime(1) = 0.001
        ! with a value for qmpy(1) set at 3000 in initial.f
        qbar = (HeatFlux(1) * ProblemTime(1)) / (ProblemTime(im) * fa)
        IF (im > 1) THEN
            DO i = 2, im
                HeatFlux(i) = qmpy(i) / (1.0_r8k - modheat)
                qbar = qbar + (HeatFlux(i) * (ProblemTime(i) - ProblemTime(i-1))) / (ProblemTime(im) * fa)
            END DO
        END IF
        amwdt = ((dcoBOL * intoft) * pi * totl * (qbar * BTUhtoMW) * (ProblemTime(im) * sectoday)) / &
          &     (mwt * rhofuel * cfv * 1.0e-6_r8k) + buin(j-1)
        amwdk = amwdt / 1000.0_r8k
        WRITE (ounit,140) amwdk, amwdt, qbar
    END IF
    IF (j <= jmin) THEN
        IF (it == 1) THEN
            HeatFlux(it) = qmpy(it) !Same as noted above.
        ELSE
            HeatFlux(it) = qmpy(it) / (1.0_r8k - modheat) !Corrected for total power generated from fission
        END IF
        delbu = ((dcoBOL * intoft) * pi * totl * (HeatFlux(it) * BTUhtoMW) * delh * hrtoday) / &
          &     (fa * mwt * rhofuel * cfv * 1.0e-6_r8k)
        bu = buold + delbu
    END IF
    delbp = delbu * qaxnorm(j-1,m) * fa
    EOSNodeburnup(j-1) = BOSNodeburnup(j-1) + delbp
    bup = EOSNodeburnup(j-1) * daytosec * 1.0E-3_r8k
140 FORMAT (///,27x,' rod ave burnup at end of life,  MWd/kgU (MWd/mtU)',f6.2,2x,"(",f9.1,")" &
      &        /27x,' rod time ave heat flux (qbar) =',f9.0,//)
    !
    END SUBROUTINE burnup
    !
    !
    !
    SUBROUTINE turbin
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : comp, enrpu39, enrpu40, enrpu41, enrpu42, buin, nr, na, brnup3, formf, &
      &                   enrch, rrev, fotmtl, prty
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called by inital and calculates the initial concentrations U-235, U-238, 
    !> Pu-239, Pu-240, Pu-241 and Pu-242.
    !>@author
    !> cognizant engineers: dd lanning, kl davis
    !>@date
    !> Sept 19, 1995
    !
    ! Input
    !
    ! buin   - initial axial burnup array (mwd/mtu)
    ! nr     - number of radial mesh points
    ! na     - number of axial nodes + 1
    ! rrev   - fuel mesh point radii array (in)
    ! por000 - fractional fuel pellet porosity (Volumetric)
    ! enrch  - initial u-235 enrichment (atom per total heavy metal atoms)
    !
    ! Global
    !
    ! formf     - 2-D array (axial,radial) of normalized form factors, correlated to the radial boundaries. (dimensionless).
    ! brnup3    - 3-D array (axial,radial,old/new) of local burnups, correlated to the radial boundaries (mwd/mtm).
    ! brnup1    - 2-D array (axial,old(2)/new(1)) for the current volume-average burnup for the axial node (mwd/mtm)
    ! ntot      - total initial number of heavy metal atoms per cm^3.
    ! coU235av  - axial array of volume average concentration of u-235 (atoms per cm^3)
    ! coU238av  - axial array of volume average concentration of u-238 (atoms per cm^3)
    ! coPu239av - axial array of volume average concentration of pu-239 (atoms per cm^3)
    ! coPu240av - axial array of volume average concentration of pu-240 (atoms per cm^3)
    ! coPu241av - axial array of volume average concentration of pu-241 (atoms per cm^3)
    ! coPu242av - axial array of volume average concentration of pu-242 (atoms per cm^3)
    ! conU235   - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of u-235 (atoms per cm^3)
    ! conU238   - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of u-238 (atoms per cm^3)
    ! conPu239  - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-239 (atoms per cm^3)
    ! conPu240  - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-240 (atoms per cm^3)
    ! conPu241  - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-241 (atoms per cm^3)
    ! conPu242  - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-242 (atoms per cm^3)
    !
    ! Internal
    !
    ! ntime  - maximum number of burnup steps during initialization
    ! dburn  - burnup increment (mwd/mtm)
    !
    INTEGER(ipk) :: i, itime
    REAL(r8k) :: mmu, mmpu, molepu, moleu, atfracpu, atfracu, dburn
    ! initial variables
    por000 = prty
    dburn = 10.0_r8k
    !
    DO lschni = 1, (na - 1)
        mmu = enrch(lschni) / 100.0_r8k * 235.0_r8k + (1.0_r8k - enrch(lschni) / 100.0_r8k) * 238.0_r8k
        mmpu = enrpu39 / 100.0_r8k * 239.0_r8k + enrpu40 / 100.0_r8k * 240.0_r8k + enrpu41 / 100.0_r8k * 241.0_r8k &
          &  + enrpu42 / 100.0_r8k * 242.0_r8k
        molepu = comp(lschni) / (mmpu + fotmtl * 16.0_r8k)
        moleu = (100.0_r8k - comp(lschni)) / (mmu + fotmtl * 16.0_r8k)
        atfracpu = molepu / (molepu + moleu)
        atfracu = moleu / (molepu + moleu)
        ! Enrichments converted to atom% in heavy metal
        enriU235(lschni) = atfracu * enrch(lschni) / 100.0_r8k
        enriU238(lschni) = atfracu * (1.0_r8k - enrch(lschni) / 100.0_r8k)
        enriPu239(lschni) = atfracpu * enrpu39 / 100.0_r8k
        enriPu240(lschni) = atfracpu * enrpu40 / 100.0_r8k
        enriPu241(lschni) = atfracpu * enrpu41 / 100.0_r8k
        enriPu242(lschni) = atfracpu * enrpu42 / 100.0_r8k
    END DO
    ! burn-up loop (mwd/tom)
    itime = 1
    DO lschni = 1, (na - 1)
        ! restore values
        brnup1 (lschni,2) = brnup1 (lschni,1)
        DO i = 1, nr
            brnup3 (lschni,i,2) = brnup3 (lschni,i,1)
            !
            conU235 (lschni,i,2) = conU235 (lschni,i,1)
            conU238 (lschni,i,2) = conU238 (lschni,i,1)
            !
            conPu239 (lschni,i,2) = conPu239 (lschni,i,1)
            conPu240 (lschni,i,2) = conPu240 (lschni,i,1)
            conPu241 (lschni,i,2) = conPu241 (lschni,i,1)
            conPu242 (lschni,i,2) = conPu242 (lschni,i,1)
        END DO
        ! new values
        ! --- new burn-up
        IF ((buin(lschni + 1) - brnup1(lschni,2)) < dburn) dburn = buin(lschni + 1) - brnup1(lschni,2)
        dburn = MAX(dburn, 0.0_r8k)
        IF (itime > 1) brnup1(lschni,1) = brnup1(lschni,2) + dburn
        DO i = 1, nr
            brnup3(lschni,i,1) = brnup3(lschni,i,2) + dburn * formf(lschni,i)
        END DO
        ! --- Call tubrnp model
        CALL tubrnp(itime)
        !
    END DO
    dburn = 10.0_r8k
    !
    END SUBROUTINE turbin
    !
    !
    !
    SUBROUTINE turbo
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : modheat, BOSNodeburnup, EOSNodeburnup, brnup3, formf, ounit, nr, na, rapow, &
      &                   gadoln, qnode, rrev, wimsburnup, oldwimsburnup, j, rprm1
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called by frpcon and calculates the radial form factor used to determine power and burnup.
    !>@author
    !> cognizant engineers: dd lanning, kl davis
    !>@date
    !> Sept 19, 95
    !
    ! Input
    !
    ! brnup1        - 2-D array (axial,old(2)/new(1)) for the current volume-average burnup for the axial node (MWd/mtU)
    ! brnup3        - 3-D array (axial,radial,old/new) of local burnups, correlated to the radial boundaries (MWd/mtU).
    ! formf         - 2-D array (axial,radial) of normalized form factors, correlated to the radial boundaries. (dimensionless).
    ! nr            - number of radial mesh points
    ! na            - number of axial nodes + 1
    ! rrev          - fuel mesh point radii array (in)
    ! por000        - fractional fuel pellet porosity
    ! j             - axial node indicator (axial node + 1)
    ! EOSNodeburnup - end of step burnup for node j (MWd/mtU)
    !
    ! Output
    !
    ! brnup1        - 2-D array (axial,old(2)/new(1)) for the current volume-average burnup for the axial node (MWd/mtU)
    ! brnup3        - 3-D array (axial,radial,old/new) of local burnups, correlated to the radial boundaries (MWd/mtU).
    ! rapow         - normalized radial power profile array
    !
    ! Global
    !
    ! ntot        - total initial number of heavy metal atoms per cm^3.
    ! coU235av    - axial array of volume average concentration of u-235 (atoms per cm^3).
    ! coU238av    - axial array of volume average concentration of u-238 (atoms per cm^3)
    ! coPu239av   - axial array of volume average concentration of pu-239 (atoms per cm^3)
    ! coPu240av   - axial array of volume average concentration of pu-240 (atoms per cm^3)
    ! coPu241av   - axial array of volume average concentration of pu-241 (atoms per cm^3)
    ! coPu242av   - axial array of volume average concentration of pu-242 (atoms per cm^3)
    ! conU235     - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of u-235 (atoms per cm^3)
    ! conU238     - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of u-238 (atoms per cm^3)
    ! conPu239    - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-239 (atoms per cm^3)
    ! conPu240    - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-240 (atoms per cm^3)
    ! conPu241    - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-241 (atoms per cm^3)
    ! conPu242    - three dimensional array (axial,radial,old/new) of boundary-specific concentrations of pu-242 (atoms per cm^3)
    !
    ! Internal
    !
    ! dburn  - burnup increment (MWd/mtU)
    ! lschni - j - 1
    !
    REAL(r8k) :: qnodeadj, dburn, gadolin, rnorm, burnup, rpow
    INTEGER(ipk) :: itime, i
    ! inital variables
    ! itime set to number not = to 1 to avoid re-initialization
    itime = -1000
    ! axial node indicator for concentration arrays
    lschni = j - 1
    ! set old values
    brnup1 (lschni,2) = brnup1 (lschni,1)
    DO i = 1, nr
        brnup3 (lschni,i,2) = brnup3 (lschni,i,1)
        !
        conU235 (lschni,i,2) = conU235 (lschni,i,1)
        conU238 (lschni,i,2) = conU238 (lschni,i,1)
        !
        conPu239 (lschni,i,2) = conPu239 (lschni,i,1)
        conPu240 (lschni,i,2) = conPu240 (lschni,i,1)
        conPu241 (lschni,i,2) = conPu241 (lschni,i,1)
        conPu242 (lschni,i,2) = conPu242 (lschni,i,1)
    END DO
    ! new values
    ! --- new burn-up
    brnup1(lschni,1) = EOSNodeburnup(j-1)
    dburn = brnup1(lschni,1) - brnup1(lschni,2)
    IF (dburn < 0.0_r8k) THEN
        WRITE (ounit,*)'negative burnup in turbo', dburn
        STOP
    END IF
    !
    DO i = 1, nr
        brnup3(lschni,i,1) = brnup3(lschni,i,2) + dburn * formf(lschni,i)
    END DO
    !
    CALL tubrnp(itime)
    !
    qnodeadj = qnode(j-1) / (1.0_r8k - modheat)
    wimsburnup(j-1) = oldwimsburnup(j-1) + (EOSNodeburnup(j-1) - BOSNodeburnup(j-1)) / 1000.0_r8k * 5.73_r8k / qnodeadj
    IF(wimsburnup(j-1) > 60.0_r8k) wimsburnup(j-1) = 60.0_r8k
    oldwimsburnup(j-1) = wimsburnup(j-1)
    ! converts formf to rapow array
    DO i = 1, nr
        rapow(nr-(i-1),j-1) = formf(lschni,i)
        IF (gadoln(j-1) > 0.0_r8k .AND. EOSNodeburnup(j-1) < 7000.0_r8k) THEN
            gadolin = gadoln(j-1) * 100.0_r8k
            rnorm = rrev(i,j-1) / rrev(nr,j-1)
            burnup = wimsburnup(j-1)
            CALL Gdradpow(rprm1, rpow, gadolin, rnorm, burnup)
            rapow(nr-(i-1),j-1) = rpow
        END IF
    END DO
    !
    END SUBROUTINE turbo
    !
    !
    !
    SUBROUTINE tubrnp (itime)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon
    USE variables_frapcon, ONLY : brnup3, formf, ounit, rrev, nr, j, rprm1
    USE Functions_frapcon, ONLY : bes2
    IMPLICIT NONE
    !>@brief
    !> Subroutine tubrnp (transuranus burn-up equations) calculates the Pu build-up and the radial power 
    !> and burn-up profiles for LWR conditions. It was developed off of the transuranus code system.
    !>@author
    !> created by k. lassmann (jan 25, 1993)
    !> modified on sept 19, 95 to interface with frapcon
    !> cognizant engineers:dd lanning, kl davis
    !
    ! Input
    !
    ! ounit   - Output file unit #
    !
    ! Output
    !
    ! formf   - Neutron flux profile
    !
    ! Internal
    !
    ! rhofue             - theoretical density (g/cm3)
    ! molwt              - molecular weight of UO2 (grams/mol)
    ! pres(i)            - concentration pu-239 due to absorption in u-238 resonances
    ! bes2 (arg,0,0)     - modified Bessel Function of the first kind of order zero
    ! bes2 (arg,1,0)     - modified Bessel Function of the first kind of order one
    ! bes2 (arg,0,1)     - modified Bessel Function of the second kind of order zero
    ! bes2 (arg,1,1)     - modified Bessel Function of the second kind of order one
    ! sf235...sf242      - Temperature corrected, gross spectrum averaged fission cross sections in barns (1e-24 cm^2)
    ! sc235..sc242       - Capture cross sections
    ! sa235..sa242       - Absorption cross sections
    ! n235..n242         - Initial concentrations, atoms/cm^3
    ! pres(radial,axial) - Incremental change in Pu-239 and U-238 due to resonance capture
    ! phibu              - Conversion constant connecting the derivative of concentration
    !                      burnup to the fission or capture rates.  (the constant "a" in reference paper)
    ! kappa              - Inverse diffusion length for neutrons (cm^-1)
    ! winner,wouter      - Weight factors used to calculate volume-averaged v
    !                      from the inner/outer boundary values of various parameters. These factors are derived
    !                      directly from the definition of volume-average across a radial ring, assuming the
    !                      parameter varies linearly with radial position.
    !
    INTEGER(ipk) :: i, ier
    INTEGER(ipk), INTENT(IN) :: itime
    REAL(r8k) :: rhofue, kappav, n235, n238, n239, n240, n241, n242, sf235, sf238, sf239, sf240, sf241, &
      &     sf242, sc235, sc238, sc239, sc240, sc241, sc242, sa235, sa238, sa239, sa240, sa241, sa242, dbu, phibu, &
      &     dsa238, dsc238, rfi, rfo, sump, rinner, presi, router, winner, wouter, preso, areai, areaf, fsc238, fsa238, &
      &     u235av, u238av, p239av, p240av, p241av, p242av, u235i, u238i, p239i, p240i, p241i, p242i, u235o, u238o, &
      &      p239o, p240o, p241o, p242o, sstot, satot, diff, arg, ari, sftot, formfq, formfi, formfo, anorm, molwt, dummy
    REAL(r8k), PARAMETER :: parma1 = 3.45_r8k
    REAL(r8k), PARAMETER :: parma2 = 3.0000_r8k
    REAL(r8k), PARAMETER :: parma3 = 300.0_r8k
    REAL(r8k), PARAMETER :: exfac1 = 0.45_r8k
    REAL(r8k), DIMENSION((na-1), nr) :: pres
    !
    rhofue = MatProp ('FUEL', 'TDENSITY', dummy)
    molwt = MatProp ('FUEL', 'MWT', dummy)
    ! Set fission and capture cross sections for LWR or HWR
    IF (rprm1 > 3.0_r8k) THEN ! LWR
        sf235 = 41.5_r8k
        sf238 = 0.00_r8k
        sf239 = 105.0_r8k
        sf240 = 0.584_r8k
        sf241 = 120.0_r8k
        sf242 = 0.458_r8k
        !
        sc235 = 9.7_r8k
        sc238 = 0.78_r8k
        sc239 = 58.6_r8k
        sc240 = 100.0_r8k
        sc241 = 50.0_r8k
        sc242 = 80.0_r8k
    ELSE ! HWR
        sf235 = 107.9_r8k
        sf238 = 0.00_r8k
        sf239 = 239.18_r8k
        sf240 = 0.304_r8k
        sf241 = 296.950_r8k
        sf242 = 0.191_r8k
        !
        sc235 = 22.3_r8k
        sc238 = 1.16_r8k
        sc239 = 125.36_r8k
        sc240 = 127.26_r8k
        sc241 = 122.41_r8k
        sc242 = 91.30_r8k
    END IF
    ! Absorption is sum of fission and capture cross sections
    sa235 = sf235 + sc235
    sa238 = sf238 + sc238
    sa239 = sf239 + sc239
    sa240 = sf240 + sc240
    sa241 = sf241 + sc241
    sa242 = sf242 + sc242
    ! initial conditions
    IF (itime == 1) THEN
        ! --- number of heavy metal atoms per cm**3 of uo2
        ntot = (1.0_r8k - por000) * rhofue / (molwt) * Avogadro
        ! --- number of uranium and plutonium atoms per cm**3
        n235 = ntot * enriU235(lschni)
        !
        n239 = ntot * enriPu239(lschni)
        n240 = ntot * enriPu240(lschni)
        n241 = ntot * enriPu241(lschni)
        n242 = ntot * enriPu242(lschni)
        !
        n238 = ntot - n235 - n239 - n240 - n241 - n242
        !
        DO i = 1, nr
             conU235 (lschni,i,1) = n235
             conU238 (lschni,i,1) = n238
             !
             conPu239 (lschni,i,1) = n239
             conPu240 (lschni,i,1) = n240
             conPu241 (lschni,i,1) = n241
             conPu242 (lschni,i,1) = n242
             !
             conU235 (lschni,i,2) = n235
             conU238 (lschni,i,2) = n238
             !
             conPu239 (lschni,i,2) = n239
             conPu240 (lschni,i,2) = n240
             conPu241 (lschni,i,2) = n241
             conPu242 (lschni,i,2) = n242
        END DO
        !
        coU235av (lschni) = n235
        coU238av (lschni) = n238
        !
        coPu239av (lschni) = n239
        coPu240av (lschni) = n240
        coPu241av (lschni) = n241
        coPu242av (lschni) = n242
    END IF
    ! --- average burn-up increment, mwd / tom
    dbu = (brnup1 (lschni,1) - brnup1 (lschni,2))
    !
    n235 = coU235av (lschni)
    n238 = coU238av (lschni)
    n239 = coPu239av (lschni)
    n241 = coPu241av (lschni)
    ! --- conversion factor
    phibu = rhofue / (3.8e-16_r8k * (sf235 * n235 + sf239 * n239 + sf241 * n241)) * 0.8815_r8k
    ! --- average increment of u-238 due to absorption (absolute)
    dsa238 = + sa238 * n238
    ! --- average increment of u-238 due to capture (absolute)
    dsc238 = + sc238 * n238
    ! --- inner and outer fuel radius, mm
    rfi = rrev(1,j-1) * intomm
    rfo = rrev(nr,j-1) * intomm
    IF (rfi < 1.e-05_r8k) rfi = 0.0_r8k
    ! local increments of u-238 and pu-239 concentration due to resonance capture
    sump = 0.0_r8k
    !
    rinner = rrev(1,j-1) * intomm
    ! --- determination of inner local quantity
    presi = 1.0_r8k + rprm1 * EXP(-parma2 * ((rfo - rinner) ** exfac1))
    pres (lschni,1) = presi
    !
    DO i = 1, (nr - 1)
        rinner = rrev(i,j-1) * intomm
        router = rrev(i+1,j-1) * intomm
        winner = (router + 2.0_r8k * rinner) / (3.0_r8k * (router + rinner))
        wouter = 1.0_r8k - winner
        ! --- determination of outer local quantity
        preso = 1.0_r8k + rprm1 * EXP(-parma2 * ((rfo - router) ** exfac1))
        pres(lschni,i+1) = preso
        areai = (router ** 2) - (rinner ** 2)
        sump = sump + (winner * presi + wouter * preso) * areai
        ! --- redefinition of inner quantity
        presi  = preso
    END DO
    ! --- area fuel
    areaf = (rfo ** 2) - (rfi ** 2)
    sump = sump / areaf
    ! --- normalising factors
    fsc238 = dsc238 / sump
    fsa238 = dsa238 / sump
    ! increment of u- and pu-concentrations
    DO i = 1, nr
        ! --- local burn-up increment, mwd / tom
        dbu  = brnup3 (lschni,i,1) - brnup3 (lschni,i,2)
        ! Uranium
        n235 = conU235 (lschni,i,2)
        n238 = conU238 (lschni,i,2)
        ! Plutonium
        n239 = conPu239 (lschni,i,2)
        n240 = conPu240 (lschni,i,2)
        n241 = conPu241 (lschni,i,2)
        n242 = conPu242 (lschni,i,2)
        ! --- u-235
        conU235 (lschni,i,1) = n235 - sa235 * n235 * phibu * dbu
        ! --- u-238
        conU238 (lschni,i,1) = n238 - fsa238 * pres(lschni,i) * phibu * dbu
        ! --- pu-239
        conPu239 (lschni,i,1) = n239 + (fsc238 * pres(lschni,i) - sa239 * n239) * phibu * dbu
        ! --- pu-240
        conPu240 (lschni,i,1) = n240 + (sc239 * n239 - sa240 * n240) * phibu * dbu
        ! --- pu-241
        conPu241 (lschni,i,1) = n241 + (sc240 * n240 - sa241 * n241) * phibu * dbu
        ! --- pu-242
        conPu242 (lschni,i,1) = n242 + (sc241 * n241 - sa242 * n242) * phibu * dbu
        !
        IF (conU235 (lschni,i,1) < 0.0_r8k) conU235 (lschni,i,1) = 0.0_r8k
        IF (conU238 (lschni,i,1) < 0.0_r8k) conU238 (lschni,i,1) = 0.0_r8k
        IF (conPu239 (lschni,i,1) < 0.0_r8k) conPu239 (lschni,i,1) = 0.0_r8k
        IF (conPu240 (lschni,i,1) < 0.0_r8k) conPu240 (lschni,i,1) = 0.0_r8k
        IF (conPu241 (lschni,i,1) < 0.0_r8k) conPu241 (lschni,i,1) = 0.0_r8k
        IF (conPu242 (lschni,i,1) < 0.0_r8k) conPu242 (lschni,i,1) = 0.0_r8k
    END DO
    ! determination of the average concentrations u-235 and pu-239 in section or slice l
    u235av = 0.0_r8k
    u238av = 0.0_r8k
    !
    p239av = 0.0_r8k
    p240av = 0.0_r8k
    p241av = 0.0_r8k
    p242av = 0.0_r8k
    ! --- determination of inner local quantities
    u235i = conU235 (lschni,1,1)
    u238i = conU238 (lschni,1,1)
    !
    p239i = conPu239 (lschni,1,1)
    p240i = conPu240 (lschni,1,1)
    p241i = conPu241 (lschni,1,1)
    p242i = conPu242 (lschni,1,1)
    !
    DO i = 1, (nr - 1)
        rinner = rrev(i,j-1) * intomm
        router = rrev(i+1,j-1) * intomm
        winner = (router + 2.0_r8k * rinner) / (3.0_r8k * (router + rinner))
        wouter = 1.0 - winner
        areai  = (router ** 2) - (rinner ** 2)
        ! --- determination of outer local quantities
        u235o  = conU235 (lschni,i+1,1)
        u238o  = conU238 (lschni,i+1,1)
        !
        p239o  = conPu239 (lschni,i+1,1)
        p240o  = conPu240 (lschni,i+1,1)
        p241o  = conPu241 (lschni,i+1,1)
        p242o  = conPu242 (lschni,i+1,1)
        !
        u235av = u235av + (winner * u235i + wouter * u235o) * areai
        u238av = u238av + (winner * u238i + wouter * u238o) * areai
        !
        p239av = p239av + (winner * p239i + wouter * p239o) * areai
        p240av = p240av + (winner * p240i + wouter * p240o) * areai
        p241av = p241av + (winner * p241i + wouter * p241o) * areai
        p242av = p242av + (winner * p242i + wouter * p242o) * areai
        ! --- redefinition of inner quantities
        u235i = u235o
        u238i = u238o
        !
        p239i = p239o
        p240i = p240o
        p241i = p241o
        p242i = p242o
    END DO
    !
    coU235av (lschni) = u235av / areaf
    coU238av (lschni) = u238av / areaf
    !
    coPu239av (lschni) = p239av / areaf
    coPu240av (lschni) = p240av / areaf
    coPu241av (lschni) = p241av / areaf
    coPu242av (lschni) = p242av / areaf
    !
    !     ==================================================================
    !        determination of the radial neutron flux profile
    !             (of relevance only at low burn-up)
    !
    !     --- determination of the inverse diffusion length
    !
    sstot = parma3 * ntot * 1.0e-24_r8k
    !
    satot = (sa235 * coU235av (lschni) + sa238 * coU238av (lschni) + sa239 * coPu239av (lschni) &
      &    + sa240 * coPu240av (lschni) + sa241 * coPu241av (lschni) + sa242 * coPu242av (lschni)) * 1.0e-24_r8k
    !
    diff = 1.0_r8k / (3.0_r8k * sstot)
    !
    !     --- average inverse diffusion length
    !
    kappav = SQRT(satot / diff)
    !
    !     ==================================================================
    !           radial neutron flux distribution, unnormalized
    !     ==================================================================
    IF (rfi == 0.0_r8k) THEN ! --- solid cylinder
        DO i = 1, nr
            arg = kappav * rrev(i,j-1) * intocm
            ! --- neutron flux profile according to bessel Function
            formf (lschni,i) = bes2 (arg,0,0,ier)
            IF (ier /= 0) THEN
                WRITE (ounit,200)
                WRITE (ounit,201) kappav, rrev(i,j-1) * intomm, i
            END IF
        END DO
    ELSE ! --- hollow cylinder
        ari = kappav * rfi * 0.1_r8k
        DO i = 1, nr
            arg = kappav * rrev(i,j-1) * intocm
            ! --- neutron flux profile according to bessel functions
            formf (lschni,i) = bes2 (arg,0,0,ier) + bes2 (ari,1,0,ier) / bes2 (ari,1,1,ier) * bes2 (arg,0,1,ier)
            IF (ier /= 0) THEN
                WRITE (ounit,202)
                WRITE (ounit,203) kappav, rfi, rrev(i,j-1) * intomm, i
            END IF
        END DO
    END IF
    ! FormFactor
    !  --- formfactor, unnormalized
    DO i = 1, nr
        sftot  = sf235 * conU235 (lschni,i,1) + sf238 * conU238 (lschni,i,1) + sf239 * conPu239 (lschni,i,1) &
          &    + sf240 * conPu240 (lschni,i,1) + sf241 * conPu241 (lschni,i,1) + sf242 * conPu242 (lschni,i,1)
        formf (lschni,i) = formf (lschni,i) * sftot * 1.0e-24_r8k
    END DO
    ! Store power fractions from each nuclide
    ! Total volume averaged power
    sftot = sf235 * coU235av(lschni) + sf238 * coU238av(lschni) + sf239 * coPu239av(lschni) &
      &    + sf240 * coPu240av(lschni) + sf241 * coPu241av(lschni) + sf242 * coPu242av(lschni)
    ! Uranium
    pfU235(lschni,it) = sf235 * coU235av(lschni) / sftot
    pfU238(lschni,it) = sf238 * coU238av(lschni) / sftot
    ! Plutonium
    pfPu239(lschni,it) = sf239 * coPu239av(lschni) / sftot
    pfPu240(lschni,it) = sf240 * coPu240av(lschni) / sftot
    pfPu241(lschni,it) = sf241 * coPu241av(lschni) / sftot
    pfPu242(lschni,it) = sf242 * coPu242av(lschni) / sftot
    !
    ! --- normalization factor
    formfq = 0.0_r8k
    ! --- determination of inner local quantity
    formfi = formf (lschni,1)
    DO i = 1, (nr - 1)
        rinner = rrev(i,j-1) * intomm
        router = rrev(i+1,j-1) * intomm
        winner = (router + 2.0_r8k * rinner) / (3.0_r8k * (router + rinner))
        wouter = 1.0_r8k - winner
        ! --- determination of outer local quantity
        formfo = formf (lschni,i+1)
        areai  = (router ** 2) - (rinner ** 2)
        formfq = formfq + (winner * formfi + wouter * formfo) * areai
        ! --- redefinition of inner quantity
        formfi = formfo
    END DO
    !
    formfq = formfq / areaf
    anorm = 1.0_r8k / formfq
    ! --- formfactor, normalized
    DO i = 1, nr
        formf (lschni,i) = formf (lschni,i) * anorm
    END DO
    !
200 FORMAT(/' w a r n i n g    from Subroutine tubrnp '/ &
      &     ' ======================================= '// &
      &     ' argument of bessel Function i0 is negative;', &
      &     ' form factor is set to 1')
    !
201 FORMAT(/' kappav           = ',e13.5/' rrev(i,j-1)*25.4 = ',e13.5/' i                = ',i5   //)
    !
202 FORMAT(/' w a r n i n g    from Subroutine tubrnp'/ &
      &     ' ======================================='// &
      &     ' argument of one of the besselfunctions is negative;', &
      &     ' form factor is set to 1')
    !
203 FORMAT(/' kappav           = ',e13.5/' rfi              = ',e13.5/ &
      &     ' rrev(i,j-1)*25.4 = ',e13.5/' i                = ',i5   //)
    !
    END SUBROUTINE tubrnp
    !
END MODULE Burn_frapcon



