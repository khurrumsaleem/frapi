MODULE Ballooning
    !>@brief
    !> This module contains the subroutines used to calculate the cladding ballooning.
    !> Subroutines include balon2 and far1
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    USE Kinds
    USE zrmodels_fraptran, ONLY : cmlimt, ckmn, caniso, caneal, cstrni
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    !
    SUBROUTINE balon2 (ftemp, strnl, strnr, strna, strnc, rad, twall, ctemp, delz, fcp, &
      &                frp, fap, acd, aad, ard, fnck, fncn, cwkf, cwnf, stemp, htcs, tstm, rtemp, acs, &
      &                aas, ars, ace, aae, are, dbyz, stresf, tstres, disp, deh, dea, rstran, rave, &
      &                stresr, stresa, radc, dz0, gadolinia)
    USE Kinds
    USE conversions_fraptran
    USE functions_fraptran, ONLY : polate, simq
    USE variables_fraptran
    USE Material_Properties, ONLY : MatProperty
    USE NCGases, ONLY : ngases
    IMPLICIT NONE 
    !>@brief
    !> the baloon Subroutine computes non symmetric cladding deformation
    !>@author
    !> coded by j. a. dearien
    !> modified by d. l. hagrman december 1980
    !
    ! Input
    !
    ! htcbal     - cladding surface heat transfer coefficient (btu/s*(ft**2)*F))
    ! tbkbal     - bulk coolant temperature (degrees F)
    ! tc0bal     - cladding average temperature when baloon is first called (degrees F).  used only when nbncal = 0
    ! tf0bal     - fuel surface temperature when baloon is first called (degrees F).  used only when nbncal = 0.
    ! qbal       - rod heat flux (btu/(s*(ft**2))).  used only when modbal = 0 and nbncal = 0
    ! rfbal      - hot radius of fuel pellet (in)
    ! r0bal      - initial (undeformed) cladding outside radius (in)
    ! h0bal      - initial (undeformed) cladding wall thickness (in)
    ! ftemp(k,j) = fuel surface temperature at node k,j at midpoint of time step dtbal (degrees F).
    !              Used only when modbal = 1 and nbncal = 1
    ! qlbal      - rod heat flux at node k,j (btu/(s*(ft**2))).
    ! stemp(k,j) = shroud surface temperature at node k,j at midpoint of time step dtbal (degrees F).
    !              Used only when modbal = 0 and nbncal = 1
    ! pcbal      - coolant pressure (psi)
    ! psbal      - fuel rod pressure (psi)
    ! rmpbal     - uniformly expanded cladding midplane radius when baloon is first called (in).  used only when nbncal equals 0
    ! flxbal     - fast neutron flux (neutrons/((m**2)*s))
    ! fabal      - additional axial force applied to cladding by constraints (pounds)
    ! kbaln      - frat-t index of ballooning node only when modbal = 1
    ! zbaln      - length of FrapTran node (inches).  used only when nbncal equals 0.
    ! nbncal     - Initialization flag
    !          0 = causes baloon to initilize itself
    !          1 = no initialization
    ! modbal     - switch which determines method of finding cladding temperatures and annealing.
    !          0 = causes cladding temperatures to be calculated using constant fuel surface heat flux assumption.
    !          1 = causes cladding temperatures to be calculated using constant fuel surface temperature assumption.
    !          2 = causes cladding temperatures and pressures to be taken from a table read in during initialization.
    !          3 = causes cladding temperatures to be constant during the entire frap - t time step.  
    !              Unlike modbals 0 to 2 cladding annealing is not considered internally in this mode.
    ! nprntb     - switch which determines the amount of information printed from within the subroutine
    !          0 = no print out unless the time step from frat-t was divided into more than 300 sub-steps
    !          1 = prints sub-step number, substep duration (s), average midwall radius at each axial sub-node (m), &
    !              average wall thickness at each axial sub-node (m), value of kntbal, cladding failure location, 
    !              cladding tangential stress at failure (Pa)
    !          2 = in addition to the information printed when nprntb = 1 values of the following arguments are printed 
    !              for each time: ctemp(k,j) in kelvins, stresf(k,j) in n/(m**2), rad(k,j) in meters, twall(k,j) in meters,
    !              delz(k,j) in meters
    !          3 = in addition to the information printed when nprntb = 2 values of the following arguments are printed
    !              for each time step: cwnf(k,j), cwkf(k,j)
    !          4 = in addition to the information printed when nprntb = 3 values of the following arguments are printed
    !              for each time step: fncn(k,j) & fnck(k,j), both in neutrons/(m**2)
    ! gmix(l)    - mole fractions of the gas components found in the gap (unitless).  used only for modbal equal to 1.
    !          l =  1 is helium
    !          l =  2 is argon
    !          l =  3 is krypton
    !          l =  4 is xenon
    !          l =  5 is hydrogen
    !          l =  6 is nitrogen
    !          l =  7 is oxygen
    !          l =  8 is carbon monoxide
    !          l =  9 is carbon dioxide
    !          l = 10 is water vapor
    ! tgbal      - gap gas temperature (degrees F)
    ! timbal     - time at start of time step.  used  only with cladding temperature and time table of modbal = 2 (s).
    ! dtbal      - frap time step size (s)
    ! v          - gas volume of swelling node at start of time step (inches**3)
    ! delz(k,j)  - axial length of node at k,j at start of time step (inches)
    !
    ! The following are used only when nbncal = 1
    !
    ! twall(k,j) - wall thickness at node k,j at start of time step (inches)
    ! rad(k,j)   - midwall radius of cladding at node k,j at start of time step (inches)
    ! strnc(k,j) - tangential component of true plastic strain at node k,j at start of time step (unitless)
    ! strna(k,j) - axial component of true plastic strain at node k,j at start of time step (unitless)
    ! strnr(k,j) - radial component of true plastic strain at node k,j at start of time step (unitless)
    ! strnl(k,j) - effective true plastic strain at node k,j at start of time step (unitless)
    ! ctemp(k,j) - cladding temperature at node k,j at midpoint of previous time step
    ! kntbal     - fuel - cladding contact index at end of previous time step
    !          0 = no contact
    !          1 = contact due to bowing
    ! emwbal     - energy generated by cladding oxidation (w/m)
    !
    ! chstrs - maximum hoop stress in balloon region (psi)
    ! frbal  - balon2 circular/radial model switch
    ! pmxbal - balon2 maximum gap pressure (Pa)
    ! r8bal  - balon2 average radius at balon2 axial node 8 (m)
    ! tcebal - maximum circumferential strain in balloon region
    ! jmnbal - balon2 radial contact node
    ! kntbal - 1 if balon2 predicts pellet-clad contact
    !
    !              far1 variables
    ! pdrato - bundle pitch to rod diameter ratio
    ! rnbnt  - ratio of balloonable cells to total cells
    ! totnb  - total number of cells in a bundle
    ! nodprm - number flow area reduction subnodes
    ! farbal - flow area reduction   (m**2/m**2)
    ! sdfar  - standard deviation of flow area reduction
    ! zfarbl - axial location flow area reduction subnodes  (m)
    ! tm1bal - clad temperature at lower node (k)
    ! tp1bal - clad temperature at upper node (k)
    ! ztmax  - elevation of ballooning node (m)
    ! zm1bal - elevation of lower node (m)
    ! zp1bal - elevation of upper node (m)
    ! zndbal - total length of fuel rod (m)
    !
    ! nradsh - number of flow shroud temp. vs Time input for radiation
    ! tshrda - array of f.s. temp vs Time pairs
    !
    ! EffFastFluStrenCoef   - effective fast fluence for strength coefficient (n/m**2)
    ! EffFastFluStrnHardExp - effective fast fluence for strain hardening exponent (n/m**2)
    ! EffColdWkStrenCoef    - effective cold work for strength coefficient (a0-a)/a0
    ! EffColdWkStrnHardExp  - effective cold work for strain hardening exponent (a0-a)/a
    !
    ! OxygenConcenAve - average oxygen concentration in beta (wt. fraction)
    ! daoxfr          - average oxygen concentration in alpha (wt. fraction)
    ! OxygenUptake    - total oxygen uptake (kg/m**2)
    ! OxiPowerGen     - heat generated by zr-h2o reaction  (W/m)
    ! dtmpcl          - maximum circumferential variation in cladding temperature (K)
    ! kaxmtp          - axial node number
    !
    ! ksub(i)         - number of subcodes used within the i th axial node of frap (unitless)
    ! dz0(i,k)        - initial length of k th sub-node of the i th axial node of frap (m)
    ! dtobal(i)       - previous time step size (s)
    ! The following are dimensioned (axial node, balloon axial sub-node, balloon azimuthal sub-node)
    ! fcp(i,k,j)      - volume weighted average COSine of the angle between cladding basal poles and the tangential direction
    ! frp(i,k,j)      - volume weighted average COSine of the angle between cladding basal poles and the radial direction
    ! fap(i,k,j)      - volume weighted average COSine of the angle between cladding basal poles and the axial direction
    ! acd(i,k,j)      - high temperature strain axisotropy coeffecient
    ! aad(i,k,j)      - high temperature strain axisotropy coeffecient
    ! ard(i,k,j)      - high temperature strain axisotropy coeffecient
    !
    ! Output
    !
    ! ifbaln     - Failure index (0 = No failure, 1 = Failure)
    ! chstrs     - Maximum hoop stress in the node considered during the current call (psi)
    ! kntbal     - fuel - cladding contact index (0 = No contact, 1 = Contact due to bowing)
    ! ctemp(k,j) = cladding temperature at axial node k, circumferential node j at midpoint of time step dtbal (degrees F).
    !              Nodes k and j are determined by baloon initializing routine
    ! strnl(k,j) = effective true plastic strain at node k,j at End of time step (unitless)
    ! strnr(k,j) = radial component of true plastic strain at node k,j at End of time step (unitless)
    ! strna(k,j) = axial component of true plastic strain at node k,j at End of time step (unitless)
    ! strnc(k,j) = tangential compokent of true plastic strain at node k,j at End of time step (unitless)
    ! rad(k,j)   - midwall radius of cladding at node k,j at end of time step (inches)
    ! twall(k,j) = wall thickness at node k,j at end of time step (inches)
    ! delz(k,j)  - axial length of node at k,j at end of time step (inches)
    ! v          - gas volume of swelling node at end of time step (inches**3)
    ! timbal     - time at end of time step (s)
    ! dtbal      - time step size of last internal time step (s)
    !
    ! Note:
    !
    ! (1) Contrary to the following comments all balon2 variables are in si except for chstrs which is converted to psi in comput
    ! (2) See Subroutine listing for relation between acd, aad, ard, strain components and effective strain
    ! (3) Varible an modified for small strains option in cstrni
    !
    ! Reference:
    !
    ! (1) j. m. kramer and l. w. deitrich, cladding failure by local plastic instability, anl-77-95 (december 1977)
    ! (2) l. j. siefken, et. al., frap-t5 a computer code for the transient analysis of oxide fuel rods, 
    !     tree-1281 and nureg/cr-0840  (june 1979)
    !
    INTEGER(ipk) :: ksub, m1, nd, nj, nj1, j, k, nk, nstep, m, jmin, neq, kerr
    INTEGER(ipk), PARAMETER :: MaxIterations = 600_ipk
    REAL(r8k) :: caxrad, caxstr, cinrad, cinwid, deltmp,fax, gtemp, PressIn, PressOut, ctemp0, hcon, f, &
      &          rai, rbar, rhtr, rout, tcl0, tfl0, tshrdf, tshrdk, w0, zblnn, delto, ctmpdf, eps, conf, &
      &          dtime, cdens,zbend, sq, cdpres, t, r, twala, ri, ro, delt, timax, ak, an, am, anl, &
      &          strncx, titest, dtcr, rdosum, rdisum, sum, hgave, hnom, rdoave, rdiave, radpo2, gpthk, &
      &          conc, acb, acbd, acbo, acbdo, rado, radi, anuh, anug, akn, acbr, acbr1, acbr2, acbr3, &
      &          ak1, ak2, ak3, ak4, phi, deltri, phi2, phi3, phi4, ftsum, ftave, deladj, &
      &          cvt, ai, ao, vc, htcg, htcr, aave, conj, conjp1, econdj, conjm1, rstrn1, &
      &          conc1, tstart, tave, tce, dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9, dum10, &
      &          dum11, ctstrt, dummyk, dummyn, dummym, dummye,strnll, dep, der, r1, r2,tr1, tr2, rmin, &
      &          radh1, radhl, radh, rsml, dspmx, dispa, d
    REAL(r8k), INTENT(IN) :: gadolinia
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), DIMENSION(5) :: b
    REAL(r8k), DIMENSION(16) :: dz0, radc, rave, stresr, stresa, ctempa
    REAL(r8k), DIMENSION(25) :: c
    REAL(r8k), DIMENSION(16,16) :: acs, aas, ars, ace, aae, are, dbyz, stresf, tstres, disp, deh, ctemp, &
      &                            strnl, strnr, strna, strnc, rad, ftemp, twall, delz, tstm, rstran, &
      &                            fcp, frp, fap, acd, aad, ard, fnck, fncn, cwnf, cwkf, dea, htcs, &
      &                            stemp, rtemp
    !
    ! the following are dummy arguments for cmlimt
    cinwid = 1.2E-4_r8k
    cinrad = 1.0E-2_r8k
    caxrad = 1.0E3_r8k
    caxstr = 1.0E6_r8k
    deltmp = 50.0_r8k
    ! number of axial subnodes set to 16 for FrapTran link
    ksub = 16
    nk = ksub
    ! nj = number of circumferential nodes used
    nj = 16
    nd = nj / 2
    m1 = nd + 1
    nj1 = nj - 1
    ! Set the shroud temperature (F)
    IF (nradsh > 0) THEN ! User defined values
        tshrdf = polate (tshrda, timbal, nradsh)
    ELSE ! Cladding average temperature
        tshrdf = tkf (tc0bal)
    ENDIF
    ! Set values
    rhtr = rfbal  ! hot radius of fuel pellet (in)
    rout = r0bal ! initial (undeformed) cladding outside radius (in)
    w0 = h0bal ! initial (undeformed) cladding wall thickness (in)
    PressOut = pcbal ! coolant pressure (Pa)
    PressIn = psbal ! rod pressure (psi)
    fax = fabal ! additional axial force applied to cladding by constraints (pounds)
    rbar = rout - 0.5_r8k * w0 ! initial (undeformed) cladding midpoint radius (in)
    gtemp = tgbal ! gas-gap temperature (F)
    ! convert input required only for initialization and find axial sub-node lengths
    IF (nbncal == 0) THEN
        tcl0 = tc0bal ! cladding average temperature, F
        tfl0 = tf0bal ! fuel surface temperature, F
        rai = rmpbal ! uniformly expanded cladding midplane radius, in
        zblnn = zbaln ! length of axial node. Either 0.08 (set in comput) or <= 0.3 (set in far1) ! Should be 8cm.
        r8bal = rbar ! initial (undeformed) cladding midpoint radius (in)
        pmxbal = psbal ! rod pressure (psi)
        frbal = 0.0_r8k ! frbal is the circular/radial model switch (the radial model is not used in this version of balon2)
        dz0(1:nk) = zblnn / nk ! axial node incremental height
        delto = dtbal ! timestep size (s)
        ! convert input required only for updates
        ! set nk equal to number of axial sub-nodes
    ELSE
        delto = dtobal ! previous timestep size (s)
        ctmpdf = 0.0_r8k
        IF (pmxbal < psbal) pmxbal = psbal
        IF (ctemp(1,1) < tc0bal) ctmpdf = tc0bal - ctemp(1,1)
        DO k = 1, nk
            DO j = 1, nj
                ctemp(k,j) = ctemp(k,j) + ctmpdf
            ENDDO
        ENDDO
    ENDIF
    !
    tshrdk = tfk (tshrdf) ! Shroud temperature (K)
    !
    DO k = 1, nk
        DO j = 1, nj
            ! Allow for future input of space varying parameters
            ! Currently all parameters assumed the same
            htcs(k,j) = htcbal ! Cladding surface heat transfer coefficient
            tstm(k,j) = tbkbal  ! Bulk coolant temperature
            stemp(k,j) = tshrdk ! Shroud temperature (K)
        ENDDO
    ENDDO
    ! set up quantities required for each call
    dtime = 0.0_r8k
    nstep = 0 ! Step counter
    cdens = 6.55E3_r8k ! cladding density from wcap-3269-41 (converted to kg/(m**3))
    zbend = 0.1_r8k ! zbend is the bending radius assumed to estimate cladding bending
    ! initialize assumeng a theta invarient initial geometry and temperature
    ! note no symmetry break in here yet
    IF (nbncal == 0) THEN
        ifbaln = 0 ! Baloon failure index. Set to 0 to indicate no failure
        kntbal = 0 ! Fuel/cladding contact index. Set to 0 to indicate no contact
        DO k = 1, nk ! Axial node iteration loop
            DO j = 1, nj ! Circumferential node iteration loop
                ! Set average cladding and fuel temperatures
                ctemp(k,j) = tcl0
                ftemp(k,j) = tfl0
                ! Set axial node lengths
                delz(k,j) = dz0(k)
                ! The following texture initializations are temporary and for typical cladding only the parameters should be
                ! passed into baloon when frap models the effect of deformation on anisotropy using the matrpo Subroutine caniso
                fcp(k,j) = 0.28_r8k
                frp(k,j) = 0.66_r8k
                fap(k,j) = 0.06_r8k
                acd(k,j) = 0.5_r8k
                aad(k,j) = 0.5_r8k
                ard(k,j) = 0.5_r8k
                ! Set up initial cold works and fluences
                fnck(k,j) = EffFastFluStrenCoef(kbaln)
                fncn(k,j) = EffFastFluStrnHardExp(kbaln)
                cwkf(k,j) = EffColdWkStrenCoef(kbaln)
                cwnf(k,j) = EffColdWkStrnHardExp(kbaln)
                ! Set up initial strains, Dimensions
                strnc(k,j) = MAX(LOG(rai / (rout - 0.5_r8k * w0)), 1.0E-6_r8k)
                strna(k,j) = 0.0_r8k
                strnr(k,j) = -strnc(k,j)
                rad(k,j) = rai
                twall(k,j) = w0 * EXP(strnr(k,j))
                ! Calculate cladding anisotropy
                CALL caniso (strnc(k,j), strna(k,j), strnr(k,j), ctemp(k,j), fcp(k,j), fap(k,j), frp(k,j), acd(k,j), &
                  &          aad(k,j), ard(k,j), acs(k,j), aas(k,j), ars(k,j), ace(k,j), aae(k,j), are(k,j))
                !
                sq = ace(k,j) * ((aae(k,j) * strnc(k,j) - are(k,j) * strna(k,j)) ** 2)  + aae(k,j) * &
                  &  ((are(k,j) * strna(k,j) - ace(k,j) * strnr(k,j)) ** 2) + are(k,j) * &
                  &  ((ace(k,j) * strnr(k,j) - aae(k,j) * strnc(k,j)) ** 2)
                IF (sq < 1.0E-20_r8k) THEN
                    strnl(k,j) = 0.0_r8k
                ELSE
                    strnl(k,j) = SQRT(sq) / (ace(k,j) * aae(k,j) + aae(k,j) * are(k,j) + are(k,j) * ace(k,j))
                ENDIF
            ENDDO
        ENDDO
        tcebal = strnc(1,1) ! Maximum circumferential strain in balloon
    ENDIF
    !
    TimestepLoop: DO
        nstep = nstep + 1
        ! Repeat while less than max # of timesteps and while cladding has not failed due to ballooning
        IF (nstep >= MaxIterations .OR. ifbaln == 1) EXIT TimestepLoop
        PressIn = pmxbal ! Internal cladding pressure
        cdpres = PressIn - PressOut ! Differential cladding pressure
        ! find local stress and maximum time step size
        ! first find azmuthal radii of curvature
        DO j = 1, nj
            ! Assume top and bottom axial nodes have a virtual neighbor with a radius that preserves the slope
            dbyz(1,j) = 0.0_r8k
            dbyz(nk,j) = 0.0_r8k
            DO k = 2, (nk - 1)
                dbyz(k,j) = 8.0_r8k * ((rad(k+1,j) - rad(k,j)) / (dz0(k+1) + dz0(k)) - (rad(k,j) - rad(k-1,j)) / &
                  &         (dz0(k) + dz0(k-1))) / (dz0(k+1) + 2.0_r8k * dz0(k) + dz0(k-1))
            ENDDO
        ENDDO
        ! Find stress components
        ! Find average thickness, inside and outside radii
        DO k = 1, nk ! Axial node iteration loop
            t = 0.0_r8k
            r = 0.0_r8k
            DO j = 1, nj ! Circumferential node iteration loop
                r = r + EXP(strnc(k,j))
                t = t + twall(k,j)
            ENDDO
            rave(k) = r * rbar / nj
            twala = t / nj
            ri = rave(k) - (twala / 2.0_r8k)
            ro = rave(k) + (twala / 2.0_r8k)
            stresr(k) = -(PressIn * ri + PressOut * ro) / (ri + ro)
            stresa(k) = (PressIn * ri ** 2 - PressOut * ro ** 2 + fax / 3.14_r8k) / (ro ** 2 - ri ** 2)
            DO j = 1, nj ! Circumferential node iteration loop
                stresf(k,j) = cdpres * rave(k) / twala - rave(k) * cdpres * (twall(k,j) - twala) / (twala * twala) + &
                  &           rave(k) * stresa(k) * dbyz(k,j) / ((EXP(strna(k,j))) ** 2) - (PressIn + PressOut) / 2.0_r8k
            ENDDO
        ENDDO
        ! Determine size of possible time step subdivision
        ! Calculate cladding temperatures using constant heat flux assumption
        ! Start by determining the maximum size of the time step using new dimensions and new stress but old temperatures
        ! If outside pressure is not less than inside pressure, skip to deformation calculation
        delt = dtbal
        delto = delt
        !
        ! Only compute the following if the internal rod pressure > external pressure
        IF (cdpres > 0.0_r8k) THEN
            ! Analyze cladding conditions at all axial and circumferential nodes to determine the maximum allowable timestep size
            timax = dtbal - dtime
            IF (kntbal == 1 .AND. timax > 1.0_r8k) timax = 1.0_r8k
            DO k = 1, nk ! Axial node iteration loop
                DO j = 1, nj ! Circumferential node iteration loop
                    ! Determine cladding equation of state
                    CALL ckmn (ctemp(k,j), OxygenConcenAve(kbaln), fnck(k,j), fncn(k,j), cwkf(k,j), cwnf(k,j), rstran(k,j), &
                      &        ak, an, am)
                    !
                    ! Determine maximum allowable timestep size
                    strncx = ABS(strnc(k,j))
                    IF (stresf(k,j) >= ((strncx ** an) * ak)) THEN
                        titest = (((strncx ** an) * ak / stresf(k,j)) ** (1.0_r8k / am)) * 10.0_r8k
                        IF ((ctemp(k,j) > 1090.0_r8k) .AND. (ctemp(k,j) < 1255.0_r8k)) titest = titest * 5.0_r8k
                        IF (titest < timax) timax = titest
                    ENDIF
                ENDDO
            ENDDO
            delt = MAX(timax, 5.0E-04_r8k)
            dtcr = (delt + delto) / 2.0_r8k
            delto = delt
            !
            !
            DO k = 1, nk ! Axial node iteration loop
                ! calculate average gap conductance
                rdosum = 0.0_r8k
                rdisum = 0.0_r8k
                sum = 0.0_r8k
                DO j = 1, nj ! Circumferential node iteration loop
                    rdosum = rdosum + rad(k,j) + 0.5_r8k * twall(k,j)
                    rdisum = rdisum + rad(k,j) - 0.5_r8k * twall(k,j)
                    sum = sum + 1.0_r8k
                ENDDO
                ! hgave = average heat transfer coefficient for gap
                hgave = htcgba
                hnom = hgave
                rdoave = rdosum / sum
                rdiave = rdisum / sum
                radpo2 = 0.5_r8k * ((rad(k,8) - 0.5_r8k * twall(k,8)) + (rad(k,9) - 0.5_r8k * twall(k,9)))
                gpthk = radpo2 - rhtr
                ! Calculate the contact gap conductance.
                ! Gas mixture is assumed to be 100%he,temperature of 1000k,pressure of 2.5 MPa
                hcon = 2.5E4_r8k
                f = hcon / hnom
                ! calculate eccentricity ratio
                eps = 1.0_r8k - 1.0_r8k / f
                !
                ! calculate fuel thermal conductivity = conf
                ! use Lucuta model with zero values for burnup and Gd
                ! tfavba, Fuel surface temperature used for fuel bulk average temperature
                ! Note: why is burnup forced to 0.01 MWd/MTU, TD forced to 95% and OMRatio forced to 2?
                conf = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tfavba, Burnup=0.1_r8k, OMRatio=2.0_r8k, &
                  &                 Fraction_TD=0.95_r8k, Gadolinia=gadolinia, Pu=compmt)
                
                ! Calculate cladding thermal conductivity
                conc = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(1,1), Flux=0.0_r8k, ColdWork=coldw)
                !
                acb = (1.0_r8k + eps) * conf / (hgave * rhtr * SQRT(1.0_r8k - eps ** 2))
                acbd = (1.0_r8k + eps) / SQRT(1.0_r8k - eps ** 2)
                acbo = (1.0_r8k - eps) * conf / (hgave * rhtr * SQRT(1.0_r8k - eps ** 2))
                acbdo = (1.0_r8k - eps) / SQRT(1.0_r8k - eps ** 2)
                ! calculate fuel surface temperature at 0 and 180 degrees
                ! (update deltmp)
                rado = rdoave
                radi = rdiave
                anuh = htcbal * rado / conc
                anug = hgave * rhtr / conf
                akn = radi / rado
                acbr = (1.0_r8k + 0.5_r8k * eps) / (anug * SQRT(1.0_r8k - eps ** 2))
                acbr1 = (1.0_r8k + 0.5_r8k * eps) / SQRT(1.0_r8k - eps ** 2)
                acbr2 = (1.0_r8k - 0.5_r8k * eps) / (anug * SQRT(1.0_r8k - eps ** 2))
                acbr3 = (1.0_r8k - 0.5_r8k * eps) / SQRT(1.0_r8k - eps ** 2)
                d = hgave * akn / htcbal
                ak1 = d * anuh * (akn ** 2 + (1.0_r8k + anuh) / (1.0_r8k - anuh)) / &
                  &   (anug * ((1.0_r8k + anuh) / (1.0_r8k - anuh) - akn ** 2))
                ak2 = d * anuh * (akn ** 4 + (2.0_r8k + anuh) / (2.0_r8k - anuh)) / &
                  &   (anug * ((2.0_r8k + anuh) / (2.0_r8k - anuh) - akn ** 4))
                ak3 = d * anuh * (akn ** 6 + (3.0_r8k + anuh) / (3.0_r8k - anuh)) / &
                  &   (anug * ((3.0_r8k + anuh) / (3.0_r8k - anuh) - akn ** 6))
                ak4 = d * anuh * (akn ** 8 + (4.0_r8k + anuh) / (4.0_r8k - anuh)) / &
                  &   (anug * ((4.0_r8k + anuh) / (4.0_r8k - anuh) - akn ** 8))
                b(1) = hgave / htcbal * radi / rado - hgave * radi / conc * LOG(radi / rado) + &
                  &    0.5_r8k * hgave * rhtr / conf + acbd
                b(2) = hgave / htcbal * radi / rado - hgave * radi / conc * LOG(radi / rado) + &
                  &    0.5_r8k * hgave * rhtr / conf
                b(3) = hgave / htcbal * radi / rado - hgave * radi / conc * LOG(radi / rado) + &
                  &    0.5_r8k * hgave * rhtr / conf + acbdo
                b(4) = acbr1 - d * anuh * LOG(akn) + 0.5_r8k * anug + d
                b(5) = acbr3 - d * anuh * LOG(akn) + 0.5_r8k * anug + d
                c(1) = 1.0_r8k
                c(2) = 1.0_r8k
                c(3) = 1.0_r8k
                c(4) = 1.0_r8k
                c(5) = 1.0_r8k
                c(6) = 1.0_r8k + ak1 + acb
                c(7) = 0.0_r8k
                c(8) = -1.0_r8k - ak1 - acbo
                c(9) = 0.5_r8k + 0.5_r8k * acbr + 0.5_r8k * ak1
                c(10) = -0.5_r8k - 0.5_r8k * acbr2 - 0.5_r8k * ak1
                c(11) = 1.0_r8k + ak2 + 2.0_r8k * acb
                c(12) = -1.0_r8k - ak2 - 2.0_r8k / (anug * SQRT(1.0_r8k - eps ** 2))
                c(13) = 1.0_r8k + ak2 + 2.0_r8k * acbo
                c(14) = -0.5_r8k - acbr - 0.5_r8k * ak2
                c(15) = -0.5_r8k - acbr2 - 0.5_r8k * ak2
                c(16) = 1.0_r8k + 3.0_r8k * acb + ak3
                c(17) = 0.0_r8k
                c(18) = -1.0_r8k - 3.0_r8k * acbo - ak3
                c(19) = -1.0_r8k - 3.0_r8k * acbr - ak3
                c(20) = 1.0_r8k + 3.0_r8k * acbr2 + ak3
                c(21) = 1.0_r8k + ak4 + 4.0_r8k * acb
                c(22) = 1.0_r8k + ak4 + 4.0_r8k / (anug * SQRT(1.0_r8k - eps ** 2))
                c(23) = 1.0_r8k + ak4 + 4.0_r8k * acbo
                c(24) = -0.5_r8k - 2.0_r8k * acbr - 0.5_r8k * ak4
                c(25) = -0.5_r8k - 2.0_r8k * acbr2 - 0.5_r8k * ak4
                neq = 5
                !
                CALL simq (c, b, neq, kerr)
                ! b(1)=eta0 , b(2)=eta1 ,b(3)=eta2 ,b(4)=eta3 ,b(5)=eta4
                DO j = 1, nj
                    phi = ((j - 1) / 16.0_r8k + 1.0_r8k / 32.0_r8k) * pi
                    deltri = qbal / hgave
                    phi2 = 2.0_r8k * phi
                    phi3 = 3.0_r8k * phi
                    phi4 = 4.0_r8k * phi
                    ftemp(k,j) = tbkbal + b(1) * deltri - 0.5_r8k * qbal * rhtr / conf + deltri * (b(2) * COS(phi) + &
                      &          b(3) * COS(phi2) + b(4) * COS(phi3) + b(5) * COS(phi4))
                ENDDO
                ftsum = 0.0_r8k
                sum = 0.0_r8k
                DO j = 1, nj
                    ftsum = ftsum + ftemp(k,j)
                    sum = sum + 1.0_r8k
                ENDDO
                ftave = ftsum / sum
                ! Adjust average fuel surface temperature calculated of njsing's method
                ! So it matches average fuel surface temperatures in i-d FrapTran calculations
                deladj = ftave - tf0bal
                DO j = 1, nj
                    ftemp(k,j) = ftemp(k,j) - deladj
                    ctempa(j) = ctemp(k,j)
                ENDDO
                DO j = 1, nj
                    ! Cladding temperature
                    ctemp0 = ctemp(k,j)
                    ! Gap thickness
                    gpthk = rad(k,j) - twall(k,j) / 2.0_r8k - rhtr
                    ! Cladding specific heat (J/kg-K) * (kg/m^3) / s
                    cvt = MatProperty (Material='CLAD', Property='SPECHEAT', Temperature=ctemp0) * cdens / dtcr
                    ! Area(inside clad surface) = 2*pi*r(i)
                    ai = 2.0_r8k * pi * (rad(k,j) - twall(k,j) / 2.0_r8k)
                    ! Area(outside clad surface) = 2*pi*r(o)
                    ao = 2.0_r8k * pi * (rad(k,j) + twall(k,j) / 2.0_r8k)
                    ! Clad volume = pi*(r(o)**2 - r(i)**2) = 2*pi*rad*twall
                    vc = 2.0_r8k * pi * rad(k,j) * twall(k,j)
                    !
                    ! htcg modification is radiation heat transfer heater to cladding
                    htcg = MatProperty (Material='GAS', Property='GCONR2', Temperature=gtemp, Pressure=PressIn, &
                      &                 Width=gpthk, GasComposition=GasFraction)
                    htcg = htcg + 0.8_r8k * 5.67E-8_r8k * (ftemp(k,j) + ctemp(k,j)) * (ftemp(k,j) * ftemp(k,j) + &
                      &    ctemp(k,j) * ctemp(k,j))
                    !
                    ! Calculate the effective heat transfer coeff to shroud by radiation
                    IF (nradsh == 0) THEN
                        htcr = 0.0_r8k
                    ELSE
                        htcr = 0.4_r8k * 5.67E-8_r8k * (ctemp(k,j) + stemp(k,j)) * (ctemp(k,j) * ctemp(k,j) + &
                          &    stemp(k,j) * stemp(k,j))
                    ENDIF
                    !
                    phi = ((j - 1) / 16.0_r8k + 1.0_r8k / 32.0_r8k) * pi
                    aave = 0.5_r8k * (ai + ao)
                    IF (j == 1) THEN
                        conj = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j))
                        conjp1 = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j+1))
                        econdj = (ctempa(j) - ctempa(j+1)) / (0.5_r8k * (rad(k,j) + rad(k,j+1)) *  0.19642_r8k) * (0.5_r8k * &
                          &      (twall(k,j) + twall(k,j+1)) * 0.5_r8k * (conj + conjp1))
                    ELSE IF (j == nj) THEN
                        conjm1 = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j-1))
                        conj = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j))
                        econdj = (ctempa(j-1) - ctempa(j)) / (0.5_r8k * (rad(k,j-1) + rad(k,j)) *  0.19642_r8k) * (0.5_r8k * &
                          &      (twall(k,j-1) + twall(k,j)) * 0.5_r8k * (conjm1 + conj))    
                    ELSE
                        conjm1 = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j-1))
                        conj = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j))
                        conjp1 = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=ctemp(k,j+1))
                        econdj = (ctempa(j-1) - ctempa(j)) / (0.5_r8k * (rad(k,j-1) + rad(k,j)) *  0.19642_r8k) * (0.5_r8k * &
                          &      (twall(k,j-1) + twall(k,j)) * 0.5_r8k * (conjm1 + conj)) + (ctempa(j) - ctempa(j+1)) / (0.5_r8k * &
                          &      (rad(k,j) + rad(k,j+1)) * 0.19642_r8k) * (0.5_r8k * (twall(k,j) + twall(k,j+1)) * &
                          &      0.5_r8k * (conj + conjp1))
                    ENDIF
                    rstrn1 = MIN(rstran(k,j), 1.0_r8k)
                    ! ctemp(k,j) = (vc * cvt * ctemp(k,j) + ai * htcg * ftemp(k,j) + ao * (htcs(k,j) * tstm(k,j) &
                    ! &        + htcr * stemp(k,j)) + emwbal + econdj * (44.0_r8k / 7.0_r8k) + (44.0_r8k / 7.0_r8k) &
                    ! &        * stresf(k,j) * aave * twall(k,j) * strnc(k,j) * EXP(rstrn1)) / (vc * cvt + ai &
                    ! &        * htcg + ao * (htcs(k,j) + htcr))
                    ctemp(k,j) = (vc * cvt * ctemp(k,j) + ai * htcg * ftemp(k,j) + ao * (htcs(k,j) * tstm(k,j) + &
                      &           htcr * stemp(k,j))) / (vc * cvt + ai * htcg + ao * (htcs(k,j) + htcr))
                    conc1 = conc
                    anuh = htcbal * rado / conc1
                    anug = hgave * rhtr / conf
                    akn = radi / rado
                    rtemp(k,j) = (ctemp(k,j) - ctemp0) / dtcr
                ENDDO
            ENDDO
            !
            ! Calculate the changes in fluence and cold work
            DO k = 1, nk
                DO j = 1, nj
                    ! oxidation ignored for now
                    tstart = ctemp(k,j) - (rtemp(k,j) * dtcr)
                    CALL caneal (flxbal, dtcr, tstart, rtemp(k,j), fnck(k,j), fncn(k,j), cwkf(k,j), cwnf(k,j))
                ENDDO
            ENDDO
            !
            timbal = timbal + delt
            ! print start-of-step information, if desired
400         IF (nprntb > 0) THEN
                WRITE(ounit,905) nstep, delt, timbal, cdpres
905             FORMAT(//'                   Time step  ',i3,'      Duration',e16.5,' sec','     net =',e15.4, &
                  &      ' Pressure diff =',e15.5)
                IF (nprntb > 1) THEN
                    WRITE(ounit,917)
917                 FORMAT(/'         Temperatures during time step -- axial nodes across circumferential nodes down the table'//)
                    WRITE(ounit,906) ((ctemp(k,j), k = 1,8),j = 1,nj)
906                 FORMAT(8e16.5)
                    WRITE(ounit,907)
907                 FORMAT ('   ')
                    WRITE(ounit,906) ((ctemp(k,j), k = 9,16),j = 1,nj)
                    IF (nprntb > 2) THEN
                        WRITE(ounit,908)
908                     FORMAT(/'      tangential stress components during time step'//)
                        WRITE(ounit,906) ((stresf(k,j), k = 1,8),j = 1,nj)
                        WRITE(ounit,907)
                        WRITE(ounit,906) ((stresf(k,j), k = 9,16),j = 1,nj)
                        WRITE(ounit,913)
913                     FORMAT(/'          cold work for strength during time step  '//)
                        WRITE(ounit,906) ((cwkf(k,j), k = 1,8),j = 1,nj)
                        WRITE(ounit,907)
                        WRITE(ounit,906) ((cwkf(k,j), k = 9,16),j = 1,nj)
                        WRITE(ounit,909)
909                     FORMAT(/'          cold work for strain hardening exponent during time step '//)
                        WRITE(ounit,906) ((cwnf(k,j), k = 1,8),j = 1,nj)
                        WRITE(ounit,907)
                        WRITE(ounit,906) ((cwnf(k,j), k = 9,16),j = 1,nj)
                        IF (nprntb > 3) THEN
                            WRITE(ounit,915)
915                         FORMAT(/'          fast neutron fluence for strength during time step'//)
                            WRITE(ounit,906) ((fnck(k,j), k = 1,8),j = 1,nj)
                            WRITE(ounit,907)
                            WRITE(ounit,906) ((fnck(k,j), k = 9,16),j = 1,nj)
                            WRITE(ounit,916)
916                         FORMAT(/'          fast neutron fluence for strain hardening exponent during time step'//)
                            WRITE(ounit,906) ((fncn(k,j), k = 1,8),j = 1,nj)
                            WRITE(ounit,907)
                            WRITE(ounit,906) ((fncn(k,j), k = 9,16),j = 1,nj)
                        ENDIF
                    ENDIF
                    IF (ifbaln == 1) THEN
                        DO k = 1, nk
                            t = 0.0_r8k
                            r = 0.0_r8k
                            DO j = 1, nj
                                r = r + EXP(strnc(k,j))
                                t = t + twall(k,j)
                            ENDDO
                            rave(k) = r * rbar / nj
                            tave = t / nj
                            tce = (rave(k) - rbar) / rbar
                            WRITE(ounit,911) k,rave(k), tave, kntbal, tce
                            WRITE(ounit,912) (j,rad(k,j), twall(k,j), delz(k,j), j = 1,nj)
                        ENDDO
                    ENDIF
                ENDIF
            ENDIF
            ! Test for failure
            IF (ifbaln == 1) EXIT TimestepLoop
            DO k = 1, nk
                DO j = 1, nj
                    CALL cmlimt (ctemp(k,j), OxygenConcenAve(kbaln), fnck(k,j), fncn(k,j), cwkf(k,j), cwnf(k,j), cinwid, cinrad, &
                      &          cdpres, caxrad, caxstr, rstran(k,j), deltmp, dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, &
                      &          dum9, dum10, dum11, ctstrt, dummyk, dummyn, dummym, dummye)
                    !
                    IF (stresf(k,j) >= ctstrt) THEN
                        ifbaln = 1
                        WRITE(ounit,910) j, k, stresf(k,j), ctstrt
                    ENDIF
                    IF (nprntb > 0 .AND. k == 8 .AND. j == 4) WRITE(ounit,918) stresf(8,4), ctstrt
                ENDDO
            ENDDO
910         FORMAT('  failed due to local stress at circumferential node ',i2,' axial node ',i2,' stress =',e12.5, &
              &    ' failure stress =',e12.5)
918         FORMAT(/'      hoop stress at k=8 j=4 is = ',e16.6,'  failure at ',e16.6,' n/m**2'/)
            ! If failure is predicted then print results and call exit
            IF (ifbaln == 1) THEN
                nprntb = 2
                GO TO 400
            ENDIF
        END IF
        tcebal = 0.0_r8k
        taxbal = 0.0_r8k
        trabal = 0.0_r8k
        DO k = 1, nk
            DO j = 1, nj
                ! Call caniso for temperature effect only
                CALL caniso (zero, zero, zero, ctemp(k,j), fcp(k,j), fap(k,j), frp(k,j), acd(k,j), aad(k,j), &
                  &          ard(k,j), acs(k,j), aas(k,j), ars(k,j), ace(k,j), aae(k,j), are(k,j))
                ! Find true effective stress
                tstres(k,j) = (acs(k,j) * ((stresf(k,j) - stresa(k)) ** 2) + aas(k,j) * ((stresa(k) - &
                  &            stresr(k))**2) + ars(k,j) * ((stresr(k) - stresf(k,j)) ** 2)) ** 0.5_r8k
                ! Find deformations during time step
                ! Insert creep-down calculation here if it is desired
                strnll = strnl(k,j)
                !
                CALL cstrni (delt, ctemp(k,j), OxygenConcenAve(kbaln), fnck(k,j), fncn(k,j), cwkf(k,j), &
                  &          cwnf(k,j), tstres(k,j), strnll)
                !
                IF (strnll < strnl(k,j)) strnll = strnl(k,j)
                dep = strnll - strnl(k,j)
                rstran(k,j) = dep / delt
                deh(j,1) = dep * (ace(k,j) * (stresf(k,j) - stresa(k)) + are(k,j) * (stresf(k,j) - stresr(k))) / tstres(k,j)
                dea(j,1) = dep * (aae(k,j) * (stresa(k) - stresr(k)) + ace(k,j) * (stresa(k) - stresf(k,j))) / tstres(k,j)
                der = dep * (are(k,j) * (stresr(k) - stresf(k,j)) + aae(k,j) * (stresr(k) - stresa(k))) / tstres(k,j)
                strnc(k,j) = strnc(k,j) + deh(j,1)
                strna(k,j) = strna(k,j) + dea(j,1)
                strnr(k,j) = strnr(k,j) + der
                strnl(k,j) = strnll
                ! Call caniso for effect of deformation on texture
                CALL caniso (deh(j,1), dea(j,1), der, ctemp(k,j), fcp(k,j), fap(k,j), frp(k,j), acd(k,j), &
                  &          aad(k,j), ard(k,j), acs(k,j), aas(k,j), ars(k,j), ace(k,j), aae(k,j), are(k,j))
                ! Find end of time step Dimensions
                twall(k,j) = twall(k,j) * EXP(der)
                rad(k,j) = rad(k,j) * EXP(deh(j,1))
                delz(k,j) = delz(k,j) * EXP(dea(j,1))
            ENDDO
            t = 0.0_r8k
            r = 0.0_r8k
            r1 = 0.0_r8k
            r2 = 0.0_r8k
            DO j = 1, nj
                r = r + EXP(strnc(k,j))
                t = t + twall(k,j)
                r1 = r1 + EXP(strnr(k,j))
                r2 = r2 + EXP(strna(k,j))
            ENDDO
            rave(k) = r * rbar / nj
            tave = t / nj
            tce = (rave(k) - rbar) / rbar
            tr1 = r1 / nj
            tr2 = r2 / nj
            IF (tce > tcebal) THEN
                tcebal = tce
                trabal = tr1 - 1.0_r8k
                taxbal = tr2 - 1.0_r8k
            ENDIF
            ! Find radial displacement due to bending
            IF (kntbal == 0) THEN
                DO j = 1, nd
                    m = j + nd
                    disp(k,j) = (dea(j,1) - dea(m,1) - (strna(k,j) - strna(k,m)) * (deh(j,1) + deh(m,1)) / 2.0_r8k) * &
                      &         (zbEnd ** 2) / ((rad(k,j) + rad(k,m)) * 4.0_r8k)
                    disp(k,m) = -disp(k,j)
                ENDDO
                ! Add bending increment to radius
                DO j = 1, nd
                    m = j + nd
                    rad(k,j) = rad(k,j) + disp(k,j)
                    rmin = rhtr + (twall(k,j) / 2.0_r8k)
                    IF (rad(k,j) < rmin) THEN
                        rad(k,m) = rad(k,m) - (rmin - rad(k,j))
                        kntbal = 1
                        rad(k,j) = rmin
                    ENDIF
                ENDDO
                DO j = m1, nj
                    m = j - nd
                    rad(k,j) = rad(k,j) + disp(k,j)
                    rmin = rhtr + (twall(k,j) / 2.0_r8k)
                    IF (rad(k,j) < rmin) THEN
                        rad(k,m) = rad(k,m) - (rmin - rad(k,j))
                        kntbal = 1
                        rad(k,j) = rmin
                    ENDIF
                ENDDO
                ! Smooth pre-contact radii to compensate for
                ! Failure of bending model to consider azmuthal neighbor interaction
                radh1 = rad(k,1)
                radhl = rad(k,1)
                rad(k,1) = (rad(k,nj) + rad(k,1) + rad(k,2)) / 3.0_r8k
                DO j = 2, nj1
                    radh = rad(k,j)
                    rad(k,j) = (radhl + rad(k,j) + rad(k,j+1)) / 3.0_r8k
                    radhl = radh
                ENDDO
                rad(k,nj) = (radhl + rad(k,nj) + radh1) / 3.0_r8k
                ! Circular cross section displacement model
                rsml = 10.0_r8k
                jmin = 0
                DO j = 1, nj
                    IF (rad(k,j) < rsml) THEN
                        jmin = j
                        rsml = rad(k,j)
                    ENDIF
                ENDDO
                jmnbal = jmin
            ELSE
                ! jmin hard wired to 4. (hot spot hard wired to radial node 4)
                ! To compute: set to jmnbal here and delete set below
                jmin = 4
                rsml = rhtr + (twall(k,jmin) * 0.5_r8k)
            ENDIF
            dspmx = rave(k) - rsml
            ! Find angle between j and jmax, then calculate displaced circle radii
            ! jmin hard wired to 4
            jmin = 4
            DO j = 1, nj
                dispa = 6.28_r8k * (jmin - j) / nj ! Should 6.28 be replaced with 2.0_r8k * pi?
                radc(j) = SQRT(rave(k) * rave(k) - SIN(dispa) * SIN(dispa) * dspmx * dspmx) - COS(dispa) * dspmx
            ENDDO
            ! Mix radial displacement and circular section models as instructed by frbal = fraction radial displacement model
            rad(k,1:nj) = radc(1:nj)
            ! Print end of time step information if desired
            IF (nprntb > 0) THEN
                tce = (rave(k) - rbar) / rbar
                WRITE(ounit,911) k, rave(k), tave, kntbal, tce
911             FORMAT(' axial node ',i3,'    ave radius =',e13.6,'    ave wall thickness =',e13.6,&
                  &    '    contact switch =',i2,'    tce =',e10.3)
                IF (nprntb > 1) WRITE(ounit,912) (j, rad(k,j), twall(k,j), delz(k,j), j = 1,nj)
912                 FORMAT('  j =',i2,' midwall radius = ',e13.6,' wall thickness = ',e13.6,' delz = ',e13.6)
            ENDIF
        ENDDO
        r8bal = rave(8)
        ! compare total time since call to input time step size and branch
        dtime = dtime + delt
        IF (dtbal <= dtime) EXIT TimestepLoop
    END DO TimestepLoop
    !
    ! Store common block information for next call
    dtobal = delto
    !
    IF (nstep >= MaxIterations) THEN ! Note: Max # of iterations reached. If it occured, STOP code.
        WRITE(ounit,914) MaxIterations
914     FORMAT('     max steps-',i3,'- exceeded in baloon-2 subcode')
        ERROR STOP 'maximum iteration exceeded in balon2'
    ELSE IF (ifbaln == 0) THEN
        ! Find max hoop stress for frail
        chstrs = 0.0_r8k
        DO k = 1, nk
            DO j = 1, nj
                IF (chstrs < stresf(k,j)) chstrs = stresf(k,j)
            ENDDO
        END DO
    END IF
    !
      END SUBROUTINE balon2
    !
    !
        SUBROUTINE far1 (GasFraction, Baln2Twall, gadolin)
    USE Kinds
    USE conversions_fraptran, ONLY : pi
    USE NCGases, ONLY : ngases
    USE variables_fraptran, ONLY : ounit
    USE bloon_h
    USE phypro_h, ONLY : compmt
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calculates flow area reduction resulting from clad ballooning calculated in balon2.
    !>@author
    !> programed by e r carlson august 81
    !
    ! GasFraction : gap gas components
    !
    ! common block bloona contains balon2 and far1 variables that are used outside of comput, or are required for restart
    !
    !                     balon2 variables
    ! chstrs = maximum hoop stress in balloon region (psi)
    ! frbal  = balon2 circular/radial model switch
    ! pmxbal = balon2 maximum gap pressure (Pa)
    ! r8bal  = balon2 average radius at balon2 axial node 8 (m)
    ! tcebal = maximum circumferential strain in balloon region
    ! jmnbal = balon2 radial contact node
    ! kntbal = 1 If balon2 predicts pellet-clad contact
    ! nbncal = 1 If balon2 has been called once
    !
    !                     far1 variables
    ! pdrato = bundle pitch to rod diameter ratio
    ! rnbnt  = ratio of balloonable cells to total cells
    ! totnb  = total number of cells in a bundle
    ! nodprm = number flow area reduction subnodes
    ! farbal = flow area reduction   (m**2/m**2)
    ! sdfar  = standard deviation of flow area reduction
    ! zfarbl = axial location flow area reduction subnodes  (m)
    !
    !
    ! common block bloonb is used to pass arguments from comput to balon2
    !
    !                     balon2 variables
    ! (see balon2 comments for description of balon2 variables)
    !
    !                     far1 variables
    ! tm1bal = clad temperature at lower node (K)
    ! tp1bal = clad temperature at upper node (K)
    ! ztmax  = elevation of ballooning node (m)
    ! zm1bal = elevation of lower node (m)
    ! zp1bal = elevation of upper node (m)
    ! zndbal = total length of fuel rod (m)
    INTEGER(ipk) :: k, j, i, ibal, nodes, i1, ilop
    INTEGER(ipk), PARAMETER :: nj = 16
    INTEGER(ipk), PARAMETER :: nk = 16
    INTEGER(ipk), PARAMETER :: nkhalf = 8
    REAL(r8k) :: denom, a, b, c, gave, gaph, fraden, fotmtl, burnup, fuelk, tmid, &
        &        dtthf, dtthp, dtth, dtax, zmin, zl, g, ecntct, g1, g3, g2, p2, &
        &        slope, zbal0, frumax, tcmid, z1, z2,tmax, tmin, sqtdum, frumid, &
        &        tcesd, frusd, sdfru, sdpb, z, delt, ti, tip1, p1, pmaxt, tim1
    REAL(r8k), INTENT(IN) :: gadolin
    REAL(r8k), PARAMETER :: sqrt2 = 1.414213562_r8k
    REAL(r8k), PARAMETER :: sqrt2p = 2.506628275_r8k
    REAL(r8k), PARAMETER :: pid4 = 0.7853981635_r8k
    REAL(r8k), PARAMETER :: timdb = 1.0E20_r8k
    REAL(r8k), DIMENSION(ngases), INTENT(IN) :: GasFraction
    REAL(r8k), DIMENSION(2) :: bl, fru
    REAL(r8k), DIMENSION(20) :: balx, sigt, tj
    REAL(r8k), DIMENSION(16) :: dz0, radc, rave, stresr, stresa
    REAL(r8k), DIMENSION(16,16) :: acs, aas, ars, ace, aae, are, dbyz, stresf, tstres, disp, &
      &                            deh, rtemp, strnl, strnr, strna, strnc, rad, Baln2Twall, &
      &                            ftemp, delz, tstm, rstran, fcp, frp, fap, acd, aad, ard, &
      &                            fnck, fncn, cwnf, cwkf, dea, htcs, stemp, ctemp = 500.0_r8k
    ! timdb = time when debug print begins
    ! quadratic coefficients for temperature profile (cramer's rule)
    denom = ztmax * zm1bal ** 2 + zm1bal * zp1bal ** 2 + zp1bal * ztmax ** 2 - ztmax * zp1bal ** 2 - &
      &     zm1bal * ztmax ** 2 - zp1bal * zm1bal ** 2
    a = (ztmax * tm1bal + zm1bal * tp1bal + zp1bal * tc0bal - ztmax * tp1bal - zm1bal * tc0bal - &
      &  zp1bal * tm1bal) / denom
    b = (tc0bal * zm1bal ** 2 + tm1bal * zp1bal ** 2 + tp1bal * ztmax ** 2 - tc0bal * zp1bal ** 2 - &
      &  tm1bal * ztmax ** 2 - tp1bal * zm1bal ** 2) / denom
    c = (tp1bal * ztmax * zm1bal ** 2 + tc0bal * zm1bal * zp1bal ** 2 + tm1bal * zp1bal * ztmax ** 2 - &
      &  tm1bal * ztmax * zp1bal ** 2 - &
      &  tp1bal * zm1bal * ztmax ** 2 - tc0bal * zp1bal * zm1bal ** 2) / denom
    ! thermal resistances
    gave = rmpbal - 0.5_r8k * h0bal * (r0bal - 0.5_r8k * h0bal) / rmpbal - rfbal
    !
    gaph = MatProperty (Material='GAS', Property='GCONR2', Temperature=tgbal, Pressure=psbal, &
      &                 Width=gave, GasComposition=GasFraction)
    
    ! Use Lucuta model for fuel thermal conductivity with zero for burnup and Gd
    fuelk = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tf0bal, Burnup=0.01_r8k, OMRatio=2.0_r8k, &
      &                  Fraction_TD=0.96_r8k, Gadolinia=gadolin, Pu=compmt)
    ! circumferential temperature variation
    dtthf = (2.0_r8k * qbal) * (rfbal / fuelk) * (1.0_r8k / gaph) / ((rfbal / fuelk) + (1.0_r8k / gaph) + (1.0_r8k / htcbal))
    dtthp = 0.001_r8k * qbal * (1.0_r8k / htcbal + 1.0_r8k / gaph)
    dtth = SQRT(dtthp ** 2 + dtthf ** 2) / tf0bal
    IF (dtth < 0.005_r8k) dtth = 0.005_r8k
    IF (dtth > 0.1_r8k) dtth = 0.1_r8k
    ! axial temperature variation
    IF (a < 0.0_r8k) THEN
        dtax = -a * ((zbaln / 2.0_r8k) ** 2) / tc0bal
        IF (nbncal == 1) THEN
            IF (dtax < 0.005_r8k) zbaln = 2.0_r8k * SQRT(0.005_r8k * tc0bal / (-a))
            IF (zbaln > 0.3_r8k) zbaln = 0.3_r8k
        ENDIF
    ELSE
        dtax = .005_r8k
        IF (nbncal == 0) zbaln = 0.3_r8k
        zmin = 0.0_r8k
        zl = zndbal
    ENDIF
    IF (dtax < 0.005_r8k) dtax = 0.005_r8k
    IF (dtax > 0.1_r8k) dtax = 0.1_r8k
    !
    DO k = 1, nk
        DO j = 1, nj
            ftemp(k,j) = tf0bal * (1.0_r8k + dtth * ( 1.0_r8k + SIN(6.2832_r8k * j / nj)) / 2.0_r8k + &
              &          dtax * SIN(pi * (k - 1.0_r8k) / (nk - 1.0_r8k)))
        ENDDO
    ENDDO
    !
    IF (timbal >= timdb) THEN
        WRITE(ounit,101) tm1bal, tc0bal, tp1bal, zm1bal, ztmax, zp1bal
        WRITE(ounit,101) ((ftemp(k,j),j=1,8),k=1,16)
        WRITE(ounit,101) a, b, c, denom
        WRITE(ounit,101) ((ftemp(k,j), j=9,16), k=1,16)
        WRITE(ounit,101) qbal, rfbal, fuelk, gaph, gave, htcbal, r0bal
        WRITE(ounit,101) dtthf ,dtthp ,dtth ,dtax ,zbaln
101     FORMAT(' far1',8e14.5)
    ENDIF
    !
    CALL balon2 (ftemp, strnl, strnr, strna, strnc, rad, Baln2Twall, ctemp, delz, fcp, &
      &          frp, fap, acd, aad, ard, fnck, fncn, cwkf, cwnf, stemp, htcs, tstm, rtemp, acs, aas, &
      &          ars, ace, aae, are, dbyz, stresf, tstres, disp, deh, dea, rstran, rave, stresr, &
      &          stresa, radc, dz0, gadolin)
    ! Initialize and return for small maximum strains
    farbal(1:20) = 0.0_r8k
    sdfar(1:20) = 0.0_r8k
    zfarbl(1:20) = 0.0_r8k
    IF (tcebal < 0.01_r8k) RETURN
    ! balloon profile
    zfarbl(1) = dz0(1) / 2.0_r8k
    DO k = 1, nk
        balx(k) = (rave(k) - (r0bal - 0.5_r8k * h0bal)) / (r0bal - 0.5_r8k * h0bal)
        IF (k > 1) zfarbl(k) = zfarbl(k-1) + (dz0(k-1) + dz0(k)) / 2.0_r8k
    ENDDO
    ! Characterize balloon
    g = pid4 / (pdrato ** 2 - pid4)
    ecntct = pdrato - 1.0_r8k
    g1 = 0.93_r8k
    g3 = g * (2.0_r8k * ecntct + 2.0_r8k) / (g1 - g * (ecntct ** 2 + 2.0_r8k * ecntct))
    g2 = (g1 - g * (ecntct ** 2 + 2.0_r8k * ecntct)) * EXP(g3 * ecntct)
    ! base shape
    fru(2) = g * (balx(2) ** 2 + 2.0_r8k * balx(2))
    IF (balx(2) > ecntct) fru(2) = g1 - g2 * EXP(-g3 * balx(2))
    IF (balx(1) < balx(3)) THEN
        slope = (balx(3) - balx(1)) / (zfarbl(3) - zfarbl(1))
        zbal0 = balx(1) - slope * zfarbl(1)
        bl(2) = zbaln + 2.0_r8k * (zbal0 - 0.33_r8k * balx(2)) / slope
        IF (bl(2) > 1.5_r8k * zbaln) bl(2) = 1.5_r8k * zbaln
    ELSE
        bl(2) = 1.5_r8k * zbaln
    ENDIF
    ! peak shape
    frumax = g * (tcebal ** 2 + 2.0_r8k * tcebal)
    IF (tcebal > ecntct) frumax = g1 - g2 * EXP(-g3 * tcebal)
    fru(1) = frumax - fru(2)
    tcmid = (balx(2) + tcebal) / 2.0_r8k
    z1 = 0.0_r8k
    z2 = zbaln
    DO i = 1, nkhalf
        IF (balx(i) < tcmid) z1 = zfarbl(i)
        IF (balx(nk+1-i) < tcmid) z2 = zfarbl(nk+1-i)
    ENDDO
    bl(1) = z2 - z1
    IF (bl(1) < dz0(8))  bl(1) = dz0(8)
    ! axial ballooning range
    IF (a < 0.0_r8k) THEN
        tmax = c - b ** 2 / (4.0_r8k * a)
        tmin = tmax - 4.66_r8k * 0.025_r8k * ABS(tmax - tbkbal) * SQRT(1.0_r8k + tcebal / 0.3_r8k)
        IF (tcebal > 0.3_r8k) tmin = tmax - 4.66_r8k * 0.025_r8k * ABS(tmax - tbkbal) * 1.4142_r8k
        sqtdum = SQRT(b ** 2 - 4.0_r8k * a * (c - tmin))
        zmin = (-b + sqtdum) / (2.0_r8k * a)
        zl = - sqtdum / a
        IF (zmin < 0.0_r8k) THEN
            zl = zl + zmin
            zmin = 0.0_r8k
        END IF
        IF ((zmin + zl) > zndbal) zl = zndbal - zmin
        IF (zl < bl(1)) zl = bl(1)
    ENDIF
    ! standard deviation of unit far and probability
    frumid = g * (tcmid ** 2 + 2.0_r8k * tcmid)
    IF (tcmid > ecntct) frumid = g1 - g2 * EXP(-g3 * tcmid)
    tcesd = 5.0_r8k * tcmid / 6.0_r8k
    frusd = g * (tcesd ** 2 + 2.0_r8k * tcesd)
    IF (tcesd > ecntct) frusd = g1 - g2 * EXP(-g3 * tcesd)
    sdfru = frumid - frusd
    sdpb = bl(2) / (8.0_r8k * zl)
    !
    IF (timbal > timdb) WRITE(ounit,101) (balx(k),k=1,16), (zfarbl(k),k=1,16), fru, bl, tmin, &
      &                                   zmin, zl, tcebal, frumid, frumax, tcmid, sdfru, sdpb
    ! loop over balloon shape
    DO ibal = 1, 2
        ! nodalization
        nodes = INT(zl / bl(ibal))
        IF ((zl / bl(ibal) - nodes) > 0.5_r8k) nodes = nodes + 1
        nodes = MAX(2,nodes)
        nodes = MIN(20,nodes)
        ! temperature and temperature uncertainty distribution
        DO i = 1, nodes
            z = zmin + zl * (i - 0.5_r8k) / nodes
            IF (ibal == 1) zfarbl(i) = zmin + zl * i / nodes
            balx(i) = zmin + zl * i / nodes + 0.0001_r8k
            tj(i) = a * z * z + b * z + c
            sigt(i) = ABS(tj(i) - tbkbal) * 0.025_r8k * SQRT(1.0_r8k + tcebal / 0.3_r8k)
            IF (tcebal > 0.3_r8k) sigt(i) = ABS(tj(i) - tbkbal) * 0.025_r8k * 1.4142_r8k
            IF (sigt(i) < 0.01_r8k) sigt(i) = 0.01_r8k
        ENDDO
        i1 = 1
        ! for each node numerically integrate between + & - 3.5 standard deviations equation (25) in fdr/cdf-ft-10 
        ! using simpson's rule ("elementary numerical analysis" conte and de boor)
        DO i = 1, nodes
            ! first increment of max temp probability
            delt = 0.1_r8k * sigt(i)
            ti = tj(i) - 3.5_r8k * sigt(i)
            tip1 = ti + delt
            p1 = 1.0_r8k
            DO j = 1, nodes
                IF (j /= i) p1 = p1 * (ERF((ti - tj(j)) / (sqrt2 * sigt(j))) + 1.0_r8k) / 2.0_r8k
            ENDDO
            p1 = p1 * EXP(- 0.5_r8k * ((ti - tj(i)) / sigt(i)) ** 2) / (sigt(i) * sqrt2p)
            pmaxt = p1 * (tip1 - ti) / 6.0_r8k
            ! sum incremental max temp probability
            DO ilop = 1, 70
                tim1 = ti
                ti = tip1
                tip1 = tip1 + delt
                tmid = (ti + tim1) / 2.0_r8k
                p1 = 1.0_r8k
                p2 = 1.0_r8k
                DO j = 1, nodes
                    IF (j /= i) THEN
                        p1 = p1 * (ERF((ti - tj(j)) / (sqrt2 * sigt(j))) + 1.0_r8k) / 2.0_r8k
                        p2 = p2 * (ERF((tmid - tj(j)) / (sqrt2 * sigt(j))) + 1.0_r8k) / 2.0_r8k
                    ENDIF
                ENDDO
                p1 = p1 * EXP(-0.5_r8k * ((ti - tj(i)) / sigt(i)) ** 2) / (sigt(i) * sqrt2p)
                p2 = p2 * EXP(-0.5_r8k * ((tmid - tj(i)) / sigt(i)) ** 2) / (sigt(i) * sqrt2p)
                pmaxt = pmaxt + p1 * (tip1 - tim1) / 6.0_r8k + 2.0_r8k * p2 * (ti - tim1) / 3.0_r8k
            ENDDO
            ! sum final increment of max temp probability
            tmid = (tip1 + ti) / 2.0_r8k
            p1 = 1.0_r8k
            p2 = 1.0_r8k
            DO j = 1, nodes
                IF (j /= i) THEN
                    p1 = p1 * (ERF((tmid - tj(j)) / (sqrt2 * sigt(j))) + 1.0_r8k) / 2.0_r8k
                    p2 = p1 * (ERF((tip1 - tj(j)) / (sqrt2 * sigt(j))) + 1.0_r8k) / 2.0_r8k
                ENDIF
            ENDDO
            p1 = p1 * EXP(-0.5_r8k * ((tmid - tj(i))/sigt(i))**2)/(sigt(i) * sqrt2p)
            p2 = p2 * EXP(-0.5_r8k * ((tip1 - tj(i))/sigt(i))**2)/(sigt(i) * sqrt2p)
            pmaxt = pmaxt + 2.0_r8k * p1 * (tip1 - ti)/3.0_r8k + p2 * (tip1 - ti) / 6.0_r8k
            !
            IF (timbal > timdb) WRITE(ounit,102) i, nodes, pmaxt, zfarbl(i), balx(i), tj(i), sigt(i)
102         FORMAT(' far1',2i5,6e12.4)
            ! incremental flow area reduction from peak shape
            IF (ibal == 1) THEN
                nodprm = nodes
                farbal(i) = fru(ibal) * pmaxt * rnbnt
            ELSE
                ! incremental flow area reduction from base shape flow area reduction standard deviation
                FAR_Loop: DO
                    IF (zfarbl(i1) > balx(i)) EXIT FAR_Loop
                    farbal(i1) = farbal(i1) + fru(ibal) * pmaxt * rnbnt
                    sdfar(i1) = SQRT((rnbnt * pmaxt / totnb) * ((frumax ** 2) * (1.0_r8k - pmaxt) + sdfru ** 2 + sdpb ** 2))
                    i1 = i1 + 1
                    IF (i1 > nodprm) EXIT FAR_Loop
                END DO FAR_Loop
            END IF
        ENDDO
    ENDDO
    !
    IF (timbal >= timdb) THEN
        WRITE(ounit,103) (zfarbl(i), i=1,nodprm)
        WRITE(ounit,104) (farbal(i), i=1,nodprm)
        WRITE(ounit,105) (sdfar(i), i=1,nodprm)
103     FORMAT('Elevation (m)                  ',6e15.4)
104     FORMAT('Flow area reduction            ',6e15.4)
105     FORMAT('Flow area reduction uncertainty',6e15.4)
    ENDIF
    !
    END SUBROUTINE far1
    !
    !
    END MODULE Ballooning












