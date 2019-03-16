MODULE deformation_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    USE zrmodels_fraptran, ONLY : cmlimt, cstrni
    !
    CONTAINS
    !
    SUBROUTINE closure (CladAveTemp, GasPress, CoolPress, PelSrfDisplin, delta, CldPlasStrn, EffStrain, &
      &                 rci, rco, rfo, OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, PelSrfStrn, &
      &                 FuelResidStrn, CldResidStrn, OldCldAxStrn, OldFuelAxStrn, n2, ldir, k, edot1, &
      &                 edot2, edot3, tcmax, FastFlux, coldw, TimeIncrement, NSteadyTrans, nedtsw, &
      &                 nconv, rmp0, CladThickness0, pitch, t0)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit, naxn, ndebug
    IMPLICIT NONE
    !>@brief
    !> Subroutine closure looks for gap closure
    !>@author
    !> Modified by B.W.Burnham, 1-6-76
    !
    INTEGER(ipk) :: n, k, n2, ldir, nsteadytrans, nedtsw, nconv
    REAL(r8k) :: alpha, beta, dx, cut, fofb, vx, fofa, a, b, x, fofx, cep, delta, rci, rco, rfo, &
      &          edot1, edot2, edot3, tcmax, fastflux, coldw, timeincrement, t, rmp0, cladthickness0, &
      &          pitch, t0
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp
    REAL(r8k), DIMENSION(:) :: CladAveTemp, GasPress, CoolPress, EffStrain, OldCladT, OldGasPrs, &
      &                        OldCoolPrs, OldPelDis, OldCldAxStrn, OldFuelAxStrn, PelSrfDisplin
    REAL(r8k), DIMENSION(:,:) :: CldPlasStrn, PelSrfStrn, FuelResidStrn, CldResidStrn
    ! rooting for roots
    !
    ! a modified false position routine operating over the interval alpha,
    ! beta with increment dx was installed by B.W.Burnham 1-6-76
    alpha = -1.0_r8k
    beta = 1.0_r8k
    dx = 0.25_r8k
    cut = 1.0e-6_r8k
    n = 0
    !
    fofb = f(beta)
    vx = 1.0_r8k
    ! If (fofb >= 0.0_r8k) GOTO 20
    fofa = f(alpha)
    vx = -1.0_r8k
    ! If (fofa <= 0.0_r8k) GOTO 20
    ! commence the moving window looking for root brackets
    a = alpha
    b = a + dx
30  fofb = f(b)
    IF (fofa * fofb < 0.0_r8k) GOTO 50
    IF (fofa * fofb == 0.0_r8k) GOTO 51
    IF (fofa * fofb > 0.0_r8k) GOTO 52
50  CONTINUE
    ! Having bracketed a root the solution iteration commences
41  CONTINUE
    x = (a * fofb - b * fofa) / (fofb - fofa)
    n = n + 1
    IF (n >= 100) THEN
        WRITE(ounit,76)
76      FORMAT(' closure - the iteration scheme has passed 100 steps--execution terminated')
        RETURN
    END IF
    fofx = f(x)
    ! Test the possibility of the iteration loop landing on a root at x or passing by the root
    IF (fofx * fofa < 0.0_r8k) GOTO 42
    IF (fofx * fofa == 0.0_r8k) GOTO 43
    IF (fofx * fofa > 0.0_r8k) GOTO 44
44  CONTINUE
    ! The root is still bracketed and the iteration loop continues
    a = x
    fofa = fofx
    fofb = 0.5_r8k * fofb
    IF (ABS(fofx) > cut) GOTO 41
    vx = x
    GOTO 22
42  CONTINUE
    ! The iteration scheme has rebracketed the root-the bounds are adjusted and the loop continues
    b = x
    fofb = fofx
    fofa = 0.5_r8k * fofa
    IF (ABS(fofx) > cut) GOTO 41
    vx = x
    GOTO 22
43  CONTINUE
    ! The iteration scheme has landed on a root at (x,fofx)
    IF (ABS(fofa) == 0.0_r8k) GOTO 60
    vx = x
    GOTO 22
51  CONTINUE
    ! The bracketing operation has found a root
    IF (fofa == 0.0_r8k) GOTO 60
    ! the bracketing operation has found a root at (b,fofb)
    vx = b
    GOTO 22
60  CONTINUE
    ! If this is the first bracket step Then f(alpha)=0. If otherwise
    ! the previous step is in error at fofb the initial Data contains a root at alpha
    vx = a
    GOTO 22
52  CONTINUE
    ! The routine has not yet bracketed a root-adjust the variables to
    ! the next mesh and CONTINUE, also test to see If the interval has been exceeded
    a = b
    b = b + dx
    fofa = fofb
    IF (a > beta) GOTO 65
    GOTO 30
65  CONTINUE
    !  the bracketing operation has overrun the interval
    WRITE(ounit,72)
72  FORMAT(' gap bracketing error: Fracas-1, Subroutine closure')
    RETURN
20  CONTINUE
    WRITE(ounit,73) alpha, beta, fofa, fofb
73  FORMAT(' gap closure error : Fracas-1, Subroutine closure'//' alpha = ',e10.3,' beta = ',e10.3, &
      &    ' fofa = ',e10.3,' fofb = ',e10.3)
22  CONTINUE
    
    ! ****update residual strains here
    OldCldAxStrn(k) = ceps(2)
    CldResidStrn(k,2) = ceps(2)
    OldFuelAxStrn(k) = OldFuelAxStrn(k) + vx * (PelSrfStrn(k,2) - OldFuelAxStrn(k))
    FuelResidStrn(k,2) = OldFuelAxStrn(k)
    ! ****update plastic strains here after gap zero solution is found
    CldPlasStrn(k,1) = cepp(1)
    CldPlasStrn(k,2) = cepp(2)
    CldPlasStrn(k,3) = cepp(3)
    EffStrain(k) = cep
    CONTAINS
        !
        REAL(r8k) FUNCTION f (t)
        IMPLICIT NONE
        REAL(r8k), INTENT(IN) :: t
        !
        f = gapt (t, CladAveTemp, GasPress, CoolPress, PelSrfDisplin, delta, CldPlasStrn, EffStrain, &
          &       rci, rco, rfo, ceps, cepp, cep, csig, OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, &
          &       t0, n2, ldir, k, edot1, edot2, edot3, tcmax, FastFlux, coldw, TimeIncrement, &
          &       nedtsw, nconv, rmp0, CladThickness0, pitch)
        !
        END FUNCTION f
    !
    END SUBROUTINE closure
    !
    !
    !
    SUBROUTINE deform
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, ftin, tfk
    USE variables_fraptran
    USE Uncertainty_Vals_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE FEModel_fraptran
    IMPLICIT NONE
    !> @brief
    !> Subroutine computes stresses and strains in fuel rod
    !
    ! stresses and strains computed at each radial node of each axial node of the rod
    ! k indicates index that runs from 1 to number of axial nodes
    ! l indicates index that runs from 1 to number of radial nodes
    !
    ! Input
    !
    ! CoolPress(k)     - vector of coolant pressure surrounding the rod at axial nodes k   (psia)
    ! GasPress(k)      - vector of gap pressure in the rod at axial node k   (psia)
    ! RadialBound(l)   - vector of radii to radial nodes l in initial fuel rod cold unstressed state  (ft)
    ! pelprm(1)-frden  - ratio of cold-state fuel density to maximum theoretical density
    ! pelprm(2)-rshd   - radius (unstressed cold state) to point of primary axial support of pellets (ft)
    !                    normally radius to outside edge of pellet dish
    ! pelprm(3)-dishv0 - cold-state dish volume per pellet (ft**3)
    ! pelprm(4)-dishd  - cold-state dish depth (ft)
    ! pelprm(5)-pelh   - cold-state pellet heigth (ft)
    ! pelprm(6)-temp0  - cold-state fuel rod temperature (F)
    ! pelprm(7)-tmeltf - melting temperature of fuel (F)
    ! pelprm(8)-TimeIncrement - time step(sec)
    ! pelprm(9)-bu     - burnup of fuel (mw-sec/kg)
    ! pelprm(16) - heat transfer coeffient at balloon node, btu/hr-ft2-F
    ! pelprm(17) - coolant temperature at balloon node, F   (input)
    ! pelprm(18) - cladding EOSTemp. at initiation of ballooning, F (input)
    ! pelprm(19) - surface heat flux of pellet surface at balloon node, btu/hr-ft2  (input)
    ! pelprm(20) - gas pressure at balloon node at start of time step, psia  (input and output)
    ! pelprm(21) - coefficient for balloon model computed by gsflow subcode  (input)
    ! pelprm(22) - same as pelprm(21)  (input)
    ! pelprm(23) - same as pelprm(21)  (input)
    ! pelprm(24) - multiplication factor for heat transfer coefficient at balloon node   (input)
    ! pelprm(25) - probability for fuel rod failure (output)
    ! pelprm(26) - vsn(1)  (defined below)
    ! pelprm(27) - probability threshold for fuel rod failure
    ! pelprm(28) - Time - current problem time  (sec) (input)
    ! pelprm(29) - as fabricated fuel grain size (microns)
    ! pelprm(30) - ratio of oxygen atoms to uranium atoms
    ! pelprm(31) - maximum density change in resintering test of 1700 C for 24 hrs (kg/m**3)
    ! pelprm(32) - fuel sintering temperature (K)
    ! pelprm(33) - fuel rod pitch (center-to-center spacing) (ft)
    ! EOSTemp(l,k)  - matrix of fuel rod temperature at radial nodes l and axial nodes k  (F)
    ! CladAveTemp(k)   - vector of average cladding temperature at axial nodes k (F)
    ! AxialNodLen(k)   - vector of fuel rod length associated with axial nodes k  (ft)
    ! EnrgyMeltZ(ll,k) - energy absorbed in melting by ll-th half mesh (two half meshes per radial node) (btu/ft**3)
    ! qmaxmelt(ll)     - maximum energy half mesh ll can absorb in melting (btu/ft**3)
    ! IFstor         -  vector storing fixed point variables that define node numbers at boundaries or are switches
    ! IFstor(4)      - naxn   (defined below)
    ! IFstor(5)      - NSteadyTrans    (defined below)
    ! IFstor(8)      - switch for cladding axial expansion calculations after fuel-cladding lockup.
    !              0 = no simplified calculations in sub. fcmi
    !              1 = simplified calculations
    ! IFstor(10)       - length of first DIMENSION in displacement array of balloon model   (input)
    ! IFstor(11)       - length of second DIMENSION   (input)
    ! IFstor(12)       - 0 on first balloon model CALL, 1 thereafter (input and output)
    ! IFstor(13)       - mode of cladding failure indicator
    !                  - fuel deformation calc. bypass switch. 0-no , 1-yes
    ! IFstor(15)       - strain rate effects suppression  0-no , 1-yes
    ! IFstor(16)       - force of fcmi calculations after fail. 0-no,1-yes
    ! IFstor(17)       - nrelax. IF nrelax-0, fcmi-2 plasticity calculation. IF nrelax-1, fcmi-2 creep calculation
    ! n1               - maximum number of radial nodes arrays are dimensioned for in main
    ! naxn             - total number of axial nodes
    ! n2               - maximum number of axial nodes arrays are dimensioned for in fraptran
    ! NSteadyTrans     - switch. NSteadyTrans 1: steady-state calculation. 2: transient calculation
    ! modfd            - switch to specify fuel deformation model (0 = free thermal expansion model, 1 = gapcon model)
    ! BOSRad(l,k)      - radial coordinate at End of last time step to radial node l at axial node k (ft)
    ! SwellDispl(k)    - displacement of fuel surface caused by fission swelling at axial node k  (ft)
    ! vs0(1)           - gas volume generated by localized cladding ballooning at End of last time step  (ft**3)
    ! dvdt             - rate of volume increase needed in region of localized cladding ballooning to keep gas pressure
    !                    from exceeding ultimate cladding internal pressure. not used IF ies(1)-0  (ft**3/sec)
    ! roi(1)           - inside radius of cladding at initiation of localized cladding ballooning at axial node at which
    !                    ballooning is occurring  (ft)
    ! ies(1)           - switch. ies(1) - 0 - no localized ballooning of rod, 1 - localized ballooning
    ! ldir             - number of Dimensions in stress-strain calculations  (3)
    ! n4               - maximum number of fuel rods arrays are Dimensioned to handle
    ! n3               - two times maximum permitted number of radial nodes
    ! RuptFailIndex(k) - failure indicator. (0 = No cladding failure at time step start at axial node k. 1 = Failure)
    ! CldPermStrn0(k)  - localized cladding strain at End of last time step
    ! kdbugx           - debug printout switch. 0 - no , 1 - yes
    !
    ! Input / Output
    !
    ! ld - index running from 1 to ldir to indicate component of stress or strain
    !    1 = hoop direction, 2 = axial direction, 3 = radial direction
    ! input arguments must be supplied value at end of last time step
    ! output arguments are supplied value at end of current time step
    ! PelSrfStrn(k,ld)     - fuel strains at pellet surface (output)
    ! CldResidStrn(k,ld)   - cladding residual strains (input and output)
    ! FuelResidStrn(k,ld)  - fuel residual strains (input and output)
    ! CldPlasStrn(k,ld)    - cladding plastic strains (input and output)
    ! EffStrain(k)         - cladding effective plastic strain (in. + out.)
    ! CldStrn(k,ld)        - total cladding strains (output)
    ! CldStress(k,ld)      - cladding stresses (psi) (output)
    ! RInterfacPrs(k)      - interfacial pressure between fuel and cladding (psi) (output)
    ! GapThick(k)          - radial width of (thermal) of gap between fuel and cladding (ft)  (output)
    ! GapIndex(k)          - gap closure indicator   (output) 0 = Open gap, 1 = Closed Gap
    ! PelSrfDispl(k)       - radial displacement of fuel outer surface (ft) (output)
    ! dcldh                - change in length of cladding from initial unstressed cold state (ft)  (output)
    ! delth                - change in length of fuel pellet stack from initial unstressed cold state (ft) (output)
    ! flwblk(1)            - percentage flow blockage caused by cladding rupture  (output)
    ! HydrogenContent      - hydrogen content of cladding (kg/m**3) (input)
    ! CesiumContent        - iodine content of cladding (kg/m**3)   (input)
    ! CesiumContent        - cesium content of cladding (kg/m**3)   (input)
    ! CrackVolume          - volume of radial cracks in fuel per unit length (ft**3/ft)  (output)
    ! BOSOxideThick        - beginning of step oxide thickness layer on cladding (inch)(input)
    ! CladEffStress        - cladding effective stress, psi (output)
    ! BulkCoolTemp         - coolant temperature (F) (input)
    ! OpenPorosity         - Open porosity at each axial node  ( input)
    ! OpenPorVol           - void volume due to Open porosity (ft3/ft) output
    ! AxBurnup             - start of fuel burnup vector (mw=sec/kg)
    ! EffStrain            - start of effective strain vector  (output)
    ! EinstabilStrain      - start of instability strain vector (output)
    ! StressAtInstStrain   - stress at instability strain vector,psi (output)
    ! CladYieldStress      - workhardened yield stress vector, psi (output)
    ! CrackWidth           - crack  width at fuel  surface  (ft) (output)
    ! TerfacePres          - interface pressure at structural gap(output,psi)
    ! CladCollapseIndex(k) - cladding collapse indicator  (output) 0=no cladding collapse, 1=cladding collapse on fuel
    ! pswll(1)             - gas pressure which caUSEs cladding hoop stress equal to ultimate stress (psia)  (output)
    ! vsn(1)               - gas volume generated by localized cladding ballooning at end of current time step (ft**3) (output)
    ! EOSRad(l,k)          - radial coordinate of radial node l at end of current time step (ft)  (output)
    ! RodFailIndex(k)      - failure indicator. 0 = no cladding failure at end of current time step, 1=cladding failure
    !
    ! OldCldAxStrn(k)      - axial strain of cladding at start of fuel=cladding lockup at axial node k
    ! OldFuelAxStrn(k)     - residual fuel axial strain at start of time step at axial node k
    !
    ! CldPermStrn(k)       - cladding permanent radial strain
    ! DishVperL(k)         - current volume of pellet dishes per unit length (ft**3/ft)
    ! OldCladT(k)          - average cladding temperature at axial node k at previous time step (F)
    ! OldGasPrs(k)         - gas gap pressure at axial node k at previous time step (psia)
    ! OldCoolPrs(k)        - coolant pressure at axial node k at previous time step (psia)
    ! OldPelDis(k)         - fuel pellet surface radial displacement at previous time step at axial node k (inches)
    ! BOSGapIndex(k)       - indicator of Open or closed gap at previous time step.
    !                    0 - Open gap
    !                    1 - closed gap
    ! RInterfacGap(k)      - raduak (mechanical) pellet-clad gap width (ft)
    ! FastFlux(k)          - average fast neutron flux cladding exposed to (n/m**2=s)
    ! pelprm(11)           - cladding cold work (ratio of final thickness to original thickness)
    ! pelprm(12)           - time cladding exposed to fast neutron flux (sec)
    ! CladMaxT(k)          - peak temperature of cladding in past at node k (F)
    ! CldStrnRat(k,ld)     - strain rate (input value - last output value )
    ! dbal                 - displacement array computed by balloon model  (input and output)
    !
    ! epsa(k,n,l)          - l=th component of total strain at radial node n at end of time step  (output)
    !                    l - 1 = hoop direction
    !                    l - 2 = axial direction
    !                    l - 3 = radial direction
    !
    ! eppa(k,n,l)          - plastic strain    (output)
    ! epa(k,n)             - effective plastic strain    (output)
    ! siga(k,n,l)          - stress (psia)     (output)
    ! reps(k,l)            - cladding strain at start of fuel=cladding contact (input & output)
    ! eps0a(k)             - array currently not used
    ! ecreep(k,n)          - effective cladding creep    (output)
    ! olepsa(k,n,l)        - total strains at start of time step     (input)
    ! eppa0(k,n,l)         - plastic strains at start of time step     (input)
    ! epa0(k,n)            - effective plastic strains at start of time step (input)
    ! ra0(k,l)             - undeformed radius to radial node l, ft    (input)
    ! edota(k,n)           - strain rate at End of time step  (1/sec) ,not used
    ! edota0(k,n)          - strain rate at start of time step (1/sec) (input)
    ! tmpmax(k,n)          - maximum previous temperature  (F)       (input)
    ! frmelt(k,n)          - fraction of way through fuel melting     (output)
    !                   0. - no melting 
    !                   1. - melting complete
    !           balon2 variables
    ! chstrs - maximum hoop stress in balloon region (psi)
    ! frbal  - balon2 circular/radial model switch
    ! pmxbal - balon2 maximum gap pressure (Pa)
    ! r8bal  - balon2 average radius at balon2 axial node 8 (m)
    ! tcebal - maximum circumferential strain in balloon region
    ! IFbaln - Balon2 failure indicator. 0 = Not failed, 1 = balon2 predicts failure
    ! jmnbal - balon2 radial contact node
    ! kntbal - 1 IF balon2 predicts pellet=clad contact
    ! nbncal - 1 IF balon2 has been called once
    !           far1 variables
    ! pdrato - bundle pitch to rod diameter ratio
    ! rnbnt  - ratio of balloonable cells to total cells
    ! totnb  - total number of cells in a bundle
    ! nodprm - number flow area reduction subnodes
    ! farbal - flow area reduction   (m**2/m**2)
    ! sdfar  - standard deviation of flow area reduction
    ! zfarbl - axial location flow area reduction subnodes  (m)
    !
    INTEGER(ipk) :: mStart, k, m, lnod, meltindex, kdbugx, nfcall, mod, npass, mmax, i, ifail1, &
      &             ifailz, indexrepack, modfd0, l, nconvg
    REAL(r8k) :: rr1, rr2, rr3, rRest, urcir1, urcir2, urcir3, detUC, urcirA, urcirB, urcirC, rrMax, &
      &          tmeltf, t0k, delthx, eth0, fdx, crakwidth, c1, c2, deltr, ur, urm1, tn, ethxp, &
      &          tkn, urmax, big, qn, qnm1, ethm1, eth, rmid, rsh0, fm, cath0, cath1, cdth1, &
      &          cladthickness, da, dasum, daxcld, deltp, dvdish, dvdsh1, pcool, pelhav, pgap, q1, &
      &          q2, qm1, caxrad, cdpres, cdth0, cinrad, cinwid, den, dr, edot, edot1, edot2, edot3, &
      &          eph, eptha, epthr, epz, qm2, rm, rr, sigmah, sigmaz, tave, tavek, thk, tkaved, fexp1, fexp2, &
      &          tkshd, tmp1, tsdh, tsum, pinf, rf0, seff, strnlim, tcladf, tcmaxf, tshd, urfold, dep, &
      &          caxstr, culste, cyldst, dgcold, dpclos, dpcold, dum1, dum2, dum4, dum6, dum8, dum9, &
      &          dum10, edott, eistab, eiunfm, epeff, erup, dlrel, rci, rco, rmp, pitc2, rpi, rmp0
    REAL(r8k), PARAMETER :: cnvstr = 1.450377377e-4_r8k
    REAL(r8k), PARAMETER :: sigmin = 1.0e-3_r8k
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    LOGICAL :: Updateknonue
    REAL(r8k), DIMENSION(3) :: factr1
    REAL(r8k), DIMENSION(igpnod) :: urcir, urrad, fm1, fm2
    REAL(r8k), DIMENSION(naxn) :: zerloj
    !
    tmeltf = pelprm(7)
    Time = pelprm(28)
    t0k = MAX(300.0_r8k, tfk(tempcs))

    delthx = 0.0_r8k
    !
    eth0 = MatProperty (Material='FUEL', Property='THEXP', Temperature=t0k, Fraction_Melt=zero, Pu=compmt)
    ! fm1 = 0.0_r8k
    ! fm2 = 0.0_r8k
    MeltIndex = 2
    fdx = 0.9_r8k
    kdbugx = 0
    IF (kdbug == 1) kdbugx = 1
    nfcall = IFstor(14)
    ! bypass fuel deformation calculations if previously computed for current temperature distribution
    IF (nfcall /= 1) THEN
        ! assume fuel thermal expansion with no stress resistance
        mod = 0
        ! compute fuel deformations at all axial nodes for the rod
        DO k = 1, naxn
            npass = 0
            CrakWidth = 0.0_r8k
            c2 = 0.0_r8k
            deltr = 0.0_r8k
            c1 = 0.0_r8k
            ur = 0.0_r8k
            urm1 = 0.0_r8k
            tn = EOSTemp(1,k)
            tkn = tfk(tn)
            ! no displacement at centerline
            urrad(1) = 0.0_r8k
            urcir(1) = 0.0_r8k
            urmax = -1.0e10_r8k
            big = 0.0_r8k
            DO m = 2, igpnod
                ! Find fraction of molten fuel
                IF (NSteadyTrans > 1) THEN  ! Transient fuel melt calculation
                    IF (qmaxmelt(m) > 1.0e-10_r8k) THEN
                        qn = EnrgyMeltZ(m,k) / qmaxmelt(m)
                    ELSE
                        qn = 0.0_r8k
                    ENDIF
                    IF (qmaxmeltp1(m-1) > 1.0e-10_r8k) THEN
                        qnm1 = EnrgyMeltZp1(m-1,k) / qmaxmeltp1(m-1)
                    ELSE
                        qnm1 = 0.0_r8k
                    ENDIF
                    !
                    fm1(m) = qnm1
                    fm2(m) = qn
                ELSE ! Steady-State calculation
                    fm1(m) = 0.0_r8k
                    fm2(m) = 0.0_r8k
                    IF (EOSTemp(m-1,k) > tmeltf) fm1(m) = 1.0_r8k
                    IF (EOSTemp(m,k) > tmeltf) fm2(m) = 1.0_r8k
                ENDIF
                ! calculate thermal expansion strain
                ethm1 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(m-1,k)), &
                  &                  Fraction_Melt=fm1(m), Pu=compmt)
                eth = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(m,k)), &
                  &                Fraction_Melt=fm2(m), Pu=compmt)
                IF (m == 2) THEN
                    IF (AxNodElevat(k) > zvoid2 .OR. AxNodElevat(k) < zvoid1) THEN
                        urrad(m) = urrad(m - 1) + ethm1 * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k + &
                          &        eth * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k
                    ELSE
                        urrad(m) = urrad(m-1) + eth * RadialBound(m)
                    ENDIF
                ELSE
                    urrad(m) = urrad(m - 1) + ethm1 * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k + &
                      &        eth * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k
                ENDIF
                urcir(m) = eth * RadialBound(m)
                IF (urcir(m) > urmax) THEN
                    urmax = urcir(m)
                    mmax = m
                ENDIF
            ENDDO
            ! Choose the larger of radial and circumferential thermal expansion
            IF (mmax < 2) mmax = igpnod
            IF (mmax /= igpnod) THEN
                ! JK begin 02
                ! find the left and right neighbour of the urcir maximum
                rr1 = RadialBound(mmax-1)
                rr2 = RadialBound(mmax)
                rr3 = RadialBound(mmax + 1)
                urcir1 = urcir(mmax-1)
                urcir2 = urcir(mmax)
                urcir3 = urcir(mmax + 1)
                ! fit the three pairs of r / urcir(r) with a quadratic polynomial and estimate the true position of
                ! the urcir maximum - it should now be continuous as a function of time
                ! determinant
                detUC = rr1 ** 2 * (rr2 - rr3) + rr2 ** 2 * (rr3 - rr1) + rr3 ** 2 * (rr1 - rr2)
                ! coefficients of the polynomial
                IF (.NOT. (detUC == (0.0_r8k))) THEN
                    urcirA = (urcir1 * (rr2 - rr3) + urcir2 * (rr3 - rr1) + urcir3 * (rr1 - rr2)) / detUC
                    urcirB = -1.0_r8k * (urcir1 * (rr2 ** 2 - rr3 ** 2) + urcir2 * (rr3 ** 2 - rr1 ** 2) + &
                      &       urcir3 * (rr1 ** 2-rr2 ** 2)) / detUC 
                    urcirC = (urcir1 * rr2 * rr3 * (rr2 - rr3) + urcir2 * rr3 * rr1 * (rr3 - rr1) + &
                      &       urcir3 * rr1 * rr2 * (rr1 - rr2)) / detUC
                    rrMax = -urcirB / (2.0_r8k * urcirA)
                    urmax = urcirA * rrMax ** 2 + urcirB * rrMax + urcirC
                    IF (rrMax>RadialBound(mmax)) THEN
                        ! The true maximum is to the right of the mmax meshpoint USE radial thermal expansion_fraptran
                        ! for the rest of the area between rrMax and meshpoint(mmax + 1)
                        rRest = RadialBound(mmax + 1) - rrMax
                        ethm1 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(mmax,k)), &
                          &                  Fraction_Melt=fm1(mmax), Pu=compmt)
                        eth = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(mmax+1,k)), &
                          &                Fraction_Melt=fm2(mmax), Pu=compmt)
                        urcir(mmax + 1) = urmax + rRest * 0.5_r8k * (ethm1 + eth)
                        mStart = mmax + 1
                    ENDIF
                    IF (rrMax<RadialBound(mmax)) THEN
                        rRest = RadialBound(mmax) - rrMax
                        ethm1 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(mmax-1,k)), &
                          &                  Fraction_Melt=fm1(mmax), Pu=compmt)
                        eth = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(mmax,k)), &
                          &                Fraction_Melt=fm2(mmax), Pu=compmt)
                        urcir(mmax) = urmax + rRest * 0.5_r8k * (ethm1 + eth)
                        mStart = mmax
                    ENDIF
                    IF (rrMax == RadialBound(mmax)) mStart = mmax
                    ! original portion of code - build up the radial exp.
                    DO m = (mStart + 1), igpnod
                        ethm1 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(m-1,k)), &
                          &                  Fraction_Melt=fm1(m), Pu=compmt)
                        eth = MatProperty (Material='FUEL', Property='THEXP', Temperature=tfk(EOSTemp(m,k)), &
                          &                Fraction_Melt=fm2(m), Pu=compmt)
                        urcir(m) = urcir(m-1) + ethm1 * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k + &
                          &        eth * (RadialBound(m) - RadialBound(m-1)) / 2.0_r8k
                    ENDDO
                ENDIF
            ENDIF

            ! JK End 02
            ! WRITE(10,1053) Time,mmax, urcir(igpnod), urrad(igpnod)
            ! 1053 FORMAT(e11.4,5x,i3,5x,e11.4,5x,e11.4)
            ! Include initial pellet deformation, defined by input parameter radpel
            IF (urcir(igpnod) < urrad(igpnod) .OR. nthermex == 0) THEN
                DO m = 1, igpnod
                    DeformedRadiusOfMesh(m,k) = RadialBoundO(m,k) + urrad(m)
                ENDDO
            ELSE
                DO m = 1, igpnod
                    DeformedRadiusOfMesh(m,k) = RadialBoundO(m,k) + urcir(m)
                ENDDO
            ENDIF
            !
            CrackVolume(k) = CrakWidth
            ! Temporarily store crack widths at fuel outer surface in CldPermStrn
            ! Note: This term is only given a value but not used. Value given is 0.0_r8k becaUSE c2 _fraptran= 0.0_r8k. IP
            CrackWidth(k) = 2.0_r8k * pi * c2
            ! add fission swelling and relocation terms to radial coordinates
            ! distribute evenly between radial nodes
            DO m = 1, igpnod
                DeformedRadiusOfMesh(m,k) = DeformedRadiusOfMesh(m,k) + (RadialBound(m) / RadialBound(igpnod)) * &
                    &                       (SwellDispl(k) + ureloc(k))
            ENDDO
            !
            IF (ndebug) THEN
                WRITE(ounit,*) ' SwellDispl array:'
                WRITE(ounit,906) (SwellDispl(i), i = 1, naxn)
            ENDIF
            ! modify DeformedRadiusOfMesh to account for transient fission gas fuel swelling
            DeformedRadiusOfMesh(igpnod,k) = DeformedRadiusOfMesh(igpnod,k) * TranFuelSwell
            ! increase thermal expansion by ratio of actual pellet radius to nominal pellet radius
            PelSrfDispl(k) = DeformedRadiusOfMesh(igpnod,k) - RadialBound(igpnod)
            ! in case cladding in contact with fuel, calculate strain rate imposed by movement of fuel pellet surface
            rmid = 0.5_r8k * (RadialBounDO(ncladi,k) + RadialBounDO(nmesh,k))
            PelSrfStrn(k,1) = (DeformedRadiusOfMesh(igpnod,k) - (RadialBound(igpnod) + &
              &                PelRadDeviat(k))) / (RadialBound(igpnod) + PelRadDeviat(k))
            PelSrfStrn(k,3) = PelSrfStrn(k,1)
            !
            DO lnod = 1, igpnod
                EOSRad(lnod,k) = DeformedRadiusOfMesh(lnod,k)
            ENDDO
            ! compute axial growth of fuel stack over node k length
            ! no stress resistance to fuel thermal expansion assumed at pellet interface.
            ! assume two adjacent pellets contact each other at pellet shoulder at radius rshd.
            rsh0 = rshd
            fm = 0.0_r8k
            ! compute shoulder temperature
            DO
                IF (RadialBound(MeltIndex-1) <= rsh0) EXIT
                IF (MeltIndex <= 2) GOTO 330
                MeltIndex = MeltIndex - 1
            END DO
            ! force shoulder radius to be greater than melt radius
320         CONTINUE
            IF (EOSTemp(MeltIndex,k) >= (tmeltf - 1.0_r8k)) rsh0 = RadialBound(MeltIndex+1)
            IF (RadialBound(MeltIndex) >= rsh0) GOTO 350
            IF (MeltIndex >= igpnod) GOTO 340
            MeltIndex = MeltIndex + 1
            GOTO 320
            !
330         CONTINUE
            tshd = EOSTemp(1,k)
            rsh0 = RadialBound(2)
            IF (tshd >= (tmeltf - 1.0_r8k)) GOTO 320
            GOTO 380
            !
340         CONTINUE
            tshd = EOSTemp(MeltIndex,k)
            GOTO 380
            !
350         CONTINUE
            deltr = (rsh0 - RadialBound(MeltIndex-1)) / (RadialBound(MeltIndex) - RadialBound(MeltIndex-1))
            tshd = EOSTemp(MeltIndex-1,k) + (EOSTemp(MeltIndex,k) - EOSTemp(MeltIndex-1,k)) * deltr
            !
380         CONTINUE
            tkshd = tfk(tshd)
            !
            ethxp = MatProperty (Material='FUEL', Property='THEXP', Temperature=tkshd, Fraction_Melt=fm, Pu=compmt)
            delthx = delthx + (ethxp - eth0) * AxialNodLen(k)
            !
            PelSrfStrn(k,2) = ethxp - eth0
            ! strain rate in axial direction at fuel surface
            edotFZ(k) = (PelSrfStrn(k,2) - PelSrfStrn0(k)) / TimeIncrement
            ! IF decreasing strain, assume slippage
            IF (edotFZ(k) < 0.0_r8k) edotFZ(k) = 0.0_r8k
            ! compute current volume of gases stored in pellet dishes
            dvdish = 0.0_r8k
            ! also compute average dish temperature
            dasum = 0.0_r8k
            tsum = 0.0_r8k
            !
            DO m = 1, igpnod
                IF (RadialBound(m) <= rshd) THEN
                    IF (m == 1) THEN
                        da = pi * (0.5_r8k * (RadialBound(m) + RadialBound(m+1))) ** 2
                        tmp1 = EOSTemp(m,k)
                    ELSEIF (RadialBound(m+1) <= rshd) THEN
                        da = pi * ((0.5_r8k * (RadialBound(m+1) + RadialBound(m))) ** 2 - &
                          &        (0.5_r8k * (RadialBound(m) + RadialBound(m-1))) ** 2)
                        tmp1 = EOSTemp(m,k)
                    ELSE
                        da = pi * (rshd ** 2 - (0.5_r8k * (RadialBound(m) + RadialBound(m-1))) ** 2)
                        tmp1 = EOSTemp(m,k)
                    ENDIF
                    dasum = dasum + da
                    tsum = tsum + da * tmp1
                    tkaved = tfk(tmp1)
                    pelhav = pelh - dishd ! Assumes dish is on top & bottom of the pellet
                    q1 = EnrgyMeltZp1(m,k)
                    IF (m == 1) THEN
                        IF (qmaxmeltp1(m) < 1.0e-10_r8k) qmaxmeltp1(m) = 1.0_r8k
                        fm = q1 / qmaxmeltp1(m)
                    ELSEIF (m == igpnod) THEN
                        q1 = EnrgyMeltZ(m,k)
                        qm1 = qmaxmelt(m)
                        IF (qm1 < 1.0e-10_r8k) qm1 = 1.0_r8k
                        fm = q1 / qm1
                    ELSE
                        q1 = EnrgyMeltZ(m,k)
                        q2 = EnrgyMeltZp1(m,k)
                        qm1 = qmaxmelt(m)
                        IF (qm1 < 1.0e-10_r8k) qm1 = 1.0_r8k
                        qm2 = qmaxmeltp1(m)
                        IF (qm2 < 1.0e-10_r8k) qm2 = 1.0_r8k
                        fm = 0.5_r8k * (q1/qm1 + q2/qm2)
                    ENDIF
                    IF (NSteadyTrans == 1 .AND. EOSTemp(m,k) < tmeltf) fm = 0.0_r8k
                    IF (NSteadyTrans == 1 .AND. EOSTemp(m,k) >= tmeltf) fm = 1.0_r8k
                    fexp1 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tkaved, Fraction_Melt=fm, Pu=compmt)
                    fexp2 = MatProperty (Material='FUEL', Property='THEXP', Temperature=tkshd, Fraction_Melt=0.0_r8k, Pu=compmt)
                    dvdsh1 = da * pelhav * (fexp1 - fexp2)
                    dvdish = dvdish + dvdsh1
                ENDIF
            ENDDO
            ! convert from volume/pellet to volume/(unit length)
            DishVperL(k) = MAX(0.0_r8k, (dishv0 - dvdish) / pelh)
            AveDishTemp(k) = tsum / dasum
            ! compute volume of voids due to fuel Open porosity associated with axial node k
            OpenPorVol(k) = OpenPorosity(k) * pi * (RadialBound(igpnod) + PelRadDeviat(k)) ** 2
        ENDDO
        !
        delth = delthx
        ! assume cladding expansion sum of pressure induced strain predicted
        ! by thin wall formula plus temperature induced strain assuming all
        ! cladding at average temperature of cladding
        ! check to see IF cladding ruptured
    ENDIF
    IFail1 = 0
    !
    DO k = 1, naxn
        IF (RuptFailIndex(k) == 1) IFail1 = 1
        IF (RuptFailIndex(k) == 4) IFail1 = 4
    ENDDO
    !
    IF (kdbugx == 1) WRITE(ounit,615) IFail1, ies(1)
615 FORMAT(' RodFailIndex = ',i5,' ies = ',i5)

    IF ((IFail1 == 1) .AND. (mechan /= 1)) THEN
        ! cladding has either failed or is undergoing localized ballooning
        IF (ndebug) WRITE(ounit,*) ' DEFORM AFTER 68'
        !
        daxcld = 0.0_r8k
        DO k = 1, naxn
            tave = CladAveTemp(k)
            ! convert average cladding temperature from F to K
            tavek = tfk(tave)
            deltp = GasPress(k) - CoolPress(k)
            pgap  = GasPress(k)
            pcool = CoolPress(k)
            DO lnod = 1, nmesh
                DeformedRadiusOfMesh(lnod,k) = BOSRad(lnod,k)
            ENDDO
            !
            CladThickness = DeformedRadiusOfMesh(nmesh,k) - DeformedRadiusOfMesh(ncladi,k)
            sigmah = (pgap * DeformedRadiusOfMesh(ncladi,k) - pcool * DeformedRadiusOfMesh(nmesh,k)) / CladThickness
            sigmaz = (pgap * DeformedRadiusOfMesh(ncladi,k) * DeformedRadiusOfMesh(ncladi,k) - pcool * &
              &       DeformedRadiusOfMesh(nmesh,k) * DeformedRadiusOfMesh(nmesh,k)) / &
              &      (DeformedRadiusOfMesh(nmesh,k) * DeformedRadiusOfMesh(nmesh,k) - &
              &       DeformedRadiusOfMesh(ncladi,k) * DeformedRadiusOfMesh(ncladi,k))
            ! zero out cladding stress-strain values passed to prntot
            EinstabilStrain(k) = 0.0_r8k
            ! EffStrain(k) = 0.0_r8k
            StressAtInstStrain(k) = 0.0_r8k
            CladYieldStress(k) = 0.0_r8k
            Vreloc(k) = 0.0_r8k
            TerfacePres(k) = 0.0_r8k
            IF (ABS(sigmah) < sigmin) sigmah = sigmin
            IF (ABS(sigmaz) < sigmin) sigmaz = sigmin
            ! compute thermal strain
            ! set time, flux, and cold work terms to zero
            cath1 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=tavek)
            cdth1 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=tavek)
            cath0 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=t0k)
            cdth0 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=t0k)

            epthr = cdth1 - cdth0
            eptha = cath1 - cath0
            ! plastic hoop strain = eph
            eph = CldPlasStrn(k,1)
            ! epz =  plastic axial strain
            epz = CldPlasStrn(k,2)
            ! compute axial expansion of cladding
            daxcld = daxcld + (epz + eptha) * AxialNodLen(k)
            ! compute total strain, mid radius of clad
            deltr = eph + epthr + 1.0_r8k
            rm = 0.5_r8k * (RadialBounDO(nmesh,k) + RadialBounDO(ncladi,k)) * deltr
            ! clad thins with pressure, expands with thermal strain
            thk = (RadialBounDO(nmesh,k) - RadialBounDO(ncladi,k)) * (1.0_r8k + epthr) ** 2 / deltr
            ! starting at inner edge, compute new mesh interval, new radii
            rr = rm - 0.5_r8k * thk
            den = nmesh - ncladi
            dr = thk / den
            !
            IF (ndebug) THEN
                WRITE(ounit,*)' DEFORM, IMMEDIATELY BEFORE DO 105'
                WRITE(ounit,*) 'k, epz, eptha = ', k, epz, eptha
            ENDIF
            DO m = ncladi, nmesh
                DeformedRadiusOfMesh(m,k) = rr
                rr = rr + dr
            ENDDO
            ! check to see IF interfacial pressure between fuel and cladding
            IF (EOSRad(igpnod,k) >= DeformedRadiusOfMesh(ncladi,k)) THEN
                ! gap closed. compute interfacial pressure, force gap thickness to be zero
                rr = EOSRad(igpnod,k)
                DO m = ncladi, nmesh
                    DeformedRadiusOfMesh(m,k) = rr
                    rr = rr + dr
                ENDDO
            ENDIF
            DO lnod = ncladi, nmesh
                EOSRad(lnod,k) = DeformedRadiusOfMesh(lnod,k)
            ENDDO
            !
            RInterfacGap(k) = EOSRad(ncladi,k) - EOSRad(igpnod,k)
            !
            IF (ndebug) WRITE(ounit,*) ' DEFORM at 130: RInterfacGap, EOSRad(clad), EOSRad(fuel) = ', &
              &                         RInterfacGap(k), EOSRad(ncladi,k), EOSRad(igpnod,k)
            !
            RInterfacGap(k) = MAX(0.0_r8k, RInterfacGap(k))
            GapThick(k) = RInterfacGap(k)
            CldStress(k,1) = sigmah
            CldStress(k,2) = sigmaz
            CldStress(k,3) = 0.5_r8k * (pgap + pcool)
            GapIndex(k) = 0
            CldStrn(k,1) = eph + epthr
            CldStrn(k,2) = epz + eptha
            CldPlasStrn(k,1) = eph
            CldPlasStrn(k,2) = epz
            CldStrn(k,3) = epthr
            IF (ndebug) WRITE(ounit,*) 'k, GapIndex(k), OldGapIndex(k) = ', k, GapIndex(k), OldGapIndex(k)
        ENDDO
        !
        dcldh = daxcld
        IF (ndebug) WRITE(ounit,*) ' ftin, dcldh = ', ftin, dcldh
        CladEffStress(k) = 0.0_r8k
        pelprm(15) = roi(1)
        RETURN
    ELSE
        DO k = 1, naxn
            ! pass thru to fcmi start of time step effective cladding stress
            ! for no sub. stack, shrink WorkSpace...
            WorkSpaceRCI(k) = BOSRad(ncladi,k)
            WorkSpaceRCO(k) = BOSRad(nmesh,k)
            ! compute parmeters needed for repack Subroutine
            ! PelRadDeviat(k) = deviation of fuel pellet radius from nominal
            ! value at axial node k  (ft)
            dpcold = (RadialBound(igpnod) + PelRadDeviat(k)) * 2.0_r8k * ftin
            dgcold = (RadialBound(ncladi) - (RadialBound(igpnod) + PelRadDeviat(k))) * 2.0_r8k * ftin * 1000.0_r8k
            ! compute added radial displacement of fuel due to
            ! "hour glassing," cracking, etc.
            pinf = RInterfacPrs(k) - GasPress(k)
            modfd0 = modfd
            ! 3/19/03 This CALL to REPACK will now RETURN a value of factr1=0; i.e. no
            ! relocation due to hourglassing, so WorkSpaceReloc=0.
            ! PelSrfDispl now only a function of thermal expansion, normal relocation
            ! and transient fuel swelling
            IndexRepack = 1
            CALL repack (factr1, IndexRepack, pinf, dpclos, dpcold, dgcold, modfd0)
            !
            WorkSpaceReloc(k) = factr1(1) * (RadialBound(igpnod) + PelRadDeviat(k))
            IF (nfcall /= 1) THEN
                PelSrfDispl(k) = PelSrfDispl(k) + WorkSpaceReloc(k)
                rmid = 0.5_r8k * (RadialBounDO(ncladi,k) + RadialBounDO(nmesh,k))
                urfold = OldPelDis0(k) / ftin
                FuelSrfStrRat(k) = (PelSrfDispl(k) - urfold) / (TimeIncrement * rmid)
                !
                IF (ndebug) WRITE(ounit,961) k, FuelSrfStrRat(k), PelSrfDispl(k), urfold, rmid
961             FORMAT(' DEFORM at stat. 808, k = ',i5,' FuelSrfStrRat = ',e11.4, &
                  &    ' urf = ',e13.6/' urfold = ',e13.6,' rmid = ',e11.4)
                !
                IF (FuelSrfStrRat(k) < 0.0_r8k) FuelSrfStrRat(k) = 0.0_r8k
            ENDIF
            AxialNodLen(k) = AxialNodLen(k) * ftin
            WorkSpaceTCMx(k) = CladMaxT(k)
        ENDDO
        !
        rf0 = RadialBound(igpnod) * ftin
        rci = RadialBound(ncladi) * ftin
        rco = RadialBound(nmesh) * ftin
        ! convert pellet radius deviation from feet to inches
        PelRadDeviat(1:naxn) = PelRadDeviat(1:naxn) * ftin
        zerloj(1:naxn) = 0.0_r8k
        !
        pitc2 = pitch * ftin
        IF (ndebug) THEN
            WRITE(ounit,903)
903         FORMAT(' debug printout from DEFORM')
            WRITE(ounit,910) n2
910         FORMAT(' n2 = ',i3)
            WRITE(ounit,904)
904         FORMAT(' WorkSpaceRCI array')
            WRITE(ounit,908) (WorkSpaceRCI(l),l = 1, naxn)
908         FORMAT(8(2x,e11.4))
            WRITE(ounit,6001)
6001        FORMAT(' gap pressure vector ')
            WRITE(ounit,908) (GasPress(l),l = 1, naxn)
            WRITE(ounit,6005)
6005        FORMAT(' coolant pressure vector ')
            WRITE(ounit,908) (CoolPress(l),l = 1, naxn)
            WRITE(ounit,6007)
6007        FORMAT(' cladding temperature vector  ')
            WRITE(ounit,908) (CladAveTemp(l),l = 1, naxn)
            WRITE(ounit,6009)
6009        FORMAT(' permanent cladding hoop strain vector ')
            WRITE(ounit,908) (CldPlasStrn(l,1),l = 1, naxn)
            WRITE(ounit,6011)
6011        FORMAT(' fuel displacement vector ')
            WRITE(ounit,908) (PelSrfDispl(l),l = 1, naxn)
            WRITE(ounit,6013)
6013        FORMAT(' equivalent permanent strain vector ')
            WRITE(ounit,908) (EffStrain(l),l = 1, naxn)
            WRITE(ounit,6015)
6015        FORMAT(' node length vector ')
            WRITE(ounit,908) (AxialNodLen(l),l = 1, naxn)
            WRITE(ounit,6017) rci, rco, dcldh
6017        FORMAT(' rci = ',e13.6,' rco = ',e13.6,' dcldh = ',e13.6)
        ENDIF
        ! CALL FE or fcmi subcode to compute cladding deformation
        IF (mechan == 1) THEN
            CALL mech (Time, TimeIncrement, nce,  naxn, ncladi, nmesh, frcoef, RadialBounDO, RadialBound, AxialNodLen, &
              &        vplen(1), CladAveTemp, EOSTemp, BulkCoolTemp, GasPress, CoolPress, PelSrfDispl, PelSrfStrn, rf0, &
              &        0.0_r8k, GapIndex, GapThick, RInterfacGap, RInterfacPrs, CldStrn, CldPlasStrn, dcldh,  dlrel, &
              &        CldElStrn, PelRadDeviat, pitch, OxygenConcenAve, EffFastFluStrenCoef, EffFastFluStrnHardExp, &
              &        EffColdWkStrenCoef, EffColdWkStrnHardExp, CldStress, TotalGasMoles, CrackVolume, CrackTemp, &
              &        VolOpenPor, OpenPorTemp, volbp, tplenb, roughc, roughf, DishVperL, AveDishTemp, RuptFailIndex, &
              &        EOSRad, CladEffStress, DeformedRadiusOfMesh, CldThermStrn, CldPermStrn, irupt, ruptstrain, nodpln, &
              &        tplna, irefine, refine, IFaila, IFail, RodFailIndex, coefk, coefn, coefm, Emodulus, CladYieldStress, &
              &        NFrapconInitialization, ntstep, EffStrain)
            ! LOJ Correction added - otherwise never converted back to ft.
            ! Also return contact pressure
            PelRadDeviat(1:naxn) = PelRadDeviat(1:naxn) / ftin
            TerfacePres(1:naxn) = RInterfacPrs(1:naxn)
            RETURN
        ELSE
            !LOJ Modified call.  Substitute PelRadDeviat with zeros
            CALL fcmi (GasPress, CoolPress, CladAveTemp, GapIndex, CldStrn, CldPlasStrn, PelSrfDispl, CldStress, &
              &        CldResidStrn, FuelResidStrn, PelSrfStrn, EffStrain, AxialNodLen, naxn, n2, WorkSpaceGAP, &
              &        WorkSpacePINT,RadialBounDO, dcldh, dlrel, OldCldAxStrn, OldFuelAxStrn, rf0, ldir, kdbugx, &
              &        OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, BOSGapIndex, WorkSpaceGAPI, WorkSpaceEPP1, &
              &        WorkSpaceEP1, WorkSpaceURC, WorkSpaceTCMx, tempcs, CldStrnRat, FastFlux, tflux, coldw, &
              &        WorkSpaceRCI, WorkSpaceRCO, NSteadyTrans, TimeIncrement, nedtsw, nconvg, pitc2, FuelSrfStrRat, &
              &        FuelSrfStrRat0, edotFZ, edotFZ0, zerloj, CldElStrn, nmesh, ncladi)
        ENDIF
        !
        IF (kdbugx == (-1)) THEN
            WRITE(ounit,900)
900         FORMAT(////,' *** non-convergence in fcmi - reduce time step ***')
            kdbugx = 0
        ENDIF
        !
        IF (kdbugx >= 1 .OR. ndebug) THEN
            WRITE(ounit,6061)
6061        FORMAT(' output quantities from fcmi ')
            WRITE(ounit,6063)
6063        FORMAT(' total cladding hoop strain vector ')
            WRITE(ounit,908) (CldStrn(l,1),l = 1, naxn)
            WRITE(ounit,6065)
6065        FORMAT(' total cladding axial strain vector ')
            WRITE(ounit,908) (CldStrn(l,2),l = 1, naxn)
            WRITE(ounit,6067)
6067        FORMAT(' permanent cladding hoop strain vector ')
            WRITE(ounit,908) (CldPlasStrn(l,1),l=1, naxn)
            WRITE(ounit,6069)
6069        FORMAT(' cladding hoop stress vector  ')
            WRITE(ounit,908) (CldStress(l,1),l = 1, naxn)
            WRITE(ounit,6071)
6071        FORMAT(' cladding axial stress vector ')
            WRITE(ounit,908) (CldStress(l,2),l = 1, naxn)
            WRITE(ounit,6073)
6073        FORMAT(' equivalent permanent strain vector ')
            WRITE(ounit,908) (EffStrain(l),l = 1, naxn)
            WRITE(ounit,6075)
6075        FORMAT(' gap thickness vector ')
            WRITE(ounit,908) (WorkSpaceGAP(l),l = 1, naxn)
            WRITE(ounit,6077)
6077        FORMAT(' interfacial pressure vector ')
            WRITE(ounit,908) (WorkSpacePINT(l),l = 1, naxn)
            WRITE(ounit,6079) dcldh
6079        FORMAT(' dcldh = ',e11.4)
        ENDIF
        ! convert output of fcmi from inches to feet
        DO k = 1, naxn
            ! store end of time step value of effective cladding stress
            dpcold = (RadialBound(igpnod) + PelRadDeviat(k)) * 2.0_r8k * ftin
            dgcold = (RadialBound(ncladi) - (RadialBound(igpnod) + PelRadDeviat(k))) * 2.0_r8k * ftin * 1000.0_r8k
            PelRadDeviat(k) = PelRadDeviat(k) / ftin
            AxialNodLen(k) = AxialNodLen(k) / ftin
            IF (WorkSpacePINT(k) < GasPress(k)) WorkSpacePINT(k) = GasPress(k)
            TerfacePres(k) = WorkSpacePINT(k) - GasPress(k)
            pinf = TerfacePres(k)
            !
            IndexRepack = 2
            CALL repack (factr1, IndexRepack, pinf, dpclos, dpcold, dgcold, modfd)
            !
            GapThick(k) = MAX(0.0_r8k, WorkSpaceGAP(k) / ftin)
            ! calculate volume strain (volume generated by fuel relocation)/
            ! (pellet volume) this quantity used in Subroutine kmod
            ! add on extra volume cracking strain to close gap
            ! If Fracas-2 relocation, thermal gap is always zero
            ! find difference between interfacial pressure at pellet midplane and pellet interfaces
            !
            IF (WorkSpacePINT(k) < GasPress(k)) WorkSpacePINT(k) = GasPress(k)
            pinf = WorkSpacePINT(k) - GasPress(k)
            !
            IndexRepack = 3
            CALL repack (factr1, IndexRepack, pinf, dpclos, dpcold, dgcold, modfd)
            !
            IF (ndebug) WRITE(ounit,*)'after 3rd REPACK CALL: factr1(1,2,3), pinf, dpcold, dgcold, modfd= ', &
              &                        factr1(1), factr1(2), factr1(3), pinf, dpcold, dgcold, modfd
            !
            RInterfacPrs(k) = pinf
            RInterfacGap(k) = MAX(0.0_r8k, WorkSpaceGAP(k) / ftin)
            CladThickness = (1.0_r8k + CldStrn(k,3)) * (RadialBounDO(nmesh,k) - RadialBounDO(ncladi,k))
            rmp0 = 0.5_r8k * (RadialBounDO(ncladi,k) + RadialBounDO(nmesh,k))
            rmp = (1.0_r8k + CldStrn(k,1)) * rmp0
            rpi = rmp - CladThickness / 2.0_r8k
            EOSRad(igpnod,k) = RadialBound(igpnod) + PelRadDeviat(k) + PelSrfDispl(k)
            !write(*,*) '1. eosrad = ', EOSRad(ncladi,k)
            !
            DO l = ncladi, nmesh
                EOSRad(l,k) = rpi + ((RadialBounDO(l,k) - RadialBounDO(ncladi,k)) / &
                  &         (RadialBounDO(nmesh,k) - RadialBounDO(ncladi,k))) * CladThickness
            ENDDO
            !write(*,*) '2. eosrad = ', EOSRad(ncladi,k)
            !
            IF (ndebug) THEN
                WRITE(ounit,*) 'EOSRad values for k = ',k
                WRITE(ounit,906) (EOSRad(i,k),i = 1,nmesh)
906             FORMAT(8(2x,e12.5))
            ENDIF
            ! GapThick0 stores value of structural gap at end of steady state conditions.
            IF (NSteadyTrans == 1) GapThick0(k) = RInterfacGap(k)
            ! IF Time less than zero, this usually means power needed
            ! to be ramped to full power using transient heat conduction
            ! solution. so assume equivalent of steady state solition for time less than zero.
            IF (Time < 0.0_r8k .AND. RInterfacGap(k) < -GapThick0(k)) GapThick0(k) = RInterfacGap(k)
            IF (GapThick(k) < 0.0_r8k) GapThick(k) = 0.0_r8k
            CldPermStrn(k) = CldPlasStrn(k,3)
        ENDDO
        !
        dcldh = dcldh / ftin
        ! axial loop

        DO k = 1, naxn
            ! get local total hydrogen for cladding properties
            cexh2l = cexh2a(k)
            pgap = GasPress(k)
            pcool = CoolPress(k)
            deltp = pgap - pcool
            IF (RInterfacPrs(k) > 0.0_r8k) deltp = RInterfacPrs(k) - pcool
            cdpres = ABS(deltp * 6894.76_r8k)
            IF (cdpres < 100.0_r8k) cdpres = 100.0_r8k
            sigmah = CldStress(k,1)
            sigmaz = CldStress(k,2)
            tave = CladAveTemp(k)
            tavek = tfk(tave)
            !
            cath1 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=tavek)
            cdth1 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=tavek)
            cath0 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=t0k)
            cdth0 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=t0k)
            !
            epthr = cdth1 - cdth0
            eph = CldStrn(k,1) - epthr
            epz = CldStrn(k,2) - (cath1 - cath0)
            CldThermStrn(k,1) = cdth1 - cdth0
            CldThermStrn(k,2) = cath1 - cath0
            edot1 = CldStrnRat(k,1)
            edot2 = 0.0_r8k
            edot3 = 0.0_r8k
            edot = edot1
            !
            DO lnod = ncladi, nmesh
                DeformedRadiusOfMesh(lnod,k) = EOSRad(lnod,k)
            ENDDO

            !
            CladThickness = DeformedRadiusOfMesh(nmesh,k) - DeformedRadiusOfMesh(ncladi,k)
            ! IFail1 is indicator of overall rod failure at start of time step
            IFail = IFail1
            CladEffStress(k) = 0.0_r8k
            EinstabilStrain(k) = 0.0_r8k
            ! EffStrain(k) = 0.0_r8k
            StressAtInstStrain(k) = 0.0_r8k
            CladYieldStress(k) = 0.0_r8k

            IF (IFail /= 1 .AND. IFail /= 4) THEN
                ! check for cladding failure
                IF (pfail <= 2.0_r8k) THEN
                    IF (nbncal /= 1) chstrs = 1.0e7_r8k
                    !
                    cinrad = 0.5_r8k * (RadialBounDO(nmesh,k) + RadialBounDO(ncladi,k)) * 0.3048_r8k
                    cinwid = (RadialBounDO(nmesh,k) - RadialBounDO(ncladi,k)) * 0.3048_r8k
                    caxrad = 100.0_r8k
                    caxstr = 0.0_r8k
                    ! CALL Subroutine cmlimt to get ultimate stress
                    CALL cmlimt (tavek, OxygenConcenAve(k), EffFastFluStrenCoef(k), EffFastFluStrnHardExp(k), &
                      &          EffColdWkStrenCoef(k), EffColdWkStrnHardExp(k), cinwid, cinrad, cdpres, &
                      &          caxrad, caxstr, edot, dtmpcl, dum1, dum2, eiunfm, eistab, dum4, erup, cyldst, &
                      &          dum6, culste, dum8, dum9, dum10, coefk(k), coefn(k), coefm(k), Emodulus(k))
                    !
                    cyldst = cyldst / 6894.76_r8k
                    strainrateterm(k) = edot
                    !
                    IF (ndebug) WRITE(ounit,902) Time, edott, edot, dum2, culste, tavek
902                 FORMAT(' DEFORM, Time = ',e13.6,' edott = ',e11.4,' edot = ',e11.4, &
                      &    ' dum2 = ',e11.4,' culste = ',e11.4/' tavek = ',e11.4)
                    ! After cmlimt in deform, if balloon subcode not called, reduce matpro instablility strain 
                    ! to account for non-uniform cladding temperature
                    IF (nbalsw == 1) eistab = 0.6_r8k * eistab
                    CladEffStress(k) = SQRT(0.5_r8k * ((CldStress(k,1) - CldStress(k,2)) ** 2 + (CldStress(k,2) - &
                        &                CldStress(k,3)) ** 2 + (CldStress(k,3) - CldStress(k,1)) ** 2))
                    ! edot = edott
                ENDIF
                !
                seff = 0.70710678_r8k * (SQRT((CldStress(k,1) - CldStress(k,2)) ** 2 + CldStress(k,2) ** 2 + &
                    &    CldStress(k,1) ** 2)) / cnvstr
                ! IF fcmi calculations to rod contact, DO not cut off at insta.stran
                IF (pitch > 1.0e-6_r8k) eistab = erup
                CladEffStress(k) = seff * cnvstr
                IFail = RuptFailIndex(k)
                IF (ndebug) WRITE(ounit,909) IFail, pfail, eiunfm, eistab, erup, EffStrain(k), eph, nbncal
909             FORMAT(' RuptFailIndex = ',i6,' pfail = ',e11.4,' eiunfm = ',e10.3,' eistab = ',e10.3, &
                  &    ' erup = ',e10.3,2x/,' EffStrain = ,',e10.3,' eph = ',e10.3,' nbncal = ',i3)
                ! Fracas-1 calculations tripped off by IFail = 1
                ! in t5 IFail = 3 tripped on balloon subcode, but in t6 IFail = 3 is
                ! not used. balloon code tripped on by knonune = 1 (node at which
                ! effective strain first exceeds maximum unIForm strain.
                !
                ! IF (EffStrain(k) >= eistab) IFail = 1
                ! IF (eph >= erup) IFail = 1
                ! Find strain limits
                IF (tavek >= 940.0_r8k .AND. tavek < 1200.0_r8k) THEN
                    strnlim = 1906.22_r8k - 7.331051_r8k * tavek + 0.01053049_r8k * tavek ** 2 &
                        &     - 6.692798e-6_r8k * tavek ** 3 + 1.587979e-9_r8k * tavek ** 4
                ELSE IF (tavek >= 1200.0_r8k .AND. tavek < 1700.0_r8k) THEN
                    strnlim = 28.1199_r8k - 7.36049e-2_r8k * tavek + 6.23050e-5_r8k * tavek ** 2 &
                        &     - 1.67939e-8_r8k * tavek ** 3
                ELSE IF (tavek >= 1700.0_r8k) THEN
                    strnlim = 0.544589_r8k
                ENDIF
                ! Check to see if strain is above strain limit
                IF (tavek >= 940.0_r8k .AND. eph >= strnlim) THEN
                    IFbaln = 1
                    IFail = 1
                ENDIF
                !
                IF (nbalsw == 1 .AND. IFail > 0) IFail = 4
                IFailz = RuptFailIndex(k)
                tcmaxf = CladMaxT(k)
                tcladf = CladAveTemp(k)
                EinstabilStrain(k) = eistab
                dep = 0.0_r8k
                StressAtInstStrain(k) = 0.0_r8k
                CladYieldStress(k) = 0.0_r8k
                epeff = ABS(EffStrain(k))

                ! find effective stress at instability strain
                CALL stress (StressAtInstStrain(k), eistab, dep, tcladf, edot1, edot2, edot3, tcmaxf, coldw, k)
                !
                epeff = EffStrain(k)
                IF (epeff > eistab) epeff = eistab
                edot1 = CldStrnRat(k,1)
                ! find effective stress at current effective strain
                CALL stress (StressAtInstStrain(k), epeff, dep, tcladf, edot1, edot2, edot3, tcmaxf, coldw, k)
                !
                IF (ndebug) WRITE(ounit,978) Time, k, StressAtInstStrain(k), edot1
978             FORMAT(' DEFORM, Time = ',e13.6,' k = ',i3,' StressAtInstStrain = ',e11.4,' edot1 = ',e11.4)
                !
                CladYieldStress(k) = cyldst
                ! Multiply by Uncertainty coefficient
                CladYieldStress(k) = CladYieldStress(k) * sigcladyieldstr
                RodFailIndex(k) = IFail
                IF (nbncal /= 1) THEN
                    Updateknonue = .TRUE.
                    ! IF plastic hoop strain exceeded 0.05 in previous iteration,
                    ! but is less than 0.05 for current iteration, turn off switch for balloon calculations.
                    IF (k == knonue .AND. CldPlasStrn(k,1) < 0.05_r8k) knonue = 0
                    IF (CldStress(k,1) <= 0.0_r8k) Updateknonue = .FALSE.
                    IF (CldPlasStrn(k,1) < 0.05_r8k) Updateknonue = .FALSE.
                    IF (RInterfacPrs(k) > 1.0e-10_r8k) Updateknonue = .FALSE.
                    IF (nbalsw == 1) Updateknonue = .FALSE.
                    IF (EffStrain(k) < eiunfm .OR. nbncal /= 0) Updateknonue = .FALSE.
                    IF (Updateknonue) THEN
                        roi(1) = 0.5_r8k * (EOSRad(ncladi,k) + EOSRad(nmesh,k))
                        knonue = k
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
        pelprm(15) = roi(1)
    ENDIF
    !
    END SUBROUTINE deform
    !
    !
    !
    SUBROUTINE fcmi (GasPress, CoolPress, CladAveTemp, GapIndex, CldStrn, CldPlasStrn, PelSrfDispl, &
      &              CldStress, CldResidStrn, FuelResidStrn, PelSrfStrn, EffStrain, AxialNodLen, &
      &              naxn, n2, WorkSpaceGAP, WorkSpacePINT, RadialBoundO, dlrod, dlrel, OldCldAxStrn, &
      &              OldFuelAxStrn, rp, ldir, idbg, OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, &
      &              OldGapIndex, WorkSpaceGAPI, WorkSpaceEPP1, WorkSpaceEP1, WorkSpaceURC, &
      &              WorkSpaceTCMx, tempcs, CldStrnRat, FastFlux, tflux, coldw, WorkSpaceRCI, &
      &              WorkSpaceRCO, NSteadyTrans, TimeIncrement, nedtsw, nconvg, pitch, FuelSrfStrRat, &
      &              FuelSrfStrRat0, EDotFZ, EDotFZ0, PelRadDeviat, CldElStrn, nmesh, ncladi)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : ftin
    USE variables_fraptran, ONLY : ounit, e, pois, CladType, Time, ndebug
    IMPLICIT NONE
    !>@brief
    !> This package of subroutines performs an elasto-plastic analysis of a typical LWR fuel rod. 
    !> Fuel radial displacements and axial strains at each axial node are input.
    !
    ! Input/Output
    !
    ! GasPress            - Internal gas pressure, same for all nodes
    ! CoolPress           - External coolant pressure, same for all nodes
    ! CladAveTemp(k)      - Clad average temperature at node k
    ! CldStress(k,ldir)   - Clad hoop,axial and radial stress at node k
    ! CldStrn(k,ldir)     - Clad hoop,axial and radial strain at node k
    ! CldPlasStrn(k,ldir) - Clad hoop,axial and radial plastic strains at node k
    ! PelSrfDispl(k)      - Fuel radial displacement at node k
    ! EffStrain(k)        - Clad eff. plastic strain at node k
    ! AxialNodLen(k)      - Length of nodes in axial direction
    ! WorkSpaceGAP(k)     - Gap width between fuel and clad at node k (in)
    ! WorkSpacePINT(k)    - Interface pressure (If any) between fuel + clad
    ! nn                  - Number of axial nodes ( l.e.q. 20 )
    ! n2                  - Maximum number of axial nodes that arrays are Dimensioned for in main
    ! variables GapIndex through WorkSpace... are only for temporary storage
    ! values are not input or output through these arrays
    ! WorkSpaceTCMx(k)    - peak cladding surface temperature in past at axial node k (F)
    ! WorkSpaceRCI(k)     - radius to cladding inner surface at axial node k (ft)
    ! WorkSpaceRCO(k)     - radius to cladding outer surface at axial node k (ft)
    ! PelRadDeviat(k)     - deviation of fuel pellet radius from nominal at axial node k  (inches)   (input)
    ! ncladi              - radial node at inside cladding surface
    ! NSteadyTrans        - switch to indicate steady-state or transient (1 = SS, 2 = transient)
    ! TimeIncrement       - time step (sec)
    ! rci                 - Cladding inside radius (in)
    ! rco                 - Cladding outside radius (in)
    ! rp                  - Fuel pellet outside radius (in)
    ! tedot - threshold temperature to bypass cladf call, cladding collapse onto fuel assumed (F)
    ! prthr - corresponding delta pressure (psi)
    !
    INTEGER(ipk), INTENT(IN) :: nmesh, ncladi
    INTEGER(ipk) :: nconvg, k, naxn, nconv, nedtsw, n2, ldir, nsteadytrans, idbg
    REAL(r8k) :: rci, rco, rmp0, cladthickness0, rcim, rcom, delta, rp, fs, edot1, edot2, edot3, &
      &          cep, deltpr, tempcs, tflux, coldw, timeincrement, pitch, rfo, edot01, edot02, &
      &          edot03, dlrod, dlrel
    REAL(r8k), PARAMETER :: tedot = 3000.0_r8k
    REAL(r8k), PARAMETER :: prthr = 100.0_r8k
    INTEGER(ipk), DIMENSION(:) :: OldGapIndex, GapIndex
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp
    REAL(r8k), DIMENSION(naxn) :: PelSrfDisplin
    REAL(r8k), DIMENSION(:) :: CladAveTemp, GasPress, AxialNodLen, CoolPress, EffStrain, PelSrfDispl, &
      &                        WorkSpaceEP1, WorkSpaceURC, WorkSpacePINT, WorkSpaceGAP, OldCldAxStrn, &
      &                        OldFuelAxStrn, OldCladT, OldGasPrs, WorkSpaceTCMx, WorkSpaceRCI, OldCoolPrs, &
      &                        OldPelDis, FastFlux, WorkSpaceRCO, PelRadDeviat, FuelSrfStrRat, FuelSrfStrRat0, &
      &                        EDotFZ, EDotFZ0, WorkSpaceGAPI
    REAL(r8k), DIMENSION(:,:) :: CldStrn, CldPlasStrn, WorkSpaceEPP1, CldStress, PelSrfStrn, FuelResidStrn, &
      &                          CldStrnRat, CldResidStrn, CldElStrn, RadialBoundO
    ! Find free clad displacement due to internal and external pressures. Compute a gap width.
    nconvg = 1
    !
    DO k = 1, naxn
        rci = RadialBoundO(ncladi,k) * ftin
        rco = RadialBoundO(nmesh,k) * ftin
        rmp0 = 0.5_r8k * (rci + rco)
        CladThickness0 = rco - rci
        PelSrfDisplin(k) = PelSrfDispl(k) * ftin
        rcim = WorkSpaceRCI(k) * ftin
        rcom = WorkSpaceRCO(k) * ftin
        delta = rci - (rp + PelRadDeviat(k))
        IF (rcim - (rp + PelRadDeviat(k)) <= delta) THEN ! when cladding not ballooning, pass cold state radial coordinates to cladf
            rcim = rci
            rcom = rco
        ENDIF
        fs = 0.0_r8k
        edot1 = CldStrnRat(k,1)
        edot2 = CldStrnRat(k,2)
        edot3 = CldStrnRat(k,3)
        cepp(1) = CldPlasStrn(k,1)
        cepp(2) = CldPlasStrn(k,2)
        cepp(3) = CldPlasStrn(k,3)
        cep = EffStrain(k)
        nconv = k
        ! If cladding obviously collapsed onto fuel, bypass cladf call
        deltpr = CoolPress(k) - GasPress(k)
        IF (ndebug) WRITE(ounit,903) k, GapIndex(k), OldGapIndex(k), CladAveTemp(k), tedot, deltpr, prthr
903     FORMAT(' FCMI before CLADF, k = ',i2,' GapIndex(k) = ',i2,' OldGapIndex(k) = ',i2/19x, &
          &    ' CladAveTemp(k) = ',e11.4,' tedot = ',e11.4,' deltpr = ',e11.4,' prthr = ',e11.4)
        IF (OldGapIndex(k) == 0 .OR. CladAveTemp(k) < tedot) THEN

            CALL cladf (GasPress(k), CoolPress(k), CladAveTemp(k), rcim, rcom, csig, ceps, cepp, &
              &         cep, fs, tempcs, edot1, edot2, edot3, WorkSpaceTCMx(k), tflux, coldw, &
              &         TimeIncrement, nedtsw, nconv, rmp0, CladThickness0, pitch, k, e, pois)
            !
            IF (nconv == 0) nconvg = 0
            CldStrnRat(k,1) = edot1
            CldStrnRat(k,2) = 0.0_r8k
            CldStrnRat(k,3) = 0.0_r8k
            CldStress(k,1) = csig(1)
            CldStress(k,2) = csig(2)
            CldStress(k,3) = 0.0_r8k
            CldStrn(k,1) = ceps(1)
            CldStrn(k,2) = ceps(2)
            CldStrn(k,3) = ceps(3)
            ! Note: If ballooned, Elastic strain will be 0.0 due to same internal & external pressure on cladding
            CldElStrn(k,1) = (csig(1) - pois * csig(2)) / e
            CldElStrn(k,2) = (csig(2) - pois * csig(1)) / e
            CldElStrn(k,3) = - pois * (csig(1) + csig(2)) / e
            WorkSpaceEPP1(k,1) = cepp(1)
            WorkSpaceEPP1(k,2) = cepp(2)
            WorkSpaceEPP1(k,3) = cepp(3)
            WorkSpaceEP1(k) = cep
            IF (ndebug) WRITE(ounit,901) k, edot1, CladAveTemp(k), cep, csig(1)
901         FORMAT(' FCMI after CLADF, k = ',i2,' edot1 = ',e11.4,' tc = ',e11.4,' cep = ',e11.4,' csig1 = ',e11.4)
            WorkSpaceURC(k) = 0.5_r8k * (rcom + rcim) * CldStrn(k,1) - 0.5_r8k * (rcom - rcim) * CldStrn(k,3)
            ! now check for interference
            WorkSpaceGAP(k) = WorkSpaceURC(k) + delta - PelSrfDisplin(k)
            IF (ndebug) WRITE(ounit,908) k, WorkSpaceGAP(k), WorkSpaceURC(k), delta, PelSrfDisplin(k), &
              &                          GapIndex(k), OldGapIndex(k), Time
908         FORMAT(' FCMI after CLADF k = ',i2,' WorkSpaceGAP = ',e11.4,' WorkSpaceURC = ',e11.4, ' delta = ',e11.4,/, &
              &    18x,' PelSrfDisplin = ',e11.4,' GapIndex = ',i2,' OldGapIndex = ',i2,' Time = ',e11.4)
            !
            IF (WorkSpaceGAP(k) <= 0.0_r8k) THEN
                GapIndex(k) = 1
                WorkSpaceGAPI(k) = - WorkSpaceGAP(k)
            ELSE
                GapIndex(k) = 0
            ENDIF
        ELSE
            GapIndex(k) = 1
        ENDIF
    ENDDO
    ! This loop determines whether cladf or couple solution is appropriate
    DO k = 1, naxn
        rci = RadialBoundO(ncladi,k) * ftin
        rco = RadialBoundO(nmesh,k) * ftin
        rmp0 = 0.5_r8k * (rci + rco)
        CladThickness0 = rco - rci
        delta = rci - (rp + PelRadDeviat(k))
        ! strain rate = (velocity of fuel surface)/(fuel pellet radius)
        edot1 = FuelSrfStrRat(k)
        edot2 = 0.0_r8k
        edot3 = 0.0_r8k
        !
        IF (GapIndex(k) == 1) THEN
            ! node k is in contact
            ! compute prescribed axial strain in clad based on last values of axial strain prior to contact. 
            ! local fuel radial displacement is passed to *couple* .
            IF (OldGapIndex(k) == 0) THEN
                nconv = k
                rcim = rci
                rcom = rco
                !
                CALL closure (CladAveTemp, GasPress, CoolPress, PelSrfDisplin, delta, CldPlasStrn, &
                  &         EffStrain, rcim, rcom, rfo, OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, &
                  &         PelSrfStrn, FuelResidStrn, CldResidStrn, OldCldAxStrn, OldFuelAxStrn, &
                  &         n2, ldir, k, edot1, edot2, edot3, WorkSpaceTCMx(k), FastFlux(k), coldw, &
                  &         TimeIncrement, NSteadyTrans, nedtsw, nconv, rmp0, CladThickness0, pitch, &
                  &         tempcs)
            ENDIF
            CldStrn(k,2) = CldResidStrn(k,2) + (PelSrfStrn(k,2) - FuelResidStrn(k,2))
            IF (ndebug) WRITE(ounit,998) k, CldStrn(k,2), PelSrfStrn(k,2), FuelResidStrn(k,2)
998         FORMAT(' FCMI after CLOSE call: k = ',i2,' CldStrn(k,2) = ',e11.4,' PelSrfStrn(k,2) = ',e11.4, &
              &    ' FuelResidStrn(k,2) = ',e11.4)
            ceps(2) = CldStrn(k,2)
            cepp(1) = CldPlasStrn(k,1)
            cepp(2) = CldPlasStrn(k,2)
            cepp(3) = CldPlasStrn(k,3)
            cep = EffStrain(k)
            ! no slip assumed in axial direction as long as interface pressure is above local gas pressure
            ! set strain rates
            edot1 = FuelSrfStrRat(k)
            edot2 = EDotFZ(k)
            edot3 = -edot1 - edot2
            edot01 = FuelSrfStrRat0(k)
            edot02 = EDotFZ0(k)
            edot03 = -edot01 - edot02
            !
            CALL couple (rci, rco, csig, ceps, cepp, cep,PelSrfDisplin(k), delta, WorkSpacePINT(k), &
              &          tempcs, idbg, edot1, edot2, edot3, WorkSpaceTCMx(k), tflux, coldw, &
              &          TimeIncrement, k, edot01, edot02, edot03, CladThickness0, e, pois)
            ! since no ballooning occurring, set strain rates to zero
            ! If idbg = -1, couple did not converge
            IF (idbg == -1) RETURN
            CldStress(k,1) = csig(1)
            CldStress(k,2) = csig(2)
            CldStress(k,3) = 0.0_r8k
            CldStrn(k,1) = ceps(1)
            CldStrn(k,2) = ceps(2)
            CldStrn(k,3) = ceps(3)
            CldElStrn(k,1) = (csig(1) - pois * csig(2)) / e
            CldElStrn(k,2) = (csig(2) - pois * csig(1)) / e
            CldElStrn(k,3) = - pois * (csig(1) + csig(2)) / e
            CldPlasStrn(k,1) = cepp(1)
            CldPlasStrn(k,2) = cepp(2)
            CldPlasStrn(k,3) = cepp(3)
            EffStrain(k) = cep
            CldStrnRat(k,1) = edot1
            IF (ndebug) WRITE(ounit,905) k, edot1, csig(1), cep, CladAveTemp(k), CldStrn(k,2), CldPlasStrn(k,2)
905         FORMAT(' FCMI after COUPLE call, k = ',i2,' edot1 = ',e11.4,' csig1 = ',e11.4,' cep = ',e11.4,/, &
              &    24x,' tc = ',e11.4,' CldStrn(k,2) = ',e11.4,' CldPlasStrn(k,2) = ',e11.4)
            CldStrnRat(k,2) = 0.0_r8k
            CldStrnRat(k,3) = 0.0_r8k
            WorkSpaceGAP(k) = 0.0_r8k
        ELSE If (GapIndex(k) == 0) THEN
            CldResidStrn(k,2) = CldStrn(k,2)
            FuelResidStrn(k,2) = PelSrfStrn(k,2)
            WorkSpaceURC(k) = 0.5_r8k * (rco + rci) * CldStrn(k,1) - 0.5_r8k * (rco - rci) * CldStrn(k,3)
            WorkSpaceGAP(k) = WorkSpaceURC(k) + delta - PelSrfDisplin(k)
            WorkSpacePINT(k) = GasPress(k)
            CldPlasStrn(k,1) = WorkSpaceEPP1(k,1)
            CldPlasStrn(k,2) = WorkSpaceEPP1(k,2)
            CldPlasStrn(k,3) = WorkSpaceEPP1(k,3)
            EffStrain(k) = WorkSpaceEP1(k)
        ELSE
            WRITE(ounit,906) GapIndex(k)
906         FORMAT(' FCMI: Gap closure index should be 0 or 1 but =: ',i9)
            ERROR STOP ' Gap closure index out of bounds in subroutine: FCMI'
        ENDIF
    ENDDO
    ! Compute rod length and displacement between fuel stack + clad
    dlrod = 0.0_r8k
    dlrel = 0.0_r8k
    !
    DO k = 1, naxn
        dlrod = dlrod + CldStrn(k,2) * AxialNodLen(k)
        dlrel = dlrel + (CldStrn(k,2) - PelSrfStrn(k,2)) * AxialNodLen(k)
    ENDDO
    !
    END SUBROUTINE fcmi
    !
    !
    !
    REAL(r8k) FUNCTION gapt (f_Input, CladT, GasPress, CoolPress, PelSrfDisplin, delta, CldPlasStrn, EffStrain, &
      &           rci, rco, rfo, ceps, cepp, cep, csig, OldCladT, OldGasPrs, OldCoolPrs, OldPelDis, &
      &           t0, n2, ldir, k, edot1, edot2, edot3, tcmax, FastFlux, coldw, TimeIncrement, &
      &           nedtsw, nconv, rmp0, CladThickness0, pitch)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit, naxn, Time, ndebug
    IMPLICIT NONE
    !>@brief
    !> Function computes the gap thickness
    !
    ! Input
    !
    ! CladT          - Cladding temperature (F)
    ! CladThickness0 - Initial cold state cladding thickness (in)
    ! CoolPress      - Coolant pressure (psia)
    ! GasPress       - Gap gas pressure (psia)
    ! OldCladT       - Previous converged cladding temperature (F)
    ! OldCoolPrs     - Previous converged coolant pressure (psia)
    ! OldGasPrs      - Previous converged gap gas pressure (psia)
    ! OldPelDis      - Previous converged outward displacement of the fuel pellet surface (in)
    ! PelSrfDisplin  - Outward displacement of the fuel pellet surface (in)
    ! rci            - Cladding inside radius (in)
    ! rco            - Cladding outside radius (in)
    !
    ! Output
    !
    INTEGER(ipk), INTENT(IN) :: k
    INTEGER(ipk) :: nconv, n2, ldir, nedtsw
    REAL(r8k), INTENT(IN) :: f_Input
    REAL(r8k) :: cep, fs, cladtemp, pi, po, ufuel, rci, rco, t0, edot1, edot2, edot3, tcmax, &
      &          fastflux, coldw, timeincrement, rmp0, cladthickness0, pitch, dum1, dum2, uclad, &
      &          delta, rfo, f
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp
    REAL(r8k), DIMENSION(:), INTENT(IN) :: CladT, GasPress, CoolPress, PelSrfDisplin, EffStrain, &
      &                                    OldCladT, OldGasPrs, OldCoolPrs, OldPelDis
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: CldPlasStrn
    !
    cepp(1) = CldPlasStrn(k,1)
    cepp(2) = CldPlasStrn(k,2)
    cepp(3) = CldPlasStrn(k,3)
    cep = EffStrain(k)
    IF (f_Input < 0.0_r8k .OR. f_Input > 1.0_r8k) THEN
        f = 0.5_r8k
    ELSE
        f = f_Input
    END IF    
    fs = 0.0_r8k
    CladTemp  = OldCladT(k) + f * (CladT(k) - OldCladT(k))
    pi = OldGasPrs(k) + f * (GasPress(k) - OldGasPrs(k))
    po = OldCoolPrs(k) + f * (CoolPress(k) - OldCoolPrs(k))
    ufuel = OldPelDis(k) + f * (PelSrfDisplin(k) - OldPelDis(k))
    IF (ndebug) WRITE(ounit,900) Time, f, OldCladT(k), CladT(k), CladTemp, OldGasPrs(k), GasPress(k), OldCoolPrs(k), CoolPress(k)
900 FORMAT(' GAPT: Time = ',e11.4,' f = ',e11.4,2x,' OldCladT = ',e11.4,'CladT = ',e11.4,' CladTemp = ', &
      &    e11.4,/,6x,' OldGasPrs = ',e11.4,' GasPress = ',e11.4,6x,' OldCoolPrs = ',e11.4,' CoolPress = ',e11.4)
    !
    CALL cladf (pi, po, CladTemp, rci, rco, csig, ceps, cepp, cep, fs, t0, edot1, edot2, edot3, &
      &         tcmax, FastFlux, coldw, TimeIncrement, nedtsw, nconv, rmp0, CladThickness0, pitch, &
      &         k, dum1, dum2)
    !
    uclad = 0.5_r8k * (rci + rco) * ceps(1) - 0.5_r8k * CladThickness0 * ceps(3)
    gapt = uclad - ufuel + delta
    IF (ndebug) WRITE(ounit,920) gapt, uclad, ufuel, delta
920 FORMAT(6x,' gapt = ',e11.4,' uclad = ',e11.4,' ufuel = ',e11.4,' delta = ',e11.4)
    !  
    END FUNCTION gapt
    !
    !
    SUBROUTINE cladf (pg, pc, tc, rci, rco, csig, ceps, cepp, cep, fs, tempcs, edot1, edot2, edot3, tcmax, &
      &               tflux, coldw, TimeIncrement, nedtsw2, nconv, rmp0, CladThickness0, pitch, k, e, v)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ndebug
    USE Dyna_h_fraptran, ONLY : cladeffstress, efffastflustrencoef, effcoldwkstrencoef, oxygenconcenave
    USE resti_h_fraptran, ONLY : knonue
    USE bloon_h_fraptran, ONLY : nbncal, tcebal, chstrs, taxbal, trabal
    USE conversions_fraptran, ONLY : pi, tfk
    USE Material_Properties_fraptran, ONLY : MatProperty
    IMPLICIT NONE
    !
    INTEGER(ipk) :: itcntg, nconv, k, nedtsw2, kload, i
    INTEGER(ipk), PARAMETER :: itgmax = 50
    REAL(r8k) :: t0k, rout0, rmp0, cladthickness0, ehlim, pitch, cepp10, tempcs, rci, pg, rco, pc, &
      &          fs, cep, sigef0, dep, sm, s1, s2, sh0, cep1, cc, tc, edot1, edot2, edot3, tcmax, &
      &          coldw, tflux, TimeIncrement, v, e, cath1, cdth1, cath0, cdth0, deppmx
    REAL(r8k), PARAMETER :: tedot = 2240.0_r8k
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, depp, alfdt
    !
    t0k = tfk(tempcs)
    ! Compute hoop strain at which an axisymmetric cluster of fuel rods contact each other
    rout0 = rmp0 + CladThickness0 / 2.0_r8k
    ehlim = (pitch / 2.0_r8k - rout0) / rout0
    cepp10 = cepp(1)
    IF (pitch < 1.0e-6_r8k) ehlim = 1.0e6_r8k
    IF (cepp(1) >= (ehlim - 1.0e-4_r8k)) THEN
        ! Cladding already constrained by adjacent fuel rods
        depp(1) = 0.0_r8k
        depp(2) = 0.0_r8k
        depp(3) = 0.0_r8k
        ceps(1) = cepp(1)
        ceps(2) = cepp(2)
        ceps(3) = cepp(3)
        csig(1) = 0.0_r8k
        csig(2) = 0.0_r8k
        RETURN
    ENDIF
    IF (nbncal == 1) THEN
        ! Ballooning has occurred
        IF (k == knonue) THEN
            depp(1) = tcebal - cepp(1)
            ceps(1) = tcebal
            csig(1) = chstrs
            depp(2) = taxbal - cepp(2)
            ceps(2) = taxbal
            depp(3) = trabal - cepp(3)
            ceps(3) = trabal
        ELSE
            depp(1) = 0.0_r8k
            ceps(1) = cepp(1)
            depp(2) = 0.0_r8k
            ceps(2) = cepp(2)
            depp(3) = 0.0_r8k
            ceps(3) = cepp(3)
            csig(1) = 0.0_r8k
        ENDIF
        ! depp(2) = 0.0_r8k
        ! depp(3) = 0.0_r8k
        ! ceps(2) = cepp(2)
        ! ceps(3) = cepp(3)
        csig(2) = 0.0_r8k
        cepp(1) = cepp(1) + depp(1)
        cepp(2) = cepp(2) + depp(2)
        cepp(3) = cepp(3) + depp(3)
        ! Add thermal strains
        cath1 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=tfk(tc))
        cdth1 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=tfk(tc))
        cath0 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=t0k)
        cdth0 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=t0k)
        !
        alfdt(1) = cdth1 - cdth0
        alfdt(2) = cath1 - cath0
        alfdt(3) = cdth1 - cdth0
        ceps(1) = ceps(1) + alfdt(1)
        ceps(2) = ceps(2) + alfdt(2)
        ceps(3) = ceps(3) + alfdt(3)
        RETURN
    ENDIF
    itcntg = 0
    nconv = 1
    itcntg = itcntg + 1
    csig(1) = (rci * pg - rco * pc) / CladThickness0
    csig(2) = (pi * (rci ** 2 * pg - rco ** 2 * pc) + fs) / (pi * (rco ** 2 - rci ** 2))
    csig(3) = - 0.5_r8k * (pg + pc)

    ! If internal pressure equals coolant pressure, assume cladding has failed,
    ! and is hydrostically loaded and that no plastic deformation should occur.
    CladEffStress(k) = SQRT(0.5_r8k * ((csig(1) - csig(2)) ** 2 + (csig(2) - csig(3)) ** 2 &
      &                + (csig(3) - csig(1)) ** 2))
    IF (ABS(pg - pc) < 1.0e-6_r8k) CladEffStress(k) = 0.0_r8k
    IF (nconv == 0) THEN
        WRITE(6,921) pg, pc, csig(1), csig(2), CladEffStress(k), cep, nedtsw2
921     FORMAT(' CLADF, pg = ',e13.6,' pc = ',e13.6, ' sh = ',e13.6,' sa = ',e13.6,' seff = ',e13.6, &
          &    ' cep = ',e13.6,' nedtsw = ',i5)
        WRITE(6,925) rci, rco, itcntg
925     FORMAT(' rci = ',e13.6,' rco = ',e13.6,' itcntg = ',i5)
    END IF
    IF (itcntg == 1) sigef0 = CladEffStress(k)
    IF (CladEffStress(k) <= 0.001_r8k) THEN
        depp(1) = 0.0_r8k
        depp(2) = 0.0_r8k
        depp(3) = 0.0_r8k
        dep = 0.0_r8k
    ELSE
        sm = (csig(1) + csig(2) + csig(3)) / 3.0_r8k
        s1 = csig(1) - sm
        s2 = csig(2) - sm
        IF (itcntg == 1) sh0 = s1
        cep1 = cep
        kload = 1
        !
        CALL strain (CladEffStress(k), cc, cep1, tc, edot1, edot2, edot3, tcmax, coldw, tflux, TimeIncrement, k, kload)
        !
        dep = cep1 - cep
        edot1 = dep / TimeIncrement
        depp(1) = 1.5_r8k * dep * s1 / CladEffStress(k)
        depp(2) = 1.5_r8k * dep * s2 / CladEffStress(k)
        depp(3) = -depp(1) - depp(2)
        IF (itcntg >= itgmax) GOTO 200
    END IF
    !
    e = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=tfk(tc), ColdWork=EffColdWkStrenCoef(k), &
      &              Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k))
    v = (e / (2.0_r8k * (MatProperty (Material='CLAD', Property='SHEAR_MOD', Temperature=tfk(tc), ColdWork=EffColdWkStrenCoef(k), &
      &                               Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k))))) - 1.0_r8k
    !
    e = e / 6894.76_r8k
    !
    cath1 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=tfk(tc))
    cdth1 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=tfk(tc))
    ! Reference
    cath0 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=t0k)
    cdth0 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=t0k)
    !
    alfdt(1) = cdth1 - cdth0
    alfdt(2) = cath1 - cath0
    alfdt(3) = cdth1 - cdth0
    !
    IF (ndebug) WRITE(6,999) tfk(tc), t0k, (alfdt(i), i = 1,3)
999 FORMAT(' CLADF: tc1 = ',e11.4,' t0k = ',e11.4,' alfdt s = ',3(2x,e11.4))
    !
    ! Mechanical strains
    IF (tc <= tedot) THEN
        ceps(1) = (csig(1) - v * csig(2)) / e + cepp(1) + depp(1)
        ceps(2) = (csig(2) - v * csig(1)) / e + cepp(2) + depp(2)
        ceps(3) = - v * (csig(1) + csig(2)) / e + cepp(3) + depp(3)
    ELSE
        ! If in strain rate dominate regime, bypass elastic strain calculation.
        ceps(1) = cepp(1) + depp(1)
        ceps(2) = cepp(2) + depp(2)
        ceps(3) = cepp(3) + depp(3)
    ENDIF
    !
    ! Thermal strains
    IF (ndebug) WRITE(6,998) v, e, (csig(i), i = 1,3), (ceps(i), i = 1,3), (cepp(i), i = 1,3)
    998 FORMAT(5x,' v = ',e11.4,' e = ',e11.4,' csig s = ',3(2x,e11.4),/,35x, &
                  ' ceps s = ',3(2x,e11.4),/,35x,' cepp s = ',3(2x,e11.4))
    !
    ceps(1) = ceps(1) +  alfdt(1)
    ceps(2) = ceps(2) +  alfdt(2)
    ceps(3) = ceps(3) +  alfdt(3)
    !
    IF (ndebug) WRITE(6,202) (ceps(i), i = 1,3)
202 FORMAT(' CLADF: ceps = ',3(e11.4,2x))
    !
200 CONTINUE
    cepp(1) = cepp(1) + depp(1)
    IF (cepp(1) <= ehlim) THEN
        cepp(2) = cepp(2) + depp(2)
        cepp(3) = cepp(3) + depp(3)
        cep = cep + dep
        nconv = 1
    ELSE
        ! Cladding first constrained by adjacent fuel rods on this time step
        ! reduce cladding strains to maximum permitted by constraints
        deppmx  = ehlim - cepp10
        dep     = sigef0 * deppmx / (1.5_r8k * sh0)
        cepp(1) = ehlim
        ceps(1) = cepp(1)
        ceps(2) = cepp(2)
        ceps(3) = cepp(3)
        depp(1) = ehlim - cepp10
        depp(2) = 0.0_r8k
        depp(3) = 0.0_r8k
        cep     = cep + ehlim - cepp10
        csig(1) = 0.0_r8k
        csig(2) = 0.0_r8k
    END IF
    !
    END SUBROUTINE cladf
    !
    !
    SUBROUTINE couple (rci, rco, csig, ceps, cepp, cep, urf, delta, pint, tempcs, idbg, edot1, &
      &                edot2, edot3, tcmax, tflux, coldw, TimeIncrement, k, edot01, edot02, edot03, &
      &                CladThickness0, e, v)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit, CladType, ndebug, Time
    USE Dyna_h_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    IMPLICIT NONE
    !
    ! CladThickness0 - Initial cold state cladding thickness (in)
    ! k              - Axial node number
    ! urf            - Outward displacement of the fuel outer surface (in)
    ! rci            - Cladding inside radius (in)
    ! rco            - Cladding outside radius (in)
    !
    INTEGER(ipk) :: nexceedtedot = 0, k, kload, kk, kreduc, idbg 
    REAL(r8k) :: urf, edot1, edot01, cep, delta, tc1, t0k, v, e, cath1, cdth1, cath0, cdth0, tedot, &
        &        aa, cladthickness0, rco, rci, bb, tempcs, dd, ee, cc, ff, sigef1, cep1, depp1,&
        &        timeincrement, depp2, edot2, depp3, edot3, depnew, edtnew, depp01, depp02, edot02,&
        &        depp03, edot03, depold, edtold, edtx2, edtx3, tcmax, cold2, tflux, coldw, dep, &
        &        olsigf, deldep, dsigf, slope, coef, emodh, emodx, emodr, eet, em, ratio1, ratio2,&
        &        ratio3, ratio, max, pint, rbar, rmp, edot, dep1, sh, sa, epscmp, zero
    REAL(r8k), PARAMETER :: eppmax = 100.0_r8k
    REAL(r8k), PARAMETER :: tcthr = 1500.0_r8k
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, depp, oldepp, nudepp, alfdt
    !

    IF (CladType == 6 .OR. CladType == 8) THEN ! Set tedot value for Zr-1%Nb = 1030K
        tedot = 1030.0_r8k
    ELSE
        tedot = 1089.0_r8k
    ENDIF
    !

    ! check here for elastic solution
    IF (ndebug) WRITE(ounit,902) CladAveTemp(k), urf, edot1, edot01, cep, FastFlux(k), delta, k
902 FORMAT(' CladAveTemp(k) = ',e11.4,' urf = ',e13.6,' edot1 = ',e13.6,' edot01 = ',e13.6/8x, &
      &    ' cep = ',e11.4, ' FastFlux = ',e11.4,' delta = ',e13.6,' k = ',i3)
    !
    depp(1:3) = 0.0_r8k
    nudepp(1:3) = 0.0_r8k
    ! compute stresses
    tc1 = tfk(CladAveTemp(k))
    t0k = tfk(tempcs)
    ! Cladding elastic modulus
    e = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=tc1, ColdWork=EffColdWkStrenCoef(k), &
      &              Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k))
    ! Cladding poisson's ratio
    v = (e / (2.0_r8k * (MatProperty (Material='CLAD', Property='SHEAR_MOD', Temperature=tc1, ColdWork=EffColdWkStrenCoef(k), &
      &                              Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k))))) - 1.0_r8k
    ! Conert units
    e = e / 6894.76_r8k
    !
    cath1 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=tc1)
    cdth1 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=tc1)
    cath0 = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=t0k)
    cdth0 = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=t0k)
    !
    alfdt(1) = cdth1 - cdth0
    alfdt(2) = cath1 - cath0
    alfdt(3) = cdth1 - cdth0
    IF (ndebug) WRITE(ounit,999) tc1, tedot, t0k, alfdt(2)
999 FORMAT(' COUPLE: tc1, tedot, t0k, alfdt(2) = ',4(e11.4,2x))
    ! If cladding deformation is highly strain rate dependent, bypass the normal couple calculations
    IF (tc1 > tedot .AND. nexceedtedot == 0) THEN
        WRITE(ounit,905)
905     FORMAT(' The cladding average temperature is greater than 1089 K, the temperature at which', &
          &    ' the cladding strain rate becomes excessive. Calculation continuing ')
!        WRITE(ounit,905)
        nexceedtedot = 1
    ENDIF
    ! If (tc1 > tedot) GOTO 200
    aa = 1.0_r8k + v * CladThickness0 / (rco + rci)
    bb = v * (CladThickness0 / (rco + rci) - 1.0_r8k)
    dd = - v
    ee = 1.0_r8k
    cc = (urf - delta) * 2.0_r8k / (rco + rci)
    cc = cc + (CladThickness0 / (rco + rci)) * (cepp(3) + depp(3) + alfdt(3))
    cc = cc - (cepp(1) + depp(1) +  alfdt(1))
    cc = cc * e
    ff = ceps(2) - (cepp(2) + depp(2) + alfdt(2))
    ff = ff * e
    csig(1) = (cc * ee - bb * ff) / (aa * ee - bb * dd)
    csig(2) = (aa * ff - cc * dd) / (aa * ee - bb * dd)
    sigef1 = SQRT(csig(1) ** 2 + csig(2) ** 2 - csig(1) * csig(2))
    cep1 = cep
    kload = 0
    depp1 = edot1 * TimeIncrement
    depp2 = edot2 * TimeIncrement
    depp3 = edot3 * TimeIncrement
    depnew = 0.4714_r8k * SQRT((depp1 - depp2) ** 2 + (depp2 - depp3) ** 2 + (depp3 - depp1) ** 2)
    edtnew = depnew / TimeIncrement
    depp01 = edot01 * TimeIncrement
    depp02 = edot02 * TimeIncrement
    depp03 = edot03 * TimeIncrement
    depold = 0.4714_r8k * SQRT((depp01 - depp02) ** 2 + (depp02 - depp03) ** 2 + (depp03 - depp01) ** 2)
    edtold = depold / TimeIncrement
    edtx2 = 0.0_r8k
    edtx3 = 0.0_r8k
    !
    IF (ndebug) WRITE(ounit,907) sigef1, csig(1), csig(2), edtnew, edtold, cep1, kload, depp2, depp02, TimeIncrement
907 FORMAT(' COUPLE, sigef1 = ',e11.4,' csig1 = ',e11.4,' csig2 = ',e11.4,' edtnew = ',e11.4/9x, &
      &    ' edtold = ',e11.4,' cep1 = ',e11.4,' kload = ',i5,' depp2 = ',e11.4,' depp02 = ',e11.4, &
      &    ' TimeIncrement = ',e11.4)
    !
    CALL strain (sigef1, cc, cep1, CladAveTemp(k), edtnew, edtx2, edtx3, tcmax, coldw, tflux, TimeIncrement, k, kload)
    !
    dep = cep1 - cep
    !
    IF (ndebug) WRITE(ounit,909) dep, cep1, edtnew
909 FORMAT(' COUPLE after STRAIN call, dep = ',e11.4,' cep1 = ',e13.6,' edtnew = ',e11.4)
    ! If (dep > 1.0_r8k) GOTO 900
    IF (dep <= 0.0_r8k) GOTO 10
    IF (dep > 0.0_r8k) GOTO 2
    ! start iteration solution here
2   CONTINUE
    kk = 1
    cep1 = cep
    zero = 0.0_r8k
    !
    CALL stress (olsigf, cep1, zero, CladAveTemp(k), edtold, edtx2, edtx3, tcmax, coldw, k)
    !
    cep1 = cep
    deldep = 0.0001_r8k
    IF (tc1 > tcthr) deldep = depnew
    !
    CALL stress (dsigf, cep1, deldep, CladAveTemp(k), edtnew, edtx2, edtx3, tcmax, coldw, k)
    !
    IF (ndebug) WRITE(ounit,913) olsigf, edtold, dsigf, edtnew, deldep
913 FORMAT(' COUPLE, olsigf = ',e11.4,' edtold = ',e11.4,' dsigf = ',e11.4,' edtnew = ',e11.4,' deldep = ',e11.4)
    !
    IF (deldep == 0.0_r8k) THEN
        slope = 0.0_r8k
    ELSE
        slope = (dsigf - olsigf) / deldep
    END IF
    coef = 2.0_r8k * (1.0_r8k + v) / (3.0_r8k * e)
    kreduc = 0
    IF (idbg == 2 .OR. idbg == 5 .OR. ndebug) WRITE(ounit,99)
99  FORMAT(5x,' couple ')
    IF (tc1 <= tcthr) THEN
        oldepp(1) = 1.0e-7_r8k
        oldepp(2) = -0.5e-7_r8k
        oldepp(3) = -0.5e-7_r8k
    ELSE
        oldepp(1) = depp1
        oldepp(2) = depp2
        oldepp(3) = depp3
        nudepp(1) = oldepp(1)
        nudepp(2) = oldepp(2)
        nudepp(3) = oldepp(3)
    END IF
    
    ! kreduc = counter on reduction of guessed plastic strain
4   kreduc = kreduc + 1
    cc = (urf - delta) * 2.0_r8k / (rco + rci)
    cc = cc + (CladThickness0 / (rco + rci)) * (cepp(3) + oldepp(3) + alfdt(3))
    cc = cc - (cepp(1) + oldepp(1) +  alfdt(1))
    cc = cc * e
    ff = ceps(2) - (cepp(2) + oldepp(2) + alfdt(2))
    ff = ff * e
    csig(1) = (cc * ee - bb * ff) / (aa * ee - bb * dd)
    csig(2) = (aa * ff - cc * dd) / (aa * ee - bb * dd)
    dep = deldep
    IF (tc1 > tcthr) GOTO 10
    emodh = (csig(1) - v * csig(2)) / e + oldepp(1)
    emodx = (csig(2) - v * csig(1)) / e + oldepp(2)
    emodr = - v * (csig(1) + csig(2)) / e - oldepp(1) - oldepp(2)
    eet = 0.471405_r8k * SQRT((emodh - emodr) ** 2 + (emodr - emodx) ** 2 + (emodx-emodh) ** 2)
    dep = (eet - coef * olsigf) / (1.0_r8k + coef * slope)
    IF (ndebug) WRITE(ounit,917) dep, eet, coef, olsigf, slope, kk
917 FORMAT(' COUPLE, dep = ',e11.4,' eet = ',e11.4,' coef = ',e11.4,' olsigf = ',e11.4,' slope = ',e11.4,' kk = ',i3)
    IF (dep > 1.0_r8k) GOTO 900
    IF (dep >= 0.0_r8k) GOTO 80
    IF (kreduc < 3) GOTO 75
    dep = 0.0_r8k
    nudepp(1) = 0.0_r8k
    nudepp(2) = 0.0_r8k
    nudepp(3) = 0.0_r8k
    GOTO 10
    !
75  CONTINUE
    oldepp(1) = 0.1_r8k * oldepp(1)
    oldepp(2) = 0.1_r8k * oldepp(2)
    oldepp(3) = 0.1_r8k * oldepp(3)
    GOTO 4
    !
80  CONTINUE
    em = (emodh + emodr + emodx) / 3.0_r8k
    nudepp(1) = (dep / eet) * (emodh - em)
    nudepp(2) = (dep / eet) * (emodx - em)
    nudepp(3) = - nudepp(1) - nudepp(2)
    !
    IF (ABS(nudepp(1)) > eppmax) GOTO 900
    IF (ABS(nudepp(2)) > eppmax) GOTO 900
    !
    ratio1 = (nudepp(1) - oldepp(1)) / oldepp(1)
    ratio2 = (nudepp(2) - oldepp(2)) / oldepp(2)
    ratio3 = (nudepp(3) - oldepp(3)) / oldepp(3)
    ratio1 = ABS(ratio1)
    ratio2 = ABS(ratio2)
    ratio3 = ABS(ratio3)
    ratio  = MAX(ratio1, ratio2, ratio3)
    !
    IF (idbg == 2 .OR. idbg == 5 .OR. ndebug) WRITE(ounit,100) kk, dsigf ,nudepp(1), nudepp(2), csig(1), csig(2)
100 FORMAT(' COUPLE  kk = ',i3,2x,' dsigf = ',e12.5,2x,'nudepp"x = ',2(e12.5,2x),'csig"s = ',2(e12.5,2x))
    IF (ratio <= 0.001_r8k) GOTO 10
    IF (ratio > 0.001_r8k) GOTO 6
6   CONTINUE
    kk = kk + 1
    IF (kk - 199 < 0) GOTO 7
    IF (kk - 199 >= 0) GOTO 14
7   CONTINUE
    oldepp(1) = nudepp(1)
    oldepp(2) = nudepp(2)
    oldepp(3) = nudepp(3)
    GOTO 4
    !
10  CONTINUE
    cep = cep + dep
    cepp(1) = cepp(1) + nudepp(1)
    cepp(2) = cepp(2) + nudepp(2)
    cepp(3) = cepp(3) + nudepp(3)
    !
    GOTO 20
    !
14  CONTINUE
    WRITE(ounit,15)  kk
15  FORMAT(' did not converge in ',i4,' iterations equivalent new plastic strain calculation in sub. couple ')
    ! Even though routine did not converge, last computed values are returned, and an error message is printed
    GOTO 10
    ! Compute interface pressure
20  CONTINUE
    pint = (CladThickness0 * csig(1) + rco * CoolPress(k)) / rci
    IF (pint < GasPress(k)) THEN
        ! For situation where pint l.t. GasPress(k), pint is set equal to GasPress(k) and elastic soln is obtained
        ! such that pint always equals GasPress(k)
        csig(1) = (rci * GasPress(k) - rco * CoolPress(k)) / CladThickness0
        rbar = 0.5_r8k * (rco + rci)
        csig(2) = ((rbar + v * CladThickness0 / 2.0_r8k) * csig(1) + rbar * e &
         &        * (cepp(1) + alfdt(1)) - 0.5_r8k * CladThickness0 * e &
         &        * (cepp(3) + alfdt(3)) - e * (urf - delta )) / (rci * v)
        ceps(1) = (csig(1) - v * csig(2)) / e + cepp(1) + alfdt(1)
        ceps(2) = (csig(2) - v * csig(1)) / e + cepp(2) + alfdt(2)
        ceps(3) = - v * (csig(1) + csig(2)) / e + cepp(3) + alfdt(3)
        pint = GasPress(k)
    ELSE
        ! Compute radial strain
        ceps(3) = - v * (csig(1) + csig(2)) / e + cepp(3) + alfdt(3)
        ceps(1) = (urf - delta + 0.5_r8k * CladThickness0 * ceps(3)) * 2.0_r8k / (rco + rci)
    END IF
    
    RETURN
    !
900 CONTINUE
    WRITE(ounit,901)
901 FORMAT(//'COUPLE: cladding plastic strain increment between time steps is too great for iteration procedure***')
    WRITE(ounit,903)
903 FORMAT(/'Reduce time step by a factor of 5'/)
    WRITE(ounit,*)'Execution stopped at time = ',Time
!    WRITE(0,901)
!    WRITE(0,903)
!    WRITE(0,*)'Execution stopped at time = ',Time
    ERROR STOP 'Due to large plastic deformation, execution stopped in Subroutine: couple'
    ! highly strain rate dependent cladding deformation
    ! compute cladding plastic strains that just close gap assume edot2 = edot3
200 CONTINUE
    CladThickness0 = rco - rci
    rmp   = 0.5_r8k * (rci + rco)
    depp1 = (urf - delta + (CladThickness0 / 2.0_r8k) * (cepp(3) + alfdt(3)) - &
     &       rmp * (cepp(1) + alfdt(1))) / (rmp + CladThickness0 / 2.0_r8k)
    depp2 = 0.0_r8k
    depp3 = - depp1 - depp2
    dep   = 0.47140452_r8k * SQRT((depp1 - depp2) ** 2 + (depp2 - depp3) ** 2 + (depp3 - depp1) ** 2)
    edot  = dep / TimeIncrement
    edtx2 = 0.0_r8k
    edtx3 = 0.0_r8k
    cep1  = cep
    dep1  = dep
    !
    CALL stress (sigef1, cep1, dep1, CladAveTemp(k), edot , edtx2, edtx3, tcmax, coldw, k)
    ! apply prandtl-reuss flow rule to compute deveatoric stresses
    ! assume radial stress = coolant pressure
    sh = 0.66667_r8k * (depp1 / dep) * sigef1
    sa = 0.66667_r8k * (depp2 / dep) * sigef1
    csig(1) = (2.0_r8k * sh + sa - (rco / (2.0_r8k * rci)) * CoolPress(k) - CoolPress(k) / 2.0_r8k) &
      &       / (1.0_r8k + CladThickness0 / (2.0_r8k * rci))
    csig(2) = 1.5_r8k * sa + csig(1) / 2.0_r8k - 0.25_r8k * ((CladThickness0 / rci) * csig(1) + &
      &       CoolPress(k) * (1.0_r8k + rco / rci))
    pint = (CladThickness0 * csig(1) + rco * CoolPress(k)) / rci
    csig(3) = - 0.5_r8k * (pint + CoolPress(k))
    cepp(1) = cepp(1) + depp1
    cepp(2) = cepp(2) + depp2
    cepp(3) = cepp(3) + depp3
    ! neglect elastic strains
    ceps(1) = cepp(1) + alfdt(1)
    ceps(2) = cepp(2) + alfdt(2)
    ceps(3) = cepp(3) + alfdt(3)
    ! make sure cladding strain puts fuel and cladding into contact
    epscmp = (urf - delta + (CladThickness0 / 2.0_r8k) * ceps(3)) / (0.5_r8k * (rci + rco))
    ! assume effective plastic strain does not accumulate as cladding plastic hoop strain oscilates
    cep = 1.15469_r8k * ABS(cepp(1))
    !
    END SUBROUTINE couple
    !
    !
    SUBROUTINE repack (factor, IndexRepack, pinf, star, dpcold, ColdDiametralGap, modfd)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This model is a simple empirical model to account for repacking and hour-glassing and the resultant early lockup
    !
    ! Input
    !
    ! ColdDiametralGap - cold diametrical gas gap (mils)
    ! dpcold           - cold state pellet diameter (inches)
    ! factor           - hoop, axial, + radial fuel strain factors 
    !                    factor(1)*(cold state radius) = displacement (radial) of fuel pellet surface
    ! IndexRepack      - signal for which quantity is to be computed
    !                1 = additional fuel radial displacement at pellet ends
    !                2 = additional fuel radial displacement at pellet waist
    !                3 = interface pressure at pellet waist (psia)
    ! modfd            - fuel  deformation model indicator
    ! pinf             - pellet-cladding interface pressure (psia)
    !
    ! Output
    !
    ! factor           - hoop, axial, + radial fuel strain factors 
    !                    factor(1)*(cold state radius) = displacement (radial) of fuel pellet surface
    ! star             - pressure range over which gap is assumed to close (psia)
    !
    INTEGER(ipk), INTENT(IN) :: IndexRepack, modfd !modfd not used.
    REAL(r8k), INTENT(IN) :: ColdDiametralGap, dpcold
    REAL(r8k), INTENT(OUT) :: star
    REAL(r8k), INTENT(INOUT) :: pinf
    REAL(r8k) :: u, tgap, rp, pstar, pins
    REAL(r8k), PARAMETER :: pstar1 = 5000.0_r8k
    REAL(r8k), PARAMETER :: pstar2 = 500.0_r8k
    REAL(r8k), DIMENSION(3), INTENT(INOUT) :: factor
    !
    ! tgap = cold-state radial gap thickness (inches)
    tgap = ColdDiametralGap / (2.0_r8k * 1000.0_r8k)
    ! rp = cold fuel pellet radius (inches)
    rp = dpcold / 2.0_r8k
    pstar = pstar2
    pins = pinf
    SELECT CASE (IndexRepack)
    CASE (2)
        IF (pins >= pstar) THEN
            factor(1) = 0.0_r8k
        ELSE
            factor(1) = (1.0_r8k - pinf / pstar)
        ENDIF
        IF (pins <= 0.0_r8k) factor(1) = 1.0_r8k
    CASE (3) ! Estimate difference between interfacial pressure at pellet interfaces and pellet midplanes
        IF (pins > pstar) THEN
            pinf = pins - pstar
        ELSE
            pinf = 0.0_r8k
        ENDIF
    CASE DEFAULT
        ! u  = added fuel radial displacement due to cracking (inches)
        ! 3/19/03 set u=0 for all cases; i.e., no hourglassing radial displacement
        u = 0.0_r8k
        ! This is the FRAP-T6 default fuel deformation (modfd=0, Fracas-1)
        ! u = 0.0025_r8k * rp
        ! If (u >= tgap) u = 0.0_r8k
        factor(1) = u / rp
        star = pstar
    END SELECT
    !
    END SUBROUTINE repack
!
!
    SUBROUTINE strain (sig, eps, epplas, temp, edot1, edot2, edot3, tcmax, coldw, tflux, &
      &                TimeIncrement, k, kload)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit, CladType, Time, ndebug
    USE Dyna_h_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE zrmodels_fraptran, ONLY : cstres, cstran
    IMPLICIT NONE
    !>@brief
    !> This Subroutine computes uniaxial strain given the stress and previous plastic stain. elastic unloading
    !> is prescribed, and the current ( work-hardened ) yield stress is computed from the plastic strain at each call.
    !>@author
    !> Programmed by m.p. bohn, Nov.18, 1974 .
    !
    ! Input
    !
    ! sig           - stress in psi
    ! epplas        - previous plastic strain (in/in)
    ! temp          - temperature in degrees fahrenheit
    ! edot1         - strain rates in hoop direction
    ! edot2         - strain rates in axial direction
    ! edot3         - strain rates in radial direction
    ! tcmax         - maximum cladding temperature in past (sec)
    ! coldw         - cold-working  (a/a0)
    ! tflux         - time span of fast neutron flux (sec)
    ! TimeIncrement - time step (sec)
    ! kload         - indicator to select cstran or cstrni
    !             0 = cstran (no cladding ballooning occurring)
    !             1 = cstrni (cladding ballooning occurring)
    ! k             - axial node number
    !
    ! Output
    !
    ! eps           - strain  (in/in)
    ! epplas        - new value of plastic strain (in/in)
    !
    INTEGER(ipk) :: nswmod, j
    INTEGER(ipk), INTENT(IN) :: kload, k
    REAL(r8k) :: TempK, e, CldYieldStress, epstst, edot, cut, StressPascals, YieldStressPascals &
      &     ,epstru, edotx, sag, eptru, etru1
    REAL(r8k), INTENT(IN) :: temp, sig, edot1, TimeIncrement
    REAL(r8k), INTENT(IN) :: edot2, edot3, tcmax, coldw, tflux !Note: None of these variables are used.
    REAL(r8k), INTENT(OUT) :: eps
    REAL(r8k), INTENT(INOUT) :: epplas
    
    TempK = tfk(temp)
    ! Cladding elastic modulus
    e = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=TempK, ColdWork=EffColdWkStrenCoef(k), &
      &              Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k))
    ! Convert units
    e = e / 6894.76_r8k
    ! Get current yield stress given epplas
    CldYieldStress = 5000.0_r8k
    edot = edot1
    epstst = -10.0_r8k
    cut = 1.0e-10_r8k
    ! check to see If cstran or cstrni should be called
    ! cstran valid for low strain rates and models elastic unloading
    ! cstrni valid for high strain rates
    nswmod = 2
    IF (kload == 0) nswmod = 1
    ! Debug
    IF (ndebug) WRITE(ounit,901) Time, epplas, edot1, TempK, kload, k
901 FORMAT(' STRAIN, Time = ',e13.6,' epplas = ',e11.4,' edot1 = ',e11.4,' TempK = ',e11.4,' kload = ',i3,' k = ',i3)
    !
    DO j = 1, 30
        eps = CldYieldStress / e + epplas
        IF (ABS(eps - epstst) < cut) EXIT
        epstst = eps
        ! If (eps < 0.0_r8k) WRITE(ounit,901) eps, CldYieldStress, e, epplas, TempK
        ! 901 FORMAT(' in strain, eps = ',e13.6,' CldYieldStress = ',e13.6,' e = ',e13.6,' epplas = ',e13.6,' TempK = ',e13.6)
        IF (eps < 0.0_r8k) eps = 0.0_r8k
        epstru = LOG(eps + 1.0_r8k)
        ! edotx = 0.0_r8k
        edotx = edot1
        ! Find yield stress assuming zero strain rate and no increase in yield stress due to yielding
        CALL cstres (TempK, OxygenConcenAve(k), EffFastFluStrenCoef(k), EffFastFluStrnHardExp(k), EffColdWkStrenCoef(k), &
          &          EffColdWkStrnHardExp(k), edotx, epstru, CldYieldStress)
        !
        CldYieldStress = CldYieldStress / 6894.76_r8k
    ENDDO
    YieldStressPascals = CldYieldStress * 6894.76_r8k
    StressPascals = sig * 6894.76_r8k
    ! Debug
    IF (ndebug) WRITE(ounit,905) StressPascals, YieldStressPascals
905 FORMAT(' STRAIN, StressPascals = ',e11.4,' YieldStressPascals = ',e11.4)
    ! Convert anisotropic effective yield stress to isotropic effective yield stress.
    ! This is a temporary replacement for including anisotropic effects via the matpro routine caniso.
    CldYieldStress = CldYieldStress / 1.19_r8k
    !
    IF ((sig - CldYieldStress) <= 0.0_r8k) THEN
        eps = epplas + sig / e
        ! edot1 = 0.0
    ELSE
        SELECT CASE (nswmod)
        CASE (2) ! High strain rate solution: convert total strain at time step start from engineering to true
            etru1 = LOG(epplas + sig / e + 1.0_r8k)
            sag = sig * 6894.76_r8k
            ! Convert isotropic effective stress to anisotropic effective stress.
            ! This is a temporary replacement for including anisotropic effects via the matpro routine caniso.
            sag = sag * 1.19_r8k
            !
            CALL cstrni (TimeIncrement, TempK, OxygenConcenAve(k), EffFastFluStrenCoef(k), EffFastFluStrnHardExp(k), &
              &          EffColdWkStrenCoef(k), EffColdWkStrnHardExp(k), sag, etru1)
            !
            IF (etru1 > 2.0_r8k) etru1 = 2.0_r8k
            eps = EXP(etru1) - 1.0_r8k
            epplas = eps - sig / e
            IF (ndebug) WRITE(ounit,908) kload, epplas, CldYieldStress
908         FORMAT(' STRAIN after cstrni call, kload = ',i2,' epplas = ',e11.4,' CldYieldStress = ',e11.4)
        CASE DEFAULT
            sag = sig * 6894.76_r8k
            ! Convert isotropic effective stress to anisotropic effective stress. 
            ! This is a temporary replacement for including anisotropic effects via the matpro routine caniso.
            sag = sag * 1.19_r8k
            ! **Note: eps taken to be true strain**
            CALL cstran (TempK ,OxygenConcenAve(k), EffFastFluStrenCoef(k), EffFastFluStrnHardExp(k), EffColdWkStrenCoef(k), &
              &          EffColdWkStrnHardExp(k), edot, sag, epstru)
            eptru = epstru - sig / e
            ! Return engineering plastic strain
            IF (eptru > 228.0_r8k) eptru = 228.0_r8k
            epplas = EXP(eptru) - 1.0_r8k
            IF (ndebug) WRITE(ounit,907) kload, epplas, CldYieldStress
907         FORMAT(' STRAIN after cstran call, kload = ',i2,' epplas = ',e11.4,' CldYieldStress = ',e11.4)
            ! Since cstran only for non-ballooning case, set strain rate to zero
            ! edot1 = 0.0_r8k
        END SELECT
    ENDIF
    !
    END SUBROUTINE strain
    !
    !
    SUBROUTINE stress (sig, eplase, dep, temp, edot1, edot2, edot3, tcmax, coldw, k)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit
    USE Dyna_h_fraptran, ONLY : EffFastFluStrenCoef, &
        EffColdWkStrenCoef, OxygenConcenAve, EffFastFluStrnHardExp, EffColdWKStrnHardExp
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE zrmodels_fraptran, ONLY : cstres
    IMPLICIT NONE
    !>@brief
    !> This Subroutine computes stress given the previous plastic strain value and an increment of plastic strain.
    !>@author
    !> programmed by m.p. bohn ,  nov.18, 1974.
    !
    ! Input
    !
    ! k      - axial node number
    ! epplas - previous plastic strain (in/in)
    ! dep    - increment of plastic strain (in/in)
    ! temp   - Temperature  (F)
    ! edot1  - strain rate in hoop direction
    ! edot2  - strain rate in axial direction
    ! edot3  - strain rate in radial direction
    ! tcmax  - maximum cladding temperature in past (F)
    ! coldw  - cladding cold work (a/a0)
    !
    ! Output
    !
    ! sig    - stress in psi
    ! epplas - new value of plastic strain (in/in)
    !
    INTEGER(ipk) :: j
    INTEGER(ipk), INTENT(IN) :: k
    REAL(r8k) :: TempK, e, epstst, edot, eps, epplas
    REAL(r8k), INTENT(IN) :: eplase, Temp, edot1, edot2, edot3, tcmax, coldw !Note: edot2, edot3, tcmax & coldw are not used.
    REAL(r8k), INTENT(OUT) :: sig
    REAL(r8k), INTENT(INOUT) :: dep
    REAL(r8k), PARAMETER :: cut = 1.0e-10_r8k
    REAL(r8k), PARAMETER :: dep_min = -0.001_r8k
    !
    epplas = LOG(eplase + 1.0_r8k)

    TempK = tfk(temp)
    ! Cladding elastic modulus
    e = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=TempK, ColdWork=EffColdWkStrenCoef(k), &
      &              Fluence=EffFastFluStrenCoef(k), OxygenConcen=OxygenConcenAve(k)) / 6894.76_r8k
    ! dep should never be negative. Check this here
    IF (dep < 0.0_r8k) THEN
        IF (dep <= dep_min) WRITE(ounit,4) dep
4       FORMAT(//24x,'Error: Negative dep in Subroutine: stress. dep = ',e12.4, &
          &     /25x,'dep was set to .0001 - RESULTS MAY BE INVALID!!')
        dep = 0.0001_r8k
    END IF
    epplas = epplas + dep
    sig = 5000.0_r8k
    !
    epstst = 0.0_r8k
    !
    DO j = 1, 30
        eps = sig / e + epplas
        IF (ABS(eps - epstst) <= cut) EXIT
        epstst = eps
        edot = edot1
        CALL cstres (TempK ,OxygenConcenAve(k) ,EffFastFluStrenCoef(k), EffFastFluStrnHardExp(k), &
          &          EffColdWkStrenCoef(k), EffColdWkStrnHardExp(k), edot, eps, sig)
        sig = sig / 6894.76_r8k
    ENDDO
    ! Convert anisotropic effective yield stress to isotropic effective yield stress. 
    ! This is a temporary replacement for including anisotropic effects via the matpro routine caniso.
    sig = sig / 1.19_r8k
    !
    END SUBROUTINE stress
!
END MODULE deformation_fraptran













