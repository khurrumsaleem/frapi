MODULE Oxidation
    !>@brief
    !> This module contains the subroutines used to calculate high temperature oxidation.
    !> Subroutines include frpcon, metwtb, chitox, and cobild
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    USE Kinds
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    !
    SUBROUTINE metwtb (t1, t2, d1, d2, drod, TimeIncrement, q, drmax)
    USE Kinds
    USE Conversions, ONLY : tfk
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes energy generated by metal-water chemical reaction using the Baker-Just model for zirconium
    !
    ! Input
    !
    ! t1            - Cladding surface temperature at start of time step (F)
    ! t2            - Cladding surface temperature at End of time step   (F)
    ! d1            - Depth of reacted metal at start of time step (inches)
    ! drod          - Outside diameter of fuel rod (ft)
    ! drmax         - Original thickness of cladding (ft)    
    ! TimeIncrement - Time step (sec)
    !
    ! Output
    !
    ! d2            - Depth of reacted metal at End of time step (inches)
    ! q             - Rate at which heat is generated by metal-water reaction (btu/sec per foot length)
    !
    ! Internal
    !
    ! rhozr         - Density of zirconium
    ! rhozry        - Density of zircaloy
    !
    ! Note:
    !
    ! (1) Since change of metal-water reaction depth with cladding radial displacement is not modeled, cold-state rod OD is
    !     passed to metwtb to preclude erroneous effects caused by geometry changes
    ! (2) Regulations specify zirconium, so rhozr is used by default and rhozry is left here for future reference
    !     but is not used in the code
    !
    REAL(r8k) :: tk1, tk2, tave, drmaxi, dm1, dm2, drodm
    REAL(r8k), INTENT(IN) :: t1, t2, d1, drod, TimeIncrement, drmax
    REAL(r8k), INTENT(OUT) :: d2, q
    REAL(r8k), PARAMETER :: cnvft = 3.28084_r8k
    REAL(r8k), PARAMETER :: hrzr = 6.45e6_r8k
    REAL(r8k), PARAMETER :: cnvbtu = 1.05487_r8k
    REAL(r8k), PARAMETER :: tstart = 1000.0_r8k
    REAL(r8k), PARAMETER :: cnvin = 2.54e-2_r8k
    REAL(r8k), PARAMETER :: rhozr = 6.49e3_r8k
    REAL(r8k), PARAMETER :: rhozry = 6.56e3_r8k
    REAL(r8k), PARAMETER :: cnvftin = 12.0_r8k   !Convert ft to inches
    ! convert cladding thickness from ft to in
    drmaxi = cnvftin * drmax
    ! convert temperature from F to K
    tk1 = tfk(t1)
    tk2 = tfk(t2)
    tave = (tk1 + tk2) / 2.0_r8k
    ! If have oxidized whole thickness or if temperature is less than minimum, skip calculation
    IF (d1 > drmaxi .OR. tave < tstart) THEN
        q = 0.0_r8k
        d2 = d1
    ELSE
        ! convert oxide layer from inches to m
        dm1 = cnvin * d1
        dm2 = SQRT(dm1 ** 2 + 1.883e-4_r8k * EXP(-45500.0_r8k / 1.987_r8k / tave) * TimeIncrement)
        d2 = dm2 / cnvin
        drodm = drod / cnvft
        ! Oxidation energy in watts/m
        q = (rhozr * hrzr * 2.014_r8k * drodm * (dm2 - dm1)) / TimeIncrement
        ! Convert q from  W/m to btu/sec-ft
        q = q / (1000.0_r8k * cnvbtu * cnvft)
        !
    ENDIF
    !
    END SUBROUTINE metwtb
    !
    !
    SUBROUTINE chitox (t1, t2, x2, x1, dt, x12, x11, drod2, drod, q, w1, w2, iStoicGrad)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> chitox computes:
    !> (1) the zircaloy oxide and xi layer thicknesses at the end of a time step
    !> (2) the power generated in the cladding during the time step due to the metal-water chemical reaction
    !> (3) the diameter of the remaining unoxidized portion of the rod
    !>@author
    !> chitox was coded by g.a. reymann  September 1976
    !> updated by g.a. reymann Dec 1976
    !> Modified by K. Geelhood, PNNL, March 2014
    !
    ! Input
    !
    ! t1    - cladding temperature at start of a time step (k)
    ! t2    - cladding temperature at end of a time step (k)
    ! x1    - zro2 thickness at start of a time step (m)
    ! w1    - oxygen uptake at the start of the time step (kg/m2)
    ! dt    - time step duration (s)
    ! x11   - xi thickness at start of a time step (m)
    ! drod  - unoxidized rod outer diameter (m)
    !
    ! Output
    !
    ! x2    - zro2 thickness at the end of a time step (m)
    ! x12   - xi thickness at the end of a time step (m)
    ! w2    - oxygen uptake at the end of the time step (kg/m2)
    ! drod2 - outer diameter of unoxidized part of cladding at the end of the time step (m) 
    !         (expansion of the lattice due to dissolved oxygen is neglected)
    ! q     - rate of heat generation per meter for a rod of initial diameter drod (w/m)
    !
    ! Reference:
    !
    ! (1) j.v. cathcart, ornl/nureg/tm-41 (august 1976)
    !
    INTEGER(ipk), INTENT(IN) :: iStoicGrad
    REAL(r8k) :: tave
    REAL(r8k), INTENT(IN) :: t1, t2, x1, w1, dt, x11, drod
    REAL(r8k), INTENT(OUT) :: x2, x12, w2, drod2, q
    ! Density of zirconium (kg/m3). Regulations specify zirconium, so rhoz is used by default
    REAL(r8k), PARAMETER :: rhoz = 6.49e3_r8k
    ! Density of zircaloy (kg/m3). rhozy is left here for future reference but not used in the code
    REAL(r8k), PARAMETER :: rhozy = 6.56e3_r8k
    !
    REAL(r8k), PARAMETER :: hrz = 6.45e6_r8k
    !
    tave  = (t1 + t2) / 2.0_r8k
    IF (tave >= 1073.0_r8k) THEN ! time-average cladding T greater than or equal to 800C (1073K)
        x2 = SQRT((x1) ** 2  + 2.0_r8k * (1.12569e-6_r8k) * EXP(-3.58908e4_r8k / (1.987_r8k * tave)) * dt)
        x12 = SQRT((x11) ** 2 + 2.0_r8k * (3.412e-5_r8k) * EXP(-4.17e4_r8k / (1.987_r8k * tave)) * dt)
        IF (iStoicGrad == 0) THEN ! weight gain for perfectly stoechiometric oxide
            w2 = SQRT(w1 ** 2 + 0.3622_r8k * EXP(-39940.0_r8k / 1.987_r8k / tave) * dt)
        ELSE ! weight gain for oxide with stoechiometric gradient
            w2 = SQRT(w1 ** 2 + 0.336_r8k * EXP(-39870.0_r8k / 1.987_r8k / tave) * dt)
        ENDIF
    ELSE ! Time-average cladding temperature less than 800C, no change in oxide thickness during time step
        x2 = x1
        x12 = x11
        w2 = w1
    ENDIF
    !
    drod2 = drod - 1.38_r8k * x2
    q = (rhoz * hrz * 2.014_r8k * drod * (x2 - x1)) / dt
    ! 
    END SUBROUTINE chitox
    !
    !
    SUBROUTINE cobild (t1, t2, dt, am5, drod, pint, ip, ip2, y8, y9, y9a, y9b, aao, abo, aco, ado, &
      &                aeo, afo, ago, aho, aio, aai, abi, aci, adi, aei, afi, agi, ahi, aii, w1, p, &
      &                persat, bwtfr,al8,iStoicGrad)
    USE Kinds
    USE Variables, ONLY : ounit
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> This program was adapted from the program "build5" written by r.e. pawel of oak ridge national laboratory (ornl).
    !> Data and analyses from j.v. cathcart of ornl are used to compute the zro2 and oxygen-stabilized alpha thicknesses on the
    !> cladding outer surface and similar equations from p. hofmann of the kernforschungszentrum karlsruhe (kfk) for
    !> oxygen-stabilized alpha layers on the cladding inner surface when there is PCMI.
    !> Oxygen profiles are computed on the basis of a finite difference method assuming the transient to be composed
    !> of a series of isothermal segments. *** cobild is valid only between 1273 and 1773 k ***
    !>@author
    !> cobild was coded by g.a. reymann in july 1977.
    !> last updated by g.a. reymann in april 1979.
    !
    ! Input
    !
    ! t1          = cladding temperature at start of time step (k).
    ! t2          = cladding temperature at End of time step (k).
    ! dt          = duration of time step (s).
    ! am5         = wall thickness of as-fabricated rod (m).
    ! drod        = diameter  of as-fabricated rod (m).
    ! pint        = pellet-cladding interface pressure (Pa).
    !
    ! Output
    !
    ! ip          = output/input pcmi parameter
    !           0 - implies no pcmi
    !           1 - implies pcmi.
    ! ip2         = output/input pcmi parameter
    !           0 - implies no pcmi
    !           2 - implies pcmi
    !               (this second pcmi parameter is needed to correctly calculate the beta thickness when pellet-cladding contact 
    !                has been made and then broken).
    ! y8          = output/input oxide thickness(m).
    ! y9          = output/input thickness of oxygen-stabilized alpha nearest to the outer cladding surface (m).
    ! y9a         = output/input thickness of oxygen-stabilized alpha nearest to the uo2 fuel (m).
    ! y9b         = output/input thickness of oxygen-stabilized alpha between y9 and y9a (m).
    ! aao to aio  = output/input oxygen concentrations at equidistant nodes in the beta
    !         aao - concentration at the alpha-beta interface
    !         aio - concentration at the inner beta surface if there is no pcmi or at the beta midpoint if there is pcmi.
    ! aai to aii  = output/input oxygen concentrations at equidistant nodes in the inner half of the beta if there is pcmi.
    !         aai - the concentration at the inner alpha-beta interface
    !         aii - the concentration at the beta midpoint.
    ! *Note: All concentrations are in weight fraction of oxygen.*
    ! w1          = output/input oxygen uptake through the outer cladding surface (kg/m**2).
    ! p           = output linear power generated by the zr+o2 = zro2 reaction(w/m).
    ! persat      = output per cent saturation of the beta (unitless).
    ! bwtfr       = output average oxygen concentration in beta (weight fraction).
    ! al8         = output beta thickness (m).
    ! 
    ! Recommended input values for a fresh rod are:
    ! pint = 0.0
    ! aao to aio, all = 0.0012
    ! aai to aii, all = 0.0
    ! y8,y9,y9a,y9b, and w1, all = 0.0
    ! ip and ip2, both = 0.0
    INTEGER(ipk) :: ipint, ip, ip2, istoicgrad, j
    REAL(r8k) :: y9a, y9b, y8, y9, w1, aq20, al80, am5, pint, y9i, aai, aio, abi, aho, aci, adi, &
      &          ago, aei, afi, afo, agi, ahi, aeo, aii, ado, aco, abo, y8i, aao, am2, al2, az3, &
      &          dt, ar, t2, t1, y5, az8, y6, y7, w2, w11, y7a, y7b, as1, al5, al8, al4, al7, am3, &
      &          am4, al6, am1, aq2, persat, y8f, p, drod, bwtfr, OxideTD, dummy = 0.0_r8k
    REAL(r8k), DIMENSION(6) :: aa, ab, ac, ad, ae, af, ag, ah, ai
    !
    OxideTD = MatProperty(Material='OXIDE', Property='TDENSITY', Temperature=0.0_r8k)
    !
    y9a   = 100.0_r8k * y9a
    y9b   = 100.0_r8k * y9b
    y8    = 100.0_r8k * y8
    y9    = 100.0_r8k * y9
    w1    = w1 / 10.0_r8k
    aq20  = 0.0_r8k
    al80  = 0.0_r8k
    am5   = 100.0_r8k * am5
    ! 
    ipint = 0
    IF (pint > 0.0_r8k) ipint = 1
    IF (ipint /= 0) am5 = am5 / 2.0_r8k
    y8i   = (w1 / (OxideTD * 0.2597_r8k)) * 0.01_r8k
    ! 
    IF (ipint /= 0 .AND. ip == 0) THEN
        ip2 = 2
        aai = aio
        abi = (aho + aio) / 2.0_r8k
        aci = aho
        adi = (ago + aho) / 2.0_r8k
        aei = ago
        afi = (afo + ago) / 2.0_r8k
        agi = afo
        ahi = (aeo + afo) / 2.0_r8k
        aii = aeo
        aio = aeo
        aho = (ado + aeo) / 2.0_r8k
        ago = ado
        afo = (aco + ado) / 2.0_r8k
        aeo = aco
        ado = (abo + aco) / 2.0_r8k
        aco = abo
        abo = (3.0_r8k * aao + 6.0_r8k * abo - aeo) / 8.0_r8k
    ELSE If (ipint == 0 .AND. ip /= 0) THEN
        ip2 = 0
        abo = aco
        aco = aeo
        ado = ago
        aeo = (aio + aii) / 2.0_r8k
        afo = agi
        ago = aei
        aho = aci
        aio = aai
        aai = 0.0_r8k
        abi = 0.0_r8k
        aci = 0.0_r8k
        adi = 0.0_r8k
        aei = 0.0_r8k
        afi = 0.0_r8k
        agi = 0.0_r8k
        ahi = 0.0_r8k
        aii = 0.0_r8k
    ENDIF
    !
    ip    = 0
    aa(1) = 100.0_r8k * aao
    ab(1) = 100.0_r8k * abo
    ac(1) = 100.0_r8k * aco
    ad(1) = 100.0_r8k * ado
    ae(1) = 100.0_r8k * aeo
    af(1) = 100.0_r8k * afo
    ag(1) = 100.0_r8k * ago
    ah(1) = 100.0_r8k * aho
    ai(1) = 100.0_r8k * aio
    GOTO 210
    !
200 aa(1) = 100.0_r8k * aai
    ab(1) = 100.0_r8k * abi
    ac(1) = 100.0_r8k * aci
    ad(1) = 100.0_r8k * adi
    ae(1) = 100.0_r8k * aei
    af(1) = 100.0_r8k * afi
    ag(1) = 100.0_r8k * agi
    ah(1) = 100.0_r8k * ahi
    ai(1) = 100.0_r8k * aii
    !
210 am2 = 1.0e10_r8k      ! am2 = artIficially large modulus.
    ! al2 = beta thickness at start of each time step.
    al2 = am5 - (1 - ip) * (2.0_r8k * y8 / 3.0_r8k + y9) - ip * (y9a + y9b)
    IF (ip2 < 1) al2 = al2 - (y9a + y9b)
    az3 = dt / 5.0_r8k    ! az3 is the time increment for the integration.
    !
    ar  = (t2 - t1) / dt
    !
    y5  = az3
550 az8 = t1 + ar * y5 - az3 * ar / 2   ! az8 = av. temp. during increment
    IF (ip == 1) GOTO 650
    !
    y6 = 2.0_r8k * (1.12569e-02_r8k) * EXP(-3.58908e04_r8k / (1.987_r8k * az8))
    y7 = 2.0_r8k * 0.761490_r8k * EXP(-4.81418e04_r8k / (1.987_r8k * az8))
    !
    y8 = SQRT(y8 ** 2 + y6 * az3)   ! y8 = oxide layer thickness after increment.
    y9 = SQRT(y9 ** 2 + y7 * az3)   ! y9 = alpha layer thickness after increment
    SELECT CASE (iStoicGrad)
    CASE (0) ! weight gain for perfectly stoechiometric oxide
        w2 = 2.0_r8k * 0.18110_r8k * EXP(-39940.0_r8k / (1.987_r8k * az8)) 
    CASE (1) ! weight gain for oxide with stoechiometric gradient
        w2 = 2.0_r8k * 0.16800_r8k * EXP(-39870.0_r8k / (1.987_r8k * az8))
    END SELECT
    !
    w1 = SQRT(w1 ** 2 + w2 * az3)   ! w1 =  total oxy. conc. after increment
    w11 = y8 * (OxideTD) * 0.26_r8k    ! w11 is the minimum oxygen in (gm/cm**2) necessary to form the oxide layer.
    IF ((w1 - w11) <= 0.0_r8k) w1 = w11
    IF (ip == 0) GOTO 671
    !
650 y7a = 0.32_r8k * EXP(-4.9e04_r8k / (1.987_r8k * az8))
    y7b = 0.70_r8k * EXP(-4.4e04_r8k / (1.987_r8k * az8))
    !
    y9a = SQRT(y9a ** 2 + y7a * az3)
    y9b = SQRT(y9b ** 2 + y7b * az3)
    !
671 y5 = y5 + az3
    IF (y5-dt <= 0.0_r8k) GOTO 550
    az8 = (t1 + t2) / 2.0_r8k   ! az8 = the temperature during the interval, (K).
    ! in steps 2100 to 2106 the beta saturation concentration in weight per cent is calculated.
    as1 = 0.12_r8k
    IF (az8 < 1239.0_r8k) GOTO 2106
    as1 = -0.0042807_r8k + SQRT((az8 / 392.46_r8k) - 3.1417_r8k)
    IF (az8 < 1373.0_r8k) GOTO 2106
    as1 = (az8 - 1081.7_r8k) / 491.157_r8k
2106 CONTINUE
    ! al5 = d(ox in beta), (cm**2/sec), dick perkins '76 Data
    al5 = (SQRT(9.0_r8k / 8.0_r8k)) * (2.480e-02_r8k) * EXP(-2.820e4_r8k / (1.987_r8k * az8))
    !
    al8 = am5 - (1 - ip) * (2.0_r8k * y8 / 3.0_r8k + y9) - ip * (y9a + y9b)
    IF (ip2 < 1) al8 = al8 - (y9a + y9b)
    IF (ip == 0) al80 = al8   ! al8 = beta thickness at end of each time step.
    !
    al4 = al2 - al8   ! al4 = delta beta during step
    al7 = al2 / 8.0_r8k
    IF (al4 > al7) THEN
        WRITE (0,2771) dt
        WRITE (ounit,2771) dt
2771    FORMAT(//20x,'delta beta is too large for',e15.5,'sec increments. Code execution stopped in cobild.')
        STOP
    ENDIF
    am3 = al4 / (2.0_r8k * al2)
    am4 = (al4 ** 2) / (2.0_r8k * (al2 ** 2))
    al6 = al8 / 8.0_r8k   ! al6 = h(x), the distance increment for fd network.
    !
    am1 = (al6 ** 2) / (al5 * az3)
    IF (am1 < 2.0_r8k) THEN
        WRITE (0,5020)
        WRITE (ounit,5020)
5020    FORMAT(//20x,'Diffusion is occurring too rapidly for this routine to accurately calculate oxygen concentrations.' &
          &       ,/,'Code execution stopped in cobild.')
        STOP
    ENDIF
    IF (am1 < am2) am2 = am1 ! stores smallest modulus for stability criterion check.
    !
    CONTINUE
    ! In steps 2260 to 2320 the initial oxygen concentrations are calculated by parabolic interpolation. 
    ! This is necessary due to the decrease in beta thickness as the other layers grow.
    aa(1) = as1
    !
    ab(1) = ab(1) + 7.0_r8k * am3 * (4.0_r8k * ac(1) - 3.0_r8k * ab(1) - ad(1)) + 49.0_r8k * am4 &
      &     * (ad(1) + ab(1) - 2.0_r8k * ac(1))
    !
    ac(1) = ac(1) + 6.0_r8k * am3 * (4.0_r8k * ad(1) - 3.0_r8k * ac(1) - ae(1)) + 36.0_r8k * am4 &
      &     * (ae(1) + ac(1) - 2.0_r8k * ad(1))
    !
    ad(1) = ad(1) + 5.0_r8k * am3 * (4.0_r8k * ae(1) - 3.0_r8k * ad(1) - af(1)) + 25.0_r8k * am4 &
      &     * (af(1) + ad(1) - 2.0_r8k * ae(1))
    !
    ae(1) = ae(1) + 4.0_r8k * am3 * (4.0_r8k * af(1) - 3.0_r8k * ae(1) - ag(1)) + 16.0_r8k * am4 &
      &     * (ag(1) + ae(1) - 2.0_r8k * af(1))
    !
    af(1) = af(1) + 3.0_r8k * am3 * (4.0_r8k * ag(1) - 3.0_r8k * af(1) - ah(1)) + 9.0_r8k * am4 &
      &     * (ah(1) + af(1) - 2.0_r8k * ag(1))
    !
    ag(1) = ag(1) + 2.0_r8k * am3 * (4.0_r8k * ah(1) - 3.0_r8k * ag(1) - ai(1)) + 4.0_r8k * am4 &
      &     * (ai(1) + ag(1) - 2.0_r8k * ah(1))
    !
    ah(1) = ah(1) + 1.0_r8k * am3 * (4.0_r8k * ai(1) - 3.0_r8k * ah(1) - ah(1)) + 1.0_r8k * am4 &
      &     * (ah(1) + ah(1) - 2.0_r8k * ai(1))
    !
    DO j = 2, 6
        !
        aa(j) = as1
        ab(j) = (aa(j-1) + (am1 - 2.0_r8k) * ab(j-1) + ac(j-1)) / am1
        ac(j) = (ab(j-1) + (am1 - 2.0_r8k) * ac(j-1) + ad(j-1)) / am1
        ad(j) = (ac(j-1) + (am1 - 2.0_r8k) * ad(j-1) + ae(j-1)) / am1
        ae(j) = (ad(j-1) + (am1 - 2.0_r8k) * ae(j-1) + af(j-1)) / am1
        af(j) = (ae(j-1) + (am1 - 2.0_r8k) * af(j-1) + ag(j-1)) / am1
        ag(j) = (af(j-1) + (am1 - 2.0_r8k) * ag(j-1) + ah(j-1)) / am1
        ah(j) = (ag(j-1) + (am1 - 2.0_r8k) * ah(j-1) + ai(j-1)) / am1
        ai(j) = ((am1 - 2.0_r8k) * ai(j-1) + 2.0_r8k * ah(j-1)) / am1
    ENDDO
    !
    aq2 = (6.56_r8k * al6) / 300.0_r8k
    aq2 = aq2 * (aa(6) + ai(6) + 2.0_r8k * (ac(6) + ae(6) + ag(6)) + 4.0_r8k * (ab(6) + ad(6) + af(6) + ah(6)))
    IF (ip == 0) aq20 = aq2 ! aq2 is the oxygen in the beta,gm/cm**2,(by simpson's rule).
    IF (ip == 1) GOTO 2750
    !
    aao = aa(6) / 100.0_r8k
    abo = ab(6) / 100.0_r8k
    aco = ac(6) / 100.0_r8k
    ado = ad(6) / 100.0_r8k
    aeo = ae(6) / 100.0_r8k
    afo = af(6) / 100.0_r8k
    ago = ag(6) / 100.0_r8k
    aho = ah(6) / 100.0_r8k
    aio = ai(6) / 100.0_r8k
    !
    IF (ipint /= 0) ip = 1
    IF (ip == 1) GOTO 200
    GOTO 2760
    !
2750 aai = aa(6) / 100.0_r8k
    abi = ab(6) / 100.0_r8k
    aci = ac(6) / 100.0_r8k
    adi = ad(6) / 100.0_r8k
    aei = ae(6) / 100.0_r8k
    afi = af(6) / 100.0_r8k
    agi = ag(6) / 100.0_r8k
    ahi = ah(6) / 100.0_r8k
    aii = ai(6) / 100.0_r8k
    am5 = 2.0_r8k * am5
    al8 = al8 + al80
    aq2 = aq2 + aq20
    !
2760 persat = 1.0e04_r8k * aq2 / (as1 * al8 * 6.56_r8k)
    !
    y8f = (w1 / (OxideTD * 0.2597_r8k)) * 0.01_r8k
    p = (6560.0_r8k * 6.45e06_r8k) * 2.014_r8k * drod * (y8f - y8i) / dt
    bwtfr = (aq2 / (6.56_r8k * al8)) - 0.0012_r8k
    bwtfr = MAX(0.0_r8k, bwtfr)
    ! awtfr = weight fraction oxygen in the alpha zircaloy. awtfr typically = 0.047.
    ! conversion to si units;
    w1 = 10.0_r8k * w1
    y8 = y8 / 100.0_r8k
    y9 = y9 / 100.0_r8k
    y9a = y9a / 100.0_r8k
    y9b = y9b / 100.0_r8k
    am5 = am5 / 100.0_r8k
    al8 = al8 / 100.0_r8k
    !
    END SUBROUTINE cobild
    !
    !
END MODULE Oxidation