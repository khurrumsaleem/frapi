MODULE Plenum
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran, ONLY : ounit
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> This module contains the correlations used to compute the temperature of the gas in the fuel rod plenum
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/13/2016
    !
    PRIVATE
    PUBLIC :: plnt
    ! Flag for printing a warning message for prandtl calculation
    ! LOGICAL, PRIVATE :: PrandtlWarning = .TRUE.
    LOGICAL, PRIVATE :: PrandtlWarning = .FALSE.   ! Supressing warning messages
    !
    CONTAINS
    !
    SUBROUTINE plnt (od, id, vp, powrod, tau, slen, diac, dias, tip, tsurf, PlenumGasMoles, GasFraction, &
      &              denc, NSteadyTrans, Ncoils, nodpln, q1, q2, abar, bbar, tp, GasPress, nconv)
    USE Kinds
    USE conversions_fraptran, ONLY : pi, sechr, psinm2, gtolb, tfk, tfr
    USE functions_fraptran, ONLY : simq
    USE variables_fraptran, ONLY : ounit, ndebug
    USE Material_Properties, ONLY : MatProperty
    USE NCGases, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes temperature of gas in fuel rod plenum
    !
    ! Input
    !
    ! od             - outer diameter of cladding in plenum region (ft)
    ! id             - inside diameter of cladding in plenum region (ft)
    ! vp             - plenum volume (spring volume     subtracted out) (ft3)
    ! powrod         - average fuel rod power (kw/ft)
    ! tau            - time step size (sec)
    ! slen           - length of spring (ft)
    ! diac           - outside diameter of spring coil (ft)
    ! dias           - diameter of spring wire (ft)
    ! tip            - temperature of top pellet in fuel stack (F)
    ! tsurf          - surface temperature of cladding in plenum region at end of time step (F)
    ! PlenumGasMoles - amount of gas in plenum (gram-moles)
    ! GasFraction(i) - fraction of gas that is of i-th composition
    !              1 = helium
    !              2 = argon
    !              3 = krypton
    !              4 = xenon
    !              5 = hydrogen
    !              6 = nitrogen
    !              7 = Steam
    ! ngases         - # of gases possible in the gas gap
    ! denc           - density of cladding  (lb/ft**3)
    ! NSteadyTrans   - steady-state or transient indicator
    !              1 = steady-state
    !              2 = transient
    ! nodpln         - number of nodes in plenum temperature numerical scheme
    ! Ncoils         - number of coils in plenum spring
    ! ndebug         - debug switch. 0 = off , 1 = on
    ! q1             - cladding surface heat flux in plenum region at start of time step  (btu/ft**2-sec)
    ! q2             - cladding surface heat flux in plenum region at end of time step  (btu/ft**2-sec)
    ! tp(j,1)        - temperature at j-th node of plenum temperature numerical scheme at start of time step (F)
    ! GasPress       - gas pressure in plenum (psia)
    !
    ! Output
    !
    ! abar, bbar     - constants such that: abar*tsur + bbar - q(2)
    !                  abar has units of btu/ft**2-sec-F, bbar has units of btu/ft**2-sec
    ! tp(j,2)        - temperature at j-th node of plenum temperature numerical scheme at end of time step (F)
    ! nconv          - convergence indicator. - 0: no convergence, - 1: convergence
    ! terr           - error in temperature at which thermal properties evaluated (F)
    !
    INTEGER(ipk) :: nn, j, itchk, i, l, numberofequations, nsing
    INTEGER(ipk), INTENT(IN) :: NSteadyTrans, Ncoils, nodpln
    INTEGER(ipk), INTENT(OUT) :: nconv
    INTEGER(ipk), PARAMETER :: MaxIterations = 200
    REAL(r8k) :: ks, kc, i1, i2, i3, i4, i5, i6, ibar3, ibar, kg, deltr, acl, aip, ps, ass, asc, vsc, &
      &          vss, vcli, vclo, vclc, vg, sfac, cfac, prod, pdens, pdenc, gtemp, deng, &
      &          beta, visco, visck, cg, pr, grip, grs, grc, grippr, grcpr, ctemp, cc, &
      &          cs, dens, hfp, tgip, hnc, hvs, vfac, emc, emr, hrads, hradc, del, theta, &
      &          thetb, hcons, a1ss, err, a1, b1, c1, c2, d2, a3, b3, d3, c3, a4, e4, c4, b4, b5, f5, &
      &          e5, e6, f6, cbar3, xbar, ybar, zbar, h, Nu
    REAL(r8k), INTENT(IN) :: tau, denc, diac, dias, PlenumGasMoles, q1, id, od, vp, slen, powrod, GasPress, tip, tsurf
    REAL(r8k), INTENT(OUT) :: abar, bbar
    REAL(r8k), INTENT(INOUT) :: q2
    REAL(r8k), PARAMETER :: terr = 1.0_r8k
    REAL(r8k), PARAMETER :: zroxid = 2.5e-7_r8k
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), DIMENSION(2) :: q
    REAL(r8k), DIMENSION(6) :: bc
    REAL(r8k), DIMENSION(36) :: ac
    REAL(r8k), DIMENSION(ngases), INTENT(IN) :: GasFraction
    REAL(r8k), DIMENSION(6,2) :: tbar, oldtp
    REAL(r8k), DIMENSION(6,2), INTENT(INOUT) :: tp
    !
    IF (ndebug) WRITE(ounit,917) denc, diac, dias
917 FORMAT(' denc = ',e13.6,' diac = ',e13.6,' dias = ',e13.6)
    !
    ! Check for errors
    IF (Ncoils < 1) THEN
        WRITE(ounit,501)
501     FORMAT(' *** zero number of coils in plenum spring specified by input. no. of coils must be # 0 - program stopped ***')
        STOP
    END IF
    IF ((id - diac) <= 0.0_r8k) THEN
        WRITE(ounit,511)
511     FORMAT(////' *** diameter of coil of plenum spring specified by input to be greater than cladding inside diameter *** ', &
          &      /,' *** program stopped *** ')
        STOP
    END IF
    IF (PlenumGasMoles == 0.0_r8k) THEN
        WRITE(ounit,555)
555     FORMAT(///'PlenumGasMoles is zero in Subroutine plnt')
        STOP
    END IF
    !
    nconv = 1
    !
    q(1) = q1
    q(2) = q2
    ! clad surface area--acl and clad thickness--deltr
    deltr = (od - id) / 2.0_r8k
    acl = vp * 4.0_r8k / id
    ! units in this Subroutine are feet,seconds, btu"s, pounds mass, degrees F
    ! insulator pellet temp.+ surface area
    aip = pi * (id ** 2) / 4.0_r8k
    ! spring surface area ass
    ps = slen / Ncoils
    ass = (SQRT((ps / 2.0_r8k) ** 2 +  diac ** 2) - dias) * Ncoils * pi ** 2 * dias
    ! spring central node surface area -dia=of central node= dias/2.**.5
    asc = ass / SQRT(2.0_r8k)
    ! spring central + surface node volumes vcs + vss
    vsc = ass * dias / 8.0_r8k
    IF (vsc <= 0.0_r8k) THEN
        WRITE(ounit,522)
522     FORMAT(////' *** Incorrect plenum spring geometry specified by input, negative or zero spring volume computed *** ', &
          &      /,' *** program stopped *** ')
        ERROR STOP 'Incorrect plenum spring geometry specified by input. Execution terminated in Subroutine: plnt'
    END IF
    vss = vsc
    ! clad interior,central and exterior node volumes,and clad surf.area
    vcli = (((id + deltr) * pi) * deltr / 4.0_r8k) * (4.0_r8k * vp / (pi * id ** 2))
    vclo = vcli
    vclc = 2.0_r8k * vclo
    acl = ((id + deltr) * pi) * (4.0_r8k * vp / (pi * id ** 2))
    ! volume of plenum gas
    vg = vp
    IF (vg <= 0.0_r8k) THEN
        WRITE(ounit,531)
531     FORMAT(////' Gas volume in plenum computed to be less than zero in Subroutine plnt - program stopped *** ')
        WRITE(ounit,533) vp, id, slen, diac, dias, Ncoils
533     FORMAT(/' In plnt, plenum volume = ',e11.4,' cladding id = ',e11.4, &
          &     ' spring length = ',e11.4,' coil d = ',e11.4,' wire d = ',e11.4,/' number of coils = ',i8)
        WRITE(ounit,535) ps, ass, vsc, vg
535     FORMAT(/' spring pitch = ',e11.4,' surface area spring = ',e11.4,' half volume spring = ',e11.4,' gas volume = ',e11.4)
        ERROR STOP 'Gas volume in plenum computed to be less than zero. Execution terminated in Subroutine: plnt'
    END IF
    ! power density of spring + clad
    ! sfac has units of (absorption coefficient)/ft
    sfac = 1.1_r8k
    cfac = 1.1_r8k
    ! average power of the rod in btu/ft**2*sec.
    prod = powrod / (1.055_r8k * pi * od)
    pdens = prod * sfac
    pdenc = prod * cfac
    pdenc = 0.0_r8k
    nn = 0
    !
    tbar(1:6,1) = tp(1:6,1)
    !
    itchk = 1
    ! Iterate up to 200 times to find solution
    IterationLoop: DO
        itchk = itchk + 1
        IF (itchk > MaxIterations) THEN
            WRITE(ounit,538)
538         FORMAT(///'*** iteration loop in Subroutine plnt did not converge - time step reduced and execution continuing *** ')
            nconv = 0
            RETURN
        END IF
        !
        oldtp(1:6,1) = tp(1:6,1)
        ! plenum gas properties,density deng,specific heat-cg,conductivity-kg,viscosity-vico
        gtemp = tfk(tbar(1,1))
        !
        kg = MatProperty (Material='GAS', Property='THERMCOND', Temperature=gtemp, Pressure=(GasPress/psinm2), &
          &               GasComposition=GasFraction)
        !
        IF (ndebug) WRITE(ounit,923) kg, gtemp
923     FORMAT(' kg = ',e13.6,' at temp(k) = ',e13.6)
        kg = (kg / 1.055e3_r8k) * 0.3048_r8k / 1.8_r8k
        deng = MatProperty (Material='GAS', Property='MOLWT', Temperature=gtemp, Pressure=(GasPress/psinm2), &
          &                 GasComposition=GasFraction) * PlenumGasMoles / (gtolb * vg)
        ! Thermal expansion coefficient of an ideal gas
        beta = 1.0_r8k / tfr(tbar(1,1))
        visco = MatProperty (Material='GAS', Property='VISCOSITY', Temperature=gtemp, Pressure=(GasPress/psinm2), &
          &                  GasComposition=GasFraction) * 2.2_r8k * 0.3048_r8k
        visck = visco / deng
        cg = 1.24_r8k
        ! prandtl number of  the  plenum gas--pr= vico*cg*deng/kg
        pr = visco * cg / kg
        ! grashof number--gr=beta*g*l**3xdeltat/visco**2 -for the spring-grs,for the insulator pellet-grip,and for the clad -grc
        grip = beta * 32.2_r8k * ABS(tbar(1,1) - tip) * id ** 3 / (visck ** 2)
        grs = beta * 32.2_r8k * ABS(tbar(1,1) - tbar(3,1)) * dias ** 3 / (visck ** 2)
        grc = beta * 32.2_r8k * ABS(tbar(1,1) - tbar(4,1)) * (vp / (pi * id ** 2 / 4.0_r8k)) ** 3 / (visck ** 2)
        grippr = grip * pr
        grcpr = grc * pr
        IF (ndebug) THEN
            WRITE(ounit,913) tbar(1,1), tbar(4,1), vp, id, beta, visco
913         FORMAT(' PLNT tbar1 = ',e13.6,' tbar4 = ',e13.6,' vp = ',e13.6,' id = ',e13.6,' beta = ',e13.6,' visco = ',e13.6)
            WRITE(ounit,914) grc,pr,deng
914         FORMAT(' grc = ',e13.6,' pr = ',e13.6,' deng = ',e13.6)
        END IF
        ! Material prop. of the clad,denc,cc and kc
        ! Cladding specific heat
        cc = MatProperty (Material='CLAD', Property='SPECHEAT', Temperature=tfk(tbar(5,1)))
        ! Cladding thermal conductivity
        kc = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=tfk(tbar(5,1)), Flux=0.0_r8k, ColdWork=0.0_r8k)
        !
        IF (ndebug) WRITE(ounit,925) cc, kc
925     FORMAT(' clad cp = ',e13.6,' clad k = ',e13.6)
        cc = (cc / (1.055e3_r8k * 1.8_r8k)) * 0.4535_r8k
        kc = kc * 0.3048_r8k / (1.055e3_r8k * 1.8_r8k)
        ! material property of the spring,cs,ks,and dens -input for s-steel
        cs = 0.12_r8k
        dens = 585.0_r8k
        ks = 0.0027_r8k
        
        ! Calculate Nusselt number and natural convection heat transfer coefficient from fuel to the plenum (hfp)
        IF (tip >= tbar(1,1)) THEN
            ! Fuel is hotter than plenum => heating up plenum
            Nu = Nusselt(Grip, pr, 'HORIZONTAL', 'PLATE', 'HEATING')
        ELSE
            ! Plenum is hotter than fuel. Plenum will lose heat to the fuel 
            ! (although this is not truly fed back into solution due to no axial conduction)
            Nu = Nusselt (Grip, Pr, 'HORIZONTAL', 'PLATE', 'COOLING')
        END IF
        hfp = kg * Nu / id
        
        ! Calculate Nusselt number from horizontal cylinders - spring
        Nu = Nusselt (grs, pr, 'HORIZONTAL', 'CYLINDER')
        hnc = kg * Nu / dias
        
        ! Calculate Nusselt number from vertical surfaces-- clad
        Nu = Nusselt (grc, pr, 'VERTICAL', 'CYLINDER')
        hvs = kg * Nu / (vp * 4.0_r8k / (pi * id ** 2))
        
        ! Radiation heat transfer coefficient between the spring and clad
        ! Black body view factor vfac
        vfac = (ass * 0.5_r8k / acl + ((acl - ass * 0.5_r8k) / acl) * (ass * 0.5_r8k / acl))
        ! Emmissivity for spring and clad assumed equal
        emc = MatProperty (Material='CLAD', Property='EMISS', Temperature=tfk(tbar(4,1)), Oxide=zroxid)
        ! Equivalent radiative conductance between spring and clad
        emr = (1.0_r8k / ((1.0_r8k - emc) / (acl * emc)  + 1.0_r8k / (acl * vfac) + (1.0_r8k - emc) / (ass * emc)))
        hrads = (emr * 1.71e-9_r8k * (tfr(tbar(3,1)) ** 2 + tfr(tbar(4,1)) ** 2) * (tfr(tbar(3,1)) + tfr(tbar(4,1))) / ass) / sechr
        hradc = hrads * ass / acl
        
        ! Coefficient for conduction between spring and clad
        del = (id - diac) * 0.5_r8k
        theta = dias / (2.0_r8k * del + dias)
        thetb = SQRT(1.0_r8k - theta ** 2)
        IF (hnc > 0.0_r8k) THEN
            hcons = 0.0_r8k
        ELSE
            hcons = (-pi / dias + (4.0_r8k / dias) * (1.0_r8k / thetb) * (ATAN((1.0_r8k - theta) / thetb) - &
              &     ATAN(-theta / thetb))) * kg / pi
        END IF
        IF (ndebug) WRITE(ounit,927) hfp, hnc, hvs, hrads, hcons
927     FORMAT(' hfp = ',e13.6,' hnc = ',e13.6,' hvs = ',e13.6,' hrads = ',e13.6,' hcons = ',e13.6)
        IF (NSteadyTrans <= 1) THEN
            ! Steady state temperature distribution
            bbar = ((vss + vsc) * pdens + (4.0_r8k * vcli) * pdenc) / acl + aip * hfp * (tip - tp(1,1)) / acl
            abar = 0.0_r8k
            q(2) = bbar
            q2 = q(2)
            tp(6,1) = tsurf
            tp(5,1) = tsurf
            tp(4,1) = tsurf
            a1ss = aip * hfp + acl * hvs + ass * hnc * (1.0_r8k - ass * hnc / (ass * (hnc + hcons)))
            IF (a1ss > 1.0e-10_r8k) THEN
                tp(1,1) = (acl * hvs * tp(4,1) + ass * hnc * (pdens * (vss + vsc) + &
                  &        hcons * ass * tp(4,1)) / (ass * (hnc + hcons)) + aip * hfp * tip) / a1ss
            ELSE
                tp(1,1) = tsurf
            END IF
            tp(3,1) = (pdens * (vss + vsc) + ass * hnc * tp(1,1) + ass * hcons * tp(4,1)) / (ass * (hnc + hcons))
            tp(2,1) = tp(3,1)
            !
            DO i = 1, 6
                err = ABS((tbar(i,1) - tp(i,1)) * 2.0_r8k)
                IF (err > terr) THEN
                    DO j = 1, 5
                        tbar(j,1) = (oldtp(j,1) + tp(j,1)) * 0.5_r8k
                    END DO
                    tbar(6,1) = tsurf
                    CYCLE IterationLoop
                END IF
            END DO
        END IF
        !
        IF (nn <= 0) THEN
            !
            DO i = 1, 5
              tbar(i,1) = tp(i,1)
              tp(i,2) = tp(i,1)
            END DO
            !
            tbar(6,1) = tsurf
            tp(6,2) = tsurf
            tbar(6,2) = tsurf
        END IF
        IF (NSteadyTrans == 1) RETURN
        nn = nn + 1
        a1 = (1.0_r8k + tau / (deng * vg * cg * 2.0_r8k) * (aip * hfp + acl * hvs + ass * hnc))
        b1 = - acl * hvs * tau / (2.0_r8k * deng * vg * cg)
        c1 = - ass * hnc * tau / (2.0_r8k * deng * vg * cg)
        i1 = tp(1,1) + tau / (deng * vg * cg) * ((aip * hfp / 2.0_r8k) * (tip + tip - tp(1,1)) + &
          &  (acl * hvs/2.0_r8k) * (tp(4,1) - tp(1,1)) + (ass * hnc / 2.0_r8k) * (tp(3,1) - tp(1,1)))
        c2 = - tau * asc * ks / (vsc * cs * dens * dias)
        d2 = (1.0_r8k + tau * asc * ks / (vsc * cs * dens * dias))
        i2 =  tp(2,1) + (pdens * vsc + (asc * ks / dias) * (tp(3,1) - tp(2,1))) * tau / (vsc * cs * dens)
        a3 = - tau * ass * hnc / (vss * cs * dens * 2.0_r8k)
        b3 = - tau * ass * (hrads + hcons) / (vss * cs * dens * 2.0_r8k)
        d3 = - tau * asc * ks / (vss * cs * dens * dias)
        c3 = (1.0_r8k - d3 - b3 - a3)
        i3 = tp(3,1) + pdens * vss * tau / (vss * cs * dens) - d3 * (tp(2,1) - tp(3,1)) - b3 * &
          &  (tp(4,1) - tp(3,1)) - a3 * (tp(1,1) - tp(3,1))
        a4 = - tau * acl * hvs / (denc * cc * vcli * 2.0_r8k)
        e4 = - tau * acl * kc / (denc * cc * vcli * deltr)
        c4 = - tau * acl * (hradc + hcons * ass / acl) / (denc * cc * vcli * 2.0_r8k)
        b4 =  (1.0_r8k - e4 - c4 - a4)
        i4 = tp(4,1)  + pdenc * vcli * tau / (denc * cc * vcli) - c4 * (tp(3,1) - tp(4,1)) - a4 * &
          &  (tp(1,1) - tp(4,1)) - e4 * (tp(5,1) - tp(4,1))
        b5 = - tau * acl * kc / (denc * cc * vclc * deltr)
        IF (ndebug) WRITE(ounit,931) tau, acl, kc, denc, cc, vclc, deltr
931     FORMAT(' tau = ',e13.6,' acl = ',e13.6,' kc = ',e13.6,' denc = ',e13.6,' cc = ',e13.6,' vclc = ',e13.6,' deltr = ',e13.6)
        f5 = - tau * acl * kc / (denc * cc * vclc * deltr)
        e5 = (1.0_r8k - b5 - f5)
        i5 = tp(5,1)  + pdenc * vclc * tau / (denc * cc * vclc) - b5 * (tp(4,1) - tp(5,1)) - f5 * (tp(6,1) - tp(5,1))
        e6 = - tau * acl * kc / (denc * cc * vclo * deltr)
        f6 = (1.0_r8k + tau / ( denc * cc * vclo) * (acl * kc/deltr))
        i6 = tp(6,1) + tau / (denc * cc * vclo) * (pdenc * vclo + acl * kc * (tp(5,1) - tp(6,1)) / deltr - acl * q(1) / 2.0_r8k)
        cbar3 = c3 - d3 * c2 / d2
        ibar3 = i3 - d3 * i2 / d2
        xbar = e5 * f6 / (e6 * b5) - f5 / b5
        ybar = (i5 / b5) - e5 * i6 / (e6 * b5)
        zbar = (((cbar3 * b4 - b3 * c4) / cbar3) - (a4 * cbar3 - a3 * c4) * (b1 * cbar3 - b3 * c1) / &
          &    (cbar3  * (a1 * cbar3 - a3 * c1)))
        ibar = i4 - ibar3 * c4 / cbar3  - ((cbar3 * a4 - a3 * c4) / cbar3) * (i1 * cbar3 - ibar3 * c1) / &
          &    (a1 * cbar3 - a3 * c1)
        h = - tau * acl / (2.0_r8k * denc * cc * vclo)
        
        ! Temperature of the clad exterior surface coefficients needed for the solution of the surface temp.
        abar = -(zbar * xbar  - f6 * e4 / e6) / (e4 * h / e6 - zbar * e5 * h / (e6 * b5))
        bbar = (ibar - zbar * ybar  - e4 * i6 / e6) / (e4 * h / e6 - zbar * e5 * h / (e6 * b5))
        
        ! Debugging
        IF (ndebug) THEN
            WRITE(ounit,951) vp, powrod, tau, tip, tsurf
951         FORMAT('in plnt, vp = ',e13.6,' powrod = ',e13.6,' tau = ',e13.6,' tip = ',e13.6,' tsurf = ',e13.6)
            WRITE(ounit,907) PlenumGasMoles, GasPress, q1, q2, abar ,bbar
907         FORMAT(' PlenumGasMoles = ',e13.6,' GasPress = ',e13.6, &
              &    ' q1 = ',e13.6,' q2 = ', e13.6,' abar = ',e13.6,' bbar = ',e13.6)
            WRITE(ounit,909)
909         FORMAT('plenum model node temperatures at start of time step ')
            WRITE(ounit,911) (tp(l,1), l = 1,nodpln)
911         FORMAT(6(2x,e13.6))
        END IF
        
        ! Simultaneously solve set of six equations for plenum temperatures
        ac(1:36) = 0.0_r8k
        !
        ac(1)  = a1
        ac(19) = b1
        ac(13) = c1
        bc(1)  = i1
        ac(14) = c2
        ac(8)  = d2
        bc(2)  = i2
        ac(3)  = a3
        ac(21) = b3
        ac(15) = c3
        ac(9)  = d3
        ac(4)  = a4
        bc(3)  = i3
        ac(22) = b4
        ac(16) = c4
        ac(28) = e4
        bc(4)  = i4
        ac(23) = b5
        ac(29) = e5
        ac(35) = f5
        bc(5)  = i5
        ac(36) = 1.0_r8k
        bc(6) = tsurf
        NumberOfEquations = 6
        !
        CALL simq (ac, bc, NumberOfEquations, nsing)
        IF (nsing == 1) THEN
            WRITE(ounit,542)
542         FORMAT(///,'simq found plnt equation set singular. Execution terminated in Subroutine: plnt')
            ERROR STOP 'simq found plnt equation set singular. Execution terminated in Subroutine: plnt'
        END IF
        tp(1,2) = bc(1)
        tp(2,2) = bc(2)
        tp(3,2) = bc(3)
        tp(4,2) = bc(4)
        tp(5,2) = bc(5)
        tp(6,2) = bc(6)
        !
        DO i = 1, 5
            tbar(i,2) = (tp(i,1) + tp(i,2)) / 2.0_r8k
        END DO
        
        ! Debugging
        IF (ndebug) THEN
            WRITE(ounit,921) itchk
921         FORMAT('Plenum model node temperatures for iteration = ',i5)
            WRITE(ounit,911) (tp(l,2),l = 1,nodpln)
        END IF

        !
        DO i = 1, 6
            err = ABS((tbar(i,1) - tbar(i,2)) * 2.0_r8k)
            IF (err > terr) THEN
                DO j = 1, 5
                    tbar(j,1) = (tp(j,1) + tp(j,2)) * 0.5_r8k
                END DO
                !
                tbar(6,1) = tsurf
                IF (itchk > (MaxIterations - 10)) WRITE(ounit,601) itchk, (tbar(j,2),j = 1,6), tsurf, powrod
601             FORMAT(//'plenum model node temperatures at iteration = ',i4,/,6(2x,e13.6),' tsurf = ',e13.6,' powrod = ',e13.6)
                CYCLE IterationLoop
            END IF
        END DO
        EXIT IterationLoop
    END DO IterationLoop
    !
    END SUBROUTINE plnt
    !
    !
    !
    REAL(r8k) FUNCTION Grashof (Length, Tplen, Tfuel)
    USE Kinds
    USE conversions_fraptran
    USE Material_Properties, ONLY : MatProperty
    USE variables_fraptran, ONLY : GasFraction
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
    
    ! Beta (Volumetric thermal expansion coefficient)
    Beta = 1.0_r8k / Tplen
    
    ! Gas Density
    Rho = MatProperty (Material='GAS', Property='DENSITY', Temperature=Tplen, GasComposition=GasFraction)
    
    ! Gas Viscosity
    Visc = MatProperty (Material='GAS', Property='VISCOSITY', Temperature=Tplen, GasComposition=GasFraction)
    
    ! Grashof Number
    Grashof = (Length ** 3) * (Rho ** 2) * Gravity_SI * Beta * ABS(Tfuel - Tplen) / (Visc ** 2)
    
    ! Check for errors
    IF (Grashof < 0.0_r8k) THEN
        WRITE (0,90) Beta, Rho, Visc, Grashof
        WRITE (ounit,90) Beta, Rho, Visc, Grashof
90      FORMAT ('Error in Grashof calculation. Beta = ',es12.4,/,30x,'Rho = ',es12.4,/,30x, &
          &     'Visc = ',es12.4,/,30x,'Grashof = ',es12.4,'. Execution Stopped')
        STOP
    END IF
    
    END FUNCTION Grashof
    !
    !
    !
    REAL(r8k) FUNCTION Prandtl (Tplen)
    USE Kinds
    USE conversions_fraptran
    USE Material_Properties, ONLY : MatProperty
    USE variables_fraptran, ONLY : GasFraction
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
    
    ! Gas specific heat, J/kg*K
    SpecHeat = MatProperty (Material='GAS', Property='SPECHEAT', Temperature=Tplen, GasComposition=GasFraction)
    
    ! Gas Viscosity, kg/m*s
    Visc = MatProperty (Material='GAS', Property='VISCOSITY', Temperature=Tplen, GasComposition=GasFraction)
    
    ! Gas Thermal Conductivity, W/m*K
    GasCond = MatProperty (Material='GAS', Property='THERMCOND', Temperature=Tplen, GasComposition=GasFraction)
    
    ! Prandtl Number
    Prandtl = SpecHeat * Visc / GasCond
    
    ! Check for errors
    IF (Prandtl <= 0.0_r8k) THEN
        WRITE (0,90) SpecHeat, Visc, GasCond, Prandtl
        WRITE (ounit,90) SpecHeat, Visc, GasCond, Prandtl
90      FORMAT ('Error in Prandtl calculation. SpecHeat = ',es12.4,/,30x,'Visc = ',es12.4,/,30x, &
          &     'GasCond = ',es12.4,/,30x,'Prandtl = ',es12.4,'. Execution Stopped')
        ERROR STOP
    END IF
    
    END FUNCTION Prandtl
    !
    !
    !
    REAL(r8k) FUNCTION Nusselt (Gr, Pr, Orientation, Geometry, HeatType)
    USE Kinds
    USE conversions_fraptran
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
    !REAL(r8k), PARAMETER :: NuMin = 1.0e-10_r8k
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
            !IF (X > 1.0E9_r8k) THEN ! Turbulent
            !    C = 0.13_r8k
            !    m = onethird
            !    IF (X > 1.0E12_r8k) OutsideBounds = .TRUE.
            !ELSE ! Laminar
            !    C = 0.59_r8k
            !    m = 0.25_r8k
            !    IF (X < 1.0E4_r8k) OutsideBounds = .TRUE.
            !END IF
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
                IF (X > 1.084398e7_r8k) THEN ! Turbulent, FRAPCON-4 uses 2.0e7
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
END MODULE Plenum












