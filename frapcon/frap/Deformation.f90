MODULE Deformation
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines subroutines related to PCMI and cladding deformation.
    !> Subroutines include couple, fcmi, fladf, gapcls, gaptf, crepr, reppr, crapf, stress, strain
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION crapf (sag, edot, ProblemTime, tc, phi, ecreep)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> crapf is called from creep.
    !> crapf computes effective stress as a function of the creep strain rate, temperature, fast
    !> neutron flux and accumulated total creep strain
    !>@author
    !> coded by m.p.bohn, dec.28,1975 based on the matpro
    !
    ! Input
    !
    ! edot        - creep strain rate (1/hr)
    ! tc          - cladding temperature (F)
    ! ProblemTime - time (hr) not used in this version
    ! phi         - flux (neutrons/m**2/sec)
    ! ecreep      - total accumulated creep strain (dimensionless)
    !
    ! Output
    !
    ! crapf        - effective stress (psi)
    !
    ! Reference:
    !
    ! The time and stress dependence:
    ! (1) In reactor tubular creep of zircaloy-2 at 260 to 3000, e.f. ibrhaim, j. nucl. mater, vol 46 (1973) pp 169-182
    ! The flux dependence:
    ! (2) ross-ross and hunt, j. nucl. mater. vol 26 (1968) pp 2 - 17
    ! The temperature dependence, depx(-q/rt), was based on a value of q = 10000 cal/mole near 573k from
    ! (3) fidleris, aecl-4766
    !
    ! Note:
    !
    ! dpdt and sig are transverse quantities
    !
    REAL(r8k), INTENT(IN) :: sag, edot, ProblemTime, tc, phi, ecreep
    REAL(r8k) :: sig, dpdt, tc_K, b, c, ak, r, e, a1, a2, a3
    !
    sig = sag * PSItoPa * 1.1547_r8k
    dpdt = (edot * sectohr) / 1.1547_r8k
    tc_K = tfk(tc)
    b = 725.2_r8k
    c = 4.967e-08_r8k
    ak = 5.129e-29_r8k
    r = 1.987_r8k
    e = ecreep / 1.1547_r8k
    a1 = SQRT(e * dpdt / 2.0_r8k) / (ak * phi)
    a2 = EXP(10000.0_r8k / (r * tc_K))
    a3 = a1 * a2
    crapf = a3 - b * EXP(c * sig) - sig
    !
    END FUNCTION crapf
    !
    !
    !
    REAL(r8k) FUNCTION gaptf (f, delta, epp, rci, rco, ceps, cepp, cep, csig, OldCladAvTemp, OldGapPress, &
      &                       OldCoolPress, OldFuelDispl, j, nrelax, dtime, ProblemTime, CreepStrain, crep, &
      &                       CladH2Concen, ExcessH2Concen, na, nplast)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : CladAveTemp, FuelSurfDispl, CladEffPlasStrain, CoolantPress, GapPress
    IMPLICIT NONE
    !>@brief
    !> This routine calculates gap thickness
    !
    ! Input
    !
    ! cep               - temporary effective plastic strain (in/in)
    ! cepp              - temporary hoop, axial, & radial plastic strains (in/in)
    ! ceps              - temporary hoop, axial, & radial strains (in/in)
    ! csig              - temporary hoop, axial, & radial stress (psi)
    ! CreepStrain       - cladding creep strain (in/in)
    ! dtime             - time increment (hrs)
    ! delta             - cold state radial gap (in)
    ! CladEffPlasStrain - cladding effective plastic strain (in/in)
    ! epp               - cladding hoop, axial, & radial strains (in/in)
    ! f                 - interpolation factor
    ! j                 - axial node index
    ! na                - maximum number of axial nodes
    ! nplast            - elastic-plastic flag
    ! nrelax            - creep flag
    ! OldCoolPress      - coolant channel pressure of previous power step (psia)
    ! OldGapPress       - rod internal gas pressure of previous power step (psia)
    ! OldCladAvTemp     - cladding average temperature-previous power step (F)
    ! OldFuelDispl      - fuel radial displacement of previous power step (in)
    ! CoolantPress      - coolant channel pressure (psia)
    ! GapPress          - rod internal gas pressure (psia)
    ! CladAveTemp       - cladding average temperature (F)
    ! ProblemTime       - end of step time (hrs)
    ! FuelSurfDispl     - fuel radial displacemen (in)
    ! rci               - cladding inside radius (in)
    ! rco               - cladding outside radius (in)
    ! CladH2Concen      - concentration of H2 in cladding (ppm)
    ! ExcessH2Concen    - concentration of H2 above solubility limit in cladding (ppm)
    !
    ! Output
    !
    ! gaptf              - gap thickness (in)
    !
    INTEGER(ipk), INTENT(IN) :: j, nrelax, na, nplast
    REAL(r8k) :: fs, testr, crep, ur, uclad, t, sigy, sigeff, poutside, pinside, excesh2, dtimef, CladH2Concen, cep
    REAL(r8k), INTENT(IN) :: f, delta, rci, rco, dtime, ProblemTime, ExcessH2Concen
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, alfdt
    REAL(r8k), DIMENSION(na) :: OldCoolPress, OldFuelDispl, OldCladAvTemp, OldGapPress, CreepStrain
    REAL(r8k), DIMENSION(na,3) :: epp
    !
    cepp(1) = epp(j,1)
    cepp(2) = epp(j,2)
    cepp(3) = epp(j,3)
    cep = CladEffPlasStrain(j)
    crep = CreepStrain(j)
    fs = 0.0_r8k
    ! Note: for deformable pellet, FuelSurfDispl is a function of time during creep
    IF (nrelax /= 1) THEN
        t = OldCladAvTemp(j) + f * (CladAveTemp(j) - OldCladAvTemp(j))
        pinside = OldGapPress(j) + f * (GapPress(j) - OldGapPress(j))
        poutside = OldCoolPress(j) + f * (CoolantPress(j) - OldCoolPress(j))
        ur = OldFuelDispl(j) + f * (FuelSurfDispl(j) - OldFuelDispl(j))
    ELSE
        dtimef = f * dtime
        t = CladAveTemp(j)
        pinside = GapPress(j)
        poutside = CoolantPress(j)
        ur = FuelSurfDispl(j)
    END IF
    testr = 550.0_r8k
    excesh2 = MIN(ExcessH2Concen, testr)
    !
    CALL cladf (pinside, poutside, t, rci, rco, csig, ceps, cepp, cep, fs, nrelax, dtimef, &
      &         ProblemTime, crep, CladH2Concen, excesh2, nplast, sigeff, sigy, alfdt, j, 2)
    !
    uclad = ((rco + rci) * ceps(1) - (rco - rci) * ceps(3)) * 0.5_r8k
    gaptf = uclad - ur + delta
    !
    END FUNCTION gaptf
    !
    !
    !
    SUBROUTINE couple (GapPress, CoolantPress, CladAveTemp, rci, rco, csig, ceps, cepp, cep, &
      &                FuelSurfDispl, delta, sigeff, sigy, RinterfacPress, nrelax, dtime, &
      &                ProblemTime, crep, CladH2Concen, ExcessH2Concen, iquit, nplast, it, &
      &                alfdt, flux)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !
    ! Input
    !
    ! cep            - temporary effective plastic strain (in/in)
    ! cepp           - temporary hoop, axial, & radial plastic strains (in/in)
    ! ceps           - temporary hoop, axial, & radial strains (in/in)
    ! CladAveTemp    - cladding average temperature (F)
    ! CladH2Concen   - concentration of H2 in cladding (ppm)
    ! CoolantPress   - coolant channel pressure (psia)
    ! crep           - creep strain (in/in)
    ! csig           - temporary hoop, axial, & radial stress (psi)
    ! delta          - cold state radial gap (in)
    ! dtime          - creep step time increment (hrs)
    ! ExcessH2Concen - concentration of H2 in cladding above solubility limit(ppm)
    ! flux           - fast neutron flux (n/m**2-sec)
    ! FuelSurfDispl  - fuel radial displacement (in)
    ! GapPress       - rod internal gas pressure (psia)
    ! iquit          - termination index
    ! it             - power-time step index
    ! nplast         - elastic-plastic flag
    !              0 = elastic calculation performed
    !              1 = plasticity calculation considered
    ! nrelax         - creep index
    !              0 = no creep calculation
    !              1 = creep calculation performed
    ! ProblemTime    - time at the beginning of creep step (hrs)
    ! rci            - cladding  inside radius (in)
    ! rco            - cladding outside radius (in)
    !
    ! Output
    !
    ! RinterfacPress - interfacial pressure (psi)
    !
    REAL(r8k) :: dep
    REAL(r8k), DIMENSION(3) :: nudepp
    REAL(r8k), DIMENSION(3) :: oldepp
    INTEGER(ipk) :: nrelax, nplast, i, iquit, it, kk
    REAL(r8k) :: warningmsg, tcak, cladavetemp, e, cr, v, cathex, cdthex, aa, rco, rci, &
      &          bb, cc, dd, FuelSurfDispl, delta, ff, sigef1, cep1, testr, excesh2, excessh2ConCen, &
      &          dsigf, slope, sm, s1, s2, depdot, crap, sigeff, coef, crep, emodh, ee, olsigf, &
      &          emodx, emodr, eet, em, ratio1, ratio2, ratio3, ratio, RinterfacPress, GapPress, &
      &          rbar, t, sigy, CladH2ConCen, CoolantPress, cep
    REAL(r8k), INTENT(IN) :: flux, ProblemTime, dtime
    ! the following data statement contains various conversion factors
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), PARAMETER :: small = 1.0e-4_r8k
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, depp, alfdt
    !
    IF (csig(1) <= -1.0e10_r8k .OR. csig(1) > 1.0e10_r8k) WARNINGMSG = 1
    tcak = tfk(CladAveTemp)
    e = MatProp ('CLAD', 'YOUNG_MOD', tcak) * PatoPSI
    cr = MatProp ('CLAD', 'SHEAR_MOD', tcak) * PatoPSI
    v = e / (2.0_r8k * cr) - 1.0_r8k
    alfdt(2) = MatProp ('CLAD', 'THEXP_AXIAL', tcak)
    alfdt(1) = MatProp ('CLAD', 'THEXP_RADIAL', tcak)
    alfdt(3) = MatProp ('CLAD', 'THEXP_RADIAL', tcak)
    aa = 1.0_r8k + v * (rco - rci) / (rco + rci)
    bb = v * ((rco - rci) / (rco + rci) - 1.0_r8k)
    dd = -v
    ee = 1.0_r8k
    IF (nrelax == 0) THEN
        depp(1) = 0.0_r8k
        depp(2) = 0.0_r8k
        depp(3) = 0.0_r8k
        nudepp(1) = 0.0_r8k
        nudepp(2) = 0.0_r8k
        nudepp(3) = 0.0_r8k
        dep = 0.0_r8k
        cc = (FuelSurfDispl - delta) * 2.0_r8k / (rco + rci)
        cc = cc + ((rco - rci) / (rco + rci)) * (cepp(3) + depp(3) + alfdt(3))
        cc = cc - (cepp(1) + depp(1) + alfdt(1))
        cc = cc * e
        ff = ceps(2) - (cepp(2) + depp(2) + alfdt(2))
        ff = ff * e
        csig(1) = (cc * ee - bb * ff) / (aa * ee - bb * dd)
        csig(2) = (aa * ff - cc * dd) / (aa * ee - bb * dd)
        IF (nplast == 0) GOTO 170
        sigef1 = SQRT(csig(1) ** 2 + csig(2) ** 2 - csig(1) * csig(2))
        cep1 = cep
        testr = 550.0_r8k
        excesh2 = MIN(ExcessH2Concen, testr)
        !
        CALL strain (sigef1, cc, cep1, CladAveTemp, excesh2)
        !
        dep = cep1 - cep
        IF ((dep - 1.0e-10_r8k) <= 0.0_r8k) GOTO 170
        ! ***start iteration solution here
        cep1 = cep
        testr = 550.0_r8k
        excesh2 = MIN(ExcessH2Concen, testr)
        !
        CALL stress (olsigf, zero, cep1, CladAveTemp, excesh2)
        !
        cep1 = cep
        !
        CALL stress (dsigf, small, cep1, CladAveTemp, excesh2)
        !
        slope = (dsigf - olsigf) / small
        coef = 2.0_r8k * (1.0_r8k + v) / (3.0_r8k * e)
    END IF
    kk = 1
    oldepp(1) = 1.0e-7_r8k
    oldepp(2) = 2.0e-7_r8k
    oldepp(3) = -3.0e-7_r8k
120 CONTINUE
    cc = (FuelSurfDispl - delta) * 2.0_r8k / (rco + rci)
    cc = cc + ((rco - rci) / (rco + rci)) * (cepp(3) + oldepp(3) + alfdt(3))
    cc = cc - (cepp(1) + oldepp(1) + alfdt(1))
    cc = cc * e
    ff = ceps(2) - (cepp(2) + oldepp(2) + alfdt(2))
    ff = ff * e
    csig(1) = (cc * ee - bb * ff) / (aa * ee - bb * dd)
    csig(2) = (aa * ff - cc * dd) / (aa * ee - bb * dd)
    IF (nrelax == 1) THEN ! ***calculate creep strains here
        sm = (csig(1) + csig(2)) / 3.0_r8k
        s1 = csig(1) - sm
        s2 = csig(2) - sm
        dep = 0.47140452_r8k * SQRT((oldepp(1) - oldepp(2)) ** 2 + (oldepp(2) - oldepp(3)) ** 2 + (oldepp(3) - oldepp(1)) ** 2)
        depdot = dep / dtime
        crap = crep + dep
        CALL creep (sigeff, depdot, ProblemTime, CladAveTemp, flux, crap, iquit)
        nudepp(1) = 1.5_r8k * dep * s1 / sigeff
        nudepp(2) = 1.5_r8k*  dep * s2 / sigeff
        nudepp(3) = -nudepp(1) - nudepp(2)
    ELSE
        emodh = (csig(1) - v * csig(2)) / e + oldepp(1)
        emodx = (csig(2) - v * csig(1)) / e + oldepp(2)
        emodr = -v * (csig(1) + csig(2)) / e - oldepp(1) - oldepp(2)
        eet = 0.471405_r8k * SQRT((emodh - emodr) ** 2 + (emodr - emodx) ** 2 + (emodx - emodh) ** 2)
        dep = (eet - coef * olsigf) / (1.0_r8k + coef*slope)
        em = (emodh + emodr + emodx) / 3.0_r8k
        nudepp(1) = (dep / eet) * (emodh - em)
        nudepp(2) = (dep / eet) * (emodx - em)
        nudepp(3) = -nudepp(1) - nudepp(2)
    END IF
    ratio1 = (nudepp(1) - oldepp(1)) / oldepp(1)
    ratio2 = (nudepp(2) - oldepp(2)) / oldepp(2)
    ratio3 = (nudepp(3) - oldepp(3)) / oldepp(3)
    ratio1 = ABS(ratio1)
    ratio2 = ABS(ratio2)
    ratio3 = ABS(ratio3)
    ratio = MAX(ratio1, ratio2, ratio3)
    IF ((ratio - 0.001_r8k) <= 0.0_r8k) GOTO 170
    kk = kk + 1
    IF (kk - 199 >= 0) GOTO 180
    oldepp(1) = nudepp(1)
    oldepp(2) = nudepp(2)
    oldepp(3) = nudepp(3)
    GOTO 120
170 CONTINUE
    IF (nrelax == 0) THEN
        cep = cep + dep
    ELSE
        crep = crep + dep
    END IF
    cepp(1) = cepp(1) + nudepp(1)
    cepp(2) = cepp(2) + nudepp(2)
    cepp(3) = cepp(3) + nudepp(3)
    GOTO 190
180 Write (ounit,220) kk
    ! ***even tho routine did not converge
    ! ***last computed values are returned, and
    ! ***an error message is printed
    GOTO 170
    ! ***compute interface pressure
190 RinterfacPress = ((rco - rci) * csig(1) + rco * CoolantPress) / rci
    IF (RinterfacPress < GapPress) THEN
        ! ***for situation where RinterfacPress l.t. GapPress
        ! RinterfacPress is set equal to GapPress and elastic soln is obtained
        ! such that RinterfacPress always equals GapPress
        csig(1) = (rci * GapPress - rco * CoolantPress) / (rco - rci)
        rbar = 0.5_r8k * (rco + rci)
        t = rco - rci
        csig(2) = ((rbar + v * t / 2.0_r8k) * csig(1) + rbar * e * (cepp(1) + alfdt(1)) - 0.5_r8k * &
          &       t * e * (cepp(3) + alfdt(3)) - e * (FuelSurfDispl - delta)) / (rci * v)
        ceps(1) = (csig(1) - v * csig(2)) / e + cepp(1) + alfdt(1)
        ceps(2) = (csig(2) - v * csig(1)) / e + cepp(2) + alfdt(2)
        ceps(3) = -v * (csig(1) + csig(2)) / e + cepp(3) + alfdt(3)
        RinterfacPress = GapPress
    ELSE
        ! ***compute radial strain
        ceps(3) = -v * (csig(1) + csig(2)) / e + cepp(3) + alfdt(3)
        ceps(1) = (FuelSurfDispl - delta + 0.5_r8k * (rco - rci) * ceps(3)) * 2.0_r8k / (rco + rci)
    END IF
220 FORMAT ('did not converge in',1x,i4,1x,' iterations in couple')
    !
    END SUBROUTINE couple
    !
    !
    !
    SUBROUTINE fcmi (IgapGapIndex, eps, epp, sig, reps, rfeps, feps, dlrod, dlrel, crep1, &
      &              OldCladStrn, OldFuelStrn, OldCladAvTemp, OldGapPress, OldCoolPress, &
      &              OldFuelDispl, IgapIndexOld, eppsav, dtime, ProblemTime, CreepStrain, repsav, &
      &              rfpsav, epp1, CladH2Concen, ExcessH2Concen, UniformAxNodStrn, iquit, jjj, it, nplast, nrelax)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon,  ONLY : CladAveTemp, sigeff, sigy, FuelSurfDispl, &
      &                    CladInSurDisp, AxialNodLength, PrevCladEffPlasStrn, CoolantPress, &
      &                    RinterfacPress, FuelCladGap, GapPress, PlastStrnep1, ounit, na, &
      &                    ThermalStrain, epsav, eps1, sig1, dp, dci, dco, nt, FastFlux
    IMPLICIT NONE
    !>@brief
    !> This package of subroutines performs an elasto-plastic analysis of a typical pwr
    !> fuel rod. fuel radial displacements and axial strains at each axial node are input.
    !> fcmi is called from fracas.
    !
    ! Input
    !
    ! dtime               - time increment (hrs)
    ! AxialNodLength      - axial node length (in)
    ! feps                - hoop, axial, & radial fuel strains
    ! it                  - power-time step number
    ! jjj                 - axial node index
    ! na                  - maximum number of axial nodes
    ! nrelax              - creep flag
    ! nplast              - elastic-plastic flag
    ! OldCoolPress        - coolant channel pressure of previous power step (psia)
    ! OldGapPress         - rod internal gas pressure of previous power step (psia)
    ! OldCladAvTemp       - cladding average temperature of previous power step (F)
    ! OldFuelDispl        - fuel radial displacement of previous power step (in)
    ! OldCladStrn         - old cladding strains - previous power step
    ! OldFuelStrn         - old fuel strains of previous power step
    ! IgapIndexOld        - old values of gap closure index
    ! CoolantPress        - coolant channel pressure (psia)
    ! GapPress            - rod internal gas pressure (psia)
    ! rci                 - cladding inside radius (in)
    ! rco                 - cladding outside radius (in)
    ! repsav              - residual cladding strain
    ! rfo                 - pellet radius (in)
    ! rfpsav              - residual fuel strain
    ! CladAveTemp         - cladding average temperature (F)
    ! ProblemTime         - end of step time (hrs)
    ! FuelSurfDispl       - fuel surface radial displacement (in)
    ! CladH2Concen(k)     - cladding H2 concentration at axial node k (ppm)
    ! ExcessH2Concen(k)   - excess cladding H2 concentration at axial node k (ppm)
    !
    ! Output
    !
    ! dlrel               - relative change in length wrt the fuel length change (in)
    ! dlrod               - change in length of the active cladding length (in)
    ! CreepStrain         - total accumulated creep strain (dimensionless)
    ! FuelCladGap         - radial gap thickness (in)
    ! IgapGapIndex        - gap closure index
    ! PrevCladEffPlasStrn - cladding effective plastic strain (in/in)
    ! epp                 - cladding hoop, axial, & radial strain (in/in)
    ! eps                 - cladding hoop, axial, & radial strain per node
    ! eppsav              - plastic cladding strain
    ! epsav               - cladding strain
    ! RinterfacPress      - interfacial pressure (psia)
    ! reps                - residual cladding strains
    ! rfeps               - residual fuel strains
    ! sig                 - cladding hoop, axial, & radial stress per node (psi)
    ! UniformAxNodStrn(k) - value of uniform strain at axial node k
    !
    INTEGER(ipk) :: j, k
    INTEGER(ipk), INTENT(IN) :: iquit, jjj, it, nplast, nrelax
    REAL(r8k) :: crep, cep, urc1, testr, gap1, fs, flux, excesh2, errormsg, dlrod, dlrel
    REAL(r8k), INTENT(IN) :: ProblemTime, dtime
    INTEGER(ipk), DIMENSION(na) :: IgapGapIndex, IgapIndexOld
    REAL(r8k), DIMENSION(na,3) :: eps, epp, epp1, sig, reps, rfeps, feps, eppsav, repsav, rfpsav
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, alfdt
    REAL(r8k), DIMENSION(na) :: OldCladStrn, OldFuelStrn, OldCladAvTemp, OldCoolPress, OldFuelDispl, &
      &                         CreepStrain, crep1, OldGapPress, CladH2Concen, ExcessH2Concen, &
      &                         UniformAxNodStrn, rfo, delta, rci, rco
    ! Find free clad displacement due to internal and external pressures. Compute a gap width.
    rfo(1:na) = dp(1:na) / 2.0_r8k
    rci(1:na) = dci(1:na) / 2.0_r8k
    rco(1:na) = dco(1:na) / 2.0_r8k
    delta(1:na) = rci(1:na) - rfo(1:na)
    DO j = 1, nt
        cepp(1) = epp(j,1)
        cepp(2) = epp(j,2)
        cepp(3) = epp(j,3)
        cep = PrevCladEffPlasStrn(j)
        crep = CreepStrain(j)
        fs = 0.0
        testr = 550.0_r8k
        excesh2 = MIN(ExcessH2Concen(j), testr)
        CALL cladf (GapPress(j), CoolantPress(j), CladAveTemp(j), rci(j), rco(j), csig, ceps, cepp, cep, &
          &         fs, nrelax, dtime, ProblemTime, crep, CladH2Concen(j), excesh2, nplast, &
          &         sigeff(j), sigy(j), alfdt, j, 1)
        ThermalStrain(j,1) = alfdt(1)
        ThermalStrain(j,2) = alfdt(2)
        ThermalStrain(j,3) = alfdt(3)
        sig1(j,1) = csig(1)
        sig1(j,2) = csig(2)
        sig1(j,3) = 0.0_r8k
        eps1(j,1) = ceps(1)
        eps1(j,2) = ceps(2)
        eps1(j,3) = ceps(3)
        epp1(j,1) = cepp(1)
        epp1(j,2) = cepp(2)
        epp1(j,3) = cepp(3)
        PlastStrnep1(j) = cep
        crep1(j) = crep
        urc1 = 0.5_r8k * ((rco(j) + rci(j)) * eps1(j,1) - (rco(j) - rci(j)) * eps1(j,3))
        ! ***now check for interference
        ! **This is the criteria for hard contact**
        gap1 = urc1 + delta(j) - FuelSurfDispl(j)
        IF (gap1 <= 0.0_r8k) THEN
            IgapGapIndex(j) = 1
        ELSE
            IgapGapIndex(j) = 0
        END IF
    END DO
    DO j = 1, nt
        ! Determine if contact has occured
        IF (IgapGapIndex(j) == 0) THEN ! Node j is NOT in contact
            k = j
            sig(k,1) = sig1(k,1)
            sig(k,2) = sig1(k,2)
            sig(k,3) = sig1(k,3)
            eps(k,1) = eps1(k,1)
            eps(k,2) = eps1(k,2)
            eps(k,3) = eps1(k,3)
            reps(k,2) = eps(k,2)
            rfeps(k,2) = feps(k,2)
            CladInSurDisp(k) = 0.5_r8k * (rco(k) + rci(k)) * eps(k,1) - 0.5_r8k * (rco(k) - rci(k)) * eps(k,3)
            FuelCladGap(k) = CladInSurDisp(k) + delta(k) - FuelSurfDispl(k)
            IF (FuelCladGap(k) <= 0.0_r8k) ERRORMSG = 1.0_r8k
            RinterfacPress(k) = 0.0_r8k
            epp(k,1) = epp1(k,1)
            epp(k,2) = epp1(k,2)
            epp(k,3) = epp1(k,3)
            PrevCladEffPlasStrn(k) = PlastStrnep1(k)
            CreepStrain(k) = crep1(k)
            !
            IF (nrelax /= 1 .AND. j == jjj) THEN
                eppsav(k,1) = epp1(k,1)
                eppsav(k,2) = epp1(k,2)
                eppsav(k,3) = epp1(k,3)
                epsav(k) = PlastStrnep1(k)
                repsav(k,1) = reps(k,1)
                repsav(k,2) = reps(k,2)
                repsav(k,3) = reps(k,3)
                rfpsav(k,1) = rfeps(k,1)
                rfpsav(k,2) = rfeps(k,2)
                rfpsav(k,3) = rfeps(k,3)
            END IF
            !
        ELSE ! Node j is in contact
            ! Compute prescribed axial strain in clad based on last values of axial strain
            ! prior to contact. local fuel radial displacement is passed to *couple* .
            IF (nrelax == 1 .OR. IgapGapIndex(j) == 0 .OR. j == jjj) THEN
                IF (IgapIndexOld(j) == 0) CALL gapcls (delta(j), epp, rci(j), rco(j), rfo(j), OldCladAvTemp, OldGapPress, &
                  &                                    OldCoolPress, OldFuelDispl, j, feps, rfeps, reps, OldCladStrn, &
                  &                                    OldFuelStrn, nrelax, dtime, ProblemTime, CreepStrain, CladH2Concen(j), &
                  &                                    ExcessH2Concen(j), na, iquit, nplast)
                eps(j,2) = reps(j,2) + (feps(j,2) - rfeps(j,2))
                ceps(2) = eps(j,2)
                cepp(1) = epp(j,1)
                cepp(2) = epp(j,2)
                cepp(3) = epp(j,3)
                cep = PrevCladEffPlasStrn(j)
                crep = CreepStrain(j)
                ! No slip assumed in axial direction as long as interface pressure is above local gas pressure
                CALL couple (GapPress(j), CoolantPress(j), CladAveTemp(j), rci(j), rco(j), csig, &
                  &          ceps, cepp, cep, FuelSurfDispl(j), delta(j), sigeff(j), sigy(j), &
                  &          RinterfacPress(j), nrelax, dtime, ProblemTime, crep, CladH2Concen(j), &
                  &          ExcessH2Concen(j), iquit, nplast, it, alfdt, FastFlux(j))
                !
                ThermalStrain(j,1) = alfdt(1)
                ThermalStrain(j,2) = alfdt(2)
                ThermalStrain(j,3) = alfdt(3)
                sig(j,1) = csig(1)
                sig(j,2) = csig(2)
                sig(j,3) = 0.0_r8k
                eps(j,1) = ceps(1)
                eps(j,2) = ceps(2)
                eps(j,3) = ceps(3)
                epp(j,1) = cepp(1)
                epp(j,2) = cepp(2)
                epp(j,3) = cepp(3)
                PrevCladEffPlasStrn(j) = cep
                CreepStrain(j) = crep
                IF (nrelax /= 1) THEN
                    eppsav(j,1) = cepp(1)
                    eppsav(j,2) = cepp(2)
                    eppsav(j,3) = cepp(3)
                    epsav(j) = cep
                    repsav(j,1) = reps(j,1)
                    repsav(j,2) = reps(j,2)
                    repsav(j,3) = reps(j,3)
                    rfpsav(j,1) = rfeps(j,1)
                    rfpsav(j,2) = rfeps(j,2)
                    rfpsav(j,3) = rfeps(j,3)
                END IF
                FuelCladGap(j) = 0.0_r8k
            END IF
        END IF
    END DO
    ! Compute rod length and displacement between fuel stack + clad
    dlrod = 0.0_r8k
    dlrel = 0.0_r8k
    DO k = 1, nt
        dlrod = dlrod + (eps(k,2)) * AxialNodLength(k)
        dlrel = dlrel + (eps(k,2) - feps(k,2)) * AxialNodLength(k)
        OldCladStrn(k) = eps(k,2)
        OldFuelStrn(k) = feps(k,2)
        IgapIndexOld(k) = IgapGapIndex(k)
        OldCladAvTemp(k) = CladAveTemp(k)
        OldGapPress(k) = GapPress(k)
        OldCoolPress(k) = CoolantPress(k)
        OldFuelDispl(k) = FuelSurfDispl(k)
    END DO
    !
    END SUBROUTINE fcmi
    !
    !
    !
    SUBROUTINE cladf (GapPress, CoolantPress, CladAveTemp, rci, rco, csig, ceps, cepp, cep, &
      &               fs, nrelax, dtime, ProblemTime, CreepStrain, CladH2Concen, ExcessH2Concen, &
      &               nplast, sigeff, sigy, alfdt, j, jcreep)
    USE Kinds
    USE conversions_frapcon
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> cladf is called from fcmi and gaptf and calculates the free standing cladding deformation
    !
    ! Input
    !
    ! csig           - temporary hoop, axial, & radial stress (psi)
    ! dtime          - time increment (hrs)
    ! fs             - this is presently equal to zero
    ! nplast         - elastic-plastic flag
    ! nrelax         - creep flag (0 = no creep calculated, 1 = creep calculated)
    ! CoolantPress   - coolant channel pressure (psia)
    ! pg             - rod internal gas pressure (psia)
    ! rci            - cladding inside radius (in)
    ! rco            - cladding outside radius (in)
    ! CladAveTemp    - cladding average temperature (F)
    ! ProblemTime    - end of step time (hrs)
    ! CladH2Concen   - concentration of H2 in cladding (ppm)
    ! ExcessH2Concen - concentration of H2 in cladding above solubility limit, (ppm)
    !
    ! Output
    !
    ! cep            - temporary effective plastic strain (im/in)
    ! cepp           - temporary hoop. axial, & radial plastic strains (in/in)
    ! ceps           - temporary hoop, axial, & radial strains (in/in)
    ! CreepStrain    - creep strain (in/in)
    !
    INTEGER(ipk) :: jcreep, i
    INTEGER(ipk), INTENT(IN) :: j, nrelax, nplast
    REAL(r8k) :: cathex, cdthex, tcak, v, sm, sigy, sigeff, s1, s2, e, depdot, dep, cr, cep1
    REAL(r8k) :: cc = 1.0_r8k
    REAL(r8k), INTENT(IN) :: rco, rci, ProblemTime, GapPress, fs, ExcessH2Concen, dtime, CoolantPress, &
      &                      CladH2Concen, CladAveTemp
    REAL(r8k), INTENT(INOUT) :: CreepStrain, cep
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp, depp, alfdt
    !
    csig(1) = (rci * GapPress - rco * CoolantPress) / (rco - rci)
    csig(2) = (pi * (rci ** 2 * GapPress - rco ** 2 * CoolantPress) + fs) / (pi * (rco ** 2 - rci ** 2))
    IF (nplast == 1) THEN
        sigeff = SQRT(csig(1) ** 2 + csig(2) ** 2 - csig(1) * csig(2))
        IF ((sigeff - 0.001_r8k) <= 0.0_r8k) THEN
            depp(1) = 0.0_r8k
            depp(2) = 0.0_r8k
            depp(3) = 0.0_r8k
            dep = 0.0_r8k
        ELSE
            sm = (csig(1) + csig(2)) / 3.0_r8k
            s1 = csig(1) - sm
            s2 = csig(2) - sm
            cep1 = cep
            IF (nrelax == 1) THEN
                CALL crepr (csig(1), depdot, CladAveTemp, GapPress, CoolantPress, rci, rco, ProblemTime, j, dtime, jcreep)
                dep = depdot * dtime
            ELSE
                CALL strain (sigeff, cc, cep1, CladAveTemp, ExcessH2Concen)
                dep = cep1 - cep
            END IF
            depp(1) = 1.5_r8k * dep * s1 / sigeff
            depp(2) = 1.5_r8k * dep * s2 / sigeff
            depp(3) = -depp(1) - depp(2)
        END IF
    ELSE
        depp(1) = 0.0_r8k
        depp(2) = 0.0_r8k
        depp(3) = 0.0_r8k
        dep = 0.0_r8k
    END IF
    tcak = tfk(CladAveTemp)
    e = MatProp ('CLAD', 'YOUNG_MOD', tcak) * PatoPSI
    cr = MatProp ('CLAD', 'SHEAR_MOD', tcak) * PatoPSI
    v = e / (2.0_r8k * cr) - 1.0_r8k
    alfdt(2) = MatProp ('CLAD', 'THEXP_AXIAL', tcak)
    alfdt(1) = MatProp ('CLAD', 'THEXP_RADIAL', tcak)
    alfdt(3) = MatProp ('CLAD', 'THEXP_RADIAL', tcak)
    ceps(1) = (csig(1) - v * csig(2)) / e + cepp(1) + depp(1)
    ceps(2) = (csig(2) - v * csig(1)) / e + cepp(2) + depp(2)
    ceps(3) = -v * (csig(1) + csig(2)) / e + cepp(3) + depp(3)
    ! ***these are mechanical strains - now add thermal strains
    ! edit here
    ceps(1) = ceps(1) + alfdt(1)
    ceps(2) = ceps(2) + alfdt(2)
    ceps(3) = ceps(3) + alfdt(3)
    cepp(1) = cepp(1) + depp(1)
    cepp(2) = cepp(2) + depp(2)
    cepp(3) = cepp(3) + depp(3)
    ! ceps(1) = 0.0_r8k
    ! ceps(2) = 0.0_r8k
    ! ceps(3) = 0.0_r8k
    ! cepp(1) = 0.0_r8k
    ! cepp(2) = 0.0_r8k
    ! cepp(3) = 0.0_r8k
    IF (nrelax == 0) THEN
        cep = cep + dep
    ELSE
        CreepStrain = CreepStrain + dep
    END IF
    !
    END SUBROUTINE cladf
    !
    !
    !
    SUBROUTINE gapcls (delta, epp, rci, rco, rfo, OldCladAvTemp, OldGapPress, OldCoolPress, &
      &                OldFuelDispl, j, feps, rfeps, reps, OldCladStrn, OldFuelStrn, &
      &                nrelax, dtime, ProblemTime, CreepStrain, CladH2Concen, ExcessH2Concen, &
      &                na, iquit, nplast)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : CladAveTemp, FuelSurfDispl, PrevCladEffPlasStrn, ounit
    IMPLICIT NONE
    !>@brief
    !> This routine seeks out the point of gap closure gapcls is called from fcmi
    !
    ! Input
    !
    ! csig          - temporary hoop, axial, & radial stress (psi)
    ! delta         - cold state radial gap (in)
    ! dtime         - time increment (hrs)
    ! CreepStrain   - creep strain (in/in)
    ! feps          - hoop, axial, & radial fuel strains
    ! j             - axial node index
    ! na            - maximum number of axial nodes
    ! nplast        - elastic-plastic flag
    ! nrelax        - creep flag
    ! OldCoolPress  - coolant channel pressure of previous power step (psia)
    ! OldGapPress   - rod internal gas pressure of previous power step (psia)
    ! OldCladAvTemp - cladding average temperature-previous power step (F)
    ! OldFuelDispl  - fuel radial displacement of previous power step (in)
    ! CoolantPress  - coolant channel pressure (psia)
    ! GapPress      - rod internal gas pressure (psia)
    ! rfo           - fuel outside radius (in)
    ! CladAveTemp   - cladding average temperature (F)
    ! ProblemTime   - end of step time (hrs)
    ! FuelSurfDispl - fuel radial displacement (in)
    ! rci           - cladding  inside radius (in)
    ! rco           - cladding outside radius (in)
    !
    ! Output
    !
    ! PrevCladEffPlasStrn - cladding effective plastic strain (in/in)
    ! epp                 - cladding hoop, axial, & radial strains (in/in)
    ! iquit               - termination index
    ! rfeps               - residual fuel strains
    ! reps                - residual cladding strains
    ! OldCladStrn         - old cladding strains - previous power step
    ! OldFuelStrn         - old fuel strains - previous power step
    !
    INTEGER(ipk) :: n, iquit, j, nrelax, nplast
    INTEGER(ipk), INTENT(IN) :: na
    REAL(r8k) :: alpha, beta, dx, cut, denom, x, fofx, t, vx, a, b, fofa, fofb, delta, cep, crep
    REAL(r8k), INTENT(IN) :: rci, rco, rfo, CladH2Concen, ExcessH2Concen, dtime, ProblemTime
    REAL(r8k), DIMENSION(3) :: csig, ceps, cepp
    REAL(r8k), DIMENSION(na) :: OldCladAvTemp, OldCoolPress, OldFuelDispl, CreepStrain, OldFuelStrn, &
      &                         OldCladStrn, OldGapPress
    REAL(r8k), DIMENSION(na,3) :: epp, feps, rfeps, reps
    ! rooting for roots
    !  f(t) = gaptf(t,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
    !    &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
    ! a modified false position routine operating over the interval alpha,
    ! beta with increment dx has been installed by b.w.burnham 1-6-76
    alpha = 0.0_r8k
    beta = 1.0_r8k
    dx = 0.25_r8k
    cut = 1.0e-10_r8k
    n = 0
    ! a check for a linear function on the alpha-beta interval
    denom = gaptf(beta,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast) - &
      &         gaptf(alpha,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
    IF (denom == 0.0_r8k) GOTO 100
    x = (alpha * gaptf(beta,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)&
      & - beta * gaptf(alpha,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)) / denom
    fofx = gaptf(x,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
    IF (ABS(fofx) > cut) GOTO 100
    vx = x
    GOTO 220
100 CONTINUE
    ! commence the moving window looking for root brackets
    a = alpha
    b = a + dx
    fofa = gaptf(a,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
110 fofb = gaptf(b,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
    IF (fofa * fofb < 0.0_r8k) GOTO 120
    IF (fofa * fofb == 0.0_r8k) GOTO 180
    IF (fofa * fofb > 0.0_r8k) GOTO 200
120 CONTINUE
    ! having bracketed a root the solution iteration commences
130 CONTINUE
    x = (a * fofb - b * fofa) / (fofb - fofa)
    n = n + 1
    IF (n >= 100) THEN
        WRITE (ounit,250)
        iquit = 1
        RETURN
    END IF
    fofx = gaptf(x,delta,epp,rci,rco,ceps,cepp,cep,csig,OldCladAvTemp,OldGapPress,OldCoolPress,OldFuelDispl, &
      &         j,nrelax,dtime,ProblemTime,CreepStrain,crep,CladH2Concen,ExcessH2Concen,na,nplast)
    ! test the possibility of the iteration loop landing on a root at x or passing by the root
    IF (fofx * fofa < 0.0_r8k) GOTO 160
    IF (fofx * fofa == 0.0_r8k) GOTO 170
    IF (fofx * fofa > 0.0_r8k) GOTO 150
150 CONTINUE
    ! the root is still bracketed and the iteration loop continues
    a = x
    fofa = fofx
    fofb = 0.5_r8k * fofb
    IF (ABS(fofx) > cut) GOTO 130
    vx = x
    GOTO 220
160 CONTINUE
    ! the iteration scheme has rebracketed the root, the bounds are adjusted and the loop continues
    b = x
    fofb = fofx
    fofa = 0.5_r8k * fofa
    IF (ABS(fofx) > cut) GOTO 130
    vx = x
    GOTO 220
170 CONTINUE
    ! the iteration scheme has landed on a root at (x,fofx)
    IF (ABS(fofa) == 0.0_r8k) GOTO 190
    vx = x
    GOTO 220
180 CONTINUE
    ! the bracketing operation has found a root
    IF (fofa == 0.0_r8k) GOTO 190
    ! the bracketing operation has found a root at (b,fofb)
    vx = b
    GOTO 220
190 CONTINUE
    ! If this is the first bracket step then f(alpha)=0. If otherwise
    ! the previous step is in error at fofb
    ! the initial data contains a root at alpha
    vx = a
    GOTO 220
200 CONTINUE
    ! the routine has not yet bracketed a root-adjust the variables to
    ! the next mesh and Continue, also test to see If the interval has been exceeded
    a = b
    b = b + dx
    fofa = fofb
    IF (a > beta) THEN
        ! the bracketing operation has overrun the interval
        WRITE (ounit,260)
        iquit = 1
        RETURN
    ELSE
        GOTO 110
    END IF
220 CONTINUE
    ! ***update residual strains here
    OldCladStrn(j) = ceps(2)
    reps(j,2) = ceps(2)
    IF (nrelax /= 1) THEN
        OldFuelStrn(j) = OldFuelStrn(j) + vx * (feps(j,2) - OldFuelStrn(j))
        rfeps(j,2) = OldFuelStrn(j)
    END IF
    ! ***update plastic strains here after gap zero solution is found
    epp(j,1) = cepp(1)
    epp(j,2) = cepp(2)
    epp(j,3) = cepp(3)
    IF (nrelax == 1) THEN
        CreepStrain(j) = crep
    ELSE
        PrevCladEffPlasStrn(j) = cep
    END IF
    ! ***can print clad stress and strain at zero gap here
    ! WRITE (ounit,100)
    ! WRITE (ounit,101) (csig(i),i = 1,3)
    ! WRITE (ounit,101) (ceps(i),i = 1,3)
    ! WRITE (ounit,101) (cepp(i),i = 1,3)
    ! WRITE (ounit,101)  reps(j,2), rfeps(j,2)
    !
250 FORMAT ('- the iteration scheme has passed 100 steps in gapcls ')
260 FORMAT (' the bracket operation has overrun the interval in gapcls')
    !
    END SUBROUTINE gapcls
    !
    !
    !
    SUBROUTINE crepr (sig, edot, CladAveTemp, GapPress, CoolantPress, rci, rco, ProblemTime, j, dtime, jcreep)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : icm, sigcreep, stold, stnew, deltimeold, deltimenew, FastFlux, &
      &                   sagold, sagnew, delsagold, delsagnew, delst, fnck, cwkf
    IMPLICIT NONE
    !
    !>@brief
    !> crepr computes (transverse) cladding creep strain rate as a function of effective stress, 
    !> temperature, fast neutron flux and accumulated total creep strain.
    !
    ! Input
    !
    ! CreepStrain - total accumulated creep strain (dimensionless)
    ! FastFlux    - flux (neutrons/m**2/sec)
    ! ProblemTime - time to end of power step (hr)
    ! CladAveTemp - cladding average temperature (F)
    ! sig         - cladding hoop stress per node (psi)
    !
    ! Output
    !
    ! edot        - creep strain rate (1/hr)
    !
    INTEGER(ipk), INTENT(IN) :: j, jcreep
    REAL(r8k) :: delsag, sr, st, sl, stdir, stdirold, sag, fluencelimback, tcak, deltime, delstdir, a, an, &
      &          e, q, r, ai, edottherm, edotthermp, c0sr, c0rxa, c0, c1, c2, tmod, ctemp, edotirr, edotirrp, &
      &          esat, eprimary
    REAL(r8k), INTENT(IN) :: sig, CladAveTemp, GapPress, CoolantPress, rci, rco, ProblemTime, dtime
    REAL(r8k), INTENT(OUT) :: edot
    !
    delsag = 0.0_r8k
    deltime = 0.0_r8k
    ! NEW MODEL
    ! New model uses a modified version of the Limback and Andersson equation
    ! ASTM STP 1295 pp. 448-468
    ! Calculate stress in each direction
    sr = (GapPress * (rci ** 2) - CoolantPress * (rco ** 2) + (rci ** 2) * (rco ** 2) * &
      &  (CoolantPress - GapPress) / ((rco + rci) / 2.0_r8k) ** 2) / ((rco ** 2) - (rci ** 2))
    st = (GapPress * (rci ** 2) - CoolantPress * (rco ** 2) - (rci ** 2) * (rco ** 2) * &
      &  (CoolantPress - GapPress) / ((rco + rci) / 2.0_r8k) ** 2) / ((rco ** 2) - (rci ** 2))
    sl = (GapPress * (rci ** 2) - CoolantPress * (rco ** 2)) / ((rco ** 2) - (rci ** 2))
    ! sag is effective stress in MPa
    ! Calculate direction of st
    IF (st == 0) THEN
        stdir = 1.0_r8k
    ELSE
        stdir = ABS(st)/st
    END IF
    IF (stold(j) == 0.0_r8k) THEN
        stdirold = 1.0_r8k
    ELSE
        stdirold = ABS(stold(j)) / stold(j)
    END IF
    ! sag = sig * PSItoMPa
    sag = 0.707_r8k * SQRT(((sl - st) ** 2) + ((st - sr) ** 2) + ((sr - sl) ** 2)) * PSItoMPa
    tcak = tfk(CladAveTemp)
    fluenceLimback = fnck / 10000.0_r8k
    IF (jcreep /= 2 .AND. ProblemTime >= 0.1_r8k) THEN
        !
        ! Is there a change in effecitve stress >5MPa (725psi)?
        !
        ! Yes
        IF (ABS(sag * stdir - sagold(j) * stdirold) > 5.0_r8k) THEN
            ! IF (ABS(sag - sagold(j)) > 5.0_r8k) THEN
            deltime = dtime
            delsag = ABS(sag * stdir - sagold(j) * stdirold)
            ! delsag = ABS(sag - sagold(j))
            delst(j) = st - stold(j)
            stnew(j) = st
            deltimenew(j) = deltime
            sagnew(j) = sag
            delsagnew(j) = delsag
        ELSE ! No
            deltime = dtime + deltimeold(j)
            delsag = delsagold(j)
            deltimenew(j) = deltime
        END IF
        IF (delst(j) == 0.0_r8k) THEN
            delstdir = 1.0_r8k
        ELSE
            delstdir = ABS(delst(j)) / delst(j)
        END IF
    ELSE
        delstdir = 1.0_r8k
    END IF
    !
    ! Thermal Creep Rate
    !
    A = 1.08e9_r8k
    IF (cwkf == 0.0_r8k .OR. icm == 5) A = 5.47e8_r8k
    an = 2.0_r8k
    IF (cwkf == 0.0_r8k .OR. icm == 5) an = 3.5_r8k
    E = 1.148e5_r8k - 59.9_r8k * tcak
    Q = 201000.0_r8k
    R = R_JmolK
    ai = 650.0_r8k * (1.0_r8k - 0.56_r8k * (1.0_r8k - EXP(-1.4e-27_r8k * (fluenceLimback ** 1.3_r8k))))
    edottherm = A * (E / tcak) * (sinh(ai * ABS(sag) / E)) ** an * EXP(-Q / (R * tcak))
    edotthermp = A * (E / tcak) * (sinh(ai * ABS(delsag) / E)) ** an * EXP(-Q / (R * tcak))
    !
    ! Irradiation Creep Rate
    !
    c0SR = 4.0985e-24_r8k
    c0RXA = 1.87473e-24_r8k
    c0 = C0SR
    IF (cwkf == 0.0_r8k) c0 = c0RXA
    c1 = 0.85_r8k
    c2 = 1.0_r8k
    tmod = tcak
    IF (tcak < 570.0_r8k) tmod = 570.0_r8k
    IF (tcak > 625.0_r8k) tmod = 625.0_r8k
    ctemp = -7.0237_r8k + 0.0136_r8k * tmod
    IF (cwkf == 0.0_r8k) ctemp = -3.18562_r8k + 0.00699132_r8k * tmod
    edotirr = c0 * (FastFlux(j) ** c1) * ((ABS(sag)) ** c2) * ctemp
    edotirrp = c0 * (FastFlux(j) ** c1) * ((ABS(delsag)) ** c2) * ctemp
    !
    ! Primary Creep
    !
    IF (ProblemTime >= 0.1_r8k) THEN
        esat = 0.0216_r8k * ((edotthermp + edotirrp) ** 0.109_r8k) * &
          &    ((2.0_r8k - TANH(3.55e4_r8k * (edotthermp + edotirrp))) ** (-2.05_r8k))
        eprimary = esat * (1.0_r8k - EXP(-52.0_r8k * SQRT((edotthermp + edotirrp) * deltime)))
        IF (ABS(sag * stdir - sagold(j) * stdirold) <= 5.0_r8k) eprimary = eprimary  -  esat *  &
          & (1.0_r8k - EXP(-52.0_r8k * SQRT((edotthermp + edotirrp) * deltimeold(j))))
        ! edotprimary = 0.5_r8k * esat * 52.0_r8k * (edottherm + edotirr) ** 0.5_r8k / ((ProblemTime) ** 0.5_r8k) *
        !   &           EXP(-52.0_r8k * (edottherm + edotirr) ** 0.5_r8k * (ProblemTime) ** 0.5_r8k)
        ! edottherm = edottherm + edotprimary
    ELSE
        eprimary = 0.0_r8k
    END IF
    !
    IF (jcreep == 2) THEN
        edot = edottherm + edotirr
    ELSE
        edot = edottherm + edotirr + eprimary / dtime * stdir * delstdir
    END IF  
    edot = edot * 1.1547_r8k
    IF (icm == 6) edot = edot * 0.8_r8k
    !
    ! add on uncertainty
    ! SRA cladding
    IF (sigcreep > 0.0_r8k .AND. cwkf /= 0.0_r8k) edot = edot * (1.0_r8k + sigcreep * 0.145_r8k)
    IF (sigcreep < 0.0_r8k .AND. cwkf /= 0.0_r8k) edot = edot / (1.0_r8k - sigcreep * 0.145_r8k)
    ! RXA cladding
    IF (sigcreep > 0.0_r8k .AND. cwkf == 0.0_r8k) edot = edot * (1.0_r8k + sigcreep * 0.216_r8k)
    IF (sigcreep < 0.0_r8k .AND. cwkf == 0.0_r8k) edot = edot / (1.0_r8k - sigcreep * 0.216_r8k)
    !
    END SUBROUTINE crepr
    !
    !
    !
    SUBROUTINE creep (sig, edot, z, CladAveTemp, phi, ecreep, iquit)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> creep is called from couple and stack and searches for the correct value of stress
    !
    ! Input
    !
    ! CladAveTemp - Cladding average temperature (F)
    ! ecreep      - Total accumulated creep strain
    ! edot        - Creep strain rate (1/hr)
    ! phi         - Flux (neutrons/m**2/sec)
    ! z           - Time (hr) - not used presently
    !
    ! Output
    !
    ! sig    - Cladding hoop stress (psia)
    ! iquit  - Termination index
    !
    ! rooting for roots
    INTEGER(ipk) :: n
    INTEGER(ipk), INTENT(OUT) :: iquit
    REAL(r8k) :: t, alpha, beta, dx, cut, cut1, a, b, fofa, fofb, vx, x, fofx
    REAL(r8k), INTENT(IN) :: z, edot, CladAveTemp, phi, ecreep
    REAL(r8k), INTENT(OUT) :: sig
    !
    ! f(t) = crapf(t,edot,z,CladAveTemp,phi,ecreep)
    ! a modified false position routine operating over the interval alph beta with increment dx
    alpha = 0.0_r8k
    beta = 900000.0_r8k
    dx = 10000.0_r8k
    cut = 5.0_r8k
    cut1 = cut
    n = 0
    a = alpha
    b = a + dx
    fofa = crapf(a,edot,z,CladAveTemp,phi,ecreep)
100 fofb = crapf(b,edot,z,CladAveTemp,phi,ecreep)
    IF (a == 0.0_r8k .AND. fofa < 0.0_r8k) THEN
        WRITE (ounit,250)
        iquit = 1
        GOTO 240
    END IF
    ! WRITE (ounit,81) a,fofa,b,fofb
    IF (fofa * fofb < 0.0_r8k) GOTO 130
    IF (fofa * fofb == 0.0_r8k) GOTO 190
    IF (fofa * fofb > 0.0_r8k) GOTO 210
130 CONTINUE
    ! having bracketed a root the solution iteration commences
140 x = (a * fofb - b * fofa) / (fofb - fofa)
    n = n + 1
    IF (n < 100) GOTO 150
    WRITE (ounit,260)
    iquit = 1
    GOTO 240
150 fofx = crapf(x,edot,z,CladAveTemp,phi,ecreep)
    ! WRITE (ounit,80) x,fofx
    ! test the possibility of the iteration loop landing on a root at x
    ! or passing by the root
    IF (fofx * fofa < 0.0_r8k) GOTO 170
    IF (fofx * fofa == 0.0_r8k) GOTO 180
    IF (fofx * fofa > 0.0_r8k) GOTO 160
160 CONTINUE
    ! the root is still bracketed and the iteration loop continues
    a = x
    fofa = fofx
    fofb = 0.5_r8k * fofb
    IF ((ABS(b - a) > cut) .AND. (ABS(fofx) > cut1)) GOTO 140
    vx = x
    GOTO 230
170 CONTINUE
    ! the iteration scheme has rebracketed the root-the bounds are
    ! adjusted and the loop continues
    b = x
    fofb = fofx
    fofa = 0.5_r8k * fofa
    IF ((ABS(b - a) > cut) .AND. (ABS(fofx) > cut1)) GOTO 140
    vx = x
    GOTO 230
180 CONTINUE
    ! the iteration scheme has landed on a root at (x,fofx)
    IF (ABS(fofa) == 0.0_r8k) GOTO 200
    vx = x
    GOTO 230
190 CONTINUE
    ! the bracketing operation has found a root
    IF (fofa == 0.0_r8k) GOTO 200
    ! the bracketing operation has found a root at (b,fofb)
    vx = b
    GOTO 230
200 CONTINUE
    ! If this is the first bracket step then f(alpha)=0. If otherwise
    ! the previous step is in error at fofb
    WRITE (ounit,270) a
    GOTO 230
210 CONTINUE
    ! been exceeded
    ! the routine has not yet bracketed a root-adjust the variables to
    ! the next mesh and CONTINUE, also test to see If the interval has
    a = b
    b = b + dx
    fofa = fofb
    IF (a > beta) GOTO 220
    GOTO 100
220 CONTINUE
    ! the bracketing operation has overrun the interval
    WRITE (ounit,280)
    iquit = 1
    GOTO 240
230 CONTINUE
    ! WRITE (ounit,74) vx,n
    sig = vx
240 CONTINUE
    !
250 FORMAT (//,5x,'***** negative fofa at a = 0 in subroutine creep ')
260 FORMAT ('- the iteration scheme has passed 100 steps--execution terminated in subroutine creep ')
270 FORMAT (' initial data contains a root at alpha=',e15.6)
280 FORMAT (' the bracket operation has overrun the interval in subroutine creep ')
    !
    END SUBROUTINE creep
    !
    !
    !
    SUBROUTINE stress (sig, dep, epplas, CladAveTemp, cexh2)
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit
    USE Material_Properties, ONLY : MatProp
    USE ZrModels
    IMPLICIT NONE
    !>@brief
    !> This Subroutine computes stress given the previous plastic strain value and an increment of plastic strain.
    !> The elastic modulus is obtained from function *celmod*, and the locus of yeild stresses is obtained from subroutine *cstres*
    !> which computes stress from strain in monotonic, uniaxial tension. stress is called from couple and stack
    !>@author
    !> programmed by m.p. bohn ,  nov.18, 1974 .
    !
    ! Input
    !
    ! cexh2       - concentration of H2 in cladding above solubility limit (ppm)
    ! CladAveTemp - cladding average temperature (F)
    ! dep         - increment of plastic strain (in/in)
    ! epplas      - previous value of plastic strain (in/in)
    !
    ! Output
    !
    ! epplas      - new value of plastic strain (in/in)
    ! sig         - stress (psia)
    !
    ! Note:
    !
    ! Both *celmod* and *cstres*  are in s.i. units, so stresses are converted to n/m**2 and 
    ! temperatures to degrees kelvin before calling these two routines.
    !
    INTEGER(ipk) :: k
    REAL(r8k) :: tcak, epstst, cut, sigpa, eps, elmod
    REAL(r8k), INTENT(IN) :: CladAveTemp, cexh2, dep
    REAL(r8k), INTENT(OUT) :: sig
    REAL(r8k), INTENT(INOUT) :: epplas
    !
    tcak = tfk(CladAveTemp)
    elmod = MatProp ('CLAD', 'YOUNG_MOD', tcak)
    ! The follow was commented out because the value for dep is fixed in
    ! the call from couple as either 0.0 (1st call) or 1.0E-4 (2nd call)
    ! Check to ensure dep is not negative
    !IF (dep < 0.0_r8k) THEN
    !    WRITE (ounit,140) dep
    !    dep = 0.0001_r8k
    !END IF
    epplas = epplas + dep
    sig = 5000.0_r8k
    epstst = -10.0_r8k
    cut = 1.0e-10_r8k
    DO k = 1, 30
        eps = sig / (elmod * PatoPSI) + epplas
        IF ((ABS(eps - epstst)) < cut) EXIT
        epstst = eps
        CALL cstres (tcak, eps, cexh2, sigpa)
        sig = sigpa * PatoPSI
    END DO
    ! convert anisotropic effective yield stress to isotropic effective yield stress
    sig = sig / 1.19_r8k
140 FORMAT (24x,'error-negative dep'/25x,'dep = ',e11.4,'was passed to Subroutine*stress*'/25x, &
      &         'dep was set to .0001- results may be invalid')
    !
    END SUBROUTINE stress
    !
    !
    !
    SUBROUTINE strain (sig, eps, epplas, CladAveTemp, ExcessH2Concen)
    USE Kinds
    USE conversions_frapcon
    USE Material_Properties, ONLY : MatProp
    USE ZrModels
    IMPLICIT NONE
    !>@brief
    !> This Subroutine computes uniaxial strain given the stress and previous plastic stain. elastic unloading
    !> is prescribed, and the current (work-hardened) yeild stress is computed from the plastic strain at each call.
    !> The elastic modulus is obtained from function *celmod*, and the locus of yeild stresses is obtained from subroutine *cstres*
    !> which computes stress from strain in monotonic, uniaxial tension. strain is called from: cladf, couple,and stack.
    !>@author
    !> programmed by m.p. bohn
    !>@date
    !> Nov.18, 1974
    !
    ! Input
    !
    ! CladAveTemp   - cladding average temperature (F)
    ! epplas - previous value of plastic strain (in/in)
    ! ExcessH2Concen - H2 concentration in cladding above solubility limit (ppm)
    ! sig    - stress (psi)
    !
    ! Output
    !
    ! epplas - new value of plastic strain (in/in)
    ! eps    - strain (in/in)
    !
    ! Note:
    !
    ! Both *celmod* and *cstres*  are in s.i. units, so stresses are converted to n/m**2 
    ! and temperatures to degrees kelvin before calling these two routines.
    INTEGER(ipk) :: k
    REAL(r8k) :: tcak, e, sigy, epstst, cut, sigypa, sag
    REAL(r8k), INTENT(IN) :: sig, CladAveTemp, ExcessH2Concen
    REAL(r8k), INTENT(OUT) :: eps
    REAL(r8k), INTENT(INOUT) :: epplas
    !
    tcak = tfk(CladAveTemp)
    e = MatProp ('CLAD', 'YOUNG_MOD', tcak) * PatoPSI
    ! get current yeild stress given epplas
    sigy = 5000.0_r8k
    epstst = -10.0_r8k
    cut = 1.0e-10_r8k
    DO k = 1, 30
        eps = sigy / e + epplas
        IF ((ABS(eps - epstst)) < cut) EXIT
        epstst = eps
        CALL cstres (tcak, eps, ExcessH2Concen, sigypa)
        sigy = sigypa * PatoPSI
    END DO
    ! Convert anisotropic effective yield stress to isotropic effective yield stress
    sigy = sigy / 1.19_r8k
    IF ((sig - sigy) <= 0.0_r8k) THEN
        eps = epplas + sig / e
    ELSE
        sag = sig * PSItoPa
        ! Convert isotropic effective stress to anisotropic effective stress
        sag = sag * 1.19_r8k
        CALL cstran (tcak, sag, ExcessH2Concen, eps)
        ! The elastic component of strain must be consistant with the unmodified yield used in cstran and cstres.
        ! These changes allow conversion from anisotropic theory to isotropic plasticity.
        epplas = eps - sig * (1.19_r8k) / e
    END IF
    !
    END SUBROUTINE strain
    !
END MODULE Deformation

