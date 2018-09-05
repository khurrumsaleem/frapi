MODULE cladding
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE variables_frapcon, ONLY : comp, FastFlux, rstran, nt, frcoef, ExcessH2Concen
    USE Material_Properties, ONLY : MatProp
    USE conversions_frapcon
    USE ZrModels
    IMPLICIT NONE
    !>@brief
    !> Cladding material properties
    !
    LOGICAL :: clad_used
    LOGICAL, PARAMETER :: clad_plast = .TRUE.
    LOGICAL, PARAMETER :: clad_creep = .TRUE.
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION clad_mat_par(temp,keyword)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Returns cladding material property
    !
    REAL(r8k) :: temp, cathex, cdthex, Gm, Em
    CHARACTER(LEN=8) :: keyword
    !
    clad_mat_par = 0.0_r8k
    !
    SELECT CASE (keyword)
    CASE ('EMOD')
        clad_mat_par = MatProp ('CLAD', 'YOUNG_MOD', temp) * PatoPSI
    CASE ('PENALTY')
        clad_mat_par = 1.0e10_r8k
    CASE ('PENALTY2')
        clad_mat_par = 1.0e6_r8k
    CASE ('PRATIO')
        Em = MatProp ('CLAD', 'YOUNG_MOD', temp) * PatoPSI
        Gm = MatProp ('CLAD', 'SHEAR_MOD', temp) * PatoPSI
        clad_mat_par = Em / (2.0_r8k * Gm) - 1.0_r8k
    CASE ('THDIL')
        IF (temp < 295.0_r8k) THEN
            cathex = 0.0_r8k
            cdthex = 0.0_r8k
        ELSE
            cathex = MatProp ('CLAD', 'THEXP_AXIAL', temp)
            cdthex = MatProp ('CLAD', 'THEXP_RADIAL', temp)
        END IF
        clad_mat_par = (cathex + cdthex) * 0.5_r8k
    CASE ('FRCOEF')
        clad_mat_par = frcoef
    END SELECT
    !
    END FUNCTION clad_mat_par
    !
    !
    !
    SUBROUTINE clad_radial_return(temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Radial return for power law yield Function
    !
    INTEGER(ipk) :: i, ia
    REAL(r8k), INTENT(IN) :: temp, mu, epseff0, taueff, dtime
    REAL(r8k), INTENT(INOUT) :: gamma, dplmod, deds
    REAL(r8k) :: emod, Kpl, mpl, npl, e0pl, taueffi, fy, dfy, srate, sepp, crate, &
      &          chrd, sigys, dgamma, cexh2, dax, daxn, ctemp
    LOGICAL :: plinc
    !
    IF (.NOT. clad_plast) RETURN
    ! Excess hydrogen
    ia = 1
    dax = ABS(elev(1) - cp(2))
    DO i = 2, nt
        daxn = ABS(elev(i) - cp(2))
        IF (daxn < dax) THEN
            ia = i
            dax = daxn
        END IF
    END DO
    cexh2 = ExcessH2Concen(ia)

    ! Material parameters
    emod = MatProp ('CLAD', 'YOUNG_MOD', temp) * PatoPSI
    CALL ckmn (temp, cexh2, Kpl, npl, mpl)
    e0pl  = 0.001_r8k
    Kpl = Kpl * PatoPSI

    ! Iterate plastic increment
    plinc = .FALSE.
    DO i = 1, 1000
        taueffi = taueff - 3.0_r8k * mu * gamma
        srate = gamma / dtime / e0pl + 1.0e-10_r8k
        crate = srate ** mpl
        sepp = epseff0 + taueffi/emod + gamma + 1.0e-10_r8k
        chrd = sepp ** npl
        sigys = Kpl * chrd * crate
        fy = taueffi - sigys
        IF (fy < 1.0e-8_r8k * sigys) EXIT
        plinc = .TRUE.
        dfy = -3.0_r8k * mu - npl * Kpl * sepp ** (npl - 1.0_r8k) * crate - &
              mpl * Kpl / (dtime * e0pl) * chrd * srate ** (mpl - 1.0_r8k)
        dgamma = -fy / dfy
        gamma = gamma + dgamma
    END DO

    IF (plinc) THEN
        deds = 0.0_r8k
        dplmod = -dfy
    END IF

    IF (i >= 1000) THEN
        !WRITE (ounit,FMT='(A)') 'ERROR in radial RETURN algorithm'
        lerror = .TRUE.
        !Call nlfemp_stop(0)
    END IF
    !
    END SUBROUTINE clad_radial_return
    !
    !
    !
    SUBROUTINE clad_creep_calc(temp,mu,dtime,epseff0,taueff,gamma,deds)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Cladding creep calculation
    !
    REAL(r8k) :: temp,mu,dtime,epseff0,taueff,gamma,deds, dax, daxn
    INTEGER(ipk) :: it, ia, i
    REAL(r8k) :: tempf,edot,taueffn,rs,drsds,dtaueff,fast_flux
    !
    IF (.NOT. clad_creep) RETURN
    ! Flux value
    ia = 1
    dax = ABS(elev(1) - cp(2))
    DO i = 2, nt
        daxn = ABS(elev(i) - cp(2))
        IF (daxn < dax) THEN
            ia = i
            dax = daxn
        END IF
    END DO
    fast_flux = FastFlux(ia)

    taueffn = MIN(taueff, 5000.0_r8k)

    IF (taueffn < 1.0e-6_r8k) RETURN

    DO it = 1, 1000
        ! Creep rate
        ! Convert temperature from K to F
        tempf = tkf(temp)
        ! Creep rate
        CALL crepr2(taueffn, edot, deds, 0.0_r8k, tempf, fast_flux, epseff0)
        deds = dtime * deds
        ! Effective creep strain increment
        gamma = dtime * edot
        rs = taueffn - taueff + 3.0_r8k * mu * gamma
        IF (ABS(rs) > 1e-6_r8k * taueffn) THEN
            drsds = 1.0_r8k + 3.0_r8k * mu * deds
            dtaueff = - rs / drsds
            IF (ABS(dtaueff) > 0.01_r8k * taueffn) dtaueff = SIGN(0.01_r8k * taueffn,dtaueff)
            taueffn = taueffn + dtaueff
        ELSE
            EXIT
        END IF
    END DO
    ! No convergence for the iteration
    IF (it >= 1000) THEN
        !WRITE (ounit,FMT='(A)') 'ERROR in radial RETURN algorithm (creep)'
        lerror = .TRUE.
        !Call nlfemp_stop(0)
    END IF
    !
    END SUBROUTINE clad_creep_calc
    !
    !
    !
    SUBROUTINE crepr2(sig, edot, deds, ProblemTime, CladAveTemp, phi, CreepStrain)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> crepr is called from cladf.
    !> crepr computes (transverse) cladding creep strain rate as a function of effective stress, temperature,
    !> fast neutron flux and accumulated total creep strain
    !>@author
    !> coded by m.p.bohn, dec.28,1975 based on the matpro function "ccrpr"
    !
    ! Input
    !
    ! CreepStrain - total accumulated creep strain (dimensionless)
    ! phi         - flux (neutrons/m**2/sec)
    ! ProblemTime - time to end of power step (hr)
    ! CladAveTemp - cladding average temperature (F)
    ! sig         - cladding hoop stress per node (psi)
    !
    ! Output
    !
    ! edot        - creep strain rate (1/hr)
    ! deds        - stress derivative of creep rate (1/(psi hr))
    !
    ! Internal
    !
    ! sag         - hoop stress in Pa
    !
    ! Reference:
    !
    ! The time and stress dependence taken from:
    ! (1) Inreactor tubular creep of zircaloy-2 at 260 to 3000 e.f. ibrhaim, j. nucl. mater, vol 46 (1973) pp 169-182
    !
    ! The flux dependence was based on:
    ! (2) Ross-ross and hunt, j. nucl. mater. vol 26 (1968) pp 2 - 17
    !
    ! The temperature dependence, depx(-q/rt), was based on a value of q = 10000 cal/mole near 573k from 
    ! (3) Fidleris, aecl-4766
    !
    REAL(r8k) :: sag, tcak, b, c, ak, r, e, a1, a2, a3
    REAL(r8k), INTENT(IN) :: sig, ProblemTime, CladAveTemp, phi, CreepStrain
    REAL(r8k), INTENT(OUT) :: edot, deds
    !
    sag = 1.1547_r8k * sig * PSItoPa
    tcak = tfk(CladAveTemp)
    b = 725.2_r8k
    c = 4.967e-08_r8k
    ak = 5.129e-29_r8k
    r = 1.987_r8k
    e = MAX(CreepStrain, 0.0001_r8k)
    a1 = ak * phi
    a2 = EXP(-10000.0_r8k / (r * tcak))
    a3 = sag + b * EXP(c * sag)
    edot = (2.0_r8k * (a1 * a2 * a3) ** 2) / e
    edot = edot * hrtosec * 1.1547_r8k
    deds = 4.0_r8k * ((a1 * a2) ** 2) * a3 * (1.0_r8k + b * c * EXP(c * sag)) / e * hrtosec * 1.1547_r8k * PSItoPa
    !
    END SUBROUTINE crepr2
    !
END MODULE cladding

