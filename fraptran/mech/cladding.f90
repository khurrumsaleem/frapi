MODULE cladding
    USE Kinds
    USE common_parameters
    USE fraptran_variables
    USE Kinds
    USE ZrModels, ONLY : ckmn
    IMPLICIT NONE
    !>@brief
    !> Cladding material properties for FEA Model

    LOGICAL :: clad_used
    LOGICAL, PARAMETER :: clad_plast = .TRUE.
    LOGICAL, PARAMETER :: clad_creep = .TRUE.

    CONTAINS
    
    FUNCTION clad_mat_par (temp, keyword)
    USE Kinds
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> Returns cladding material property
    REAL(r8k) :: temp, clad_mat_par
    CHARACTER(LEN=8) :: keyword
    REAL(r8k) :: cathex, cdthex, Gm, Em
    REAL(r8k) :: dax, daxn
    INTEGER(ipk) :: ia, i

    clad_used = .TRUE.

    clad_mat_par = 0.0_r8k

    ia = 1
    dax = ABS( elev(1) - cp(2) )
    DO i = 2, nax
        daxn = ABS( elev(i) - cp(2) )
        IF (daxn < dax) THEN
            ia = i
            dax = daxn
        ENDIF
    ENDDO

    SELECT CASE ( keyword )
    CASE ( 'EMOD' )
        clad_mat_par = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=Temp, ColdWork=clad_EffColdWkStrenCoef(ia),&
          &                         Fluence=clad_EffFastFluStrenCoef(ia), OxygenConcen=clad_OxygenConcenAve(ia)) / 6894.757_r8k
    CASE ( 'PENALTY' )
        clad_mat_par = 1.0e10_r8k
    CASE ( 'PENALTY2' )
        clad_mat_par = 1.0e6_r8k
    CASE ( 'PRATIO' )
        Em = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=Temp, ColdWork=clad_EffColdWkStrenCoef(ia), &
          &               Fluence=clad_EffFastFluStrenCoef(ia), OxygenConcen=clad_OxygenConcenAve(ia)) / 6894.757_r8k
        Gm = MatProperty (Material='CLAD', Property='SHEAR_MOD', Temperature=Temp, ColdWork=clad_EffColdWkStrenCoef(ia), &
          &               Fluence=clad_EffFastFluStrenCoef(ia), OxygenConcen=clad_OxygenConcenAve(ia)) / 6894.757_r8k
        clad_mat_par = Em/(2.0_r8k*Gm) - 1.0_r8k
    CASE ( 'THDIL' )
        IF ( temp < 295.0_r8k ) THEN
            cathex = 0.0_r8k
            cdthex = 0.0_r8k
        ELSE
            cathex = MatProperty (Material='CLAD', Property='THEXP_AXIAL', Temperature=temp)
            cdthex = MatProperty (Material='CLAD', Property='THEXP_RADIAL', Temperature=temp)
        ENDIF
        clad_mat_par = (cathex + cdthex) * 0.5_r8k
    CASE ( 'FRCOEF' )
        clad_mat_par = clad_frcoef
    END SELECT

    END FUNCTION clad_mat_par
    !
    !
    !
    SUBROUTINE clad_radial_Return (temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
    USE Kinds
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> Radial return for power law yield function
    REAL(r8k), INTENT(IN) :: temp, mu, epseff0, taueff, dtime
    REAL(r8k), INTENT(INOUT) :: gamma, dplmod, deds
    REAL(r8k) :: emod, Kpl, mpl, npl, e0pl, taueffi, fy, dfy, srate, sepp, crate, &
      &          chrd, sigys, dgamma, cexh2, dax, daxn
    INTEGER(ipk) :: i, ia
    LOGICAL :: plinc

    IF ( .NOT.clad_plast ) RETURN
    IF ( taueff < 1.0_r8k ) RETURN

    ! Axial node number
    ia = 1
    dax = ABS( elev(1) - cp(2) )
    DO i = 2, nax
        daxn = ABS( elev(i) - cp(2) )
        IF ( daxn < dax ) THEN
            ia = i
            dax = daxn
        ENDIF
    ENDDO

    ! Material parameters
    ! Elastic Modulus
    emod = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=Temp, ColdWork=clad_EffColdWkStrenCoef(ia), &
      &                 Fluence=clad_EffFastFluStrenCoef(ia), OxygenConcen=clad_OxygenConcenAve(ia)) / 6894.757_r8k
    srate = 0.0_r8k
    CALL ckmn (temp,clad_OxygenConcenAve(ia),clad_EffFastFluStrenCoef(ia), &
      &  clad_EffFastFluStrnHardExp(ia),clad_EffColdWkStrenCoef(ia), &
      &  clad_EffColdWkStrnHardExp(ia),srate,Kpl,npl,mpl)
    e0pl  = 0.001_r8k
    Kpl = Kpl / 6.894757e3_r8k
    dplmod = 0.0_r8k
    deds = 0.0_r8k

    ! Iterate plastic increment
    plinc = .FALSE.
    DO i = 1, 1000
        taueffi = taueff - 3.0_r8k * mu * gamma
        srate = gamma / dtime / e0pl + 1.0e-10_r8k
        crate = srate ** mpl
        sepp = epseff0 + taueffi / emod + gamma + 1.0e-10_r8k
        chrd = sepp ** npl
        sigys = Kpl * chrd * crate
        fy = taueffi - sigys
        IF ( fy < 1.0e-8_r8k * sigys ) EXIT
        plinc = .TRUE.
        dfy = -3.0_r8k * mu - npl * Kpl * sepp ** (npl - 1.0_r8k) * crate - &
          &   mpl * Kpl / (dtime * e0pl) * chrd * srate ** (mpl - 1.0_r8k)
        dgamma = -fy / dfy
        gamma = gamma + dgamma
    ENDDO

    IF ( plinc ) THEN
        deds = 0.0_r8k
        dplmod = -dfy
    ENDIF

    IF ( i >= 1000 )lerror = .TRUE.

    END SUBROUTINE clad_radial_Return
    !
    !
    !
    SUBROUTINE clad_creep_calc (temp, mu, dtime, epseff0, taueff, gamma, deds)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Cladding creep calculation
    REAL(r8k) :: temp, mu, dtime, epseff0, taueff, gamma, deds
    INTEGER(ipk) :: it
    REAL(r8k) :: tempf, edot, taueffn, rs, drsds, dtaueff

    RETURN

    IF ( .NOT.clad_creep ) RETURN

    IF ( taueffn < 1.0e-6_r8k ) RETURN

    taueffn = MIN(taueff, 5000.0_r8k)

    DO it = 1, 1000
        ! Creep rate
        ! Temperature in F
        tempf = 1.8_r8k * temp - 459.67_r8k
        ! Creep rate
        !Call ??????
        edot = 0.0_r8k
        deds = dtime*deds
        ! Effective creep strain increment
        gamma = dtime*edot
        rs = taueffn - taueff + 3.0_r8k * mu * gamma
        IF ( ABS(rs) > 1.0e-6_r8k * taueffn ) THEN
        drsds = 1.0_r8k + 3.0_r8k * mu * deds
        dtaueff = - rs/drsds
        IF (ABS(dtaueff) > 0.01_r8k * taueffn) dtaueff = SIGN(0.01_r8k * taueffn, dtaueff)
        taueffn = taueffn + dtaueff
        ELSE
        EXIT
        ENDIF
    ENDDO

    ! No convergence for the iteration
    IF ( it >= 1000 ) lerror = .TRUE.

    END SUBROUTINE clad_creep_calc

END MODULE cladding


