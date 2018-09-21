MODULE materials_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE cladding_frapcon
    USE m5_cladding_frapcon
    USE nuclear_fuel_frapcon
    IMPLICIT NONE
    !
    INTEGER(ipk) :: nmat ! Number of materials
    INTEGER(ipk), PARAMETER :: MAX_DATA = 1000 ! Maximum number of data pairs
    REAL(r8k) :: mat_data(2,MAX_DATA) ! Temporary array for material data

    TYPE mat_parameter
        CHARACTER(LEN=8) :: label
        INTEGER(ipk) :: ndata
        REAL(r8k), POINTER :: data(:,:)
        TYPE(mat_parameter), POINTER :: next
    END TYPE mat_parameter

    TYPE mat_type
        INTEGER(ipk) :: label ! Label for material data set
        INTEGER(ipk) :: number ! Internal number for material
        LOGICAL :: plastic ! Plastic calculation
        LOGICAL :: contact ! Contact material set
        LOGICAL :: creep ! Creep calulation
        LOGICAL :: volstr ! Volumetric strain is given in the input
        REAL(r8k) :: maxepseff ! Maximum effective plastic strain
        REAL(r8k) :: maxsigeff ! Maximum effective stress
        REAL(r8k) :: maxplSED ! Maximum plastic SED
        REAL(r8k) :: maxsigma(6) ! Maximum stress components
        REAL(r8k) :: minsigma(6) ! Minimum stress components
        TYPE(mat_parameter), POINTER :: first_par
        TYPE(mat_type), POINTER :: next
    END TYPE mat_type

    TYPE(mat_type), POINTER :: &
        first_mat,last_mat ! Pointers to the first and last materials
    !
    CONTAINS
    !
    SUBROUTINE mat_init()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pointers
    
    nmat = 0
    first_mat => NULL()
    last_mat => NULL()

    ! Initialize library materials
    clad_used = .FALSE.
    m5_clad_used = .FALSE.
    fuel_used = .FALSE.

    END SUBROUTINE mat_init
    !
    !
    !
    SUBROUTINE mat_create(mat)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create material database if label does not exist
    !
    INTEGER(ipk), INTENT(IN) :: mat
    TYPE(mat_type), POINTER :: matdb

    ! Try to find material number in existing materials database
    matdb => first_mat
    DO WHILE (ASSOCIATED(matdb))
        IF (matdb%label == mat) THEN
            EXIT
        ELSE
            matdb => matdb%next
        END IF
    END DO

    ! Create new material entry if label does not exist
    IF (.NOT. ASSOCIATED(matdb)) THEN
        nmat = nmat + 1
        ALLOCATE(matdb)
        matdb%label = mat
        matdb%number = nmat
        matdb%maxepseff = 0.0_r8k
        matdb%maxsigeff = 0.0_r8k
        matdb%maxplSED = 0.0_r8k
        matdb%maxsigma = 0.0_r8k
        matdb%minsigma = 0.0_r8k
        matdb%plastic = .FALSE.
        matdb%contact = .FALSE.
        matdb%creep = .FALSE.
        matdb%volstr = .FALSE.
        matdb%first_par => NULL()
        matdb%next => NULL()
        IF (.NOT. ASSOCIATED(last_mat)) THEN
            first_mat => matdb
            last_mat => matdb
        ELSE
            last_mat%next => matdb
            last_mat => matdb
        END IF
    END IF

    END SUBROUTINE mat_create
    !
    !
    !
    SUBROUTINE mat_create_par (mat, label, ndata)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !
    INTEGER(ipk), INTENT(IN) :: mat, ndata
    CHARACTER(LEN=8), INTENT(IN) :: label
    INTEGER(ipk) :: i
    TYPE(mat_type), POINTER :: matdb
    TYPE(mat_parameter), POINTER :: current_par

    ! Try to find material number in existing materials database
    matdb => first_mat
    DO WHILE (ASSOCIATED(matdb))
        IF (matdb%label == mat) THEN
            EXIT
        ELSE
            matdb => matdb%next
        END IF
    END DO

    ! Create new material entry if label does not exist
    IF (.NOT. ASSOCIATED(matdb)) THEN
        nmat = nmat + 1
        ALLOCATE(matdb)
        matdb%label = mat
        matdb%number = nmat
        matdb%maxepseff = 0.0_r8k
        matdb%maxsigeff = 0.0_r8k
        matdb%maxplSED = 0.0_r8k
        matdb%maxsigma = 0.0_r8k
        matdb%minsigma = 0.0_r8k
        matdb%plastic = .FALSE.
        matdb%contact = .FALSE.
        matdb%creep = .FALSE.
        matdb%volstr = .FALSE.
        matdb%first_par => NULL()
        matdb%next => NULL()
        IF (.NOT. ASSOCIATED(last_mat)) THEN
            first_mat => matdb
            last_mat => matdb
        ELSE
            last_mat%next => matdb
            last_mat => matdb
        END IF
    END IF

    IF (label(1:4) == 'NULL') RETURN

    ! Plastic, creep, and contact calculation flags
    IF (label(1:5) == 'SIGYS') matdb%plastic = .TRUE.
    IF (label(1:6) == 'CREEP1') matdb%creep = .TRUE.
    IF (label(1:7) == 'PENALTY') matdb%contact = .TRUE.
    IF (label(1:6) == 'FRCOEF') matdb%contact = .TRUE.

    ! Replace existing PARAMETER
    current_par => matdb%first_par
    DO WHILE (ASSOCIATED(current_par))
        IF (current_par%label == label) THEN
            DEALLOCATE(current_par%data)
            EXIT
        END IF
        current_par => current_par%next
    END DO

    ! Create new PARAMETER
    IF (.NOT. ASSOCIATED(current_par)) THEN
        ALLOCATE(current_par)
        current_par%label = label
        current_par%next => matdb%first_par
        matdb%first_par => current_par
    END IF

    ! ALLOCATE memory for material data
    current_par%ndata = ndata
    ALLOCATE(current_par%data(2,ndata))

    ! Save material data to the database
    DO i = 1,ndata
        current_par%data(1:2,i) = mat_data(1:2,i)
    END DO

    END SUBROUTINE mat_create_par
    !
    !
    !
    REAL(r8k) FUNCTION mat_par (number, temp, keyword)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Linear interpolation for the material parameter given as a function of temperature
    !
    INTEGER(ipk) :: number, i
    REAL(r8k) :: temp, alpha
    CHARACTER(LEN=8) :: keyword
    TYPE(mat_type), POINTER :: current
    TYPE(mat_parameter), POINTER :: current_par
    REAL(r8k), POINTER :: mdata(:,:)

    ! Use library properties for material numbers greater than 999_frapcon
    SELECT CASE (number)
    CASE (0)
        mat_par = 0.0_r8k
        RETURN
    CASE (1001)
        ! UO2 fuel
        IF (.NOT. clad_used) THEN
            CALL mat_create_par(1001,'NULL    ',0)
            clad_used = .TRUE.
        END IF

        ! UO2 fuel properties
        mat_par = fuel_mat_par (temp, keyword)
        RETURN
    CASE (1002)
        ! Zircaloy cladding 
        IF (.NOT. clad_used) THEN
            CALL mat_create_par(1002,'NULL    ',0)
            clad_used = .TRUE.
        END IF

        ! Cladding properties
        mat_par = clad_mat_par (temp, keyword)
        RETURN
    CASE (1003)
        ! M5 cladding 
        IF (.NOT. m5_clad_used) THEN
            CALL mat_create_par(1003,'NULL    ',0)
            m5_clad_used = .TRUE.
        END IF

        ! M5 Cladding properties
        mat_par = m5_clad_mat_par (temp, keyword)
        RETURN
    END SELECT

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        IF (current%label == number) EXIT
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') &
            'ERROR: can not find material label ',number
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    current_par => current%first_par
    DO WHILE (ASSOCIATED(current_par))
        IF (current_par%label == keyword) EXIT
        current_par => current_par%next
    END DO

    ! Return zero value if material parameter is not defined
    IF (.NOT. ASSOCIATED(current_par)) THEN
        IF (keyword(1:4) == 'EMOD') THEN
            WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR material PARAMETER EMOD needed for material ', number
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        ELSE IF (keyword(1:7) == 'PENALTY') THEN
            WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR material PARAMETER PENALTY needed for material ', number
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        ELSE
            mat_par = 0.0_r8k
            RETURN
        END IF
    END IF

    ! Make a linear interpolation
    mdata => current_par%data(1:2,1:current_par%ndata)
    mat_par = mdata(2,1)
    IF (mdata(1,1) > temp) RETURN
    DO i = 2, current_par%ndata
        IF (mdata(1,i) > temp) THEN
            alpha = (temp - mdata(1,i-1)) / (mdata(1,i) - mdata(1,i-1))
            mat_par = mdata(2,i-1) + alpha * (mdata(2,i) - mdata(2,i-1))
            RETURN
        END IF
        mat_par = mdata(2,i)
    END DO

    END FUNCTION mat_par
    !
    !
    !
    FUNCTION mat_plastic (mat)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Return true if material has plasticity properties
    !
    INTEGER(ipk) :: mat
    LOGICAL :: mat_plastic
    TYPE(mat_type), POINTER :: current

    ! Use library properties for material numbers greater than 999_frapcon
    SELECT CASE (mat)
    CASE (0)
        mat_plastic = .FALSE.
        RETURN
    CASE (1001)
        ! UO2 fuel properties
        mat_plastic = fuel_plast
        RETURN
    CASE (1002)
        ! Cladding properties
        mat_plastic = clad_plast
        RETURN
    CASE (1003)
        ! M5 Cladding properties
        mat_plastic = m5_clad_plast
        RETURN
    END SELECT

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        IF (current%label == mat) EXIT
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR: can not find material label ', mat
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    mat_plastic = current%plastic

    END FUNCTION mat_plastic
    !
    !
    !
    FUNCTION mat_volstr (mat)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Return true if material has prescribed volumetric strains
    !
    INTEGER(ipk) :: mat
    LOGICAL :: mat_volstr
    TYPE(mat_type), POINTER :: current

    IF (mat == 0) THEN
        mat_volstr = .FALSE.
        RETURN
    END IF

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        IF (current%label == mat) EXIT
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR: can not find material label ', mat
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    mat_volstr = current%volstr

    END FUNCTION mat_volstr
    !
    !
    !
    FUNCTION mat_creep (mat)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Return true if material has creep properties
    !
    INTEGER(ipk) :: mat
    LOGICAL :: mat_creep
    TYPE(mat_type), POINTER :: current

    ! Use library properties for material numbers greater than 999_frapcon
    SELECT CASE (mat)
    CASE (0)
        mat_creep = .FALSE.
        RETURN
    CASE (1001)
        ! UO2 fuel properties
        mat_creep = fuel_creep
        RETURN
    CASE (1002)
        ! Cladding properties
        mat_creep = clad_creep
        RETURN
    CASE (1003)
        ! M5 Cladding properties
        mat_creep = m5_clad_creep
        RETURN
    END SELECT

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        IF (current%label == mat) EXIT
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR: can not find material label ', mat
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    mat_creep = current%creep

    END FUNCTION mat_creep
    !
    !
    !
    SUBROUTINE mat_radial_return (mat, temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Radial return
    !
    INTEGER(ipk), INTENT(IN) :: mat
    REAL(r8k),  INTENT(IN) :: temp, mu, epseff0, dtime, taueff
    REAL(r8k),  INTENT(INOUT) :: gamma
    REAL(r8k),  INTENT(OUT) :: dplmod, deds
    INTEGER(ipk) :: i
    REAL(r8k) :: sigys, siginf, delta, plmod, fy, dfy, dgamma, tmp, epseff
    LOGICAL :: plinc

    ! Use library properties for material numbers greater than 999_frapcon
    SELECT CASE (mat)
    CASE (1001)
        ! UO2 fuel properties
        CALL fuel_radial_return(temp, mu, dtime, epseff0, taueff, gamma, dplmod)
        RETURN
    CASE (1002)
        ! Cladding properties
        CALL clad_radial_return(temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
        RETURN
    CASE (1003)
        ! Cladding properties
        CALL m5_clad_radial_return(temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
        RETURN
    END SELECT

    ! Use default isotropic plasticity model_frapcon

    ! No plastic calculation
    IF (.NOT. mat_plastic(mat)) RETURN

    ! Yield Function parameters
    sigys  = mat_par(mat,temp,'SIGYS   ')
    siginf = mat_par(mat,temp,'SIGINF  ')
    delta  = mat_par(mat,temp,'DELTA   ')
    plmod  = mat_par(mat,temp,'PLMOD   ')

    ! Evaluate yield Function
    plinc = .FALSE.
    DO i = 1, 1000
        epseff = epseff0 + gamma
        tmp = EXP(-delta*epseff)
        fy = taueff - (sigys + plmod * epseff + (siginf - sigys) * (1.0_r8k - tmp)) - 3.0_r8k * mu * gamma
        IF (fy < 1.0e-8_r8k * sigys) EXIT
        plinc = .TRUE.
        dfy = -plmod - (siginf - sigys) * tmp * delta - 3.0_r8k * mu
        dgamma = -fy / dfy
        gamma = gamma + dgamma
    END DO

    IF (plinc) THEN
        dplmod = -dfy
        deds = 0.0_r8k
    END IF

    IF (i >= 1000) lerror = .TRUE.

    END SUBROUTINE mat_radial_return
    !
    !
    !
    SUBROUTINE mat_creep_calc(mat, temp, mu, dtime, epseff0, taueff, gamma, deds)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Return creep strain increment using Nortons power law
    !
    ! de/dt = C1 x taueff**C2 x epseff**C3 x EXP(-C4/(temp + C5))
    !
    !
    INTEGER(ipk) :: mat
    REAL(r8k), INTENT(IN) :: mu, temp, dtime, epseff0, taueff
    REAL(r8k), INTENT(INOUT) :: gamma, deds
    REAL(r8k) :: C1, C2, C3, C4, C5
    INTEGER(ipk) :: i
    REAL(r8k) :: edot, taueffn, dtaueff, rs, drsds, epseff

    deds = 0.0_r8k

    ! Use library properties for material numbers greater than 999_frapcon
    SELECT CASE (mat)
    CASE (1001)
        ! UO2 fuel properties
        ! No creep properies implemented (yet)
        RETURN
    CASE (1002)
        ! Cladding properties
        CALL clad_creep_calc(temp,mu,dtime,epseff0,taueff,gamma,deds)
        RETURN
    CASE (1003)
        ! Cladding properties
        CALL m5_clad_creep_calc(temp,mu,dtime,epseff0,taueff,gamma,deds)
        RETURN
    END SELECT

    ! No plastic calculation
    IF (.NOT. mat_creep(mat)) RETURN

    ! Creep rate equation parameters
    C1 = mat_par(mat,temp,'CREEP1  ')
    C2 = mat_par(mat,temp,'CREEP2  ')
    C3 = mat_par(mat,temp,'CREEP3  ')
    C4 = mat_par(mat,temp,'CREEP4  ')
    C5 = mat_par(mat,temp,'CREEP5  ')

    taueffn = taueff

    IF (taueffn < 1.0e-10_r8k) RETURN

    DO i = 1, 1000
        epseff = MAX(epseff0 + gamma, 1.0e-25_r8k)
        edot = C1 * (taueffn ** C2) * (epseff ** C3) * EXP(-C4 / (temp + C5))
        deds = dtime * C1 * C2 * taueffn ** (C2 - 1.0_r8k) * epseff ** C3 * EXP(-C4 / (temp + C5))
        gamma = dtime * edot
        rs = taueffn - taueff +  3.0_r8k * mu * gamma
        IF (ABS(rs) > 1.0e-6_r8k * taueffn) THEN
            drsds = 1.0_r8k + 3.0_r8k * mu * deds
            dtaueff = - rs / drsds
            IF (ABS(dtaueff) > 0.01_r8k * taueffn) dtaueff = SIGN(0.01_r8k * taueffn, dtaueff)
            taueffn = taueffn + dtaueff
        ELSE
            EXIT
        END IF
    END DO

    ! No convergence for the iteration
    IF (i >= 1000) lerror = .TRUE.

    END SUBROUTINE mat_creep_calc
    !
    !
    !
    SUBROUTINE mat_maximums (mat, epseff, sigeff, plSED, sigma)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update maximum parameters for the material
    !
    INTEGER(ipk), INTENT(IN) :: mat
    REAL(r8k), INTENT(IN) :: epseff(:),sigeff(:),plSED(:),sigma(:,:)
    INTEGER(ipk) :: id,snd,ig,ng
    TYPE(mat_type), POINTER :: current

    snd = SIZE(sigma,1)
    ng = SIZE(sigma,2)

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        IF (current%label == mat) THEN
            DO ig = 1,ng
                current%maxepseff = MAX(current%maxepseff,epseff(ig))
                current%maxsigeff = MAX(current%maxsigeff,sigeff(ig))
                current%maxplSED = MAX(current%maxplSED,plSED(ig))
                DO id = 1, snd
                    current%maxsigma(id) = MAX(current%maxsigma(id),sigma(id,ig))
                    current%minsigma(id) = MIN(current%minsigma(id),sigma(id,ig))
                END DO
            END DO
            EXIT
        END IF
        current => current%next
    END DO

    END SUBROUTINE mat_maximums
    !
    !
    !
    SUBROUTINE mat_write_output (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write material data to the output file
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(mat_type), POINTER :: current
    TYPE(mat_parameter), POINTER :: current_par

    WRITE (UNIT=nunit) nmat

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%maxepseff
        WRITE (UNIT=nunit) current%maxsigeff
        WRITE (UNIT=nunit) current%maxplSED

        current_par => current%first_par
        DO WHILE (ASSOCIATED(current_par))
            WRITE (UNIT=nunit) current_par%label
            WRITE (UNIT=nunit) current_par%ndata
            WRITE (UNIT=nunit) current_par%data(1:2,1:current_par%ndata)
            current_par => current_par%next
        END DO

        WRITE (UNIT=nunit) 'End     '
        current => current%next
    END DO

    END SUBROUTINE mat_write_output
    !
    !
    !
    SUBROUTINE mat_read_output (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write material data to the output file
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i,number,mat,ndata
    CHARACTER(LEN=8) :: keyword
    TYPE(mat_type), POINTER :: current,previous
    TYPE(mat_parameter), POINTER :: current_par,previous_par,last_par

    READ(UNIT=nunit,ERR=20,END=20) number
    current => first_mat
    nmat = 0
    DO i = 1,number
        READ(UNIT=nunit,ERR=20,END=20) mat
        IF (ASSOCIATED(current)) THEN
            current%label = mat
            current%number = i
            last_mat => current
        ELSE
            ALLOCATE(current)
            current%label = mat
            current%number = i
            current%first_par => NULL()
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_mat)) THEN
                first_mat => current
                last_mat => current
            ELSE
                last_mat%next => current
                last_mat => current
            END IF
        END IF
        READ(UNIT=nunit) current%maxepseff
        READ(UNIT=nunit) current%maxsigeff
        READ(UNIT=nunit) current%maxplSED

        last_par => NULL()
        current_par => current%first_par
        DO
            READ(UNIT=nunit,ERR=20,END=20) keyword
            IF (keyword(1:3) == 'End') EXIT
            IF (keyword(1:5) == 'SIGYS') current%plastic = .TRUE.
            IF (keyword(1:6) == 'CREEP1') current%creep = .TRUE.
            READ(UNIT=nunit,ERR=20,END=20) ndata
            READ(UNIT=nunit,ERR=20,END=20) mat_data(1:2,1:ndata)
            IF (ASSOCIATED(current_par)) THEN
                ! Overwrite existing material PARAMETER
                current_par%label = keyword
                IF (current_par%ndata /= ndata) THEN
                    current_par%ndata = ndata
                    DEALLOCATE(current_par%data)
                    ALLOCATE(current_par%data(2,ndata))
                END IF
            ELSE
                ! Create new material PARAMETER
                ALLOCATE(current_par)
                current_par%label = keyword
                current_par%ndata = ndata
                current_par%next => NULL()
                ALLOCATE(current_par%data(2,ndata))
                IF (.NOT. ASSOCIATED(last_par)) THEN
                    current%first_par => current_par
                ELSE
                    last_par%next => current_par
                END IF
            END IF
            current_par%data(1:2,1:ndata) = mat_data(1:2,1:ndata)
            last_par => current_par
            current_par => current_par%next
        END DO

        ! Deallocate all surplus material parameters
        DO WHILE (ASSOCIATED(current_par))
            previous_par => current_par
            current_par => current_par%next
            DEALLOCATE(previous_par%data)
            DEALLOCATE(previous_par)
        END DO
        IF (ASSOCIATED(last_par)) last_par%next => NULL()
       
        current => current%next
        nmat = i
    END DO

    ! Deallocate all surplus material sets
    IF (number == 0) THEN
        first_mat => NULL()
        last_mat => NULL()
    END IF
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        ! deallocate temperature dependent material properties
        current_par => previous%first_par
        DO WHILE (ASSOCIATED(current_par))
            previous_par => current_par
            current_par => current_par%next
            DEALLOCATE(previous_par%data)
            DEALLOCATE(previous_par)
        END DO
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_mat)) last_mat%next => NULL()

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading materials database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE mat_read_output
    !
    !
    !
    SUBROUTINE mat_deallocate()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate materials database
    !
    TYPE(mat_type), POINTER :: current,previous
    TYPE(mat_parameter), POINTER :: current_par,previous_par

    current => first_mat
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        ! deallocate temperature dependent material properties
        current_par => previous%first_par
        DO WHILE (ASSOCIATED(current_par))
            previous_par => current_par
            current_par => current_par%next
            DEALLOCATE(previous_par%data)
            DEALLOCATE(previous_par)
        END DO
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE mat_deallocate
    !
END MODULE materials_frapcon



