MODULE materials_frap
    USE Kinds
    USE common_parameters
    USE cladding
    USE m5_cladding
    USE nuclear_fuel
    IMPLICIT NONE
    !>@brief
    !> Material parameters

  INTEGER(ipk) :: nmat ! Number of materials
  INTEGER(ipk), PARAMETER :: MAX_Data = 1000 ! Maximum number of Data pairs
  REAL(r8k) :: mat_Data(2,MAX_Data) ! Temporary array for material Data


  TYPE mat_parameter
     CHARACTER(LEN=8) :: label
     INTEGER(ipk) :: nData
     REAL(r8k), DIMENSION(:,:), POINTER :: Data
     TYPE(mat_parameter), POINTER :: next
  END TYPE mat_parameter


  TYPE mat_type
     INTEGER(ipk) :: label ! Label for material Data set
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
       first_mat,last_mat ! POINTERs to the first and last materials


CONTAINS
    SUBROUTINE mat_init()
    ! Initialize POINTERs
    IMPLICIT NONE

    nmat = 0
    first_mat => NULL()
    last_mat => NULL()

    ! Initialize library materials
    clad_used = .FALSE.
    m5_clad_used = .FALSE.
    fuel_used = .FALSE.

    RETURN

  END SUBROUTINE mat_init


    SUBROUTINE mat_create(mat)
    ! Create material Database If label does not exist
    INTEGER(ipk), INTENT(IN) :: mat
    TYPE(mat_type), POINTER :: matdb

    ! Try to find material number in existing materials Database
    matdb => first_mat
    DO WHILE ( ASSOCIATED(matdb) )
       IF ( matdb%label == mat ) THEN
          EXIT
       ELSE
          matdb => matdb%next
       ENDIF
    ENDDO

    ! Create new material entry If label does not exist
    IF ( .NOT.ASSOCIATED(matdb) ) THEN
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
       IF ( .NOT.ASSOCIATED(last_mat) ) THEN
          first_mat => matdb
          last_mat => matdb
       ELSE
          last_mat%next => matdb
          last_mat => matdb
       ENDIF
    ENDIF

    RETURN

  END SUBROUTINE mat_create


    SUBROUTINE mat_create_par(mat,label,nData)
    INTEGER(ipk), INTENT(IN) :: mat,nData
    CHARACTER(LEN=8), INTENT(IN) :: label
    INTEGER(ipk) :: i
    TYPE(mat_type), POINTER :: matdb
    TYPE(mat_parameter), POINTER :: current_par

    ! Try to find material number in existing materials Database
    matdb => first_mat
    DO WHILE ( ASSOCIATED(matdb) )
       IF ( matdb%label == mat ) THEN
          EXIT
       ELSE
          matdb => matdb%next
       ENDIF
    ENDDO

    ! Create new material entry If label does not exist
    IF ( .NOT.ASSOCIATED(matdb) ) THEN
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
       IF ( .NOT.ASSOCIATED(last_mat) ) THEN
          first_mat => matdb
          last_mat => matdb
       ELSE
          last_mat%next => matdb
          last_mat => matdb
       ENDIF
    ENDIF

    IF ( label(1:4) == 'NULL' ) RETURN

    ! Plastic, creep, and contact calculation flags
    IF ( label(1:5) == 'SIGYS' ) matdb%plastic = .TRUE.
    IF ( label(1:6) == 'CREEP1' ) matdb%creep = .TRUE.
    IF ( label(1:7) == 'PENALTY' ) matdb%contact = .TRUE.
    IF ( label(1:6) == 'FRCOEF' ) matdb%contact = .TRUE.

    ! Replace existing parameter
    current_par => matdb%first_par
    DO WHILE ( ASSOCIATED(current_par) )
       IF ( current_par%label == label ) THEN
          DEALLOCATE(current_par%Data)
          EXIT
       ENDIF
       current_par => current_par%next
    ENDDO

    ! Create new parameter
    IF ( .NOT.ASSOCIATED(current_par) ) THEN
       ALLOCATE(current_par)
       current_par%label = label
       current_par%next => matdb%first_par
       matdb%first_par => current_par
    ENDIF

    ! Allocate memory for material Data
    current_par%nData = nData
    ALLOCATE(current_par%Data(2,nData))

    ! Save material Data to the Database
    DO i=1,nData
       current_par%Data(1:2,i) = mat_Data(1:2,i)
    ENDDO

    RETURN

  END SUBROUTINE mat_create_par


  FUNCTION mat_par(number,temp,keyword)
    ! Linear interpolation for the material parameter given as a function of
    ! temperature
    IMPLICIT NONE
    INTEGER(ipk) :: number,i
    REAL(r8k) :: temp,mat_par,alpha
    CHARACTER(LEN=8) :: keyword
    TYPE(mat_type), POINTER :: current
    TYPE(mat_parameter), POINTER :: current_par
    REAL(r8k), DIMENSION(:,:), POINTER :: mData

    ! Use library properties for material numbers greater than 999
    SELECT CASE ( number )
    CASE ( 0 )
       mat_par = 0.0_r8k
       RETURN
    CASE ( 1001 )
       ! UO2 fuel
       IF ( .NOT.clad_used ) THEN
          CALL mat_create_par(1001,'NULL    ',0)
          clad_used = .TRUE.
       ENDIF

       ! UO2 fuel properties
       mat_par = fuel_mat_par(temp,keyword)
       RETURN
    CASE ( 1002 )
       ! Zircaloy cladding 
       IF ( .NOT.clad_used ) THEN
          CALL mat_create_par(1002,'NULL    ',0)
          clad_used = .TRUE.
       ENDIF

       ! Cladding properties
       mat_par = clad_mat_par(temp,keyword)
       RETURN
    CASE ( 1003 )
       ! M5 cladding 
       IF ( .NOT.m5_clad_used ) THEN
          CALL mat_create_par(1003,'NULL    ',0)
          m5_clad_used = .TRUE.
       ENDIF

       ! M5 Cladding properties
       mat_par = m5_clad_mat_par(temp,keyword)
       RETURN
    END SELECT

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == number ) EXIT
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR: can not find material label ',number
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    current_par => current%first_par
    DO WHILE ( ASSOCIATED(current_par) )
       IF ( current_par%label == keyword ) EXIT
       current_par => current_par%next
    ENDDO

    ! RETURN zero value If material parameter is not defined
    IF ( .NOT.ASSOCIATED(current_par) ) THEN
       IF ( keyword(1:4) == 'EMOD' ) THEN
          WRITE(UNIT=6,FMT='(/,A,I0,/)') &
               'ERROR material parameter EMOD needed for material ',number
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ELSE If ( keyword(1:7) == 'PENALTY' ) THEN
          WRITE(UNIT=6,FMT='(/,A,I0,/)') &
               'ERROR material parameter PENALTY needed for material ',number
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ELSE
          mat_par = 0.0_r8k
          RETURN
       ENDIF
    ENDIF

    ! Make a linear interpolation
    mData => current_par%Data(1:2,1:current_par%nData)
    mat_par = mData(2,1)
    IF ( mData(1,1) > temp ) RETURN
    DO i=2,current_par%nData
       IF ( mData(1,i) > temp ) THEN
          alpha=(temp - mData(1,i-1))/(mData(1,i) - mData(1,i-1))
          mat_par = mData(2,i-1) + alpha*(mData(2,i) - mData(2,i-1))
          RETURN
       ENDIF
       mat_par = mData(2,i)
    ENDDO

    RETURN

  END FUNCTION mat_par


  FUNCTION mat_plastic(mat)
    ! RETURN True If material has plasticity properties
    IMPLICIT NONE
    INTEGER(ipk) :: mat
    LOGICAL :: mat_plastic
    TYPE(mat_type), POINTER :: current

    ! Use library properties for material numbers greater than 999
    SELECT CASE ( mat )
    CASE ( 0 )
       mat_plastic = .FALSE.
       RETURN
    CASE ( 1001 )
       ! UO2 fuel properties
       mat_plastic = fuel_plast
       RETURN
    CASE ( 1002 )
       ! Cladding properties
       mat_plastic = clad_plast
       RETURN
    CASE ( 1003 )
       ! M5 Cladding properties
       mat_plastic = m5_clad_plast
       RETURN
    END SELECT

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == mat ) EXIT
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR: can not find material label ',mat
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    mat_plastic = current%plastic

    RETURN

  END FUNCTION mat_plastic


  FUNCTION mat_volstr(mat)
    ! RETURN True If material has prescribed volumetric strains
    IMPLICIT NONE
    INTEGER(ipk) :: mat
    LOGICAL :: mat_volstr
    TYPE(mat_type), POINTER :: current

    IF ( mat == 0 ) THEN
       mat_volstr = .FALSE.
       RETURN
    ENDIF

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == mat ) EXIT
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR: can not find material label ',mat
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    mat_volstr = current%volstr

    RETURN

  END FUNCTION mat_volstr


  FUNCTION mat_creep(mat)
    ! RETURN True If material has creep properties
    IMPLICIT NONE
    INTEGER(ipk) :: mat
    LOGICAL :: mat_creep
    TYPE(mat_type), POINTER :: current

    ! Use library properties for material numbers greater than 999
    SELECT CASE ( mat )
    CASE ( 0 )
       mat_creep = .FALSE.
       RETURN
    CASE ( 1001 )
       ! UO2 fuel properties
       mat_creep = fuel_creep
       RETURN
    CASE ( 1002 )
       ! Cladding properties
       mat_creep = clad_creep
       RETURN
    CASE ( 1003 )
       ! M5 Cladding properties
       mat_creep = m5_clad_creep
       RETURN
    END SELECT

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == mat ) EXIT
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR: can not find material label ',mat
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    mat_creep = current%creep

    RETURN

  END FUNCTION mat_creep


    SUBROUTINE mat_radial_Return(mat,temp,mu,dtime,epseff0,taueff,gamma, &
       dplmod,deds)
    ! Radial RETURN
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: mat
    REAL(r8k), INTENT(IN) :: temp,mu,epseff0,dtime,taueff
    REAL(r8k), INTENT(INOUT) :: gamma
    REAL(r8k), INTENT(OUT) :: dplmod,deds
    INTEGER(ipk) :: i
    REAL(r8k) :: sigys,siginf,delta,plmod,fy,dfy,dgamma,tmp,epseff
    LOGICAL :: plinc

    ! Use library properties for material numbers greater than 999
    SELECT CASE ( mat )
    CASE ( 1001 )
       ! UO2 fuel properties
       CALL fuel_radial_Return(temp,mu,dtime,epseff0,taueff,gamma,dplmod)
       RETURN
    CASE ( 1002 )
       ! Cladding properties
       CALL clad_radial_Return(temp,mu,dtime,epseff0,taueff,gamma,dplmod,deds)
       RETURN
    CASE ( 1003 )
       ! Cladding properties
       CALL m5_clad_radial_Return(temp,mu,dtime,epseff0,taueff,gamma, &
            dplmod,deds)
       RETURN
    END SELECT

    ! Use default isotropic plasticity model

    ! No plastic calculation
    IF ( .NOT.mat_plastic(mat) ) RETURN

    ! Yield function parameters
    sigys  = mat_par(mat,temp,'SIGYS   ')
    siginf = mat_par(mat,temp,'SIGINF  ')
    delta  = mat_par(mat,temp,'DELTA   ')
    plmod  = mat_par(mat,temp,'PLMOD   ')

    ! Evaluate yield function
    plinc = .FALSE.
    DO i=1,1000
       epseff = epseff0 + gamma
       tmp = EXP(-delta*epseff)
       fy = taueff &
            - (sigys + plmod*epseff + (siginf - sigys)*(1.0_r8k - tmp)) &
            - 3.0_r8k*mu*gamma
       IF ( fy < 1.0e-8_r8k*sigys ) EXIT
       plinc = .TRUE.
       dfy = -plmod - (siginf - sigys)*tmp*delta - 3.0_r8k*mu
       dgamma = -fy/dfy
       gamma = gamma + dgamma
    ENDDO

    IF ( plinc ) THEN
       dplmod = -dfy
       deds = 0.0_r8k
    ENDIF

    IF ( i >= 1000 ) lerror = .TRUE.

    RETURN

  END SUBROUTINE mat_radial_Return


    SUBROUTINE mat_creep_calc(mat,temp,mu,dtime,epseff0,taueff,gamma,deds)
    ! RETURN creep strain increment using Nortons power law
    !
    ! de/dt = C1 x taueff**C2 x epseff**C3 x EXP( -C4/(temp + C5) )
    !
    IMPLICIT NONE
    INTEGER(ipk) :: mat
    REAL(r8k), INTENT(IN) :: mu,temp,dtime,epseff0,taueff
    REAL(r8k), INTENT(INOUT) :: gamma,deds
    REAL(r8k) C1,C2,C3,C4,C5
    INTEGER(ipk) :: i
    REAL(r8k) :: edot,taueffn,dtaueff,rs,drsds,epseff

    deds = 0.0_r8k

    ! Use library properties for material numbers greater than 999
    SELECT CASE ( mat )
    CASE ( 1001 )
       ! UO2 fuel properties
       ! No creep properies implemented (yet)
       RETURN
    CASE ( 1002 )
       ! Cladding properties
       CALL clad_creep_calc(temp,mu,dtime,epseff0,taueff,gamma,deds)
       RETURN
    CASE ( 1003 )
       ! Cladding properties
       CALL m5_clad_creep_calc(temp,mu,dtime,epseff0,taueff,gamma,deds)
       RETURN
    END SELECT

    ! No plastic calculation
    IF ( .NOT.mat_creep(mat) ) RETURN

    ! Creep rate equation parameters
    C1 = mat_par(mat,temp,'CREEP1  ')
    C2 = mat_par(mat,temp,'CREEP2  ')
    C3 = mat_par(mat,temp,'CREEP3  ')
    C4 = mat_par(mat,temp,'CREEP4  ')
    C5 = mat_par(mat,temp,'CREEP5  ')

    taueffn = taueff

    IF ( taueffn < 1.0e-10_r8k ) RETURN

    DO i=1,1000
       epseff = MAX(epseff0 + gamma,1.0e-25_r8k)
       edot = C1*(taueffn**C2)*(epseff**C3)*EXP(-C4/(temp + C5) )
       deds = &
            dtime*C1*C2*taueffn**(C2-1.0_r8k)*epseff**C3*EXP(-C4/(temp + C5) )
       gamma = dtime*edot
       rs = taueffn - taueff +  3.0_r8k*mu*gamma
       IF ( ABS(rs) > 1e-6_r8k*taueffn ) THEN
          drsds = 1.0_r8k + 3.0_r8k*mu*deds
          dtaueff = - rs/drsds
          IF (ABS(dtaueff)>0.01_r8k*taueffn) &
               dtaueff=SIGN(0.01_r8k*taueffn,dtaueff)
          taueffn = taueffn + dtaueff
       ELSE
          EXIT
       ENDIF
    ENDDO

    ! No convergence for the iteration
    IF ( i >= 1000 ) lerror = .TRUE.

    RETURN

  END SUBROUTINE mat_creep_calc


    SUBROUTINE mat_maximums(mat,epseff,sigeff,plSED,sigma)
    ! Update maximum parameters for the material
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: mat
    REAL(r8k), DIMENSION(:), INTENT(IN) :: epseff, sigeff, plSED
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: sigma
    INTEGER(ipk) :: id,snd,ig,ng
    TYPE(mat_type), POINTER :: current

    snd = SIZE(sigma,1)
    ng = SIZE(sigma,2)

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == mat ) THEN
          DO ig=1,ng
             current%maxepseff = MAX(current%maxepseff,epseff(ig))
             current%maxsigeff = MAX(current%maxsigeff,sigeff(ig))
             current%maxplSED = MAX(current%maxplSED,plSED(ig))
             DO id=1,snd
                current%maxsigma(id) = MAX(current%maxsigma(id),sigma(id,ig))
                current%minsigma(id) = MIN(current%minsigma(id),sigma(id,ig))
             ENDDO
          ENDDO
          EXIT
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE mat_maximums


    SUBROUTINE mat_Write_output(nunit)
    ! WRITE material Data to the output file
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(mat_type), POINTER :: current
    TYPE(mat_parameter), POINTER :: current_par

    WRITE(UNIT=nunit) nmat

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%maxepseff
       WRITE(UNIT=nunit) current%maxsigeff
       WRITE(UNIT=nunit) current%maxplSED

       current_par => current%first_par
       DO WHILE ( ASSOCIATED(current_par) )
          WRITE(UNIT=nunit) current_par%label
          WRITE(UNIT=nunit) current_par%nData
          WRITE(UNIT=nunit) current_par%Data(1:2,1:current_par%nData)
          current_par => current_par%next
       ENDDO

       WRITE(UNIT=nunit) 'End     '
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE mat_Write_output


    SUBROUTINE mat_read_output(nunit)
    ! WRITE material Data to the output file
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i,number,mat,nData
    CHARACTER(LEN=8) :: keyword
    TYPE(mat_type), POINTER :: current,previous
    TYPE(mat_parameter), POINTER :: current_par,previous_par,last_par

    Read(UNIT=nunit,ERR=20,End=20) number
    current => first_mat
    nmat = 0
    DO i=1,number
       Read(UNIT=nunit,ERR=20,End=20) mat
       IF ( ASSOCIATED(current) ) THEN
          current%label = mat
          current%number = i
          last_mat => current
       ELSE
          ALLOCATE(current)
          current%label = mat
          current%number = i
          current%first_par => NULL()
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_mat) ) THEN
             first_mat => current
             last_mat => current
          ELSE
             last_mat%next => current
             last_mat => current
          ENDIF
       ENDIF
       Read(UNIT=nunit) current%maxepseff
       Read(UNIT=nunit) current%maxsigeff
       Read(UNIT=nunit) current%maxplSED

       last_par => NULL()
       current_par => current%first_par
       Do
          Read(UNIT=nunit,ERR=20,End=20) keyword
          IF ( keyword(1:3) == 'End' ) EXIT
          IF ( keyword(1:5) == 'SIGYS' ) current%plastic = .TRUE.
          IF ( keyword(1:6) == 'CREEP1' ) current%creep = .TRUE.
          Read(UNIT=nunit,ERR=20,End=20) nData
          Read(UNIT=nunit,ERR=20,End=20) mat_Data(1:2,1:nData)
          IF ( ASSOCIATED( current_par ) ) THEN
             ! OverWrite existing material parameter
             current_par%label = keyword
             IF ( current_par%nData /= nData ) THEN
                current_par%nData = nData
                DEALLOCATE(current_par%Data)
                ALLOCATE(current_par%Data(2,nData))
             ENDIF
          ELSE
             ! Create new material parameter
             ALLOCATE(current_par)
             current_par%label = keyword
             current_par%nData = nData
             current_par%next => NULL()
             ALLOCATE(current_par%Data(2,nData))
             IF ( .NOT.ASSOCIATED(last_par) ) THEN
                current%first_par => current_par
             ELSE
                last_par%next => current_par
             ENDIF
          ENDIF
          current_par%Data(1:2,1:nData) = mat_Data(1:2,1:nData)
          last_par => current_par
          current_par => current_par%next
       ENDDO

       ! Deallocate all surplus material parameters
       DO WHILE ( ASSOCIATED(current_par) )
          previous_par => current_par
          current_par => current_par%next
          DEALLOCATE(previous_par%Data)
          DEALLOCATE(previous_par)
       ENDDO
       IF ( ASSOCIATED(last_par) ) last_par%next => NULL()
       
       current => current%next
       nmat = i
    ENDDO

    ! Deallocate all surplus material sets
    IF ( number == 0 ) THEN
       first_mat => NULL()
       last_mat => NULL()
    ENDIF
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       ! deallocate temperature dependent material properties
       current_par => previous%first_par
       DO WHILE ( ASSOCIATED(current_par) )
          previous_par => current_par
          current_par => current_par%next
          DEALLOCATE(previous_par%Data)
          DEALLOCATE(previous_par)
       ENDDO
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_mat) ) last_mat%next => NULL()

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading materials Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE mat_read_output


    SUBROUTINE mat_deallocate()
    ! Deallocate materials Database
    IMPLICIT NONE
    TYPE(mat_type), POINTER :: current,previous
    TYPE(mat_parameter), POINTER :: current_par,previous_par

    current => first_mat
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       ! deallocate temperature dependent material properties
       current_par => previous%first_par
       DO WHILE ( ASSOCIATED(current_par) )
          previous_par => current_par
          current_par => current_par%next
          DEALLOCATE(previous_par%Data)
          DEALLOCATE(previous_par)
       ENDDO
       DEALLOCATE(previous)
    ENDDO

  END SUBROUTINE mat_deallocate

END MODULE materials_frap
