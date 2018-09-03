MODULE Data_grid
    USE Kinds
    USE common_parameters
    USE materials_frap
    USE quad4
    IMPLICIT NONE
    !>@brief
    !> Handle input data that is given in dIfferent mesh than what the finite element model uses

  TYPE grid_1d_type
     INTEGER(ipk) :: label ! ID label for the grid
     INTEGER(ipk) :: naxn  ! Number of axial meshes
     INTEGER(ipk) :: nradn ! Number of radial meshes
     REAL(r8k), DIMENSION(:), POINTER :: elevation ! Axial coordinates
     REAL(r8k), DIMENSION(:,:), POINTER :: radius  ! Radial coordinates
     REAL(r8k), DIMENSION(:,:), POINTER :: Data    ! Temporary array for Data
     TYPE(grid_1d_type), POINTER :: next
  END TYPE grid_1d_type


  TYPE(grid_1d_type), POINTER :: first_grid_1d,last_grid_1d

CONTAINS


    SUBROUTINE grid_1d_create(label,naxn,nradn,elev,rad)
    ! Create 1D input grid
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,naxn,nradn
    REAL(r8k), INTENT(IN) :: elev(naxn),rad(nradn,naxn)
    TYPE(grid_1d_type), POINTER :: new_grid_1d
    INTEGER(ipk) :: i

    ! Look for the grid from the Database
    new_grid_1d => first_grid_1d
    DO WHILE( ASSOCIATED(new_grid_1d) )
       IF ( new_grid_1d%label == label ) EXIT
       new_grid_1d => new_grid_1d%next
    ENDDO
    IF ( .NOT.ASSOCIATED(new_grid_1d) ) THEN
       ALLOCATE(new_grid_1d)
       new_grid_1d%elevation => NULL()
       new_grid_1d%radius => NULL()
       new_grid_1d%Data => NULL()
       IF ( .NOT.ASSOCIATED(first_grid_1d) ) THEN
          first_grid_1d => new_grid_1d
          last_grid_1d => new_grid_1d
       ELSE
          last_grid_1d%next => new_grid_1d
          last_grid_1d => new_grid_1d
       ENDIF
    ELSE
       DEALLOCATE(new_grid_1d%elevation,new_grid_1d%radius,new_grid_1d%Data)
    ENDIF

    new_grid_1d%label = label
    new_grid_1d%naxn = naxn
    new_grid_1d%nradn = nradn
    new_grid_1d%next => NULL()
    ALLOCATE(new_grid_1d%elevation(naxn))
    ALLOCATE(new_grid_1d%radius(nradn,naxn))
    ALLOCATE(new_grid_1d%Data(nradn,naxn))
    new_grid_1d%Data = 0.0_r8k
    new_grid_1d%elevation(1:naxn) = elev(1:naxn)
    DO i=1,naxn
       new_grid_1d%radius(1:nradn,i) = rad(1:nradn,i)
    ENDDO

    RETURN

  END SUBROUTINE grid_1d_create


  FUNCTION grid_1d_interp(label,x) RESULT (valuei)
    ! Find interpolated value "valuei" at coordinate position "x" using input
    ! Data grid "label"
    IMPLICIT NONE
    INTEGER(ipk) :: label
    REAL(r8k) :: x(2),valuei,tmp1,tmp2
    INTEGER(ipk) :: naxn,nradn,ax1,ax2,r11,r12,r21,r22
    TYPE(grid_1d_type), POINTER :: current

    ! Look for the grid from the Database
    current => first_grid_1d
    DO WHILE( ASSOCIATED(current) )
       IF ( current%label == label ) EXIT
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) THEN
       WRITE(UNIT=6,FMT='(A,I0)') 'ERROR cannot find Data grid ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    naxn = current%naxn
    nradn = current%nradn

    ax1 = 1
    DO ax2=1,naxn
       IF ( current%elevation(ax2) > x(2) ) EXIT
       ax1 = ax2
    ENDDO
    ax2 = MIN(naxn,ax2)

    ! Find bounding radial nodes at lower radial mesh
    r11 = 1
    DO r12=1,nradn
       IF ( current%radius(r12,ax1) > x(1) ) EXIT
       r11 = r12
    ENDDO
    r12 = MIN(nradn,r12)

    ! Find bounding radial nodes at upper radial mesh
    r21 = 1
    DO r22=1,nradn
       IF ( current%radius(r22,ax2) > x(1) ) EXIT
       r21 = r22
    ENDDO
    r22 = MIN(nradn,r22)

    ! Radial interpolations
    IF ( r12 == r11 ) THEN
       tmp1 = current%Data(r12,ax1)
    ELSE
       tmp1 = ( current%Data(r12,ax1) - current%Data(r11,ax1) ) * &
            ( x(1) - current%radius(r11,ax1) ) / &
            ( current%radius(r12,ax1) - current%radius(r11,ax1) ) + &
            current%Data(r11,ax1)
    ENDIF
    IF ( r22 == r21 ) THEN
       tmp2 = current%Data(r22,ax2)
    ELSE
       tmp2 = ( current%Data(r22,ax2) - current%Data(r21,ax2) ) * &
            ( x(1) - current%radius(r21,ax2) ) / &
            ( current%radius(r22,ax2) - current%radius(r21,ax2) ) + &
            current%Data(r21,ax2)
    ENDIF

    ! Axial interpolation
    IF ( ax1 == ax2 ) THEN
       valuei = tmp2
    ELSE
       valuei = ( tmp2 - tmp1 ) * ( x(2) - current%elevation(ax1) ) / &
            ( current%elevation(ax2) - current%elevation(ax1) ) + tmp1
    ENDIF

    RETURN

  END FUNCTION grid_1d_interp


    SUBROUTINE grid_1d_temp(label)
    ! Interpolate temperaturesa from the Data grid to the nodes
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(node_type), POINTER :: current_node

    current_node => first_node
    DO WHILE ( ASSOCIATED(current_node) )
       current_node%temp_End = grid_1d_interp(label,current_node%x0)
       current_node => current_node%next
    ENDDO

    RETURN

  END SUBROUTINE grid_1d_temp


    SUBROUTINE grid_1d_volstr(label,mat)
    ! Read volumetric strain Data
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,mat
    TYPE(quad4_type), POINTER :: current_quad4
    TYPE(mat_type), POINTER :: current_mat
    INTEGER(ipk) :: ig
    REAL(r8k) :: x(3)

    ! Change the volumetric strain calculation flag in materials Database
    current_mat => first_mat
    DO WHILE ( ASSOCIATED(current_mat) )
       IF ( current_mat%label == mat ) THEN
          current_mat%volstr = .TRUE.
          EXIT
       ENDIF
       current_mat => current_mat%next
    ENDDO

    ! Volumetric strains in QUAD4 elements
    current_quad4 => first_quad4
    DO WHILE ( ASSOCIATED(current_quad4) )
       IF ( current_quad4%mat == mat ) THEN
          DO ig=1,4
             x(1:2) = current_quad4%x0(1:2,ig)
             current_quad4%epsth_End(ig) = grid_1d_interp(label,x)
          ENDDO
       ENDIF
       current_quad4 => current_quad4%next
    ENDDO

    RETURN

  END SUBROUTINE grid_1d_volstr


    SUBROUTINE grid_deallocate()
    ! Deallocate all Data grid variables
    IMPLICIT NONE
    TYPE(grid_1d_type), POINTER :: current,previous

    current => first_grid_1d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous%elevation)
       DEALLOCATE(previous%radius)
       DEALLOCATE(previous%Data)
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE grid_deallocate
END MODULE Data_grid
