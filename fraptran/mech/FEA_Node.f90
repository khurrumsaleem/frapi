MODULE FEA_Node
    !>@brief
    !> This module contains the subroutine used to create a node.
    !> Subroutines include create_node
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    USE Kinds
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    !
    SUBROUTINE create_node (label, node_x0)
    USE Kinds
    USE common_parameters
    IMPLICIT NONE
    !>@brief
    !> Create node
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: node_x0(3)
    TYPE(node_type), POINTER :: new_node, current

    ! Check for multiple definitions for the same node label
    current => first_node
    DO WHILE ( ASSOCIATED(current) )
        IF ( current%label == label ) THEN
            WRITE(UNIT=6,FMT='(A,I0,A)') 'ERROR: node ',label,' defined twice'
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        ENDIF
        current => current%next
    ENDDO

    ! Allocate memory for new node
    nnodes = nnodes + 1
    ALLOCATE(new_node)

    ! Initialization of the FE model is needed
    dof_numbering = .TRUE.
    init_fe = .TRUE.

    ! Initialize node parameters
    new_node%label = label
    IF ( label > max_node ) max_node = label
    new_node%number = nnodes
    new_node%ngroup = ngroup
    new_node%x0 = node_x0
    new_node%dof_status = 0
    IF ( nd == 2 ) new_node%dof_status(3) = -1
    new_node%u0 = 0.0_r8k
    new_node%v0 = 0.0_r8k
    new_node%a0 = 0.0_r8k
    new_node%fint0 = 0.0_r8k
    new_node%temp0 = tref
    new_node%force_flag = .FALSE.
    new_node%force => NULL()
    new_node%force0 => NULL()
    new_node%force_End => NULL()

    new_node%next => NULL()

    ! Add node to the linked list
    IF ( .NOT.ASSOCIATED(last_node) ) THEN
        first_node => new_node
        last_node => new_node
    Else
        last_node%next => new_node
        last_node => new_node
    ENDIF

    ! Initialize nodal arrays
    IF ( nnodes > n_array ) THEN
        DO
            n_array = n_array + 10000
            IF ( n_array > nnodes ) EXIT
        ENDDO

        IF ( ALLOCATED(x0) ) DEALLOCATE(x0,x,u,v,a,temp,Fgrav,Fext,Fint,enumber, &
            dof_number,node_labels,du,fres,Real_tmp,int_tmp)
        ALLOCATE(x0(nd,n_array),x(nd,n_array),u(nd,n_array),v(nd,n_array), &
            a(nd,n_array),temp(n_array),Fgrav(nd,n_array),Fext(nd,n_array), &
            Fint(nd,n_array),enumber(n_array),dof_number(nd,n_array), &
            node_labels(n_array),du(nd*n_array),fres(nd*n_array), &
            Real_tmp(nd*n_array),int_tmp(nd*n_array))

        init_fe = .TRUE.
        enumber(1:nnodes) = 0

        current => first_node
        DO WHILE ( ASSOCIATED(current) )
            x0(1:nd,current%number) = current%x0(1:nd)
            u(1:nd,current%number) = current%u0(1:nd)
            x(1:nd,current%number) = x0(1:nd,current%number) + u(1:nd,current%number)
            v(1:nd,current%number) = current%v0(1:nd)
            a(1:nd,current%number) = current%a0(1:nd)
            node_labels(current%number) = current%label
            current => current%next
        ENDDO
    ELSE
        x0(1:nd,new_node%number) = new_node%x0(1:nd)
        u(1:nd,new_node%number) = new_node%u0(1:nd)
        x(1:nd,new_node%number) = x0(1:nd,new_node%number) + u(1:nd,new_node%number)
        v(1:nd,new_node%number) = new_node%v0(1:nd)
        a(1:nd,new_node%number) = new_node%a0(1:nd)
        node_labels(new_node%number) = new_node%label
        enumber(new_node%number) = 0
    ENDIF

    END SUBROUTINE create_node    
    !
    !
END MODULE FEA_Node












