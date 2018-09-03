MODULE bloon_h
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module reeplaces the comdeck bloona & bloonb
    TYPE bloon_var
        INTEGER(ipk) :: ifbaln      !1 If balon2 predicts failure
        INTEGER(ipk) :: jmnbal      !balon2 radial contact node
        INTEGER(ipk) :: kntbal      !1 If balon2 predicts pellet-clad contact
        INTEGER(ipk) :: nbncal      !1 If balon2 has been called once
        INTEGER(ipk) :: nodprm      !number flow area reduction subnodes
        INTEGER(ipk) :: kbaln       !input frat-t index of ballooning node only when modbal = 1
        ! Method of finding cladding temperatures and annealing
        INTEGER(ipk) :: modbal
        ! Amount of information printed from within the Subroutine baloon2
        INTEGER(ipk) :: nprntb
        REAL(r8k) :: chstrs         !Max hoop stress in balloon region (psi)
        REAL(r8k) :: frbal          !balon2 circular/radial model switch
        REAL(r8k) :: pmxbal         !balon2 maximum gap pressure (Pa)
        REAL(r8k) :: r8bal          !balon2 average radius at balon2 axial node 8 (m)
        REAL(r8k) :: tcebal         !maximum circumferential strain in balloon region
        REAL(r8k) :: pdrato         !User supplied bundle pitch to rod diameter ratio
        REAL(r8k) :: rnbnt          !User supplied ratio of balloonable cells to total cells
        REAL(r8k) :: totnb          !User supplied total # of rods in fuel bundle
        REAL(r8k) :: trabal         !
        REAL(r8k) :: taxbal         !
        REAL(r8k) :: dtbal          !frap time step size (s)
        REAL(r8k) :: dtobal         !frap previous time step size (s)
        REAL(r8k) :: emwbal         !energy generated by cladding oxidation (W/m)
        REAL(r8k) :: fabal          !input additional axial force applied to cladding by constraints (pounds)
        REAL(r8k) :: flxbal         !input fast neutron flux (neutrons/((m**2)*s))
        REAL(r8k) :: htcbal         !input cladding surfact heat transfer coefficient (btu/s*(ft**2)*F))
        REAL(r8k) :: h0bal          !input initial (undeformed) cladding wall thickness (in)
        REAL(r8k) :: pcbal          !input coolant pressure (psi)
        REAL(r8k) :: psbal          !input fuel rod pressure (psi)
        REAL(r8k) :: qbal           !input rod heat flux (btu/(s*(ft**2))).  used only when modbal = 0 and nbncal = 0
        REAL(r8k) :: qlbal          !input rod heat flux at node k,j (btu/(s*(ft**2))).
        REAL(r8k) :: rfbal          !input hot radius of fuel pellet (in)
        REAL(r8k) :: rmpbal         !input uniformly expanded cladding midplane radius when baloon is first called (in).
        REAL(r8k) :: r0bal          !input initial (undeformed) cladding outside radius (in)
        REAL(r8k) :: tbkbal         !input bulk coolant temperature (degrees F)
        ! Cladding average temperature when baloon is first called (degrees F). Used only when nbncal = 0
        REAL(r8k) :: tc0bal
        ! Fuel surface temperature when baloon is first called (degrees F).  used only when nbncal = 0.
        REAL(r8k) :: tf0bal
        ! Gap gas temperature (degrees F)
        REAL(r8k) :: tgbal
        ! Time at start of time step. used only with cladding temperature and time table of modbal = 2 (s).
        REAL(r8k) :: timbal
        REAL(r8k) :: tm1bal         !clad temperature at lower node (K)
        REAL(r8k) :: tp1bal         !clad temperature at upper node (K)
        REAL(r8k) :: ztmax          !elevation of ballooning node (m)
        REAL(r8k) :: zm1bal         !elevation of lower node (m)
        REAL(r8k) :: zp1bal         !elevation of upper node (m)
        REAL(r8k) :: zndbal         !total length of fuel rod (m)    
        REAL(r8k) :: htcgba         !Avg heat transfer coefficient for gap
        ! Temperature
        REAL(r8k) :: tfavba
        ! Input length of FrapTran node (inches).  used only when nbncal equals 0
        REAL(r8k) :: zbaln
        ! Arrays
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: farbal      ! Flow area reduction   (m**2/m**2)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: sdfar       ! Standard deviation of flow area reduction
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: zfarbl      ! Axial location flow area reduction subnodes  (m)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ablona      ! Used for restart. Length = lblona = 73
    END TYPE bloon_var
    !
    INTEGER(ipk) :: ifbaln
    INTEGER(ipk) :: jmnbal
    INTEGER(ipk) :: kntbal
    INTEGER(ipk) :: nbncal
    INTEGER(ipk) :: nodprm
    INTEGER(ipk) :: kbaln
    INTEGER(ipk) :: modbal
    INTEGER(ipk) :: nprntb
    REAL(r8k) :: chstrs
    REAL(r8k) :: frbal
    REAL(r8k) :: pmxbal
    REAL(r8k) :: r8bal          !balon2 average radius at balon2 axial node 8 (m)
    REAL(r8k) :: tcebal         !maximum circumferential strain in balloon region
    REAL(r8k) :: pdrato         !User supplied bundle pitch to rod diameter ratio
    REAL(r8k) :: rnbnt          !User supplied ratio of balloonable cells to total cells
    REAL(r8k) :: totnb          !User supplied total # of rods in fuel bundle
    REAL(r8k) :: trabal         !
    REAL(r8k) :: taxbal         !
    REAL(r8k) :: dtbal          !frap time step size (s)
    REAL(r8k) :: dtobal         !frap previous time step size (s)
    REAL(r8k) :: emwbal         !energy generated by cladding oxidation (W/m)
    REAL(r8k) :: fabal          !input additional axial force applied to cladding by constraints (pounds)
    REAL(r8k) :: flxbal         !input fast neutron flux (neutrons/((m**2)*s))
    REAL(r8k) :: htcbal         !input cladding surfact heat transfer coefficient (btu/s*(ft**2)*F))
    REAL(r8k) :: h0bal          !input initial (undeformed) cladding wall thickness (in)
    REAL(r8k) :: pcbal          !input coolant pressure (psi)
    REAL(r8k) :: psbal          !input fuel rod pressure (psi)
    REAL(r8k) :: qbal           !input rod heat flux (btu/(s*(ft**2))).  used only when modbal = 0 and nbncal = 0
    REAL(r8k) :: qlbal
    REAL(r8k) :: rfbal
    REAL(r8k) :: rmpbal
    REAL(r8k) :: r0bal
    REAL(r8k) :: tbkbal
    REAL(r8k) :: tc0bal
    REAL(r8k) :: tf0bal
    REAL(r8k) :: tgbal
    REAL(r8k) :: timbal
    REAL(r8k) :: tm1bal
    REAL(r8k) :: tp1bal
    REAL(r8k) :: ztmax
    REAL(r8k) :: zm1bal
    REAL(r8k) :: zp1bal
    REAL(r8k) :: zndbal
    REAL(r8k) :: htcgba
    REAL(r8k) :: tfavba
    REAL(r8k) :: zbaln
    ! Arrays
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: farbal
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: sdfar
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: zfarbl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ablona
    !
    END MODULE bloon_h