module fraptran2

    USE Kinds_fraptran
    USE variables_fraptran
    USE setup_fraptran, ONLY : Main, Input_Echo
    USE TH_Link_fraptran
    USE FuelRod_Data_fraptran, ONLY : Allocate_Rods, FRAPTRAN_Vars, fraptran_rod
    USE variables_fraptran, ONLY : coupled, iunit, ounit, plotunit, frtrunit, h2ounit, fcunit, dakotaunit, nrestart, ncards, &
      &                   title, codeid, defsize, pre_na, pre_nr, Allocate_Variables
    USE frapc_fraptran
    USE Dyna_h_fraptran
    USE collct_h_fraptran
    USE resti_h_fraptran
    USE excb_h_fraptran
    USE scalr_h_fraptran
    USE FissionGasRelease_h_fraptran
    USE NCGases_fraptran
    USE FEA_Setup_fraptran
    USE ErrorMsg_fraptran, ONLY : namelist_read_error
    USE frapc_fraptran
    USE sth2x_fraptran, ONLY : sth2xi
    USE Initialization_fraptran, ONLY : initia
    USE Read_Input_fraptran
    USE timestep_fraptran, ONLY : crank6, setup6, comput, store6
    USE ErrorMsg_fraptran, ONLY :errori
    USE CoolantProperties_fraptran, ONLY : tc1, tc2
    USE htcb_h_fraptran
    USE cnvt_fraptran
    use Uncertainties_fraptran, only :  AllocateUncertaintyvars
    use Uncertainty_Vals_fraptran
    use arrayallocate, only : clone

    implicit none

    character (len=200), target :: namerf

    type, public :: fraptran_driver

        include "ft_pointers_h.f90"
        include "ft_replicants_h.f90"

        contains

        procedure :: make => p_make
        procedure :: init => p_init
        procedure :: next => p_next
        procedure :: deft => p_deft
        procedure :: load => p_load
        procedure :: dump => p_dump
        procedure :: destroy => p_destroy

    end type fraptran_driver

    contains

    subroutine p_make(this, naxn_, nfmesh_, ncmesh_, verbose)

        implicit none

        class (fraptran_driver), intent(inout) :: this

        INTEGER(ipk) :: k, i, InputStat = 0
        INTEGER(ipk) :: radial, axial, ntimepairs
        INTEGER(ipk) :: lmax, lrest, l1
        integer :: naxn_, nfmesh_, ncmesh_
        integer :: size1, total
        real(8) :: dt
        logical :: verbose

        is_export = .true.

        naxn = naxn_
        nfmesh = nfmesh_
        ncmesh = ncmesh_

        ! Set the # of axial, radial and timesteps to allocate the code's values on.
        defsize = 2
        pre_na = naxn + 25
        pre_nr = nfmesh + ncmesh + 1
        pre_nt = defsize
        radial = pre_nr
        axial = pre_na
        ntimepairs = 1  !# of time history pairs from T/H Code
        ntimesteps = pre_nt
        naxialnodes = pre_na
        nradialnodes = pre_nr
        size1 = 2 * naxialnodes * ntimesteps
        total = 10 * naxialnodes + 6

        include 'ft_v_allocate_h.f90'

        tt     => prop(1)
        CoolantPress => prop(2)
        v      => prop(3)
        ubar   => prop(4)
        hbar   => prop(5)
        beta   => prop(6)
        kappa  => prop(7)
        csubp  => prop(8)
        x      => prop(9)
        psat   => prop(10)
        vsubf  => prop(11)
        vsubg  => prop(12)
        usubf  => prop(13)
        usubg  => prop(14)
        hsubf  => prop(15)
        hsubg  => prop(16)
        betaf  => prop(17)
        betag  => prop(18)
        kappaf => prop(19)
        kappag => prop(20)
        csubpf => prop(21)
        csubpg => prop(22)

        include 'ft_r_allocate_h.f90'

        !call Allocate_Variables (ntimepairs, radial, axial)
        !call Allocate_Gas
        !call AllocateUncertaintyvars
        !ALLOCATE (buradv(1:naxialnodes,1:nradialnodes))
        !ALLOCATE (bufrad(1:nradialnodes,1:naxialnodes))

        call init() ! This is FEA init subroutine

! Initialize FE model (Only useable when not coupled)
!        IF (.NOT. coupled) THEN
!            CALL init()
!            CALL default_values()
!        ENDIF

        ncard2 = ncards
        t12 = 0.d0
        t0 = 0.d0
        nrest2 = NRestart !????

        ! Echo input file to output file
        !CALL Input_Echo

        t22 = dt
        tmax = dt
        ! Start code execution

        t1 = t12
        t2 = t22
        ndtad = ndtadv

        ! lafrap = total length of afrap array
        !lafrap = afrap(2)
        ! lafrap = 1
        !lmax = lafrap

        !ncards = 1

        ! Read the restart array
!        READ(unit = frtrunit, iostat = InputStat) (afrap(lrest), lrest = 4,lmax)


        ! Initialize water property tables (This is done in cominp if no restart)
!        l1 = 17000
        !Get water properties
!        CALL sth2xi (aasth, l1)
!        IF (l1 < 0) Call errori (2,0)

!        tmax = t2

!        t0 = t1
!        tc1 = t1
!        tc2 = t2

        ! If ncards = 0 , cold startup

        include 'ft_associate_h.f90'

        this % coolant = 'OFF'
        this % mheat = 'OFF'
        this % bheat = 'OFF'
        this % reflood = 'OFF'
        this % internal = 'OFF'
        this % metal = 'OFF'
        this % deformation = 'OFF'
        this % inst = 'OFF'
        this % radiation = 'OFF'
        this % relocmodel = 'FRAPCON-3.3'
        this % geomet = 0
        this % nvol1 = 0
        this % lowpl = 0
        this % pressu = 0
        this % massfl = 0
        this % coreav = 0
        this % chf = 0
        this % filmbo = 0
        this % nucbo = 0
        this % coldwa = 0
        this % axpow = 0
        this % bowing = 0
        this % spefbz = 0
        this % geometry = 0
        this % nbundl = 0
        this % refloodtime = 0
        this % radiat = 0
        this % ruptur = 0
        this % liquid = 0
        this % inlet = 0
        this % reflo = 0
        this % pressure = 0
        this % collaps = 0
        this % frapt4 = 0
        this % geom = 0
        this % temp = 0
        this % tape2 = 0
        this % nvol2 = 0
        this % unitin = 0
        this % unitout = 0
        this % res = 0
        this % pow = 0
        this % gasflo = 0
        this % jfb = 0
        this % upppl = 0
        this % zone = 0
        this % soltyp = 0
        this % cenvoi = 0
        this % noball = 0
        this % baker = 0 
        this % cathca = 0
        this % idoxid = 0

!        write(*,*) 'butemp = ', this % butemp(1:6)
!        write(*,*) 'butemp = ', butemp(1:6)
!        write(*,*) 'dtdkta: ', this % dtdkta(1)
!        stop

    end subroutine p_make


    subroutine p_load(this)

        class (fraptran_driver), intent(inout) :: this

        include 'ft_load_h.f90'

    end subroutine p_load


    subroutine p_dump(this)

        class (fraptran_driver), intent(inout) :: this

        include 'ft_dump_h.f90'

    end subroutine p_dump


    subroutine p_init(this)

        class (fraptran_driver), intent(inout) :: this

        logical :: is_open

        inquire (unit = fcunit, opened = is_open)
        if (is_open) close (unit = fcunit)
        open (unit = fcunit, file = this % namerf)

        call cardin
        call initia

        close (fcunit)

    end subroutine p_init

    subroutine p_deft(this)

        class (fraptran_driver), intent(inout) :: this

        ! Default values for the FEA model
        call default_values()

        ! Default values for FRAPTRAN's variables
        include "ft_default_h.f90"

    end subroutine p_deft

    subroutine p_next(this, dt)

        class (fraptran_driver), intent(inout) :: this

        real(8) :: dt

        time = 0.D0
        tmax = time + dt
        tend = time + dt
        t1   = time
        t2   = time + dt
        dtp  = dt

        CALL setup6

        n2 = 1

        ! Calculate results for current time step
        CALL comput

        ! Store results for current time step
        CALL store6

        ! Set variable saying it's no longer the first call to FRAPTRAN
        first_pass = .FALSE.

        pbh(1)           = pbh(3)               
        dtmaxa(1)        = dtmaxa(3)               
        hbh(1)           = hbh(3)               
        hupta(1)         = hupta(3)               
        hinta(1)         = hinta(3)               
        gbh(1)           = gbh(3)               
        explenumt(1)     = explenumt(3)               
        dtpoa(1)         = dtpoa(3)               
        RodAvePower(1)   = RodAvePower(3)               
        dtplta(1)        = dtplta(3)               
        FuelGasSwell(1)  = FuelGasSwell(3)               
        temptm(1)        = temptm(3)               
        relfraca(1)      = relfraca(3)               
        prestm(1)        = prestm(3)               
        fldrat(1)        = fldrat(3)               
        gasphs(1)        = gasphs(3)               
        hlqcl(1)         = hlqcl(3)               
        htca(1,:)        = htca(3,:)               
        tblka(1,:)       = tblka(3,:)               
        gasths(1,:)      = gasths(3,:)               

    end subroutine p_next

    subroutine p_destroy(this)

        class (fraptran_driver), intent(inout) :: this

    end subroutine p_destroy

    subroutine FRAPTRAN_1_5a
        USE Kinds_fraptran
        USE variables_fraptran, ONLY : Time
        USE setup_fraptran, ONLY : Main
        USE TH_Link_fraptran
        USE FuelRod_Data_fraptran, ONLY : Allocate_Rods, FRAPTRAN_Rod
        IMPLICIT NONE
        !>@brief
        !> This is the driver for the FRAPTRAN Code.
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> May 16, 2016
        !
        ! *Note:*
        ! The source code in this file can be used as an outline to compile FRAPTRAN as a library in other codes
        !
        INTEGER(ipk) :: N_Rods = 1
        INTEGER(ipk) :: Rod_ID = 1
    
        ! If this is the first call to FRAPTRAN, allocate how many rods you will be modeling (If standalone, 1 rod)
        IF (.NOT. ALLOCATED (FRAPTRAN_Rod)) CALL Allocate_rods(numrods = N_Rods)
    
        ! Check to see if this particular fuel rod has been calculated before.
        ! If it has, then the array below will have already been allocated, and the code should update itself with this rod's values.
        IF (ALLOCATED(FRAPTRAN_Rod(Rod_ID)%powrd%RodAvePower)) THEN
            ! Update with rod's values
            CALL FRAPTRAN_Rod(Rod_ID)%Update
            ! Update coolant conditions
            CALL Update_Coolant
        END IF
    
        ! Start the FRAPTRAN execution
        CALL Main
    
        ! Save the rod data in case FRAPTRAN is called again at a later time or for a different rod
        CALL FRAPTRAN_Rod(Rod_ID)%Remember
    
        ! Display the end time of the calculation
        WRITE (0, 100) Time
100     FORMAT (/,'Code execution finished with a problem time of ',f12.4,' seconds',/)
        !
        end subroutine FRAPTRAN_1_5a

end module fraptran2