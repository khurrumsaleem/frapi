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
    use Uncertainty_Vals_fraptran

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

        call Allocate_Variables (ntimepairs, radial, axial)
        call Allocate_Gas

        n1 = 1
        n2 = 1
        n3 = 2
        n4 = 1
        ldir = 3
        ntapou = 2
        npbh   = 50
        nhbh   = 25
        ngbh   = 100
        nhinta = 50
        nhupta = 50
        lcoold = 1952
        lflect = 630
        lblona = 73
        lprntb = 37
        lexcb  = 110
        lresr1 = 136
        lhtcb  = 12
        ndap1  = 21
        naxn   = 1
        ntco   = 4
        ntplot = 1
        kdbug  = 0
        ndtmax = 40
        ncards = 1
        ngaspr = 0
        ngastmp = 0
        nodpln = 6
        ! Set by IP
        idx2 = 1
        tmax = 0.0_r8k
        ncool2 = 0 !????
        ! Set defaults
        ! These variables are arbitrarily set to 1 until DD is removed
        ldialb = 1
        lphypr = 1
        ! End of arbitrarily set variables
        lresi2 = 68
        lresi3 = 18
        lmatpc = 3
        lsclr1 = 29
        lsclr2 = 57
        lsclr3 = 25
        lcolct = 671
        TranSwell = 0
        lthyd = 1516
        MaximumIterations = 120
        IndexFC2Print = 0
        IndexGrainBndSep = 0
        NRestart = 0
        TranFuelSwell = 1.0_r8k
        coldw  = 0.0_r8k
        ! relfraca = 0.0_r8k
        FuelGasSwell = 0.0_r8k
        ! set default values for modeling fission gas release due to fuel grain disintegration.
        gbse(1) = 0.0_r8k
        gbse(2) = 5000.0_r8k
        gbse(3) = 1.0_r8k
        gbse(4) = 1.0_r8k
        gbse(5) = 0.0_r8k

        call init()

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

        size1 = 2 * naxialnodes * ntimesteps
        total = 10 * naxialnodes + 6

        include 'ft_associate_h.f90'
        include 'ft_allocate_h.f90'

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

        call default_values()

    end subroutine p_deft

    subroutine p_next(this, dt)

        class (fraptran_driver), intent(inout) :: this

        real(8) :: dt

        time = 0.d0

        CALL setup6

        n2 = 1

        ! Calculate results for current time step
        CALL comput

        ! Store results for current time step
        CALL store6

        ! Set variable saying it's no longer the first call to FRAPTRAN
        first_pass = .FALSE.

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