module m_state

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
    use m_array_clone, only : array_clone

    implicit none

    integer, parameter :: ifile = 1394
    integer :: counter = 0

    contains
    subroutine printstate(i, fname, mode)
        implicit none
        character(*) :: fname, mode
        integer :: i
        counter = counter + 1
        if (i == counter) then
            open (ifile, file = fname, status = 'unknown', Form = 'formatted')
            include "ft_print_h.f90"
            close (ifile)
            select case (mode)
            case ('exit')
                stop
                call exit(1)
            case ('none')
                continue
            end select
        endif
    end subroutine printstate
end module m_state