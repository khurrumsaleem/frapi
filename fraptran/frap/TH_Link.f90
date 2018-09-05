MODULE TH_link
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to link FRAPTRAN to a TH code.
    !> Subroutines include Update_Coolant
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    !
    CONTAINS
    !
    SUBROUTINE Update_Coolant ()
    USE Kinds
    USE frapc
    USE variables_fraptran
    USE conversions_fraptran, ONLY : ftom
    USE FuelRod_Data, ONLY : FRAPTRAN_Vars
    IMPLICIT NONE
    !>@brief
    !> This subroutine updates the coolant arrays (axial) that are used to store coolant data when linked with a T/H Code
    !> when the T/H Code is passing bulk coolant temperature, heat transfer coefficient & pressure data.
    !> Note that FRAPTRAN can only read 1 value for HTC & Bulk Coolant Temperature (not separate values for Liquid & Vapor).
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/25/2014
    
    END SUBROUTINE Update_Coolant
    
END MODULE TH_link

