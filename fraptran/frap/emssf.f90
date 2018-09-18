MODULE emssf_fraptran
    USE Kinds_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE emssf1(tclad, alpha, zroxid, fe)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    IMPLICIT NONE
    !>@brief
    !> Subroutine emssf2 computes emissivity factor for clad to coolant radiation heat transfer calculations
    !
    ! Input
    !
    ! tclad  - Peak clad temperature (F)
    ! alpha  - Coolant void fraction
    ! zroxid - Oxide layer thickness
    !
    ! Output
    !
    ! fe     - Emissivity factor for clad to coolant radiation heat transfer
    !
    REAL(r8k), INTENT(IN) :: tclad, alpha
    REAL(r8k), INTENT(OUT) :: fe
    REAL(r8k), INTENT(INOUT) :: zroxid
    REAL(r8k) :: ec, ev, el
    
    ! Emissivity of cladding
    ec = MatProperty (Material='CLAD', Property='EMISS', Temperature=tfk(tclad), Oxide=zroxid)
    
    ! Eemissivity of vapor
    ev = 0.2_r8k
    
    ! Emissivity of liquid water
    el = 1.0_r8k
    
    ! Emissiviy factor
    fe = 1.0_r8k / (1.0_r8k / ec + 1.0_r8k / (ev * alpha + (1.0_r8k - alpha) * el) - 1.0_r8k)
    
    END SUBROUTINE emssf1
    !
    !
    !
    SUBROUTINE emssf2 (Tfuel, Tclad, rf, gpthk, zroxid, fe)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    IMPLICIT NONE
    !>@brief
    !> Subroutine emssf2 computes emissivity factor for fuel-cladding gap radiation heat transfer calculations
    !>@author
    !> Modified by I. Porter, NRC, March 2014 to clean coding and convert to .f90
    !
    ! Input
    !
    ! Tfuel  - Temperature of outside surface of fuel (F)
    ! Tclad  - Peak clad temperature (F)
    ! rf     - Radius of outside surface of fuel (ft)
    ! gpthk  - Thickness of fuel-cladding gap (ft)
    ! zroxid - Oxide layer thickness (m)
    !
    ! Output
    !
    ! fe     - Emissivity factor for fuel-cladding gap heat transfer
    !
    REAL(r8k), INTENT(IN) :: Tfuel, Tclad, rf, gpthk
    REAL(r8k), INTENT(OUT) :: fe
    REAL(r8k), INTENT(INOUT) :: zroxid
    REAL(r8k) :: Efuel, Eclad, rc
    
    ! Fuel emissivity
    Efuel = MatProperty (Material='FUEL', Property='EMISS', Temperature=tfk(Tfuel))
    
    ! Cladding emissivity
    Eclad = MatProperty (Material='CLAD', Property='EMISS', Temperature=tfk(Tclad), Oxide=zroxid)
    
    ! Set cladding inner radius
    rc = rf + gpthk
    
    ! Combined emissivity
    fe = 1.0_r8k / (1.0_r8k / Efuel + (rf / rc) * (1.0_r8k / Eclad - 1.0_r8k))
    
    END SUBROUTINE emssf2
    !
END MODULE emssf_fraptran













