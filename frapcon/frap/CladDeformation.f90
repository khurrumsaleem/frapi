MODULE CladDeformation
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to calculate cladding deformation due to thermal expansion
    !> and irradiation growth. Subroutines include cexpan, gldgro
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE cexpan
    USE Kinds
    USE Conversions
    USE Variables, ONLY : CladAveTemp, txa, txal, txc, j
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from main and computes cladding axial and radial thermal expansion
    !>@author
    !> cexpan was coded by g a berna in january 1978.
    !
    ! Input
    !
    ! CladAveTemp - cladding average temperature (F)
    !
    ! Output
    !
    ! txa    - clad thermal exp coef in radial direction (in/in-F)
    ! txal   - clad thermal exp coef in axial direction (in/in-F)
    ! txc    - cladding thermal expansion coefficient (1/F)
    !
    ! Internal
    !
    ! tempF - cladding average temperature (F)
    ! tempK - cladding average temperature (K)
    ! expc  - cladding expansion coefficient
    !
    REAL(r8k) :: expc, tempF, tempK
    !
    tempF = CladAveTemp(j-1)
    tempK = tfk(tempF)
    expc = MatProp ('CLAD', 'THEXP_RADIAL', tempK)
    txa = expc / (tempF - 70.0_r8k)
    txc = expc
    expc = MatProp ('CLAD', 'THEXP_AXIAL', tempK)
    txal = expc / (tempF - 70.0_r8k)
    !
    END SUBROUTINE cexpan
    !
    !
    !
    SUBROUTINE cldgro
    USE Kinds
    USE Conversions
    USE Variables, ONLY : CladIrradGrowStrn, j
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> cldgro is called from frpcon and computes the cladding incremental cladding growth
    !
    ! Input
    !
    ! j    - axial node index
    !
    ! Output
    !
    ! CladIrradGrowStrn - axial strain due to cladding irradiation growth
    !
    REAL(r8k) :: agrow, dummy = 0.0_r8k
    !
    ! Get the new cladding axial growth
    agrow = MatProp ('CLAD', 'AXIAL_GROW', dummy)
    CladIrradGrowStrn(j-1) = CladIrradGrowStrn(j-1) + agrow
    !
    END SUBROUTINE cldgro
    !
END MODULE CladDeformation