MODULE Cs_I
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutine used to calculate Cs and I in the gap.
    !> Subroutines include cesiod
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/15/2016
    !
    CONTAINS
    !
    SUBROUTINE cesiod (time, ftmax, delbu, dtime, ftemp, fraden, cs, roid, buloc)
    USE Kinds
    USE phypro_h, ONLY : ftmelt
    IMPLICIT NONE
    !>@brief 
    !> cesiod calculates the amounts of cesium and iodine isotopes available to the fuel rod gap.
    !>@author
    !> cesiod was coded by d. l. hagrman january 1977
    !
    ! Input
    !
    ! time    - time at operating temperature at end of step (s)
    ! ftmax   - maximum temperature attained by the meshpoint during operation prior to burnup step considered (k)
    ! delbu   - burnup during the step considered (mw-s/kg metal)
    ! dtime   - duration of the burnup step considered (s)
    ! ftemp   - fuel meshpoint temperature (k)
    ! fraden  - fractional fuel density (ratio of actual density to theoretical density)
    ! buloc   - local burnup of fuel, mw.s/kg
    !
    ! Output
    !
    ! roid(1) - net specific release of iodine     (kg iodine /kg fuel)
    ! roid(2) - net specific release of iodine 127 (kg iodine 127/kg fuel) stable
    ! roid(3) - net specific release of iodine 129 (kg iodine 129/kg fuel) half lIfe 1.72e07 years
    ! roid(4) - net specific release of iodine 131 (kg iodine 131/kg fuel) half lIfe 8.05 days
    ! roid(5) - net specific release of iodine 132 (kg iodine 132/kg fuel) half lIfe 2.3 hours
    ! roid(6) - net specific release of iodine 133 (kg iodine 133/kg fuel) half lIfe 21 hours
    ! roid(7) - net specific release of iodine 134 (kg iodine 134/kg fuel) half lIfe 52 minutes
    ! roid(8) - net specific release of iodine 135 (kg iodine 135/kg fuel) half lIfe 6.7 hours
    ! cs(1)   - net specific release of cesium     (kg cesium/kg fuel)
    ! cs(2)   - net specific release of cesium 133 (kg cesium 133/kg fuel) stable
    ! cs(3)   - net specific release of cesium 135 (kg cesium 135/kg fuel) half lIfe 2.9e06 years
    ! cs(4)   - net specific release of cesium 137 (kg cesium 137/kg fuel) half lIfe 33 years
    ! cs(5)   - net specific release of cesium 138 (kg cesium 138/kg fuel) half lIfe 32.2 minutes
    ! ftmax   - maximum temperature attained by the meshpoint during operation to the End of the burnup step considered (k)
    !
    ! Reference:
    !
    ! (1) j. belle, uranium dioxide. properties and nuclear applications (july 1961)
    ! (2) s. katcoff, nucleonics 16 (april 1958) pp 78-85
    ! (3) s. katcoff, nucleonics 18 (nov 1960) pp 201-208
    ! (4) b. f. rider, a survey and evaluation of thermal fission yields for u-235, pu-239, u-233, and pu-241 geap-5356 (Sept 1967)
    !
    ! Note: 
    !
    ! This model is for lwr reactors only**
    !
    REAL(r8k) :: b, fisr, a, dmax, dnow, anui4, anui5, anui6, anui7, anui8, anuc5, f
    REAL(r8k), INTENT(IN) :: time, delbu, dtime, ftemp, fraden, buloc
    REAL(r8k), INTENT(INOUT) :: ftmax
    REAL(r8k), PARAMETER :: yuid2  = 9.50e-12_r8k
    REAL(r8k), PARAMETER :: yuid3  = 5.94e-11_r8k
    REAL(r8k), PARAMETER :: yuid4  = 3.10e-02_r8k
    REAL(r8k), PARAMETER :: yuid5  = 4.30e-02_r8k
    REAL(r8k), PARAMETER :: yuid6  = 6.90e-02_r8k
    REAL(r8k), PARAMETER :: yuid7  = 7.90e-02_r8k
    REAL(r8k), PARAMETER :: yuid8  = 6.10e-02_r8k
    REAL(r8k), PARAMETER :: yucs2  = 5.04e-10_r8k
    REAL(r8k), PARAMETER :: yucs3  = 4.98e-10_r8k
    REAL(r8k), PARAMETER :: yucs4  = 4.85e-10_r8k
    REAL(r8k), PARAMETER :: yucs5  = 6.60e-02_r8k
    REAL(r8k), PARAMETER :: almdi4 = 9.97e-07_r8k
    REAL(r8k), PARAMETER :: almdi5 = 9.17e-06_r8k
    REAL(r8k), PARAMETER :: almdi6 = 8.37e-05_r8k
    REAL(r8k), PARAMETER :: almdi7 = 2.22e-04_r8k
    REAL(r8k), PARAMETER :: almdi8 = 2.87e-05_r8k
    REAL(r8k), PARAMETER :: almdc5 = 3.59e-04_r8k
    REAL(r8k), DIMENSION(5), INTENT(OUT) :: cs
    REAL(r8k), DIMENSION(8), INTENT(OUT) :: roid
    ! Units conversion
    b = buloc * 0.88_r8k
    fisr = delbu * 0.88_r8k / (1.732e10_r8k * dtime)
    ! 
    IF (ftemp > ftmax) ftmax = ftemp
    ! Find diffusion radius and diffusion constants
    a = 3.0_r8k * fraden * (10.0_r8k ** (20.61_r8k - fraden * (67.90_r8k - 46.0_r8k * fraden)))
    IF (ftmax <= 1134.054_r8k) THEN
        dmax = 1.0e-19_r8k
        dnow = 1.0e-19_r8k
    ELSE
        dmax = 6.6e-06_r8k / EXP(3.6086e4_r8k / ftmax)
        dnow = 6.6e-06_r8k / EXP(3.6086e4_r8k / ftemp)
    END IF
    ! Find escape rate coefficients 
    anui4 = 3.0_r8k * SQRT(dnow * almdi4) / a
    anui5 = 3.0_r8k * SQRT(dnow * almdi5) / a
    anui6 = 3.0_r8k * SQRT(dnow * almdi6) / a
    anui7 = 3.0_r8k * SQRT(dnow * almdi7) / a
    anui8 = 3.0_r8k * SQRT(dnow * almdi8) / a
    anuc5 = 3.0_r8k * SQRT(dnow * almdc5) / a
    ! Find specific release for long lived isotopes
    ! Check for fuel melt
    IF (ftmax >= ftmelt) THEN
        roid(2) = yuid2 * b
        roid(3) = yuid3 * b
        cs(2) = yucs2 * b
        cs(3) = yucs3 * b
        cs(4) = yucs4 * b
    ELSE
        ! Specific releases without melt follow
        ! Check to see If release fraction is greater than one
        f = 2.257_r8k * SQRT(dmax * time) / a - 1.5_r8k * dmax * time / (a ** 2)
        IF (f > 1.0_r8k) THEN
            roid(2) = yuid2 * b
            roid(3) = yuid3 * b
            cs(2) = yucs2 * b
            cs(3) = yucs3 * b
            cs(4) = yucs4 * b
        ELSE
            f = MAX(f, 0.0_r8k)
            roid(2) = yuid2 * b * f
            roid(3) = yuid3 * b * f
            cs(2) = yucs2 * b * f
            cs(3) = yucs3 * b * f
            cs(4) = yucs4 * b * f
        ENDIF
    ENDIF
    ! Find specific releases for short lived isotopes
    roid(4) = fisr * 131 * yuid4 * anui4 / ((anui4 + almdi4) * almdi4)
    roid(5) = fisr * 132 * yuid5 * anui5 / ((anui5 + almdi5) * almdi5)
    roid(6) = fisr * 133 * yuid6 * anui6 / ((anui6 + almdi6) * almdi6)
    roid(7) = fisr * 134 * yuid7 * anui7 / ((anui7 + almdi7) * almdi7)
    roid(8) = fisr * 135 * yuid8 * anui8 / ((anui8 + almdi8) * almdi8)
    cs(5) = fisr * 138 * yucs5 * anuc5 / ((anuc5 + almdc5) * almdc5)
    ! Calculate sums
    roid(1) = SUM(roid(2:UBOUND(roid,1)))
    cs(1) = SUM(cs(2:UBOUND(cs,1)))
    !
    END SUBROUTINE cesiod
!
END MODULE Cs_I

