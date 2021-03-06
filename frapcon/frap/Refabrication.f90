MODULE Refabrication_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit, gasmo, x, qf, totl, cpl, it, airin, an2in, argin, fgin, hein, &
      &                   kryin, xein, cpl, totl, deltaz, jmin, jmax, Spring
    USE RunProperties_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the refabrication parameters_frapcon
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/10/2014
    !
    INTEGER(ipk), TARGET :: irefab             ! Timestep to start using refabricated values (Default = 10,000)
    INTEGER(ipk), TARGET :: nrefab1            ! Lower axial node for refabrication
    INTEGER(ipk), TARGET :: nrefab2            ! Upper axial node for refabrication
    REAL(r8k), TARGET :: cplrefab              ! Refabricated upper plenum length
    REAL(r8k), TARGET :: vsrefab               ! Number of spring turns in refabricated upper plenum
    REAL(r8k), TARGET :: dspgrefab             ! New plenum spring coil diameter
    REAL(r8k), TARGET :: dspgwrefab            ! New plenum spring wire diameter
    REAL(r8k), TARGET :: fgpavrefab            ! Fill gas pressure at time step of refabrication
    REAL(r8k), TARGET :: airrefab              ! Fraction of air in refabricated rod
    REAL(r8k), TARGET :: n2refab               ! Fraction of nitrogen in refabricated rod
    REAL(r8k), TARGET :: arrefab               ! Fraction of argon in refabricated rod
    REAL(r8k), TARGET :: fgrefab               ! Fraction of fission gas in refabricated rod
    REAL(r8k), TARGET :: herefab               ! Fraction of helium in refabricated rod (Default = 1.0)
    REAL(r8k), TARGET :: krrefab               ! Fraction of krypton in refabricated rod
    REAL(r8k), TARGET :: xerefab               ! Fraction of xenon in refabricated rod
    !
    CONTAINS
    !
    SUBROUTINE RodRefabrication
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine writes out the information needed for fuel rod refabrication
    !>@author
    !> Ken Geelhood, PNNL & Ian Porter, NRC
    INTEGER(ipk) :: i
    !
    jmin = nrefab1 + 1
    jmax = nrefab2 + 1
    Spring%dspg = dspgrefab
    Spring%dspgw = dspgwrefab
    Spring%vs = vsrefab
    Spring%Vcold = Spring%vs * (pi * (Spring%dspgw / 2.0_r8k) ** 2) * pi * (Spring%dspg - Spring%dspgw)
    cpl = cplrefab
    totl = SUM(deltaz(jmin-1:jmax-1))
    !
    CALL pghead
    !
    WRITE (ounit, 221) jmin-1, jmax-1, totl, totl*ftom, cpl, cpl*intom*100.0_r8k, &
      & dspgrefab, dspgrefab*intom*100.0_r8k, dspgwrefab, dspgwrefab*intom*100.0_r8k, &
      & vsrefab, fgpavrefab, fgpavrefab*PSItoMPa, airrefab*100.0_r8k, n2refab*100.0_r8k, &
      & arrefab*100.0_r8k, fgrefab*100.0_r8k, herefab*100.0_r8k, krrefab*100.0_r8k, xerefab*100.0_r8k
221 FORMAT(133('x'),/,48x,'Refabrication of the Rod has Occurred',/,/, &
      & 48x,'Irradiation proceeding with nodes ',i3, ' to ', i3,/, &
      & 48x,'New stack length is                  ',f10.3,' ft (',f10.4,' m)',/, &
      & 48x,'New plenum length is                 ',f10.3,' in (',f10.4,' cm)',/, &
      & 48x,'New plenum spring coil diameter is   ',f10.3,' in (',f10.4,' cm)',/, &
      & 48x,'New plenum spring wire diameter is   ',f10.3,' in (',f10.4,' cm)',/, &
      & 48x,'New number of plenum spring turns is ',f10.2,/, &
      & 48x,'New fill gas pressure is             ',f9.2,' psi (',f9.2,' MPa)',/, &
      & 52x,'with relative composition:',/, &
      & 52x,'Air         - ',f6.2,'%',/, &
      & 52x,'Nitrogen    - ',f6.2,'%',/, &
      & 52x,'Argon       - ',f6.2,'%',/, &
      & 52x,'Fission Gas - ',f6.2,'%',/, &
      & 52x,'Helium      - ',f6.2,'%',/, &
      & 52x,'Krypton     - ',f6.2,'%',/, &
      & 52x,'Xenon       - ',f6.2,'%',/,/, &
      & 32x,'Axial power profiles are re-normalized over new length of rod and are shown below:',/,/)
    !
    CALL axhef2 (qf, x)
    !
    END SUBROUTINE RodRefabrication
    !
    !
    !
    SUBROUTINE UpdateRefabGases
    IMPLICIT NONE
    !>@brief
    !> This subroutine updates the gases in the rod when the refabrication timestep is reached
    !>@author
    !> Ian Porter, NRC and Ken Geelhood, PNNL
    !>@date
    !> 6/22/2014
    airin = gasmo(it-1) * airrefab
    an2in = gasmo(it-1) * n2refab
    argin = gasmo(it-1) * arrefab
    fgin = gasmo(it-1) * fgrefab
    hein = gasmo(it-1) * herefab
    kryin = gasmo(it-1) * krrefab
    xein = gasmo(it-1) * xerefab
    !
    END SUBROUTINE UpdateRefabGases
    !
    !
    !
    SUBROUTINE axhef2 (qf1, x1)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : nunits, na, deltaz, qaxnorm, iq, qend, WorkArray1, WorkArray2, WorkArray3, &
      &                   ounit, naxim, jn, jpeak, avgqi, fa, ir1, jjc, nt, jmin, jmax
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from setup and calculates the axial power shape factors
    !
    ! Input
    !
    ! deltaz - length of each axial increment (ft)
    ! fa     - axial hot channel factor - ratio of peak to ave power
    ! qf1    - pointwise axial heat flux normalization factors
    ! x1     - axial stations corresponding to qf1 entries (ft)
    ! iq     - axial power shape index
    !        = 0, user input profile
    !        = 1, chopped cosine profile used
    ! ir1    - number of axial increments plus one
    ! jjc    - total number of axial shapes
    ! jn     - number of points describing input axial shape
    ! na     - maximum number of axial nodes
    ! nt     - number of axial increments
    ! nunits - unit input index
    !        = 0, si units used
    !        = 1, british units used
    ! qend   - normalized heat flux at the top of the stack
    ! ftom   - feet to meters (0.3048)
    !
    ! Output
    !
    ! avgqi      - average of the q"s for each axial shape
    ! WorkArray1 - work array
    ! WorkArray2 - work array
    ! WorkArray3 - work array
    ! q          - unnormalized axial increment power factors
    ! jpeak      - peak power increment indicator
    !
    INTEGER(ipk) :: k, m, j, i, jnm, jt, l, ii, jn1, jn2, j1, j2, incr
    REAL(r8k) :: sumdeltaz, xca, qmax, ql, xl, qr, xr, alfz, csa, cos, cs1, fzc, sin, cs2, &
      &          dtrm, daf, rdaf, sha, af2, af1, hll, hal, sumq, xc, xcsi
    REAL(r8k), DIMENSION(:), INTENT(INOUT) :: qf1, x1
    REAL(r8k), DIMENSION(naxim) :: qf, x
    !
    qf = 0.0_r8k
    x = 0.0_r8k
    k = 0
    DO m = 1, jjc
        ! Selection based on Axial power Shape
        SELECT CASE (iq)
        CASE (0) ! User input Axial Power Profile
            jnm = jn(m)
            DO i = 1, jnm
                k = k + 1
                x(i) = x1(k)
                qf(i) = qf1(k)
            END DO
            xca = 0.0_r8k
            qmax = 0.0_r8k
            jt = 2
            ql = qf(1)
            xl = x(1)
            DO j = 2, ir1
                xca = xca + deltaz(j-1)
                qaxnorm(j-1,m) = 0.0_r8k
110             IF (xca < x(jt)) THEN
                    qr = qf(jt-1) + (xca - x(jt-1)) * (qf(jt) - qf(jt-1)) / (x(jt) - x(jt-1))
                    xr = xca
                ELSE
                    qr = qf(jt)
                    xr = x(jt)
                    IF (xr <= 0.0_r8k) xr = xl
                END IF
                qaxnorm(j-1,m) = qaxnorm(j-1,m) + (ql + qr) * (xr - xl) / 2.0_r8k
                ql = qr
                xl = xr
                jt = jt+1
                IF (x(jt-1) > 0.0_r8k) THEN
                    IF ((xca - x(jt-1)) > 0.0_r8k) GOTO 110
                    IF ((xca - x(jt-1)) < 0.0_r8k) jt = jt - 1
                END IF
                qaxnorm(j-1,m) = qaxnorm(j-1,m) / deltaz(j-1)
                IF (qaxnorm(j-1,m) > qmax) THEN
                    qmax = qaxnorm(j-1,m)
                    jpeak(m) = j
                END IF
            END DO
        CASE (1) ! Calculation of chopped cosine axial shape
            IF (fa < 1.5708_r8k) THEN
                alfz = -6.3676150_r8k + fa * (7.9078237_r8k + fa * (2.3783171_r8k + fa * (-4.8940147_r8k + fa * 1.4154868_r8k)))
                csa = cos(alfz)
                fzc = alfz / sin(alfz)
            ELSE
                alfz = -1996.04088_r8k + fa * (4312.13343_r8k + fa * (-3140.42333_r8k + fa * (663.485456_r8k + fa * &
                  &    (77.9300994_r8k + fa * (83.4741199_r8k + fa * (-82.2246027_r8k + fa * 15.4145631_r8k))))))
                csa = cos(alfz)
                cs1 = 1.0_r8k / (1.0_r8k - csa)
                fzc = alfz * (1.0_r8k - csa) / (sin(alfz) - alfz * csa)
                cs2 = fzc * csa * cs1
            END IF
            DO l = 1, ir1
                WorkArray3(l) = 0.0_r8k
                WorkArray2(l) = 0.0_r8k
            END DO
            dtrm = 1.0_r8k / nt
            daf = 2.0_r8k * alfz * dtrm
            rdaf = 1.0_r8k / daf
            sha = 0.0_r8k
            af2 = -alfz
            DO l = 1, nt
                af1 = af2
                af2 = af1 + daf
                hll = fzc * cos(af1)
                hal = fzc * (sin(af2) - sin(af1)) * rdaf
                IF (fa >= 1.5708_r8k) THEN
                    hll = cs1 * hll - cs2
                    hal = cs1 * hal - cs2
                END IF
                sha = sha + hal
                WorkArray3(l) = hll
                WorkArray2(l) = hal
            END DO
            WorkArray3(1) = MAX(WorkArray3(1), 0.0_r8k)
            WorkArray3(ir1) = MAX(fzc * csa, 0.0_r8k)
            WorkArray1(1) = 0.0_r8k
            DO l = 1, nt
                WorkArray1(l+1) = WorkArray1(l) + WorkArray2(l) * dtrm
            END DO
            qaxnorm(1,m) = WorkArray2(1) / fzc
            qmax = 0.0_r8k
            DO i = 2, ir1
                qaxnorm(i,m) = WorkArray2(i) / fzc
                IF (qaxnorm(i-1,m) > qmax) THEN
                    qmax = qaxnorm(i-1,m)
                    jpeak(m) = i
                END IF
            END DO
        END SELECT
        !
        sumq = 0.0_r8k
        sumdeltaz = 0.0_r8k
        DO i = jmin-1, jmax-1
            sumq = sumq + qaxnorm(i,m) * deltaz(i)
            sumdeltaz = sumdeltaz + deltaz(i)
        END DO
        avgqi(m) = sumq / sumdeltaz
        IF (iq <= 0) THEN
            DO i = 1, nt
                qaxnorm(i,m) = qaxnorm(i,m) / avgqi(m)
            END DO
            avgqi(m) = 1.0_r8k
        END IF
    END DO
    !
    WRITE (ounit,360)
    IF (iq == 0 .AND. fa == 1) WRITE (ounit,370) nt
    IF (nunits == 0) WRITE (ounit,390)
    IF (nunits == 1) WRITE (ounit,380)
    IF (iq == 0) THEN
        IF (nunits == 0) x1 = x1 * ftom
        jn1 = 1
        jn2 = jn(1)
        DO m = 1, jjc
            WRITE (ounit,400) m, jpeak(m)-1
            WRITE (ounit,410) (i,x1(i),i = jn1,jn2)
            WRITE (ounit,420) (i,qf1(i),i = jn1,jn2)
            IF (m < jjc) THEN
                jn1 = jn(m) + jn1
                jn2 = jn2 + jn(m+1)
            END IF
        END DO
        IF (nunits == 0) x1 = x1 * mtoft
        WRITE (ounit,440)
    ELSE
        WRITE (ounit,430) fa,nt
        WRITE (ounit,441)
    END IF
    WRITE (ounit,445)
    j1 = 1
    j2 = MIN(jjc, 8)
    xc = 0.0_r8k
    DO j = jmin-1, jmax-1
        IF (j == 1) THEN
            xc = deltaz(j) / 2.0_r8k
        ELSE
            xc = xc + (deltaz(j-1) + deltaz(j)) / 2.0_r8k
        END IF
        xcsi = xc * ftom
        incr = j
        WRITE (ounit,450) incr, xc, xcsi, (qaxnorm(j,m),m=j1,j2)
    END DO
    WRITE (ounit,460) (avgqi(m),m=j1,j2)
    WRITE (ounit,470) (qend(m),m=j1,j2)
    IF (jjc > 8) THEN
        j1 = 9
        j2 = MIN(jjc, 16)
        IF (iq == 0) THEN
            WRITE (ounit,440)
        ELSE
            WRITE (ounit,441)
        END IF
        WRITE (ounit,451)
        xc = 0.0_r8k
        DO j = jmin-1, jmax-1
            IF (j == 1) THEN
                xc = deltaz(j) / 2.0_r8k
            ELSE
                xc = xc + (deltaz(j-1) + deltaz(j)) / 2.0_r8k
            END IF
            xcsi = xc * ftom
            incr = j
            WRITE (ounit,450) incr, xc, xcsi, (qaxnorm(j,m),m=j1,j2)
        END DO
        WRITE (ounit,460) (avgqi(m),m=j1,j2)
        WRITE (ounit,470) (qend(m),m=j1,j2)
        IF (jjc > 16) THEN
            IF (iq == 0) THEN
                WRITE (ounit,440)
            ELSE
                WRITE (ounit,441)
            END IF
            WRITE (ounit,454)
            j1 = 17
            j2 = MIN(jjc, na)
            xc = 0.0_r8k
            DO j = jmin-1, jmax-1
                IF (j == 1) THEN
                    xc = deltaz(j) / 2.0_r8k
                ELSE
                    xc = xc + (deltaz(j-1) + deltaz(j)) / 2.0_r8k
                END IF
                xcsi = xc * ftom
                incr = j
                WRITE (ounit,450) incr, xc, xcsi, (qaxnorm(j,m),m=j1,j2)
            END DO
            WRITE (ounit,460) (avgqi(m),m=j1,j2)
            WRITE (ounit,470) (qend(m),m=j1,j2)
        END IF
    END IF
    !
360 FORMAT (35x,'xxxxxxxxxxxxxxx  axial power shape information  xxxxxxxxxxxxxxx')
370 FORMAT (/5x,'axial distribution is input',10x,'rod average power is input',10x,'number of axial nodes=',i3)
380 FORMAT (60x,"x's  in feet  ")
390 FORMAT (59x,"x's  in meters")
400 FORMAT (/10x,'xxxxxxxxxxxxxxxxxxxx  input axial shape number ',i3,'  xxxxxxxxxxxxxxxxxxxx peak node is',i3 /)
410 FORMAT (7(' x(',i4,')=',f8.4,1x))
420 FORMAT (7('qf(',i4,')=',f8.4,1x))
430 FORMAT (/5x,'axial distribution is chopped cosine function',5x, &
      &         'peak to average power ratio =',f7.4,5x,'number of increments =',i3)
440 FORMAT (/,3x,'increment     axial station             normalized',/,15x,'feet       meters          heat flux ')
441 FORMAT (/,3x,'increment     axial station           unnormalized',/,15x,'feet       meters          heat flux ')
445 FORMAT (46x,'1st',8x,'2nd',8x,'3rd',8x,'4th',8x,'5th',8x,'6th',8x,'7th',8x,'8th')
450 FORMAT (5x,i3,4x,f8.4,3x,f9.5,8x,8(3x,f8.4))
451 FORMAT (46x,'9th',8x,'10th',7x,'11th',7x,'12th',7x,'13th',7x,'14th',7x,'15th',7x,'16th')
454 FORMAT (46x,'17th',7x,'18th',7x,'19th',7x,'20th')
460 FORMAT (/,36x,'avgqi =',f8.4,7(3x,f8.4))
470 FORMAT (/,6x,'normalized heat flux at top of stack =',f7.4,7(4x,f7.4))
    !
    END SUBROUTINE axhef2
    !
END MODULE Refabrication_frapcon



