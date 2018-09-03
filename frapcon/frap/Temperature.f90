MODULE Temperature
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to calculate fuel and cladding 
    !> temperature.
    !> Subroutines include cldrp, conduc, energy, formfa, fueltp, tmpsub, emssf2, gaprs
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    PRIVATE :: formfa
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION formfa (fi, fo, ri, ro)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This function computes the fuel ring averaged form factor or burnup based upon 
    !> boundary weighting factors identical to those used in subroutine tubrnp.
    !
    ! Input
    !
    ! fi      - form factor (power factor) or burnup at the inner surface of the volume
    ! fo      - form factor (power factor) or burnup at the outer surface of the volume
    ! ri      - radius as measured from the fuel centerline to the inner surface of the fuel ring
    ! ro      - radius as measured from the fuel centerline to the outer surface of the fuel ring
    !
    ! Output
    !
    ! formfa   - volume average form factor (power factor)
    !
    ! Internal
    !
    ! winner  - weighting factor for fi (see Subroutine tubrnp)
    ! wouter  - weighting factor for fo (see Subroutine tubrnp)
    !
    ! Reference:
    !
    ! facsimile from dd lanning to kl davis, sept 8, 95
    !
    REAL(r8k) :: winner, wouter
    REAL(r8k), INTENT(IN) :: fi, fo, ri, ro
    !
    winner = (ro + 2.0_r8k * ri) / (3.0_r8k * (ro + ri))
    wouter = 1.0_r8k - winner
    formfa  = fi * winner + fo * wouter
    !
    END FUNCTION formfa
    !
    !
    !
    SUBROUTINE cladrp
    USE Kinds
    USE Conversions
    USE Variables, ONLY : CladAveTemp, qc, dco, dci, j, tci, tco, SurfTempOxide, EOSZrO2Thk, zoxk
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and computes the cladding inside surface temperature as well as 
    !> the cladding average temp.
    !>@author
    !> cladrp was coded by G A Berna, November 1977.
    !> Updted by Ian Porter, NRC, June 2015
    !
    ! Input
    !
    ! dci    - Cladding inside diameter (in)
    ! dco    - Cladding outside diameter (in)
    ! j      - Axial node index plus one
    ! qc     - Heat flux at node j (btu/hr-ft**2)
    ! CladAveTemp   - Cladding average temperature (F)
    ! EOSZrO2Thk    - ZrO2 thickness at end of power-time step (ft)
    ! SurfTempOxide - Oxide outer surface temperature (F)
    ! zoxk   - ZrO2 thermal conductivity (W/m-K)
    !
    ! Output
    !
    ! CladAveTemp - Cladding average temperature (F)
    ! tco         - Cladding outside surface temperature (F)
    ! tci         - Cladding inside surface temperature (F)
    !
    ! Internal
    !
    ! cthcon - Cladding thermal conductivity (W/m-K)
    ! tcak   - Average cladding temperature (K)
    !
    !
    REAL(r8k) :: tcak, cthcon
    !
    ! Calculate the cladding outer surface temperature, tco (F)
    tco = SurfTempOxide(j-1) + qc(j-1) * (dco(j-1) * intoft) * LOG((dco(j-1) * intoft + 2.0_r8k * EOSZrO2Thk(j-1)) &
      &   / (dco(j-1) * intoft)) / (2.0_r8k * zoxk)
    ! Set average cladding temperature, tcak (K)
    tcak = tfk(CladAveTemp(j-1))
    ! Get cladding thermal conductivity (W/m*K)
    cthcon = MatProp ('CLAD', 'THERMCOND', tcak)
    ! Cladding inner surface temperature, tci (F)
    tci = tco + qc(j-1) * (dco(j-1) * intoft) * LOG(dco(j-1) / dci(j-1)) / (2.0_r8k * cthcon * WmKtoBhftF)
    ! Cladding average temperature (assumes linear profile, F)
    CladAveTemp(j-1) = 0.5_r8k * (tci + tco)
    !
    END SUBROUTINE cladrp
    !
    !
    !
    SUBROUTINE conduc
    USE Kinds
    USE Conversions
    USE Variables, ONLY : GapAveTemp, dco, gpthe, qc, ounit, dltgc, hgapt, hgap, hsolid, &
      &                   hgapr, tci, k, dphf, dphfrl, j
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and computes gap conductance and gap temperature drop
    !>@author
    !> conduc was coded by g a berna in january 1978.
    !> conduc was changed at pnl in 1979
    !> conduc was modified by g a berna in April 1996.
    !
    ! Input
    !
    ! dco    - as fabricated cladding outside diameter (in)
    ! dltgc  - temperature drop across the gap (F)
    ! dphfrl - diameter of the pellet hot & swelled (in)
    ! j      - axial node index
    ! k      - gap iteration indicator
    ! qc     - heat flux at node j (btu/hr-ft**2)
    ! tci    - cladding inside surface temperature (F)
    !
    ! Output
    !
    ! dltgc  - temperature drop across the gap (F)
    !
    REAL(r8k) :: tpel, gapd
    !
    GapAveTemp(j-1) = tci + 0.5_r8k * dltgc(k-1)
    tpel = tci + dltgc(k-1)
    gapd = gpthe(k) / 24.0_r8k
    IF (gpthe(k) < 0.0_r8k) gapd = 0.0_r8k
    !
    CALL gaprs (gapd, tpel)
    !
    dltgc(k) = qc(j-1) * dco(j-1) / (hgapt * (dphfrl + gapd * 24.0_r8k))
    
    ! Check for error
    IF (dltgc(k) < 0.0_r8k) THEN
        WRITE (0,10) dltgc(k)
        WRITE (ounit,10)
10      FORMAT ('Error in conduc. dltgc < 0.0_r8k.',/,'dltgc = ',es14.7)
    ENDIF
    
    END SUBROUTINE conduc
    !
    !
    !
    SUBROUTINE energy
    USE Kinds
    USE Conversions
    USE Variables, ONLY : PelAveTemp, StoredEnergy, nr, tfring, hrad, j, tref
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> This routine computes the stored energy at the volumetric average temperature -tbar by summing the
    !> contributions of each radial ring in a given axial increment.
    !>@author
    !> This Subroutine is called from frpcon and was coded by g a berna
    !
    ! Input
    !
    ! hrad   - hot fuel ring radius (meters)
    ! j      - axial node index
    ! nr     - number of radial nodes
    ! tfring - fuel ring temperature (F)
    ! tref   - reference temperature upon which the stored energy is based (F)
    !
    ! Output
    !
    ! StoredEnergy - stored energy at tbar for axial increment j (joules/kg)
    ! tbar   - volumetric fuel average temperature at node j (F)
    !
    INTEGER(ipk) :: l
    REAL(r8k) :: tbar1, tbar2, sumenr, denom, vol, sumcpt, enthl_tbar2, enthl_tref, trefK
    !
    tbar1 = 0.0_r8k
    sumenr = 0.0_r8k
    denom = hrad(1,j-1) ** 2 - hrad(nr,j-1) ** 2
    DO l = 2, nr
        ! Convert to degrees k from f
        tbar2 = tfk(tfring(l-1))
        vol = (hrad(l-1,j-1) * hrad(l-1,j-1) - hrad(l,j-1) * hrad(l,j-1)) / denom
        ! Get enthalpy at current temperature
        enthl_tbar2 = MatProp ('FUEL', 'ENTHALPY', tbar2)
        ! Get reference enthalpy
        trefK = tfk(tref)
        enthl_tref = MatProp ('FUEL', 'ENTHALPY', trefK)
        sumcpt = enthl_tbar2 - enthl_tref
        sumenr = sumenr + sumcpt * vol
        tbar1 = tbar1 + tbar2 * vol
    END DO
    StoredEnergy(j-1) = sumenr
    PelAveTemp(j-1) = tkf(tbar1)
    !
    END SUBROUTINE energy
    !
    !
    !
    SUBROUTINE fueltp (tfs, q, fden)
    USE Kinds
    USE Conversions
    USE Material_Properties, ONLY : MatProp
    USE Variables, ONLY : brnup3, nr, rrev, rrapow, FuelCondFactor, tfuelr2, rtran, j
    USE Comde
    IMPLICIT NONE
    !>@brief
    !> This subroutine computes the steady state fuel temperature distribution using the finite difference approach found
    !> in INEL-95/0220. It is called by subroutine tmpsub.
    !
    ! Input
    !
    ! tfs     - fuel surface temperature (K)
    ! nr      - number of radial mesh points (see figure 4)
    ! rrev    - radial distance to each mesh point, and unlike the r array, node 1 is at the fuel
    !           center line or line of symmetry and node m is on the fuel surface (in)
    ! q       - volumetric heat generation rate (W/m^3) adjusted for axial power shape
    ! tfuelr2 - initial radial temperature array that contains initial temperature guesses used to lookup
    !           fuel thermal conductivity. It is also used to return calculated temperatures (K)
    ! fden    - input fuel density (ratio of actual density to theoretical density)
    ! rrapow  - rapow array reversed to match nodaliztion in fueltp
    ! gaph    - hot state unrelocated radial gap (m)
    ! gapt    - hot state relocated radial gap (m)
    ! rtran   - radius out to the region where cracks begin (m)
    ! brnup3  - three dimensional array (axial,radial,old/new) of local burnups, correlated to the radial boundaries (MWd/mtm).
    ! lschni  - axial node index from Subroutine turbo
    !
    ! Output
    !
    ! tfuelr2             - array containing the calculated steady state temperature profile (K)
    ! FuelCondFactor(j-1) - conductivity multiplier to account for cracking
    !
    ! Internal
    !
    ! i          - DO loop counter
    ! dellm(nr)  - array of radial distance between mesh points to the left of mesh point nr
    ! delrm(nr)  - array of radial distance between mesh points to the right of mesh point nr
    ! delslm(nr) - surface-gradient weight array left of nr (see equation 2)
    ! delsrm(nr) - surface-gradient weight array right of nr (see equation 2)
    ! delvrm(nr) - volume-gradient weight array right of nr (see equation 2)
    ! delvlm(nr) - volume-gradient weight array left of nr (see equation 2)
    ! qlm(nr)    - array, radial power factor left nr
    ! qrm(nr)    - array, radial power factor right nr
    ! tavelm(nr) - array, average temperature left nr (W/m*K)
    ! taverm(nr) - array, average temperature right nr (W/m*K)
    ! tconlm(nr) - array, thermal conductivity left nr (W/m*K)
    ! tconrm(nr) - array, thermal conductivity right nr (W/m*K)
    ! a(nr)      - coefficient array
    ! b(nr)      - coefficient array
    ! c(nr)      - coefficient array
    ! d(nr)      - coefficient array
    ! e(nr)      - coefficient array
    ! f(nr)      - coefficient array
    ! nfac       - the node where the fuel cracking begins
    ! bpave      - average burnup for a given ring at axial node j (MWd/mtu)
    !
    ! **maximum # of mesh points set by nr**
    !
    INTEGER(ipk) :: i, nfac
    REAL(r8k) :: tlkf, ftemp, bpave
    REAL(r8k), INTENT(IN) :: tfs, q, fden
    REAL(r8k), DIMENSION(nr) :: dellm, delrm, delslm, delsrm, delvrm, delvlm, qlm, qrm, taverm, &
      &                         tavelm, tconrm, tconlm, a, b, c, d, e, f
    ! Determine the node where fuel cracking begins
    DO i = 1, nr
        IF ((rrev(i,j-1) * intom) >= rtran) THEN
            nfac = i
            EXIT
        END IF
    END DO
    ! Calculate the qrm (rrapow is from tubrnp) (start with equation 5)
    qrm(1) = formfa(rrapow(1),rrapow(2),rrev(1,j-1),rrev(2,j-1))
    DO i = 2, (nr - 1)
        qrm(i) = formfa(rrapow(i),rrapow(i+1),rrev(i,j-1),rrev(i+1,j-1))
    END DO
    qrm(nr) = 0.0_r8k
    ! Calculate qlm (start with equation 5)
    qlm(1) = 0.0_r8k
    DO i = 2, nr
        qlm(i) = qrm(i-1)
    END DO
    ! Calculate dellm and delrm arrays
    dellm(1) = 0.0_r8k
    delrm(1) = (rrev(2,j-1) - rrev(1,j-1)) * intom
    DO i = 2, (nr - 1)
        dellm(i) = (rrev(i,j-1) - rrev(i-1,j-1)) * intom
        delrm(i) = (rrev(i+1,j-1) - rrev(i,j-1)) * intom
    END DO
    dellm(nr) = (rrev(nr,j-1) - rrev(nr-1,j-1)) * intom
    delrm(nr) = 0.0_r8k
    ! Calculate delsrm
    delsrm(1) = ((2.0_r8k * pi) / delrm(1)) * (rrev(1,j-1) * intom + delrm(1) / 2.0_r8k)
    DO i = 2, (nr-1)
        delsrm(i) = ((2.0_r8k * pi) / delrm(i)) * (rrev(i,j-1) * intom + delrm(i) / 2.0_r8k)
    END DO
    delsrm(nr) = 0.0_r8k
    ! Calculate delslm
    delslm(1) = 0.0_r8k
    DO i = 2, nr
        delslm(i) = delsrm(i-1)
    END DO
    ! Calculate the delvrm (see equation 2)
    delvrm(1) = (pi * delrm(1)) * (rrev(1,j-1) * intom + delrm(1) / 4.0_r8k)
    DO i = 2, (nr - 1)
        delvrm(i) = (pi * delrm(i)) * (rrev(i,j-1) * intom + delrm(i) / 4.0_r8k)
    END DO
    delsrm(nr) = 0.0_r8k
    ! Calculate delvlm (see equation 2)
    delvlm(1) = 0.0_r8k
    DO i = 2, nr
        delvlm(i) = (pi * dellm(i)) * (rrev(i,j-1) * intom - dellm(i) / 4.0_r8k)
    END DO
    ! Calculate average temperature arrays
    taverm(nr) = 0.0_r8k
    DO i = 1, (nr - 1)
        taverm(i) = (tfuelr2(i) + tfuelr2(i+1)) / 2.0_r8k
    END DO
    tavelm(1) = 0.0_r8k
    DO i = 2, nr
        tavelm(i) = (tfuelr2(i-1) + tfuelr2(i)) / 2.0_r8k
    END DO
    ! Setup thermal conductivity arrays
    tconrm(nr) = 0.0_r8k
    DO i = 1, (nr - 1)
        ftemp = taverm(i)
        bpave = formfa(brnup3(lschni,i,1),brnup3(lschni,i+1,1),rrev(i,j-1),rrev(i+1,j-1))
        ! Call Fuel Thermal Conductivity
        tconrm(i) = MatProp ('FUEL', 'THERMCOND', ftemp, bpave)
    END DO
    tconlm(1) = 0.0_r8k
    DO i = 2, nr
        tconlm(i) = tconrm(i-1)
    END DO
    ! Calculate the coefficient arrays (see equations 16 - 18)
    ! Coefficients for mesh point 1
    d(1) = q * qrm(1) * delvrm(1)
    c(1) = -tconrm(1) * delsrm(1)
    b(1) = -c(1)
    a(1) = 0.0_r8k
    ! Coefficients for mesh point nr
    DO i = 2, (nr-1)
        d(i) = q * (qlm(i) * delvlm(i) + qrm(i) * delvrm(i))
        c(i) = -(tconrm(i) * delsrm(i))
        a(i) = -tconlm(i) * delslm(i)
        b(i) = -a(i) - c(i)
    END DO
    ! Gaussian arrays (equations 26 & 27)
    e(1) = c(1) / b(1)
    f(1) = d(1) / b(1)
    DO i = 2, (nr - 1)
        e(i) = c(i) / (b(i) - a(i) * e(i-1))
        f(i) = (d(i) - a(i) * f(i-1)) / (b(i) - a(i) * e(i-1))
    END DO
    ! Temperature profile calculated (equation 29)
    tfuelr2(nr) = tfs
    DO i = (nr - 1), 1, -1
        tfuelr2(i) = -e(i) * tfuelr2(i+1) + f(i)
    END DO
    !
    END SUBROUTINE fueltp
    !
    !
    !
    SUBROUTINE tmpsub
    USE Kinds
    USE Conversions
    USE Variables, ONLY : modheat, FuelTempRestruRad, HealedCrackRadius, OldHealedCrackRadius, porosnew, &
      &                   tfuelr, rapow, nr, na, crad, Power, rc, PelSurfTemp, rrev, rrapow, tfuelr2, dltgc, &
      &                   PelSurfTemp, PelCentTemp, it, j, iquit, rp, transt, rtran, k, tci, fden
    IMPLICIT NONE
    !>@brief
    !> This subroutine computes the fuel temperature distribution.
    !>@author
    !> Coded by g a berna in april '78
    !> Modified by k l davis, august '95 to use the Subroutine fueltp to calculate the fuel temperature profile
    !
    ! Input
    !
    ! crad                 - cold state radii of fuel radial nodes (in)
    ! tfuelr               - fuel temperature array for this axial node (j)
    ! j                    - axial node index
    ! it                   - power-time step index
    ! na                   - number of axial nodes
    ! nr                   - number of radial nodes
    ! pi                   - 3.14159...
    ! porosnew             - linear porosity fraction of the fuel
    ! Power                - power  (kW/ft)
    ! r                    - fuel node radii (in)
    ! rrev                 - reversed node numbering of r (in)
    ! rp                   - radius of the pellet (in)
    ! rc                   - pellet core radius (in)
    ! OldHealedCrackRadius - maximum previous value of rtran attained (m)
    ! transt               - temperature above which no cracking is assumed (K) 80% of tsint
    ! PelSurfTemp          - fuel surface temperature (F)
    ! PelCentTemp          - fuel centerline temperature (F)
    ! rapow                - normalized radial power profile array
    ! rrapow               - rapow array reversed to match nodaliztion in fueltp
    !
    ! Output
    !
    ! iquit             - code stop index
    ! FuelCondFactor    - fracas relocation crack factor
    ! rtran             - radius out to the region where cracks begin (m)
    ! HealedCrackRadius - storage for axial values of rtran (m)
    ! tfuelr            - fuel node temperatures (F)
    ! FuelTempRestruRad - temperature at the restructured radius, rtran (F)
    !
    ! Internal
    !
    ! i          - DO loop counter
    ! dellm(nr)  - array of radial distance between mesh points to the left of mesh point nr
    ! delrm(nr)  - array of radial distance between mesh points to the right of mesh point nr
    ! delslm(nr) - surface-gradient weight array left of nr (see equation 2)
    ! delsrm(nr) - surface-gradient weight array right of nr (see equation 2)
    ! delvrm(nr) - volume-gradient weight array right of nr (see equation 2)
    ! delvlm(nr) - volume-gradient weight array left of nr (see equation 2)
    ! qlm(nr)    - array, radial power factor left nr
    ! qrm(nr)    - array, radial power factor right nr
    ! tavelm(nr) - array, average temperature left nr (W/m*K)
    ! taverm(nr) - array, average temperature right nr (W/m*K)
    ! tconlm(nr) - array, thermal conductivity left nr (W/m*K)
    ! tconrm(nr) - array, thermal conductivity right nr (W/m*K)
    ! a(nr)      - coefficient array
    ! b(nr)      - coefficient array
    ! c(nr)      - coefficient array
    ! d(nr)      - coefficient array
    ! e(nr)      - coefficient array
    ! f(nr)      - coefficient array
    ! nfac       - the node where the fuel cracking begins
    ! bpave      - average burnup for a given ring at axial node j (MWd/mtu)
    !
    INTEGER(ipk) :: i, nfac
    REAL(r8k) :: tfsK, rps, rii, rid, rd, q
    !
    PelSurfTemp(j-1) = tci + dltgc(k)
    ! Units conversion, inches to meter
    rii = rc(j-1) * intom
    rd = rp * intom
    rid = rii / rd
    ! Convert F to K
    tfsK = tfk(PelSurfTemp(j-1))
    ! The min value of rtran is OldHealedCrackRadius
    IF (OldHealedCrackRadius(j-1) > rtran) rtran = OldHealedCrackRadius(j-1)
    ! The following computes the average fractional density of the fuel at an axial node considering
    ! initial density and fuel densification
    fden(j-1) = 0.0_r8k
    rps = rp ** 2 - rc(j-1) ** 2
    DO i = 2, nr
        fden(j-1) = fden(j-1) + porosnew(i-1, j-1) * (crad(i-1, j-1) ** 2 - crad(i, j-1) ** 2) / rps
    END DO
    ! Assume isotropic
    fden(j-1) = 1.0_r8k - 3.0_r8k * fden(j-1)
    ! Radial nodalization is reversed in Subroutine fueltp converts r in inches to rrev in meters
    DO i = 1, nr
        rrapow(nr-(i-1)) = rapow(i, j-1)
    END DO
    ! Calculate volumetric heat generation. Axial power factor taken into account in pow
    ! Modified by IP to allow for a reduction in the volumetric heat generation value due to direct moderator heating.
    q = Power(j-1) * kWfttoWm / (pi * ((rrev(nr, j-1) * intom) ** 2 - (rrev(1, j-1) * intom) ** 2)) * (1.0_r8k - modheat)
    ! Rhe call to fueltp computes the temperature profile in the fuel
    CALL fueltp (tfsK, q, fden(j-1))
    ! Set the tfuelr array
    ! Also set the ttt array used in fracas II
    DO i = 1, nr
        tfuelr(nr-(i-1)) = tkf(tfuelr2(i))
    END DO
    ! Determine the restructured radius and temperature
    ! nfac is the node where fuel cracking begins
    nfac = -1
    DO i = 1, nr
        IF (tfuelr2(i) <= transt) THEN
            nfac = i
            EXIT
        END IF
    END DO
    IF (nfac == -1) THEN
        nfac = nr
        i = nr
    END IF
    rtran = rrev(nfac,j-1) * intom
    HealedCrackRadius(j-1) = rtran
    FuelTempRestruRad(j-1) = tkf(tfuelr2(i))
    PelCentTemp(j-1) = tfuelr(nr)
    !
    END SUBROUTINE tmpsub
    !
    !
    !
    SUBROUTINE emssf2 (tf, tc, rf, gpthk, fe)
    USE Kinds
    USE Conversions
    USE Material_Properties
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes emissivity factor for fuel-cladding gap radiation heat transfer calculations.
    !
    ! Input
    !
    ! tf     - temperature of outside surface of fuel (F)
    ! tc     - temperature of inside surface of cladding (F)
    ! rf     - radius of outside surface of fuel (ft)
    ! gpthk  - thickness of fuel-cladding gap (ft)
    !
    ! Output
    !
    ! fe     - to be computed emissivity factor
    !
    ! Internal
    !
    ! emissf - fuel emissivity
    ! emissc - clad emissivity
    !
    REAL(r8k) :: rc, emissf, emissc, tempK
    REAL(r8k), INTENT(IN) :: tf, tc, rf, gpthk
    REAL(r8k), INTENT(OUT) :: fe
    !
    ! Convert fuel temperature from f to k
    tempK = tfk(tf)
    ! Compute fuel emissivity
    emissf = MatProp ('FUEL', 'EMISS', tempK)
    ! Convert cladding temperature from f to k
    tempK = tfk(tc)
    ! Compute cladding emissivity
    emissc = MatProp ('CLAD', 'EMISS', tempK)
    !
    rc = rf + gpthk
    fe = 1.0_r8k / (1.0_r8k / emissf + (rf / rc) * (1.0_r8k / emissc - 1.0_r8k))
    !
    END SUBROUTINE emssf2
    !
    !
    !
    SUBROUTINE gaprs (gapd, tpel)
    USE Kinds
    USE Conversions
    USE Material_Properties
    USE Variables, ONLY : brnup3, gaphtcmult, nr, RinterfacPress, GapAveTemp, &
      &                   gases, j, gapmin, hgap, hgapr, hsolid, frden, hgapt, coldwk, &
      &                   roughc, roughf, dphfrl, j, press, tci, it, fden, & 
                          TotalHGap, hgapt_flag     ! YU JIANKAI ! TotalHGap, hgapt_flag
    USE Comde
    USE Gas, ONLY : ngases, ncGasProperties
    IMPLICIT NONE
    !>@brief
    !> This Subroutine computes gap conductance accounting for radiation heat transfer across gas gap and gap thickness change.
    !
    ! Input
    !
    ! coldwk         - cladding cold work
    ! frden          - ratio of fuel density to theoretical density
    ! gases          - absolute mole fractions
    !              Gas index number key
    !              1 = helium
    !              2 = argon
    !              3 = krypton
    !              4 = xenon
    !              5 = hydrogen
    !              6 = nitrogen
    !              7 = air
    !              8 = water vapor
    ! gapmin         - minimum diameteral gap thickness based on roughness (in)
    ! gaphtcmult     - gap conductance multiplier (user supplied, > 0. Default = 1.0)
    ! gapd           - gas gap thickness including roughness adjustment(radial ft)
    ! nr             - number of radial nodes in the fuel pellet
    ! RinterfacPress - pellet-cladding interfacial pressure (psia)
    ! press          - pressure of gas in gas gap (psia)
    ! dphfrl         - diameter to outside surface of fuel (in)
    ! roughc         - arithmetic mean roughness height of cladding (in)
    ! roughf         - arithmetic mean roughness height of fuel (in)
    ! tci            - temperature of inside surface of cladding (F)
    ! tc_rank        - clad temperature (Rankine)
    ! tf_rank        - fuel temperature (Rankine)
    ! tpel           - temperature of outside surface of fuel (F)
    ! GapAveTemp     - temperature of gas in gap (F)
    !
    ! Output
    !
    ! hgap           - Conduction contribution to conductance (btu/hr-ft**2-F)
    ! hgapr          - Radiation contribution to conductance (btu/hr-ft**2-F)
    ! hsolid         - Contact contribution to conductance (btu/hr-ft**2-F)
    ! hgapt          - Total gap conductance (btu/hr-ft**2-F)
    !
    INTEGER(ipk) :: i
    REAL(r8k) :: tgk, gpcon, fa, gpthm, fe, tf_rank, tc_rank, ahe, axe, slop, &
      &          rint, sum, adr, ags, djump, djmpft, tfsi, tcsi, bupp, conc, conf, &
      &          fkm, hmeyer, pfc1, cee, thc, testr, prel, ruffm, rufcm, r, rmult
    REAL(r8k), INTENT(IN) :: gapd, tpel
    REAL(r8k), PARAMETER :: sbc = 0.4806e-12_r8k ! Stefan-Boltzman constant
    REAL(r8k), PARAMETER :: thkmin = 1.0e-10_r8k
    !
    ! Convert gas gap temperature from F to K
    tgk = tfk(GapAveTemp(j-1))
    ! Compute conductivity of gas in gas gap and convert from W/m-K to btu/sec/(ft-F)
    gpcon = MatProp ('GAS', 'THERMCOND', tgk) * WmKtoBsftF
    ! Compute radiation heat transfer across gas gap. transform to an effective conductivity
    ! Assume configuration factor equals 1
    fa = 1.0_r8k
    gpthm = gapd
    IF (gpthm < thkmin) gpthm = thkmin
    ! Compute emissivity factor fe
    CALL emssf2 (tpel, tci, dphfrl / 24.0_r8k, gpthm, fe)
    ! Convert temperature to rankine
    tf_rank = tfr(tpel)
    tc_rank = tfr(tci)
    hgapr = sbc * fa * fe * ((tf_rank ** 2) + (tc_rank ** 2)) * (tf_rank + tc_rank)
    hgapr = hgapr * hrtosec
    ! Compute temperature jump distance,using recommended form in report bnwl 1984
    ahe = 0.425_r8k - 2.3e-4_r8k * tgk
    axe = 0.749_r8k - 2.5e-4_r8k * tgk
    slop = (axe - ahe) / 127.0_r8k
    rint = ahe - 4.0_r8k * slop
    sum = 0.0_r8k
    DO i = 1, ngases
        adr = SQRT(ncGasProperties(i)%MWt)
        ags = slop * ncGasProperties(i)%MWt + rint
        sum = sum + ags * gases(i) / adr
    END DO
    djump = 2.23_r8k * (gpcon * SQRT(tgk) / press) / sum
    ! Convert djump from cm to ft
    djmpft = djump * cmtoft
    ! Check for gap closure
    IF ((gapd <= gapmin / 24.0_r8k) .OR. RinterfacPress(j-1) > 0.0_r8k) THEN
        ! *** Closed gap ***
        ! Conductivity computed by formula shown on page a-10 of bnwl-1778
        ! Convert fuel and cladding surface temperatures from F to K
        tfsi = tfk(tpel)
        tcsi = tfk(tci)
        ! Use the burnup at the fuel surface from Subroutine tubrnp
        bupp = brnup3(lschni,nr,1)
        ! Set Cladding Thermal Conductivity
        conc = MatProp ('CLAD', 'THERMCOND', tcsi)
        ! Set Fuel Thermal Conductivity
        ! Modified by IP, 2/21/2015 to use fden rather than frden
        ! The fuel density used for calculating thermal conductivity was based off of as-fabricated conditions
        ! but is now calculated based off of current conditions. This is how its calculated in subroutine tmpsub
        ! End of modification
        conf = MatProp ('FUEL', 'THERMCOND', tfsi, bupp)
        ! Formula in report bnwl-1984
        ! fkm is harmonic mean conductivity in W/m-K
        fkm = 2.0_r8k * conf * conc / (conf + conc)
        ! Meyer's Hardness
        hmeyer = MatProp ('CLAD', 'MEYER_HARD', tcsi) * PatoPSI
        ! Convert interfacial pressure from psi to kg/cm**2
        pfc1 = RinterfacPress(j-1) * PSItokgcm2
        ! The change from 1.8 to 1.0 multiplier goes with change to gapmin in F
        cee = 1.0_r8k * EXP(-0.00125_r8k * pfc1)
        ! 
        thc = cee * (roughf + roughc) + djump * cmtoin - 5.5e-5_r8k
        thc = MAX(thc, gapmin * 0.5_r8k)
        testr = 0.0_r8k
        prel = MAX(RinterfacPress(j-1) / hmeyer, testr)
        ! Convert fuel (roughf) and cladding (roughc) roughness to meters
        ruffm = roughf * intom
        rufcm = roughc * intom
        ! r = (roughc**2+roughf**2)**0.5
        r = SQRT(rufcm ** 2 + ruffm ** 2)
        ! Find hsolid (W/m^2-K)
        IF (prel > 0.0087_r8k) THEN
            rmult = 2.9_r8k
            hsolid = 0.4167_r8k * fkm * prel / (r * EXP(5.738_r8k - 0.528_r8k * LOG(ruffm * 3.937e7_r8k))) * rmult
        ELSE IF (prel > 0.003_r8k) THEN
            rmult = 333.3_r8k * prel
            hsolid = 0.4167_r8k * fkm * prel / (r * EXP(5.738_r8k - 0.528_r8k * LOG(ruffm * 3.937e7_r8k))) * rmult
        ELSE IF (prel >= 9.0e-6_r8k) THEN
            hsolid = 0.00125_r8k * fkm / (r * EXP(5.738_r8k - 0.528_r8k * LOG(ruffm * 3.937e7_r8k)))
        ELSE
            hsolid = 0.4167_r8k * fkm * SQRT(prel) / (r * EXP(5.738_r8k - 0.528_r8k * LOG(ruffm * 3.937e7_r8k)))
        END IF
        ! Convert hsolid from W/(m^2*K)to Btu/hr/(ft^2*F)
        hsolid = hsolid * Wm2KtoBhft2F
        hgap = gpcon / (thc * intoft)
        hgap = hgap * hrtosec
        hgapt = hgap + hsolid + hgapr
    ELSE
        ! *** Open Gap ***
        hgap = gpcon / (gapd + djmpft)
        hgap = hgap * hrtosec
        hgapt = hgap + hgapr
    END IF
    ! Apply gap HTC mulitplier
    hgapt = hgapt * gaphtcmult
    hsolid = hsolid * gaphtcmult
    hgapr = hgapr * gaphtcmult
    hgap = hgap * gaphtcmult
    !
	! modified by YU JIANKAI
    if(hgapt_flag) then
       hgapt = TotalHgap(j-1)
       hgap = hgapt - hgapr - hsolid 
    end if
    ! 
    END SUBROUTINE gaprs
    !
END MODULE Temperature