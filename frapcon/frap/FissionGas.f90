MODULE FissionGas
    USE Kinds
    USE Conversions
    USE Functions, ONLY : terp
    USE Variables, ONLY : ounit, na, rc, ngasr
    USE Material_Properties
    IMPLICIT NONE
    !>@brief
    !> This module contains the fission gas release subroutines
    !
    ! IFBA Variables
    REAL(r8k), SAVE :: B_atoms, ZrB2_MolarMass
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Vol_ZrB2
    REAL(r8k), PARAMETER :: ZrB2_TDen = 6.08_r8k !g/cm^3
    ! Variables used only in subroutine ANS54
    TYPE ANS_54
        REAL(r8k), POINTER :: pfave => NULL()
        REAL(r8k), DIMENSION(:), POINTER :: decay => NULL()
        REAL(r8k), DIMENSION(:), POINTER :: half => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: pf => NULL()
    END TYPE ANS_54
    !
    ! Variables used only in subroutine FRAPFGR
    TYPE FRAP_FGR
        INTEGER(ipk), DIMENSION(:,:,:), POINTER :: openp => NULL()
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: rimthick
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: restructure
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: porosity
    END TYPE FRAP_FGR
    !
    TYPE SCANAIR
        REAL(r8k), DIMENSION(:,:), POINTER :: conGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: rdGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: intraGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: interGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: resolGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: proGas => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: relGas => NULL()
    END TYPE SCANAIR
    !
    TYPE FGRModels
        TYPE (ANS_54) :: ANS54
        TYPE (FRAP_FGR) :: FRAPFGR
        TYPE (SCANAIR) :: SCAN
        REAL(r8k), DIMENSION(:), POINTER :: ansr => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: gp => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: gpold => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: rls => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: rlsold => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: brn => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: brnrefab => NULL()
        REAL(r8k), DIMENSION(:), POINTER :: EOSNodeburnuprefab => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: brnold => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: grnold => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: frdenold => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: gbtot => NULL()
        REAL(r8k), DIMENSION(:,:), POINTER :: tempkold => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: gb => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: grs => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: gg => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: gbold => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: ggold => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: grsold => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g1 => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g2 => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g3 => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g4 => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g1old => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g2old => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g3old => NULL()
        REAL(r8k), DIMENSION(:,:,:), POINTER :: g4old => NULL()
    END TYPE FGRModels
    !
    TYPE (FGRModels), SAVE, TARGET :: FGRData
    !
    LOGICAL, PRIVATE :: FGRAllocated = .FALSE.
    !
    REAL(r8k), DIMENSION(:,:), POINTER, PRIVATE :: gp, gpold, rls, rlsold, brn, brnrefab, brnold, grnold, &
      &                                            frdenold, gbtot, tempkold, conGas, rdGas, intraGas, &
      &                                            interGas, resolGas, proGas, relGas
    REAL(r8k), DIMENSION(:), POINTER, PRIVATE :: EOSNodeburnuprefab
    REAL(r8k), DIMENSION(:,:,:), POINTER, PRIVATE :: gb, grs, gg, gbold, ggold, grsold, g1, g2, g3, &
      &                                              g4, g1old, g2old, g3old, g4old
    !
    CONTAINS
    !
    SUBROUTINE ALLOCATE_FGR_Variables
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ngasr, na
    IMPLICIT NONE
    !>@brief
    !> This subroutine allocates the variables used for fission gas release calculations
    !>@author
    !> Ian Porter, NRC
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> 9/1/2014
    !
    ! Scalars
    ALLOCATE (FGRData%ANS54%pfave)
    ! ngasr
    ALLOCATE (FGRData%ansr(1:ngasr))
    ! ngasr x na
    ALLOCATE (FGRData%gp(1:ngasr,1:na))
    ALLOCATE (FGRData%gpold(1:ngasr,1:na))
    ALLOCATE (FGRData%rls(1:ngasr,1:na))
    ALLOCATE (FGRData%rlsold(1:ngasr,1:na))
    ALLOCATE (FGRData%brn(1:ngasr,1:na))
    ALLOCATE (FGRData%brnrefab(1:ngasr,1:na))
    ALLOCATE (FGRData%EOSNodeburnuprefab(1:na))
    ALLOCATE (FGRData%brnold(1:ngasr,1:na))
    ALLOCATE (FGRData%grnold(1:ngasr,1:na))
    ALLOCATE (FGRData%frdenold(1:ngasr,1:na))
    ALLOCATE (FGRData%gbtot(1:ngasr,1:na))
    ALLOCATE (FGRData%tempkold(1:ngasr,1:na))
    ALLOCATE (FGRData%ANS54%pf(1:ngasr,1:na))
    ALLOCATE (FGRData%ANS54%decay(1:11))
    ALLOCATE (FGRData%ANS54%half(1:11))
    ALLOCATE (FGRData%SCAN%conGas(1:ngasr,1:na))
    ALLOCATE (FGRData%SCAN%rdGas(1:ngasr+1,1:na))
    ALLOCATE (FGRData%SCAN%intraGas(1:ngasr,1:na))
    ALLOCATE (FGRData%SCAN%interGas(1:ngasr,1:na))
    ALLOCATE (FGRData%SCAN%resolGas(1:ngasr,1:na))
    ALLOCATE (FGRData%SCAN%proGas(1:ngasr,1:na))
    ALLOCATE (FGRData%SCAN%relGas(1:ngasr,1:na))
    ! ngasr x na x 2
    ALLOCATE (FGRData%FRAPFGR%openp(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%FRAPFGR%rimthick(1:na))
    ALLOCATE (FGRData%FRAPFGR%restructure(1:na,1:ngasr))
    ALLOCATE (FGRData%FRAPFGR%porosity(1:na,1:ngasr))
    ALLOCATE (FGRData%gb(1:ngasr,1:na,1:3))
    ALLOCATE (FGRData%grs(1:ngasr,1:na,1:3))
    ALLOCATE (FGRData%gg(1:ngasr,1:na,1:3))
    ALLOCATE (FGRData%gbold(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%ggold(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%grsold(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g1(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g2(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g3(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g4(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g1old(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g2old(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g3old(1:ngasr,1:na,1:2))
    ALLOCATE (FGRData%g4old(1:ngasr,1:na,1:2))
    !
    ! Declare values to 0.0
    FGRData%ansr = 0.0_r8k
    FGRData%gp = 0.0_r8k
    FGRData%gpold = 0.0_r8k
    FGRData%rls = 0.0_r8k
    FGRData%rlsold = 0.0_r8k
    FGRData%brn = 0.0_r8k
    FGRData%brnrefab = 0.0_r8k
    FGRData%EOSNodeburnuprefab = 0.0_r8k
    FGRData%brnold = 0.0_r8k
    FGRData%grnold = 0.0_r8k
    FGRData%frdenold = 0.0_r8k
    FGRData%gbtot = 0.0_r8k
    FGRData%tempkold = 0.0_r8k
    FGRData%FRAPFGR%openp = 0
    FGRData%FRAPFGR%rimthick = 0.0_r8k
    FGRData%FRAPFGR%restructure = 0.0_r8k
    FGRData%FRAPFGR%porosity = 0.0_r8k
    FGRData%gb = 0.0_r8k
    FGRData%grs = 0.0_r8k
    FGRData%gg = 0.0_r8k
    FGRData%gbold = 0.0_r8k
    FGRData%ggold = 0.0_r8k
    FGRData%grsold = 0.0_r8k
    FGRData%g1 = 0.0_r8k
    FGRData%g2 = 0.0_r8k
    FGRData%g3 = 0.0_r8k
    FGRData%g4 = 0.0_r8k
    FGRData%g1old = 0.0_r8k
    FGRData%g2old = 0.0_r8k
    FGRData%g3old = 0.0_r8k
    FGRData%g4old = 0.0_r8k
    FGRData%ANS54%half = 0.0_r8k
    FGRData%ANS54%pfave = 0.0_r8k
    FGRData%ANS54%pf = 0.0_r8k
    FGRData%ANS54%decay = 0.0_r8k
    !
    FGRData%SCAN%conGas = 0.0_r8k
    FGRData%SCAN%rdGas = 0.0_r8k
    FGRData%SCAN%intraGas = 0.0_r8k
    FGRData%SCAN%interGas = 0.0_r8k
    FGRData%SCAN%resolGas = 0.0_r8k
    FGRData%SCAN%proGas = 0.0_r8k
    FGRData%SCAN%relGas = 0.0_r8k
    ! Assign local pointers used only in module FissionGas
    gp => FGRData%gp
    gpold => FGRData%gpold
    rls => FGRData%rls
    rlsold => FGRData%rlsold
    brn => FGRData%brn
    brnrefab => FGRData%brnrefab
    EOSNodeburnuprefab => FGRData%EOSNodeburnuprefab
    brnold => FGRData%brnold
    grnold => FGRData%grnold
    frdenold => FGRData%frdenold
    gbtot => FGRData%gbtot
    tempkold => FGRData%tempkold
    gb => FGRData%gb
    grs => FGRData%grs
    gg => FGRData%gg
    gbold => FGRData%gbold
    ggold => FGRData%ggold
    grsold => FGRData%grsold
    g1 => FGRData%g1
    g2 => FGRData%g2
    g3 => FGRData%g3
    g4 => FGRData%g4
    g1old => FGRData%g1old
    g2old => FGRData%g2old
    g3old => FGRData%g3old
    g4old => FGRData%g4old
    conGas => FGRData%SCAN%conGas
    rdGas => FGRData%SCAN%rdGas
    intraGas => FGRData%SCAN%intraGas
    interGas => FGRData%SCAN%interGas
    resolGas => FGRData%SCAN%resolGas
    proGas => FGRData%SCAN%proGas
    relGAs => FGRData%SCAN%relGas
    !
    FGRAllocated = .TRUE.
    !
    END SUBROUTINE ALLOCATE_FGR_Variables
    !
    !
    !
    SUBROUTINE gaspro
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, imox, comp, moxtype, EOSNodeburnup, dp, na, fgmgp, hemgp, &
      &                   deltaz, qc, rc, it, j, frden, sgapf, dcoBOL
    USE Comde
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and computes the fission gas and helium production.
    !>@author
    !> This routine was coded by g a berna in december 1977.
    !
    ! Input
    !
    ! dcoBOL - cladding outside diameter (in)
    ! deltaz - node length (ft)
    ! it     - power-time step index
    ! j      - axial node index
    ! na     - maximum number of axial nodes
    ! qc     - heat flux at node j (btu/hr-ft**2)
    ! sgapf  - number of fission gas atoms produced per 100 fissions
    ! ProblemTime - time (sec)
    !
    ! Output
    !
    ! fgmgp   - cumulative fission gas production (gm-moles)
    ! hemgp   - cumulative helium gas production (gm-moles)
    !
    INTEGER(ipk) :: lt, il, i
    REAL(r8k) :: burnup
    REAL(r8k), PARAMETER, DIMENSION(2) :: A1 = [  1.5350E-4_r8k, -2.4360E-4_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: A2 = [  2.1490E-3_r8k,  3.6059E-3_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: B1 = [ -2.9080E-3_r8k,  3.3790E-3_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: B2 = [  9.7223E-2_r8k,  5.3658E-2_r8k ]
    !
    burnup = EOSNodeburnup(j-1)
    il = it - 1
    IF (it == 1) il = it
    DO lt = il, it
        i = 2 + lt - it
        fgmgp(j-1,i) = qc(j-1) * ProblemTime(lt) * dcoBOL * deltaz(j-1) * sgapf * 9.95e-18_r8k
        fgmgp(j-1,i) = fgmgp(j-1,i) * 4.0_r8k
        IF (imox == 0) THEN
            hemgp(j-1,i) = qc(j-1) * ProblemTime(lt) * dcoBOL * deltaz(j-1) * 2.98e-18_r8k
        ELSE
            hemgp(j-1,i) = MAX((((A1(moxtype) * comp(j-1) + A2(moxtype)) * burnup ** 2 + (B1(moxtype) * comp(j-1) + &
                &              B2(moxtype)) * burnup) * 0.88_r8k / 4.0_r8k / 1.0e6_r8k * frden * 11.0_r8k * pi / 4.0_r8k * &
                &              (dp(j-1) ** 2 - (2.0_r8k * rc(j-1)) ** 2) * in2tocm2 * deltaz(j-1) * ftocm + &
                &              (2.11e10_r8k * enriPu239(j-1) + 7.77e10_r8k * enriPu240(j-1) + 8.49e8_r8k * enriPu241(j-1) + &
                &              1.36e9_r8k * enriPu242(j-1)) / Avogadro * ProblemTime(lt) * pi / 4.0_r8k * (dp(j-1) ** 2 - &
                &              (2.0_r8k * rc(j-1)) ** 2) * in2tocm2 * deltaz(j-1) * ftocm), 1.0E-30_r8k)
        END IF
    END DO
    !
    END SUBROUTINE gaspro
    !
    !
    !
    SUBROUTINE fgasre
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, jst, qmpy, den, ngasmod, Power, dci, ifbarel, tfuelr, rapow, &
      &                   nr, im, ngasr, crad, deltaz, dp, rc, ProblemTime, na, angr, fmgr, hmgr, &
      &                   hemgp, fgmgp, HeProd, he, heold, Boron10, ang, ounit, nplot, releas, &
      &                   rdotwrt, it, j, totl, grnsize, ifba, b10, zrb2thick, zrb2den, &
      &                   press, afgr, angi, delh, delbp, ir1, gasflg, dcoBOL, nvoid, rdot, &
      &                   ah2ogr, ah2og, h2omi, EOSNodeBurnup
    USE Refabrication, ONLY : irefab
    IMPLICIT NONE
    !
    !>@brief
    !> Subroutine fgasre computes the fission gas production and release, the helium production and release,
    !> and the nitrogen release for a given axial node. fgasre is called by the frpcon subroutine.
    !>@author
    !> This Subroutine was modified for use in frap-t and frapcon by g a berna from frap-s in oct 77.
    !> The Subroutine was modified by DD Lanning and K Geelhood in 1995 to better accommodate a new fission gas release
    !> subroutine (MASSIH) and modified version of RADAR.
    !> See Letters:
    !> Lanning to Siefken, 7/24/95, "Recommended Fission Gas Release Models for FRAPCON-3" 
    !> Lanning to Davis, 8/17/95, "Recommended Updates to FRAPCON Regarding use of subroutines RADAR and TUBRNP for ...."
    !
    !
    ! Input
    !
    ! afgr          - additional fractional gas release factor
    ! ang           - cumulative n2 concentration in fuel per power step (moles)
    ! angi          - initial nitrogen concentration in fuel (moles)
    ! EOSNodeBurnup - burnup to eos for axial node j (MWd/mtU)
    ! BOSNodeBurnup - burnup to bos for axial node j (MWd/mtU)
    ! delbp       - incremental burnup this time step, this axial node
    ! dcoBOL      - as fabricated clad o.d.
    ! delh        - power-time step time (hours)
    ! deltaz      - length of the axial node (ft)
    ! den         - percent of theoretical density of the fuel
    ! dp          - pellet diameter
    ! gasflg      - convergence flag on gas pressure iteration
    ! press       - rod internal gas pressure (psia)
    ! hemgp       - cumulative helium  gas production per time step & node
    ! im          - total number of time steps
    ! it          - power-time step index
    ! iter        - gas loop iteration index
    ! j           - axial node index
    ! jst         - pointer array for time dependent axial power profile
    ! ir1         - number of axial nodes plus one
    ! ngasr       - fission gas release model index
    !               >= 6, ans-5.4
    !               >= 11, ans-5.4 using modified Massih model
    ! nmesh       - cladding outside surface node index
    ! nr          - maximum number of radial nodes
    ! nvoid       - central void index
    !           0 = no central void
    !           1 = central void exists
    ! q           - axial power profile array
    ! qmpy        - time dependent power array
    ! r           - cold state radial node locations from cl (in) (radial node 1 is at the fuel surface)
    ! rc          - radius of pellet annulus
    ! rnodls      - cold state radial node locations from cl (in)
    ! tfgfr       - rod fractional fission gas release
    ! tfuelr      - fuel ring boundary temperature array (F)
    ! ProblemTime - time step array
    ! tlast       - time to beginning of power-time step (sec)
    ! tnow        - time to end of power-time step (sec)
    ! totl        - total active fuel length (ft)
    !
    ! Output
    !
    ! angr   - nitrogen release per node & power step (moles)
    ! fmgr   - cumulative fission gas release per node & time (moles)
    ! hmgr   - helium release per node & power step (moles)
    ! releas - fraction short-lived radioactive gases released
    !
    ! Note:
    !
    ! y(aa,bb) = (1.e0-EXP(-0.436e-4*(bb-2.e4)))/(1.e0+(0.665e0/aa)*EXP(-0.1107e-3*(bb-2.e4)))
    ! i=1, time to beginning of power-time step
    ! i=2, time to End of the power-time step
    INTEGER(ipk) :: i, l, irv, il, nt, lt, n, m, nrings
    REAL(r8k) :: coeff, coef1, brnup, fgas, bufree, fl, flh, fnn, rr, rpstn, frd, frdh, &
      &          frdn, flh2o, frdh2o
    REAL(r8k), DIMENSION(2) :: times
    REAL(r8k), DIMENSION(nr) :: dplh, dpn, dph2o, dv, rv
    REAL(r8k), DIMENSION(nr,2) :: fh, fn, fh2o
    !
    ! If first pass through subroutine, allocate variables
    IF (.NOT. FGRAllocated) CALL ALLOCATE_FGR_Variables
    !
    times(1) = ProblemTime(it-1)
    times(2) = ProblemTime(it)
    nt = ir1 - 1
    rdot = 0.0_r8k
    DO i = 1, 2
        lt = i
        n = 1
        IF (nvoid == 1) n = 2
        DO l = n, nr
            !
            ! Calculation of helium fraction release  (booth diffusion model)
            ! IF (tfuelr(l) <= 1112.0) dplh(l) = 0.28e-5 * EXP((4.e+04/1.986) * (11.e+0/ 1673. -1.e+0/873.))
            ! the above commented out equation is replaced with the one below
            IF (tfuelr(l) <= 1112.0_r8k) THEN
                dplh(l) = 0.452847e-10_r8k
            ELSE
                dplh(l) = 0.28e-5_r8k * EXP((4.0e4_r8k / 1.986_r8k) * (1.0_r8k / 1673.0_r8k - 1.8_r8k / (tfuelr(l) + 459.4_r8k)))
            END IF
            
            IF (times(lt) <= (1.0_r8k / (pi ** 2 * dplh(l)))) THEN
                fh(l,i) = 4.0_r8k * SQRT(dplh(l) * times(lt) / pi) - 3.0_r8k * dplh(l) * times(lt) / 2.0_r8k
                IF (fh(l,i) > 0.57_r8k) THEN
                    coeff = pi * pi * dplh(l) * times(lt)
                    coef1 = coeff
                    IF (coef1 > 200.0_r8k) coef1 = 200.0_r8k
                    fh(l,i) = 1.0_r8k + (0.607927_r8k * EXP(-coef1) - 0.653644_r8k) / coeff
                    IF (fh(l,i) > 1.0_r8k) fh(l,i) = 1.0_r8k
                END IF
            ELSE
                coeff = pi * pi * dplh(l) * times(lt)
                coef1 = coeff
                IF (coef1 > 200.0_r8k) coef1 = 200.0_r8k
                fh(l,i) = 1.0_r8k + (0.607927_r8k * EXP(-coef1) - 0.653644_r8k) / coeff
                IF (fh(l,i) > 1.0_r8k) fh(l,i) = 1.0_r8k
            END IF
            !
            ! Nitrogen fractional release
            dpn(l) = 0.173_r8k * EXP(-33400.0_r8k / (1.9869_r8k * tfk(tfuelr(l))))
            coeff = pi * pi * dpn(l) * times(lt)
            coef1 = MIN(200.0_r8k, coeff)
            IF (coeff > 1.0_r8k) THEN
                fn(l,i) = 1.0_r8k - 6.0_r8k * EXP(-coef1) / (pi ** 2)
            ELSE
                fn(l,i) = 6.0_r8k * SQRT(dpn(l) * times(lt) / pi) - 3.0_r8k * dpn(l) * times(lt)
            END IF
            IF (fn(l,i) > 1.0_r8k) fn(l,i) = 1.0_r8k
            !
            ! Water fractional release
            dph2o(l) = 0.0_r8k
            fh2o(l,i) = 0.0_r8k
        END DO
        ! i=1, time is referenced to beginning of power-time step
        IF (i == 2) THEN
            ! calculation of fission gas release (ans-5.4 model)
            brnup = EOSNodeburnup(j-1) - delbp / 2.0_r8k
            ! calculate flux depression array
            ! Use rapow from Subroutine radar
            DO irv = 1, nr
                dv(irv) = crad(irv,j-1) * 2.0_r8k
                ! rv is a Function of dv
                rv(irv) = rapow(irv,j-1)
            END DO
            !
            SELECT CASE (ngasmod)
            CASE (1)
                CALL ans54 (brnup, rv, dv)
            CASE (2)
                CALL massih (rv, dv)
            CASE (3)
                CALL frapfgr (rv, dv)
            CASE (4)
                CALL ans54_2011 (brnup, rv, dv)
            END SELECT
        END IF
        ! Calculate helium production in IFBA
        IF (it == 1) THEN
            ! As-fabricated Boron concentration (5.840e22 atoms B/cm^3 assuming 90% TD)
            ! Molar mass (grams ZrB2 per mol) 91.224 g Zr/mol, b10 atom% (10 g/mol) + (1-b10) atom% (11g/mol)
            ZrB2_MolarMass = 91.224 + 2.0_r8k * ((1.0_r8k - b10 / 100.0_r8k) * 11.0_r8k + (b10 / 100.0_r8k) * 10.0_r8k)
            ! atoms Boron / cm3 ZrB2
            ! = ZrB2 Theoretical Density * As-Fabricated %TD * Avogadro's # / ZrB2 Molar Mass * 2 atoms Boron per atom Zr
            B_atoms = ZrB2_TDen * zrb2den / 100.0_r8k * Avogadro / ZrB2_MolarMass * 2.0_r8k
            ! Set B10 concentration the same at each axial node (based on user input)
            Boron10(j-1) = b10
            heold(j-1) = 0.0_r8k
            ! Volume of ZrB2 at each axial node
            IF (.NOT. ALLOCATED(Vol_ZrB2)) ALLOCATE(Vol_ZrB2(1:na-1))
            Vol_ZrB2(j-1) = pi * ((dp(j-1) / 2.0_r8k + zrb2thick) ** 2 - (dp(j-1) / 2.0_r8k) ** 2) * in2tocm2 * deltaz(j-1) * ftocm
        END IF
        ! HeProd in Atoms He/cm^3/s
        ! factor of 2.46 because Richard Pagh used this for Density.
        ! 5.472 is 90% TD (6.08 is TD)
        HeProd(j-1) = (-(9.66127e8_r8k * ifba + 1.088109e11_r8k) * (Boron10(j-1) ** 2) + (-2.10296e10_r8k * ifba + &
          &           4.88343e13_r8k) * Boron10(j-1)) * Power(j-1) / 5.64_r8k * 6.08_r8k * zrb2den / 100.0_r8k / 4.53_r8k
        ! Convert from rate to He/cm^3
        HeProd(j-1) = HeProd(j-1) * delh * hrtosec
        ! Ensure does not produce more than the B-10 that's available
        IF (HeProd(j-1) > Boron10(j-1) * B_atoms / 100.0_r8k) HeProd(j-1) = Boron10(j-1) * B_atoms / 100.0_r8k        
        ! He in atoms He/cm^3
        he(j-1) = heold(j-1) + HeProd(j-1)
        IF (gasflg) THEN
            heold(j-1) = he(j-1)
            IF (it == irefab) heold(j-1) = 0.0_r8k
            ! Deplete B-10 concentration(%) = Last conc. - He atoms produced (1 He atom produced by 1 B-10 atom)
            Boron10(j-1) = MAX(0.0_r8k, (Boron10(j-1) - HeProd(j-1) / B_atoms * 100.0_r8k))
        END IF
        ! Calculation of the increment average gas fraction release of fission gas, helium, and nitrogen
        fl = 0.0_r8k
        flh = 0.0_r8k
        fnn = 0.0_r8k
        flh2o = 0.0_r8k
        m = 2
        IF (nvoid == 1) m = 3
        DO il = m, nr
            rr = -(crad(il,j-1) ** 2 - crad(il-1,j-1) ** 2) / 2.0_r8k
            flh = flh + (fh(il,i) + fh(il-1,i)) * rr
            fnn = fnn + (fn(il,i) + fn(il-1,i)) * rr
            flh2o = flh2o + (fh2o(il,i) + fh2o(il-1,i)) * rr
        END DO
        nrings = nr - 1
        IF (nvoid == 1) nrings = nrings - 1
        rpstn = (crad(1,j-1) ** 2) * nrings
        frd = rdot
        ! Helium release
        frdh = flh / rpstn
        ! Nitrogen release
        frdn = fnn / rpstn
        ! Water release
        frdh2o = flh2o / rpstn
        ! Calculation of gas releases in moles
        ! Fission gas release
        fmgr(j-1,i) = frd * fgmgp(j-1,i)
        ! Helium release
        hmgr(j-1,i) = frdh * hemgp(j-1,i)
        ! Helium release from IFBA. Convert He/cm^3 to moles of gas
        ! New Model
        ifbarel(j-1) = he(j-1) * Vol_ZrB2(j-1) / Avogadro
        ! Nitrogen release
        angr(j-1,i) = frdn * angi / (totl / deltaz(j-1))
        ! Water release
        ah2ogr(j-1,i) = frdh2o * h2omi / (totl / deltaz(j-1))
    END DO
    ang(j-1,2) = MAX((ang(j-1,1) - angr(j-1,2) + angr(j-1,1)), 0.0_r8k)
    ah2og(j-1,2) = MAX((ah2og(j-1,1) - ah2ogr(j-1,2) + ah2ogr(j-1,1)), 0.0_r8k)
    !
250 FORMAT (' Time step, Axial node, Incremental burnup'/1x,i6,5x,i6,e11.2,/, &
      &     ' Incremental burnup greater than 2500 MWd/mtU - see ans54')
    ! Capture rdot for printing out
    rdotwrt(j-1) = rdot
    !
    END SUBROUTINE fgasre
    !
    !
    !
    SUBROUTINE ans54 (brnup, rv, dv)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, jst, qmpy, dt, prdct, ansd, den, ounit, crad, nr, im, &
                          qaxnorm, ngasr, na, rc, tfuelr, dp, releas, it, j, ir1, dcoBOL, rdot
    IMPLICIT NONE
    !> @brief
    !> The fission gas release is calculated inside the time step and gas pressure loops for each axial region.
    !> The fuel temperatures are calculated inside the axial loop.
    !> Only the diffusion parameter, ansd, needs to be saved for each radial and axial region every time step,
    !> ansd (along with prdct) is tri-dimensioned (fissiongasradial,axial,timestep)
    !> Although the local gas production rate and burnup are also needed they are calculated as they are needed.
    !> ans54 is called from fgasre.
    !> The ans standard calls for at least six radial regions and ten or more axial regions.
    !> The subroutine establishes the radial divisions but the number of axial regions is a user input.
    !> Also, the time step size should be 2000 MWd/mtU burnup or shorter.
    !> The diffusion parameters for iodine, cesium, and tellurium are not calculated by the code.
    !> @author
    !> This Subroutine (ans54) was coded by f e panisko and w n rausch of battelle,
    !> Pacific Northwest Laboratories, august 1979 (pnl-3077)
    !
    ! Input
    !
    ! it          - current time step number
    ! brnup       - axial local mid step value in MWd/mtU
    ! dcoBOL      - fabricated clad od, inches
    ! dp(j-1)     - fabricated fuel dia.,inches
    ! dv          - diameter array associated with rv
    ! fuelTD      - theoretical density (g/cm3)
    ! flow        - fraction release predicted by the low temp stable model
    ! flowr       - fraction release predicted by the low temp radioactive model
    ! frden       - fraction of theoretical density
    ! j           - axial region counter for frapcon
    ! j-1         - axial region counter for ans54
    ! jst         - pointer for time dependent axial profile
    ! ir1         - total number of axial nodes
    ! npow        - total no. of axial regions
    ! ngasr       - number of radial regions in the fuel, user input as ngasr
    ! im          - total no. of steps
    ! rv          - fuel flux depression array
    ! nr          - number of flux depression regions in the fuel
    ! ProblemTime - time history array, cumulative in seconds
    ! q           - axial power profile array
    ! qmpy        - time dependent power array
    ! sp          - specific power (megawatts per metric ton of fuel)
    ! powr(qc)    - fuel surface heat flux for an axial region
    ! rc(j-1)     - radius of the annulus
    ! tt and ts   - the fuel radial temp(F) and Dimension (in) arrays
    ! wf          - weighting factor for the radioactive gas release fraction to compensate for 
    !               different production rates in different regions
    ! crad        - fuel node radii (in)
    ! tfuelr      - fuel node temperatures (F)
    ! nr          - number of thermal radial nodes
    !
    ! Output
    !
    ! rdot        - gas release fraction per axial node per cumulative time
    ! releas      - fraction released for the radioactive gases
    !
    INTEGER(ipk) :: n1, itime, i1, i2, i3, i4, i5, i6, jj, i, jx, ii, total
    REAL(r8k) :: totb, tempk, tempf, routr, routr2, preex, x, y, summ, powr, rdott, pftot, frden, &
      &          flxtot, flow, dvoidz, ansda, ansdia, fbuaxl, fburad, buring, a1, a5, a6, delex, deltim, &
      &          rtime, wf, xmu, taut, arg, a, z, c, p, e, g, frac, ii2, arg2, h, o, sp, flowr, fuelTD, temp
    INTEGER(ipk), PARAMETER :: nshortnuclides = 11
    REAL(r8k), POINTER :: pfave
    REAL(r8k), INTENT(IN) :: brnup
    REAL(r8k), PARAMETER :: pi2 = pi ** 2
    REAL(r8k), PARAMETER :: pi4 = pi2 ** 2
    REAL(r8k), DIMENSION(2) :: tau, gtau
    REAL(r8k), DIMENSION(nr) :: rv, dv
    REAL(r8k), DIMENSION(ngasr) :: f, flxfac, flxfacb
    REAL(r8k), DIMENSION(:), POINTER :: half, decay, ansr
    REAL(r8k), DIMENSION(:,:), POINTER :: pf
    ! Assign pointers
    pfave => FGRData%ANS54%pfave
    pf => FGRData%ANS54%pf
    half => FGRData%ANS54%half
    ansr => FGRData%ansr
    decay => FGRData%ANS54%decay
    !
    fuelTD = MatProp ('FUEL', 'TDENSITY', temp)
    frden = den / 100.0_r8k
    dvoidz = 2.0_r8k * rc(j-1)
    IF (na < 11) WRITE (ounit,350) (na-1)
    flxtot = 0.0_r8k
    ansda = (dp(j-1) ** 2 - dvoidz ** 2) / 4.0_r8k / REAL(ngasr)
    routr = dp(j-1) / 2.0_r8k
    ! calculate relative production rates in each ngasr fuel region
    ! all the radial regions formed here have equal areas, not equal widths.
    ! ansr = radius term
    ! ansdia = diameter term
    ! ansda = area term
    ! routr = radius term
    ! routr2 = area term
    DO i1 = 1, ngasr
        routr2 = routr ** 2
        ansr(i1) = SQRT(routr2 - ansda / 2.0_r8k)
        ansdia = ansr(i1) * 2.0_r8k
        flxfac(i1) = terp(ansdia,dv,rv,nr)
        IF (i1 /= ngasr) routr = SQRT(routr2 - ansda)
        flxtot = flxtot + flxfac(i1)
    END DO
    ! Normalize the production rate so their sum equals 1
    flxfac(1:ngasr) = flxfac(1:ngasr) / flxtot
    dt(1) = 0.0_r8k
    DO n1 = 2, im
        ! dt(n1) is the time increment for step n1 (units are seconds)
        dt(n1) = ProblemTime(n1) - ProblemTime(n1-1)
    END DO
    ! j-1 and now i1 are the local axial region numbers
    i1 = j - 1
    ! check on the size of problem
    ! ** top of the radial region gas release loop ******************
    totb = 0.0_r8k
    !
    DO i2 = 1, ngasr
        prdct(i2,j-1,it) = flxfac(i2) * REAL(ngasr)
        tempf = terp(ansr(i2),crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        ! ansd at 1400 deg.c =2.2e-10=d*EXP(-72300./(1673r))
        ! solving for d gives 0.61377
        ! so ansd(i2,i1,it) = 0.61377*EXP(-72300./(1.987*tempk))
        ansd(i2,i1,it) = 0.61377_r8k * EXP(-36386.0_r8k / tempk)
        ! obtain mid-time step pellet ring burnup
        fbuaxl = qmpy(1) * qaxnorm(j-1, jst(1)) * ProblemTime(1)
        fburad = qmpy(1) * qaxnorm(j-1, jst(1)) * prdct(i2,i1,1) * ProblemTime(1)
        IF (it > 2) THEN
            DO itime = 2, (it - 1)
                fbuaxl = fbuaxl + qmpy(itime) * qaxnorm(j-1,jst(itime)) * dt(itime)
                fburad = fburad + qmpy(itime) * qaxnorm(j-1,jst(itime)) * prdct(i2,i1,itime) * dt(itime)
            END DO
        END IF
        fbuaxl = fbuaxl + qmpy(it) * qaxnorm(j-1,jst(it)) * dt(it) / 2.0_r8k
        fburad = fburad + qmpy(it) * qaxnorm(j-1,jst(it)) * prdct(i2,i1,it) * dt(it) / 2.0_r8k
        buring = brnup * fburad / fbuaxl
        bup = brnup * prdct(i2,j-1,it)
        ! calculate diffusion coefficient for local burnup
        ansd(i2,i1,it) = ansd(i2,i1,it) * 100.0_r8k ** (buring / 28000.0_r8k)
        a6 = 0.0_r8k
        preex = 0.0_r8k
        f(i2) = 0.0_r8k
        ! when it eq 1 time is assumed to be zero so skip gas release
        IF (it == 1) EXIT
        ! *** top of the time step loop *********
        DO i3 = 2, it
            IF (dt(i3) >= 14400.0_r8k) THEN
                ! convert surface heat flux to kw/ft
                powr = (qmpy(i3) * BTUhtokW * qaxnorm(j-1,jst(i3))) * (dcoBOL * intoft) * pi
                tau(1) = 0.0_r8k
                DO i4 = i3, it
                    tau(1) = tau(1) + ansd(i2,i1,i4) * dt(i4)
                END DO
                tau(2) = 0.0_r8k
                tau(2) = tau(1) - ansd(i2,i1,i3) * dt(i3)
                a1 = 0.0_r8k
                ! The following cards are for the finite sum
                DO i5 = 1, 2
                    gtau(i5) = 0.0_r8k
                    IF (i5 /= 2 .OR. i3 /= it) THEN
                        IF (tau(i5) <= 0.1_r8k) THEN
                            gtau(i5) = 1.0_r8k - 4.0_r8k * SQRT(tau(i5) / pi) + 1.5_r8k * tau(i5)
                        ELSE
                            summ = 0.0_r8k
                            DO i6 = 1, 3
                                x = REAL(i6)
                                x = x * x
                                y = -(x * pi2 * tau(i5))
                                IF (y >= -200.0_r8k) summ = summ + EXP(y) / (x * x * pi4)
                            END DO
                            gtau(i5) = (1.0_r8k / (15.0_r8k * tau(i5)) - (6.0_r8k * summ / tau(i5)))
                        END IF
                    END IF
                END DO
                a1 = tau(1) * gtau(1) - tau(2) * gtau(2)
                a5 = a1 * prdct(i2,j-1,i3) * powr / ansd(i2,i1,i3)
                delex = prdct(i2,j-1,i3) * dt(i3) * powr
                ! The following card is to obviate the need for double precision
                IF (a5 > delex) a5 = delex
                a6 = a6 + a5
                preex = preex + delex
            END IF
        END DO
        ! *** bottom of the time step loop *********
        IF (preex == 0.0_r8k) THEN
            preex = 1.0_r8k
            a6 = 1.0_r8k
        END IF
        f(i2) = 1.0_r8k - a6 / preex
        totb = totb + preex
        flxfacb(i2) = preex
    END DO
    !
    IF (it > 1) THEN
        rdott = 0.0_r8k
        DO i = 1, ngasr
            flxfacb(i) = flxfacb(i) / totb
            rdott = rdott + flxfacb(i) * f(i)
        END DO
        rdot = rdott
        ! bottom of the radial region gas release loop ******************
        ! low temperature release
        flow = 0.7e-7_r8k * brnup
        IF (flow > rdot) rdot = flow
    END IF
    !
    ! The remainder of the subroutine is for the calculation of the radioactive gas release fraction
    ! for a series of different half-lives.
    ! The statements calculate the release averaged over the whole rod.
    ! The ans short half-lived release standard is valid only for when no previous buildup is present.  
    ! This requires a shutdown of a 4*half-life period of time. The Subroutine does not consider this problem.
    IF (j == 2) THEN
        IF (it == 1 .OR. jst(it) /= 1) THEN
            ! Calculate gas production factors to use as weighting factors for average release fraction for the rod
            pftot = 0.0_r8k
            DO jj = 1, (na - 1)
                DO i = 1, ngasr
                    pf(i,jj) = qaxnorm(jj,jst(it)) * flxfac(i)
                    pftot = pftot + pf(i,jj)
                END DO
            END DO
            pfave = pftot / ((na - 1) * ngasr)
        END IF
        deltim = 0.5_r8k
        DO i = 1, nshortnuclides
            rtime = 1.0_r8k + (i-1) * deltim
            ! Half is the half lives of the isotopes ranging on a log scale from 10 to 1000000 seconds
            half(i) = 10.0_r8k ** rtime
            decay(i) = 1.0_r8k / half(i)
            releas(i) = 0.0_r8k
        END DO
    END IF
    IF (ProblemTime(it) < oneday) RETURN
    DO i = 1, ngasr
        wf = pf(i,j-1) / pfave
        DO jx = 1, nshortnuclides
            xmu = decay(jx) / ansd(i,j-1,it)
            taut = ansd(i,j-1,it) * ProblemTime(it)
            arg = taut * xmu
            arg = MIN(arg, 675.0_r8k)
            IF (taut - 0.1_r8k <= 0.0_r8k) THEN
                a = 3.0_r8k / (1.0_r8k - EXP(-arg))
                z = ERF(SQRT(xmu * taut))
                c = 2.0_r8k * SQRT(xmu * taut / pi) * EXP(-arg)
                p = (z - c) / SQRT(xmu)
                e = (1.0_r8k + xmu * taut) * EXP(-arg)
                g = (1.0_r8k - e) / xmu
                frac = a * (p - g)
            ELSE
                a = 1.0_r8k / TANH(SQRT(xmu))
                z = 3.0_r8k * (a / SQRT(xmu) - 1 / xmu)
                c = 6.0_r8k * xmu / (EXP(arg) - 1.0_r8k)
                p = 0.0_r8k
                DO ii = 1, 3
                    ii2 = ii * ii
                    arg2 = ii2 * pi2 * taut
                    IF (arg2 > 675.0_r8k) arg2 = 675.0_r8k
                    h = 1.0_r8k - EXP(-arg2)
                    o = ii2 * pi2 * (ii2 * pi2 + xmu)
                    p = p + h / o
                END DO
                frac = z - c * p
                ! frac is the release fraction at any one node(radial,axial,time step)
            END IF
            frac = frac * wf
            ! release sums the fraction released for each axial node during a time step
            releas(jx) = releas(jx) + frac
        END DO
    END DO
    IF (j /= na) RETURN
    ! the following statements will be executed only once each time step
    total = ngasr * (na - 1)
    DO i = 1, nshortnuclides
        releas(i) = releas(i) / total
        ! convert surface heat flux to kw/ft
        powr = (qmpy(it) * BTUhtokW) * (dcoBOL * intoft) * pi
        ! convert kw/ft to mw/mtm (specific power)
        sp = powr * 0.001_r8k * (1.0_r8k / (fttoin * ((dp(j-1) / 2.0_r8k) ** 2 * pi)) * &
          &  1.0_r8k / (frden * fuelTD)) * 1.0e6_r8k * 6.102e-2_r8k
        flowr = (0.7e-7_r8k * SQRT(decay(i)) + 2.0e-12_r8k * sp) / decay(i)
        IF (flowr > releas(i)) releas(i) = flowr
    END DO
    !
    ! The results are output by a call to gasplt
350 FORMAT (1x,100('*')/5x,'problem may not satisfy conditions for ans-5.4 fission gas release standard'/5x, &
      &                    'number of axial regions only',i3/1x,100('*'))
    !
    END SUBROUTINE ans54
    !
    !
    !
    SUBROUTINE massih (rv, dv)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : imox, den, sigfgr, igas, Power, crad, nr, ngasr, tfuelr, dp, rc, &
      &                   na, ounit, nplot, j, it, gasflg, delbp, press, grnsize, delh, rdot, &
      &                   EOSNodeburnup
    USE Refabrication, ONLY : irefab
    IMPLICIT NONE
    !>@brief
    !> Subroutine massih is called by fgasre and returns cumulative fission gas release for the axial region (rdot)
    !> This Subroutine is based on the solution to the booth diffusion problem by k. forsberg and a. r. massih, 
    !> journal of nuclear material. volume 135, 1985, pp. 140 to 148.
    !>@author
    !> This version was programmed by d.d. lanning and k. geelhood
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> june,19
    !
    ! Input
    !
    ! it      - current time step number
    ! j-1     - axial region number
    ! ngasr   - number of equal volume radial rings (must be greater than 10) for FGR calculations
    ! nr      - number of radial nodes
    ! gasflg  - convergence signal on gas pressure iteration
    ! EOSNodeBurnup - axial region burnup at End of step (MWd/mtU)
    ! delbp   - burnup increment (MWd/mtU)
    ! delh    - time increment (hr)
    ! dp      - pellet outer diameter (inches)
    ! crad    - radii of nodes from fuel centerline (in)
    ! rc      - pellet inner diameter (inches)
    ! den     - pellet density (per cent theoretical density)
    ! press   - gas pressure in the rod (psi)
    ! rv,dv   - array of rel.radial powers and diameters (in inches)
    ! grnsize - input grain size (effective diameter, microns)
    !
    ! major internal variables (all concentrations in moles/m^3)
    !
    ! gp      - concentration of produced fission gas
    ! gb      - concentration of gas at grain boundaries
    ! gg      - concentration of gas within grains
    ! grs     - concentration of gas re-solved
    ! rls     - concentration of gas released
    ! gs      - saturation concentraion
    ! d       - diffusion constant, m^2 per second
    ! brn     - ring-specific burnup, MWd/mtU
    ! fr      - ring-specific cumulative release fraction
    ! tfuelr  - fuel node temperatures (F)
    !
    ! Output
    !
    ! rdot    - pellet vol. average cumulative release fraction
    !
    INTEGER(ipk) :: i1, i2, icnt, i
    REAL(r8k) :: dels, frden, ansdia, rpext, grn, dvoid, ansda, routr, flxtot, routr2, tempf, tempk, delbrn, &
      &          delprod, fit, ttest, bup, testr, bupm, bb1, d, dtest, rmoxmult, deltau, dtau, arg1, f1, &
      &          arg2, f2, arg3, f3, arg4, f4, term1, term2, term3, rq, delg1, delg2, delg3, delg4, delgg, &
      &          delgb, factor, delgrs, rns, gs, delrls, flow, check
    REAL(r8k), DIMENSION(nr) :: rv, dv
    REAL(r8k), DIMENSION(ngasr) :: flxfc, fr
    REAL(r8k), DIMENSION(:), POINTER :: ansr
    ! massih's constants for approximating the integration kernal
    REAL(r8k), PARAMETER :: a1 = 0.231545_r8k
    REAL(r8k), PARAMETER :: a2 = 0.0948453_r8k
    REAL(r8k), PARAMETER :: a3 = 0.0282898_r8k
    REAL(r8k), PARAMETER :: a4 = 0.645320_r8k
    REAL(r8k), PARAMETER :: b1 = 72.5968_r8k
    REAL(r8k), PARAMETER :: b2 = 796.773_r8k
    REAL(r8k), PARAMETER :: b3 = 29083.0_r8k
    REAL(r8k), PARAMETER :: b4 = 10.2469_r8k
    !
    ! Assign pointer
    ansr => FGRData%ansr
    ! units changes
    dels = delh * hrtosec
    frden = den / 100.0_r8k
    ! rpext is pressure on bubbles, in atmospheres
    rpext = press * PSItoATM
    ! grn = grain size (radius) in meters (grnsize is fixed at 10 microns, so grn is fixed at 5 microns)
    grn = (grnsize / 2.0_r8k) * umtom
    IF (it > 1) THEN
        ! ring volume calculation (equal-volume rings)
        dvoid = 2.0_r8k * rc(j-1)
        ansda = (dp(j-1) ** 2 - dvoid ** 2) / 4.0_r8k / REAL(ngasr)
        routr = dp(j-1) / 2.0_r8k
        flxtot = 0.0_r8k
        DO i1 = 1, ngasr
            routr2 = routr * routr
            ansr(i1) = SQRT(routr2 - ansda / 2.0_r8k)
            ansdia = ansr(i1) * 2.0_r8k
            flxfc(i1) = terp(ansdia, dv, rv, nr)
            If (i1 /= ngasr) routr = SQRT(routr2 - ansda)
            flxtot = flxtot + flxfc(i1)
        END DO
        ! normalize ring radial power factors to total = 1.0
        flxfc(1:ngasr) = flxfc(1:ngasr) / flxtot
        !
        ! *******radial node loop********
        !
        rdot = 0.0_r8k
        DO i2 = 1, ngasr
            !
            ! ******calculate ring-average temperature (K)***********
            !
            tempf = terp(ansr(i2),crad(:,j-1),tfuelr,nr)
            tempk = tfk(tempf)
            !
            ! moles gas produced per cubic meter = MWd/mtU*mtU/m^3*stpcm^3/MWd*moles/stpcm^3
            ! = delbp*(frden*10.96*0.88)*31.*(1./22420) = delbp*frden*0.013336
            ! also multiply by flxfc(i2)*ngasr to distribute production according
            ! to the radial power distribution. similarly distribute the burnuup increment.
            !
            delbrn = delbp * flxfc(i2) * REAL(ngasr)
            brn(i2,j-1) = brnold(i2,j-1) + delbrn
            delprod = delbp * frden * 0.013336_r8k * flxfc(i2) * REAL(ngasr)
            gp(i2,j-1) = gpold(i2,j-1) + delprod
            ! fit = massih's recomended resolution rate (m/s) = b*lambda
            ! factor of 250 on resolution comes from fit to steady-state and
            ! power-ramp integral rod fgr data
            ! factor changed to 300 by KJ Geelhood on 11-14-02
            !
            fit = 1.84e-14_r8k * 300.0_r8k
            !
            ! resolution not permitted on extremely short time steps (< 1 day)
            ttest = delh * hrtoday
            IF (ttest < 1.0_r8k) fit = 0.0_r8k
            !
            ! calculate diffusion constant
            !
            ! calcualte burnup enhancment factor bb1
            ! Form and PARAMETER values based on guidance from ans5.4 plus
            ! reference to low-and high-burnup fgr data for cases with fgr.
            ! Last change (brn-25000) changed to (brn-21000) made by CE Beyer on 5/29/97
            ! (bupm/35000) changed to (bupm/40000) by KJ Geelhood on 11-14-02
            !
            bup = brn(i2,j-1) - 21000.0_r8k
            testr = 0.0_r8k
            bupm = MAX(bup, testr)
            bb1 = MIN(20000.0_r8k, 100.0_r8k ** (bupm / 40000.0_r8k))
            ! Factor of 1.15 on the activation energy (q/r) term comes from fit to steady-state integral rod fgr data.
            d = bb1 * 2.14e-13_r8k * EXP((-22884.0_r8k * 1.15_r8k) / MIN(tempk, 1850.0_r8k))
            ! Diffusion constant not permitted to fall below the low-temperature Function proposed by massih/forsberg.
            dtest = 1.51e-17_r8k * EXP(-9508.0_r8k / tempk)
            IF (d < dtest) d = dtest
            ! Factor of 14.0 on diffusion constant comes from fit to steady-state and especially power-ramp fgr data.
            ! Factor changed to 12.0 on 11-14-02 by KJ Geelhood
            d = d * fttoin
            ! If fuel is MOX, multiply d by a factor of 1.75.  Change made by DD Lanning and KJ Geelhood on 2/12/03
            IF (imox > 0) THEN
                rmoxmult = 1.75_r8k
                d = d * rmoxmult
            END IF
            ! Add on uncertainty
            IF (sigfgr > 0.0_r8k) THEN
                d = d * (1.0_r8k + sigfgr * 1.0_r8k)
            ELSE
                d = d / (1.0_r8k - sigfgr * 1.0_r8k)
            END IF
            !
            deltau = d * dels
            dtau = deltau / grn ** 2
            ! term1, term2, term3, and rq refer to terms in equation
            ! number 45 in forsberg/massih, jnm 135 (with no re-solution,
            ! i.e., h4 = 0, and production constant througout the time step)
            ! f1,f2,f3,  and g1,g2,g3 are defined in equation 43.
            !
            ! alternate calculations shown here for f1, f2, and f3 are
            ! done to prevent underflows (i.e., numbers less
            ! than 1.e-38) or poor approximations (exponent arguments smaller than 4.0e-4_r8k absolute)
            arg1 = b1 * dtau
            IF (arg1 > 4.0e-4_r8k .AND. arg1 < 85.0_r8k) f1 = EXP(-b1 * dtau) - 1.0_r8k
            IF (arg1 < 4.0e-4_r8k) f1 = -b1 * dtau
            IF (arg1 > 85.0_r8k) f1 = -1.0_r8k
            arg2 = b2 * dtau
            IF (arg2 > 4.0e-4_r8k .AND. arg2 < 85.0_r8k) f2 = EXP(-b2 * dtau) - 1.0_r8k
            IF (arg2 < 4.0e-4_r8k) f2 = -b2 * dtau
            IF (arg2 > 85.0_r8k) f2 = -1.0_r8k
            arg3 = b3 * dtau
            IF (arg3 > 4.0e-4_r8k .AND. arg3 < 85.0_r8k) f3 = EXP(-b3 * dtau) - 1.0_r8k
            IF (arg3 < 4.0e-4_r8k) f3 = -b3*dtau
            IF (arg3 > 85.0_r8k) f3 = -1.0_r8k
            arg4 = b4 * dtau
            IF (arg4 > 4.0e-4_r8k .AND. arg4 < 85.0_r8k) f4 = EXP(-b4 * dtau) - 1.0_r8k
            IF (arg4 < 4.0e-4_r8k) f4 = -b4 * dtau
            IF (arg4 > 85.0_r8k) f4 = -1.0_r8k
            term1 = f1 * a1 / b1 + f2 * a2 / b2 + f3 * a3 / b3 + f4 * a4 / b4
            ! short-term approximation
            term2 = 2.2568_r8k * (dtau ** 1.5_r8k) - 1.5_r8k * (dtau ** 2)
            ! long-term approximation
            IF (dtau > 0.1_r8k) term2 = dtau - 0.0616_r8k * (1.0_r8k - EXP(-9.87_r8k * dtau))
            !
            term3 = f1 * g1old(i2,j-1,1) + f2 * g2old(i2,j-1,1) + f3 * g3old(i2,j-1,1) + f4 * g4old(i2,j-1,1)
            rq = delprod / (term2 - term1) / (grn ** 2)
            delg1 = -rq * (grn ** 2) * (a1 / b1) * f1 + f1 * g1old(i2,j-1,1)
            delg2 = -rq * (grn ** 2) * (a2 / b2) * f2 + f2 * g2old(i2,j-1,1)
            delg3 = -rq * (grn ** 2) * (a3 / b3) * f3 + f3 * g3old(i2,j-1,1)
            delg4 = -rq * (grn ** 2) * (a4 / b4) * f4 + f4 * g4old(i2,j-1,1)
            g1(i2,j-1,1) = g1old(i2,j-1,1) + delg1
            g2(i2,j-1,1) = g2old(i2,j-1,1) + delg2
            g3(i2,j-1,1) = g3old(i2,j-1,1) + delg3
            g4(i2,j-1,1) = g4old(i2,j-1,1) + delg4
            delgg = delg1 + delg2 + delg3 + delg4
            gg(i2,j-1,1) = ggold(i2,j-1,1) + delgg
            delgb = (grn ** 2) * rq * term2 - term3
            !
            ! this is the re-solution factor, derived from the forsberg/massih jnm 1
            factor = grn * fit / (3.0_r8k * d)
            !
            ! re-solution partition of delgb done here.
            !
            delgrs = 0.0_r8k
            delgrs = delgb * factor / (1.0_r8k + factor)
            delgb = delgb / (1.0_r8k + factor)
            gb(i2,j-1,1) = gbold(i2,j-1,1) + delgb
            grs(i2,j-1,1) = grsold(i2,j-1,1) + delgrs
            ! calculation of the grain boundary saturation concentration, gs.
            ! saturation concentration in moles/m^2, assuming bubble size is 0.5 mi
            ! dihedral half-angle = 50 degrees, and surface tension = 0.6 j/m^2.
            rns = (5.7589e-9_r8k / tempk) * (2.4e6_r8k + rpext * 1.013e5_r8k)
            gs = 3.0_r8k * rns / (2.0_r8k * grn)
            IF ((tempk - tempkold(i2,j-1)) > 300.0_r8k .AND. Power(j-1) > 9.0_r8k .AND. ttest < 0.17_r8k) gs = gs * 0.5_r8k
            !
            ! test for grain boundary gas saturation
            ! if not saturated, no gas release
            ! if saturated, release all grain boundary gas and all resolved gas upon saturation
            ! then zero out grain boundary and re-solved gas inventories.
            !
            IF (gb(i2,j-1,1) <= gs) THEN
                delrls = 0.0_r8k
            ELSE
                delrls = gb(i2,j-1,1) + grs(i2,j-1,1)
                gb(i2,j-1,1) = 0.0_r8k
                grs(i2,j-1,1) = 0.0_r8k
            END IF
            !
            ! Update total gas release
            !
            rls(i2,j-1) = rlsold(i2,j-1) + delrls
            !
            ! If the option to suppress FGR before a given time step is active, set gas release to zero
            !
            IF (it <= igas) rls(i2,j-1) = 0.0_r8k
            !
            ! Compute gas release fraction
            !
            fr(i2) = rls(i2,j-1) / gp(i2,j-1)
            !
            ! low-temperature gas release (taken from ans5.4 model)
            !
            flow = 0.7e-7_r8k * brn(i2,j-1)
            IF (it >= irefab+1) flow=0.7e-7_r8k * (brn(i2,j-1) - brnrefab(i2,j-1))
            IF (flow > fr(i2) .AND. it > igas) fr(i2) = flow
            check = (rls(i2,j-1) + gg(i2,j-1,1) + gb(i2,j-1,1) + grs(i2,j-1,1)) / gp(i2,j-1)
            rdot = fr(i2) * brn(i2,j-1) / EOSNodeburnup(j-1) / REAL(ngasr) + rdot
            !
            ! de-bug Write statements
            ! IF (gasflg) WRITE (ounit, 111) i2,brn(i2,j-1), tempk,fr(i2),check
            !111     FORMAT(2x,'i2=',i5,3x,'brn=',e10.4,3x,'tempk=',e10.4,3x,'release fraction=',e10.4,3x,'check=',e10.4)
            !
            ! update the 'old' variables to the 'new' values, once per time step,
            ! when gas pressure convergence is acheived (gasflg = .TRUE.)
            IF (gasflg) THEN
                gbold(i2,j-1,1) = gb(i2,j-1,1)
                gpold(i2,j-1) = gp(i2,j-1)
                ggold(i2,j-1,1) = gg(i2,j-1,1)
                grsold(i2,j-1,1) = grs(i2,j-1,1)
                g1old(i2,j-1,1) = g1(i2,j-1,1)
                g2old(i2,j-1,1) = g2(i2,j-1,1)
                g3old(i2,j-1,1) = g3(i2,j-1,1)
                g4old(i2,j-1,1) = g4(i2,j-1,1)
                rlsold(i2,j-1) = rls(i2,j-1)
                IF (it == irefab) THEN
                    rlsold(i2,j-1) = 0.0_r8k
                    brnrefab(i2,j-1) = brn(i2,j-1)
                ENDIF
                brnold(i2,j-1) = brn(i2,j-1)
                tempkold(i2,j-1) = tempk
            END IF
            gg(i2,j-1,3) = gg(i2,j-1,1)
            gb(i2,j-1,3) = gb(i2,j-1,1)
            grs(i2,j-1,3) = grs(i2,j-1,1)
        END DO
        IF (it == irefab) EOSNodeburnuprefab(j-1) = EOSNodeburnup(j-1)
        ! Added gas release to account for high-burnup, low-power lwr gas releas
        ! An additional 1 percent gas release is accumulated for every 10 GWd/MTU above 40 GWd/MTU
        IF (it < irefab+1 .AND. rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k .AND. it > igas) &
          & rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - 40000.0_r8k) / 10000.0_r8k
        IF (it >= irefab+1 .AND. rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k &
          & .AND. it > igas) rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - &
          & MAX(40000.0_r8k, EOSNodeburnuprefab(j-1))) / 10000.0_r8k
        !
        ! CS/FPI CAD/1467/FRAPCON
        ! Instrumentation Scanair
        IF (nplot > 1) CALL scangas (gp, gg, gb, grs, rls, ansr)
        ! CS/FPI
    END IF
    !
    END SUBROUTINE massih
    !
    !
    !
    SUBROUTINE frapfgr (rv, dv)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : den, sigfgr, Power, gasavail1, gasavail2, crad, nr, ngasr, tfuelr, rc, &
      &                   dp, na, nplot, j, it, gasflg, delbp, delh, press, grnsize, rdot, EOSNodeburnup
    USE Refabrication, ONLY : irefab
    IMPLICIT NONE
    !>@brief
    !> Subroutine frapfgr is called by fgasre and returns cumulative fission gas release for the axial region (rdot)
    !> as well as gas on the grain boundaries for initialization of the FRAPTRAN FGR model.
    !> This Subroutine is based on a modified solution to the booth diffusion problem in Ref. (1)
    !>@author
    !> this version was programmed by k.j geelhood
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> October 23, 2007
    !
    ! Input
    !
    ! it      - current time step number
    ! j-1     - axial region number
    ! ngasr   - number of rings (must be greater than 10)
    ! nr      - number of radial nodes
    ! gasflg  - convergence signal on gas pressure iteration
    ! EOSNodeburnup - axial region burnup at end of step (MWd/mtU)
    ! delbp   - burnup increment (MWd/mtU)
    ! delh    - time increment (hr)
    ! dp(j-1) - pellet outer diameter (inches)
    ! crad    - radii of nodes from fuel centerline (in)
    ! rc      - pellet inner diameter (inches)
    ! den     - pellet density (per cent theoretical density)
    ! press   - gas pressure in the rod (psi)
    ! rv,dv   - array of rel.radial powers and diameters (in inches)
    !
    ! Output
    !
    ! rdot    - pellet vol. average cumulative release fraction
    !
    ! Internal (all concentrations in moles/m^3)
    !
    ! gp      - concentration of produced fission gas
    ! gb      - concentration of gas at grain boundaries
    ! gg      - concentration of gas within grains
    ! grs     - concentration of gas re-solved
    ! rls     - concentration of gas released
    ! gs      - saturation concentraion
    ! d       - diffusion constant, m^2 per second
    ! brn     - ring-specific burnup, MWd/mtU
    ! fr      - ring-specific cumulative release fraction
    ! tfuelr  - fuel node temperatures (F)
    !
    ! Reference:
    !
    ! (1)  k. forsberg and a. r. massih, journal of nuclear material, volume 135, 1985, pp. 140 to 148.
    !
    INTEGER(ipk) :: ken, timestep, axnode, i2, k2, i1
    REAL(r8k) :: dels, frden, rpext, grn, dvoid, ansda, routr, flxtot, routr2, ansdia, rdot2, &
      &          tempf, tempk, delbrn, pow, grnfuel, rimedge, rimstart, delprod, &
      &          d, bup, testr, bupm, adr, bb1, bken, z, deltau, dtau, arg1, f1, arg2, f2, arg3, f3, &
      &          arg4, f4, resolterm, term1, term2, term3, rq, delg1, delg2, delg3, delg4, delgg, dktest, &
      &          delgb, testken, rns, gs, delrls
    INTEGER(ipk), DIMENSION(:,:,:), POINTER :: openp
    REAL(r8k), DIMENSION(nr) :: rv, dv
    REAL(r8k), DIMENSION(ngasr) :: flxfc, fr
    REAL(r8k), DIMENSION(:), POINTER :: ansr
    ! Massih's constants for approximating the integration kernal
    REAL(r8k), PARAMETER :: a1 = 0.231545_r8k
    REAL(r8k), PARAMETER :: a2 = 0.0948453_r8k
    REAL(r8k), PARAMETER :: a3 = 0.0282898_r8k
    REAL(r8k), PARAMETER :: a4 = 0.645320_r8k
    REAL(r8k), PARAMETER :: b1 = 72.5968_r8k
    REAL(r8k), PARAMETER :: b2 = 796.773_r8k
    REAL(r8k), PARAMETER :: b3 = 29083.0_r8k
    REAL(r8k), PARAMETER :: b4 = 10.2469_r8k
    ! Assign Pointers
    ansr => FGRData%ansr
    ! Assign Pointers
    openp => FGRData%FRAPFGR%openp
    ! units changes
    dels = delh * hrtosec !Convert hours to seconds
    frden = den / 100.0_r8k
    ! rpext is pressure on bubbles, in atmospheres
    rpext = press * PSItoATM
    ! rpext = (400.0 * PSItoATM)
    ! grn = grain size (radius) in meters (fixed at 5 microns)
    grn = grnsize / 2.0_r8k * 1.0e-6_r8k
    If (it == 1) THEN
        ! Initialize variables on time step 1
        DO i2 = 1, ngasr
            DO k2 = 1, 2
                gb(i2,j-1,k2) = 0.0_r8k
                gbold(i2,j-1,k2) = 0.0_r8k
                gp(i2,j-1) = 0.0_r8k
                gpold(i2,j-1) = 0.0_r8k
                gg(i2,j-1,k2) = 0.0_r8k
                ggold(i2,j-1,k2) = 0.0_r8k
                grs(i2,j-1,k2) = 0.0_r8k
                grsold(i2,j-1,k2) = 0.0_r8k
                g1(i2,j-1,k2) = 0.0_r8k
                g2(i2,j-1,k2) = 0.0_r8k
                g3(i2,j-1,k2) = 0.0_r8k
                g4(i2,j-1,k2) = 0.0_r8k
                g1old(i2,j-1,k2) = 0.0_r8k
                g2old(i2,j-1,k2) = 0.0_r8k
                g3old(i2,j-1,k2) = 0.0_r8k
                g4old(i2,j-1,k2) = 0.0_r8k
                rls(i2,j-1) = 0.0_r8k
                rlsold(i2,j-1) = 0.0_r8k
                brn(i2,j-1) = 0.0_r8k
                brnold(i2,j-1) = 0.0_r8k
                grnold(i2,j-1) = grn
            END DO
        END DO
    ELSE
        ! ring volume calculation (equal-volume rings)
        dvoid = 2.0_r8k * rc(j-1)
        ansda = (dp(j-1) ** 2 - dvoid ** 2) / 4.0_r8k / REAL(ngasr)
        routr = dp(j-1) / 2.0_r8k
        flxtot = 0.0_r8k
        DO i1 = 1, ngasr
            routr2 = routr * routr
            ansr(i1) = SQRT(routr2 - ansda / 2.0_r8k)
            ansdia = ansr(i1) * 2
            flxfc(i1) = terp(ansdia, dv, rv,nr)
            If (i1 /= ngasr) routr = SQRT(routr2 - ansda)
            flxtot = flxtot + flxfc(i1)
        END DO
        ! normalize ring radial power factors to total = 1.0
        flxfc(1:ngasr) = flxfc(1:ngasr) / flxtot
        ! *******radial node loop********
        rdot = 0.0_r8k
        rdot2= 0.0_r8k
        DO i2 = 1, ngasr
            ! ******calculate ring-average temperature (K)***********
            tempf = terp(ansr(i2),crad(:,j-1),tfuelr,nr)
            tempk = tfk(tempf)
            ! moles gas produced per cubic meter = MWd/mtU*mtU/m^3*stpcm^3/MWd*moles/stpcm^3
            ! = delbp*(frden*10.96*0.88)*31.*(1./22420) = delbp*frden*0.013336
            ! also multiply by flxfc(i2)*ngasr to distribute production according
            ! to the radial power distribution. similarly distribute the burnuup increment.
            delbrn = delbp * flxfc(i2) * REAL(ngasr)
            brn(i2,j-1) = brnold(i2,j-1) + delbrn
            pow = Power(j-1) * flxfc(i2) * REAL(ngasr)
            ! Calculate Grain Growth using Khoruzhii Model JNM 265 p.112
            grnfuel = graingro(pow, delh, grnold(i2,j-1), tempk, dp(j-1), rc(j-1), den)
            ! Calculate Rim thickness from Manzel Optical Microscopy Data
            ! JNM 301 p. 170 If temperature is less than 1000 Deg. C
            If (tempk > 1273.15_r8k) THEN
                FGRData%FRAPFGR%restructure(j-1,i2) = 0.0_r8k
            ELSE 
                IF (EOSNodeburnup(j-1) > 30000.0_r8k) THEN
                    FGRData%FRAPFGR%rimthick(j-1) = 1.439e-6_r8k / 25400.0_r8k * ((EOSNodeburnup(j-1) / 1000.0_r8k) ** 4.427_r8k)
                ELSE
                    FGRData%FRAPFGR%rimthick(j-1) = 0.0_r8k
                END IF
                ! Find edge and start of rim
                rimedge = dp(j-1) / 2.0_r8k - FGRData%FRAPFGR%rimthick(j-1)
                rimstart = dp(j-1) / 2.0_r8k - 2.0_r8k * FGRData%FRAPFGR%rimthick(j-1)
                IF (ansr(i2) >= rimedge) THEN
                    FGRData%FRAPFGR%restructure(j-1,i2) = 1.0_r8k
                ELSEIF (ansr(i2) >= rimstart) THEN
                    FGRData%FRAPFGR%restructure(j-1,i2) = (ansr(i2) - rimstart) / (rimedge - rimstart)
                ELSE
                    FGRData%FRAPFGR%restructure(j-1,i2) = 0.0_r8k
                ENDIF
            END IF
            grn = FGRData%FRAPFGR%restructure(j-1,i2) * 0.35e-6_r8k + (1.0_r8k - FGRData%FRAPFGR%restructure(j-1,i2)) * grnfuel
            ! Calculate Porosity Using Model base on observations of high burnup fuel
            ! JNM 231p179, JNM 288p20, LWR Fuel Performance, 2000
            IF (brn(i2,j-1) >= 60000.0_r8k) THEN
                FGRData%FRAPFGR%porosity(j-1,i2) = 11.283_r8k * LOG(brn(i2,j-1) / 1000.0_r8k) - 45.621_r8k
            ELSE
                FGRData%FRAPFGR%porosity(j-1,i2) = 0.0_r8k
            END IF
            frden = den / 100.0_r8k - FGRData%FRAPFGR%porosity(j-1,i2) / 100.0_r8k
            !
            delprod = delbp * frden * 0.013336_r8k * flxfc(i2) * REAL(ngasr)
            gp(i2,j-1) = gpold(i2,j-1) + delprod
            ! Calculate diffusion for standard grains and restructured grains separately.
            DO k2 = 1, 2
                IF (k2 == 1) grn = grnfuel
                IF (k2 == 2) grn = 0.075e-6_r8k
                ! Calculate diffusion constant
                IF (tempk < 675.0_r8k) THEN
                    d = 1.51e-17_r8k * EXP(-9508.0_r8k / 675.0_r8k)
                ELSE IF (tempk < 1381.0_r8k) THEN
                    d = 1.51e-17_r8k * EXP(-9508.0_r8k / tempk)
                ELSE IF (tempk < 1650.0_r8k) THEN
                    d = 2.14e-13_r8k * EXP(-22884.0_r8k / tempk)
                ELSE IF (tempk < 1850.0_r8k) THEN
                    d = 7.14433e-10_r8k * EXP(-34879.0_r8k / tempk)
                ELSE
                    d = 7.14433e-10_r8k * EXP(-34879.0_r8k / 1850.0_r8k)
                END IF
                ! calcualte burnup enhancment factor bb1 Form and PARAMETER values based on guidance from ans5.4 plus
                ! reference to low-and high-burnup fgr data for cases with fgr.
                bup = brn(i2,j-1) - 21000.0_r8k
                testr = 0.0_r8k
                bupm = MAX(bup, testr)
                adr = MIN(brn(i2,j-1), 12000.0_r8k) / 12000.0_r8k * 10.0_r8k
                bb1 = 10.0_r8k ** (bupm / 40000.0_r8k) + adr
                bken = brn(i2,j-1) / 1000.0_r8k
                IF (k2 == 2) bb1 = MAX(1.0_r8k, -2.54132e-7_r8k * bken ** 4.0_r8k + 3.91491e-5_r8k * bken ** 3.0_r8k + &
                  & 2.32780e-2_r8k * bken ** 2.0_r8k - 3.92956_r8k * bken + 1.56649e2_r8k)
                IF (k2 == 2 .AND. bken > 90.0_r8k) bb1 = bb1 * (0.0084_r8k * bken + 0.2523_r8k)
                IF (bb1 > 49.81_r8k .AND. k2 == 1) bb1 = 49.81_r8k
                d = d * bb1
                ! User error function to account for effect of power on D
                z = pow - 3.0_r8k
                d = d / (1.0_r8k + 3.0_r8k * 0.5_r8k * (1.0_r8k - ERF(z)))
                ! add on uncertainty
                IF (sigfgr > 0.0_r8k) d = d * (1.0_r8k + sigfgr * 1.0_r8k)
                IF (sigfgr < 0.0_r8k) d = d / (1.0_r8k - sigfgr * 1.0_r8k)
                ! Assume all gas in restructured grains diffuses out.  Only resolved gas remaines in these grains.
                IF (k2 == 1) THEN
                    deltau = d * dels
                    dtau = deltau / (grn ** 2)
                    ! term1, term2, term3, and rq refer to terms in equation number 45 in forsberg/massih, jnm 135
                    ! (with no re-solution, i.e., h4 = 0, and production constant througout the time step)
                    ! f1,f2,f3, and g1,g2,g3 are defined in equation 43.
                    ! Alternate calculations shown here for f1, f2, and f3 are done to prevent underflows or poor approximations
                    arg1 = b1 * dtau
                    IF (arg1 > 4.0e-4_r8k .AND. arg1 < 85.0_r8k) f1 = EXP(-b1 * dtau) - 1.0_r8k
                    IF (arg1 < 4.0e-4_r8k) f1 = -b1 * dtau
                    IF (arg1 > 85.0_r8k) f1 = -1.0_r8k
                    arg2 = b2 * dtau
                    IF (arg2 > 4.0e-4_r8k .AND. arg2 < 85.0_r8k) f2 = EXP(-b2 * dtau) - 1.0_r8k
                    IF (arg2 < 4.0e-4_r8k) f2 = -b2 * dtau
                    IF (arg2 > 85.0_r8k) f2 = -1.0_r8k
                    arg3 = b3 * dtau
                    IF (arg3 > 4.0e-4_r8k .AND. arg3 < 85.0_r8k) f3 = EXP(-b3 * dtau) - 1.0_r8k
                    IF (arg3 < 4.0e-4_r8k) f3 = -b3 * dtau
                    IF (arg3 > 85.0_r8k) f3 = -1.0_r8k
                    arg4 = b4 * dtau
                    IF (arg4 > 4.0e-4_r8k .AND. arg4 < 85.0_r8k) f4 = EXP(-b4 * dtau) - 1.0_r8k
                    IF (arg4 < 4.0e-4_r8k) f4 = -b4 * dtau
                    IF (arg4 > 85.0_r8k) f4 = -1.0_r8k
                    ! Resolution term as a function of temp and use in the calculation of term1, term2, and term3
                    IF (tempk <= 1528.77_r8k) THEN
                        resolterm = MAX(1.0_r8k, 0.14009_r8k * EXP(0.00282_r8k * tempk))
                    ELSE
                        resolterm = MAX(1.0_r8k, 22.976_r8k - 0.0082_r8k * tempk)
                    END IF
                    term1 = (f1 * a1 / b1 + f2 * a2 / b2 + f3 * a3 / b3 + f4 * a4 / b4) / resolterm
                    ! Short-term approximation of term2
                    term2 = (2.2568_r8k * dtau ** 1.5_r8k - 1.5_r8k * dtau ** 2) / resolterm
                    ! Long-term approximation of term2
                    IF (dtau > 0.1_r8k) term2 = (dtau - 0.0616_r8k * (1.0_r8k - EXP(-9.87_r8k * dtau))) / resolterm
                    !
                    term3 = (f1 * g1old(i2,j-1,k2) + f2 * g2old(i2,j-1,k2) + f3 * g3old(i2,j-1,k2) + &
                      &      f4 * g4old(i2,j-1,k2)) / resolterm
                    rq = delprod / (term2 - term1) / grn ** 2
                    delg1 = -rq * grn ** 2 * (a1 / b1) * f1 + f1 * g1old(i2,j-1,k2)
                    delg2 = -rq * grn ** 2 * (a2 / b2) * f2 + f2 * g2old(i2,j-1,k2)
                    delg3 = -rq * grn ** 2 * (a3 / b3) * f3 + f3 * g3old(i2,j-1,k2)
                    delg4 = -rq * grn ** 2 * (a4 / b4) * f4 + f4 * g4old(i2,j-1,k2)
                    delgg = delg1 + delg2 + delg3 + delg4
                    ! Make sure increase in gas in grains is <= increase in production + gb gas from previous time steps
                    ! (sum of sources)
                    dktest = delprod + gbold(i2,j-1,k2) + grsold(i2,j-1,k2)
                    IF (delgg > dktest) THEN
                        delgg = dktest
                        delg1 = delgg * delg1 / (delg1 + delg2 + delg3 + delg4)
                        delg2 = delgg * delg2 / (delg1 + delg2 + delg3 + delg4)
                        delg3 = delgg * delg3 / (delg1 + delg2 + delg3 + delg4)
                        delg4 = delgg * delg4 / (delg1 + delg2 + delg3 + delg4)
                    END IF
                    ! Makes sure that decrease in gas in grains is not greater than what was on grains in previous time step
                    ! (gas on grains for this time step cannot be negative)
                    dktest = -ggold(i2,j-1,k2)
                    IF (delgg < dktest) THEN
                        delgg = dktest
                        delg1 = delgg * delg1 / (delg1 + delg2 + delg3 + delg4)
                        delg2 = delgg * delg2 / (delg1 + delg2 + delg3 + delg4)
                        delg3 = delgg * delg3 / (delg1 + delg2 + delg3 + delg4)
                        delg4 = delgg * delg4 / (delg1 + delg2 + delg3 + delg4)
                    END IF
                    !
                    g1(i2,j-1,k2) = g1old(i2,j-1,k2) + delg1
                    g2(i2,j-1,k2) = g2old(i2,j-1,k2) + delg2
                    g3(i2,j-1,k2) = g3old(i2,j-1,k2) + delg3
                    g4(i2,j-1,k2) = g4old(i2,j-1,k2) + delg4
                    !
                    gg(i2,j-1,k2) = ggold(i2,j-1,k2) + delgg
                    delgb = delprod - delgg
                    ! simple calculation for restructured grains
                ELSE IF (k2 == 2) THEN
                    ! resultion factor from Massih
                    testken = 0.075e-6_r8k * 1.84e-14_r8k / 3.0_r8k / d
                    gg(i2,j-1,k2) = (gbold(i2,j-1,k2)) * testken / (1 + testken)
                    delgb = delprod - (gg(i2,j-1,k2) - ggold(i2,j-1,k2))
                END IF
                gb(i2,j-1,k2) = gbold(i2,j-1,k2) + delgb
                ! calculation of the grain boundary saturation concentration, gs.
                ! saturation concentration in moles / m^2, assuming bubble size is 0.5 mi
                ! dihedral half-angle = 50 degrees, and surface tension = 0.6 j / m^2.
                rns = (4.36e-8_r8k / tempk) * (0.48e6_r8k + rpext * 1.013e5_r8k)
                gs = 3.0_r8k * rns / (2.0_r8k * grn)
                ! test for grain boundary gas saturation
                IF (openp(i2,j-1,k2) /= 1) THEN
                    IF (gb(i2,j-1,k2) <= gs) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = gb(i2,j-1,k2) - 0.65_r8k * gs
                        gb(i2,j-1,k2) = 0.65_r8k * gs
                        openp(i2,j-1,k2) = 1
                    END IF
                ELSE
                    IF (gb(i2,j-1,k2) <= gs * 0.65_r8k) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = gb(i2,j-1,k2) - 0.65_r8k * gs
                        gb(i2,j-1,k2) = 0.65_r8k * gs
                    END IF
                END IF
                ! combine release from standard grains and restructured grains
                IF (k2 == 1) rls(i2,j-1) = rlsold(i2,j-1) + delrls * (1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2)))
                IF (k2 == 2) rls(i2,j-1) = rls(i2,j-1) + delrls * SQRT(FGRData%FRAPFGR%restructure(j-1,i2))
            END DO
            gbtot(i2,j-1) = (1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * gb(i2,j-1,1) + &
              &              SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * gb(i2,j-1,2)
            ! Calculate fractional release and rdot
            fr(i2) = rls(i2,j-1) / gp(i2,j-1)
            rdot = fr(i2) * brn(i2,j-1) / EOSNodeburnup(j-1) / REAL(ngasr) + rdot
            rdot2 = rdot2 + gbtot(i2,j-1) / gp(i2,j-1) * brn(i2,j-1) / EOSNodeburnup(j-1) / REAL(ngasr)
            ! Calculate gas available for Transient Release
            gasavail1(i2,j-1) = ((1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * (gb(i2,j-1,1)) + &
              &                 SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * (gb(i2,j-1,2))) * brn(i2,j-1) / &
              &                 gp(i2,j-1) / EOSNodeburnup(j-1) / REAL(ngasr)
            gasavail2(i2,j-1) = ((1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * (0.05_r8k * gg(i2,j-1,1)) + &
              &                 SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * (gg(i2,j-1,2))) * brn(i2,j-1) / gp(i2,j-1) &
              &                 / EOSNodeburnup(j-1) / REAL(ngasr)
            ! Combine gas in grains, on grain boundaries, and resolved from restructured and non-restrucutred grains for plotting
            gg(i2,j-1,3) = (1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * gg(i2,j-1,1) + &
              &             SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * gg(i2,j-1,2)
            gb(i2,j-1,3) = (1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * gb(i2,j-1,1) + &
              &             SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * gb(i2,j-1,2)
            grs(i2,j-1,3) = (1.0_r8k - SQRT(FGRData%FRAPFGR%restructure(j-1,i2))) * grs(i2,j-1,1) + &
              &              SQRT(FGRData%FRAPFGR%restructure(j-1,i2)) * grs(i2,j-1,2)
            ! Check for convergence on rod pressure
            IF (gasflg) THEN
                ! Update old values for next time step
                DO k2 = 1, 2
                    gbold(i2,j-1,k2) = gb(i2,j-1,k2)
                    gpold(i2,j-1) = gp(i2,j-1)
                    ggold(i2,j-1,k2) = gg(i2,j-1,k2)
                    grsold(i2,j-1,k2) = grs(i2,j-1,k2)
                    g1old(i2,j-1,k2) = g1(i2,j-1,k2)
                    g2old(i2,j-1,k2) = g2(i2,j-1,k2)
                    g3old(i2,j-1,k2) = g3(i2,j-1,k2)
                    g4old(i2,j-1,k2) = g4(i2,j-1,k2)
                    rlsold(i2,j-1) = rls(i2,j-1)
                    IF (it == irefab) rlsold(i2,j-1) = 0.0_r8k
                    brnold(i2,j-1) = brn(i2,j-1)
                    grnold(i2,j-1) = grnfuel
                END DO

            END IF
        END DO
        IF (it == irefab) EOSNodeburnuprefab(j-1) = EOSNodeburnup(j-1)
        !
        ! Added gas release to account for high-burnup, low-power lwr gas release
        ! An additional 1 percent gas release is accumulated for every 10 GWd/MTU above 40 GWd/MTU
        !
        IF (it < irefab+1 .AND. rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k) &
          & rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - 40000.0_r8k) / 10000.0_r8k
        IF (it >= irefab+1 .AND. rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k) &
          & rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - &
          & MAX(40000.0_r8k, EOSNodeburnuprefab(j-1))) / 10000.0_r8k
        !
        !
        ! CS/FPI CAD/1467/FRAPCON
        ! Instrumentation Scanair
        IF (nplot > 1) CALL scangas (gp, gg, gb, grs, rls, ansr)
        ! CS/FPI
        !
    END IF
    !
    END SUBROUTINE frapfgr
    !
    !
    !
    REAL(r8k) FUNCTION graingro (pow, delh, grnin, tempK, dfs, rc, den)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !
    ! Input
    !
    ! pow      - local power, kW/ft
    ! delh     - time incrament, hours
    ! grnin    - grain size (radius) from previous time step, m
    ! tempK    - temperature, K
    ! dfs      - pellet outer diameter, in
    ! rc       - pellet inner radius, in
    ! den      - percent theoretical density
    !
    ! Output
    !
    ! graingro - grain size (radius) at end of time step, m
    !
    INTEGER(ipk) :: i, j, itime
    REAL(r8k) :: fdot, grn, grnold, k, inva, invamax, invair, dadt, dtime, frden
    REAL(r8k), INTENT(IN) :: pow, delh, grnin, tempK, dfs, rc, den
    !
    frden = den / 100.0_r8k
    ! Convert grain size in meters and radius to microns and diameter
    grnold = grnin * 1.0e6_r8k * 2.0_r8k
    fdot = pow * 0.6713_r8k / frden / (dfs ** 2.0_r8k - (rc * 2.0_r8k) ** 2.0_r8k)
    ! fdot = pow * 0.6713_r8k / frden / (0.413_r8k ** 2.0_r8k)
    itime = MAX(NINT(delh), 1)
    dtime = delh / REAL(itime)
    !
    k = 5.24e7_r8k * EXP(-32100.0_r8k / tempK)
    invamax = 1.0_r8k / 2.23e3_r8k * EXP(7620.0_r8k / tempK)
    invair = fdot / 50.0_r8k * tempK / 1400.0_r8k * 1.0_r8k / 326.5_r8k * EXP(5620.0_r8k / tempK)
    DO i = 1, itime
        inva = 1.0_r8k / grnold
        dadt = MAX(0.0_r8k, k * (inva - invamax - invair))
        grn = grnold + dtime * dadt
        grnold = grn
    END DO
    ! Convert output from microns and diameter to meters and radius
    graingro = grn * 1.0e-6_r8k * 0.5_r8k
    !
    END FUNCTION graingro
    !
    !
    !
    SUBROUTINE totgas
    USE Kinds
    USE Conversions
    USE Variables, ONLY : acmfg, acmn2, acmhe, acmh2, acmh2o, amgpt, hmgpt, he_ifba, imox, &
      &                   den, EOSNodeburnup, dp, deltaz, amfhe, amfh2, amfn2, amfarg, amfkry, &
      &                   amfxe, amfh2o, ifbarel, addgmles, ngasr, angr, fmgr, hmgr, tn2fr, &
      &                   fgmgp, hemgp, ang, gasmo, nread, gases, it, kryin, ir1, h2in, th2ofr, &
      &                   airin, angi, an2in, argin, fgin, hein, h2omi, xein, tfgfr, thefr, &
      &                   amffg, sgapf, h2oin, ah2ogr, gmlesAsFab, gmlesReleased, jmin, jmax
    USE Refabrication, ONLY : irefab
    IMPLICIT NONE
    !>@brief
    !> The Subroutine totgas calculates the cumulative gas release of fission gas, helium, and nitrogen for the entire rod.
    !> The total moles of gas and mole fractions are also computed.
    !>@author
    !> coded for frapcon by g a berna in october 1977 using the routine of the same name in fraps
    !
    ! Input
    !
    ! airin         - initial moles of air in the rod
    ! amfh2         - absolute mole fraction of hydrogen
    ! ang           - cumulative n2 concentration in fuel per power step (moles)
    ! angi          - initial nitrogen concentration in the fuel (moles)
    ! angr          - nitrogen release per node & power step (moles)
    ! an2in         - initial moles of n2 in the rod
    ! argin         - initial moles of argon in the rod
    ! EOSNodeburnup - burnup at each axial node at End of step (MWd/mtu)
    ! fgin          - initial moles of fission gas in the rod
    ! fgmgp         - fission gas production for each node (moles)
    ! fmgr          - cumulative fission gas release per node & time step(moles)
    ! fuelTD        - theoretical density (g/cm3)
    ! gases         - mole fractions of gas constituents
    !             1 - helium
    !             2 - argon
    !             3 - krypton
    !             4 - xenon
    !             5 - hydrogen
    !             6 - nitrogen
    !             7 - Air
    !             8 - water vapor
    ! hein          - initial moles of helium in the rod
    ! hemgp         - helium  production for each node (moles)
    ! hmgr          - cumulative helium release per node & time step (moles)
    ! h2omi         - initial water content in the fuel (moles)
    ! ir1           - number of axial nodes plus one
    ! it            - power-time step index
    ! kryin         - initial moles of krypton in the rod
    ! ngasr         - number of radial segments for fission gas release model
    ! nread         - restart Read index
    ! ntstep        - time step index
    ! ir1           - number of axial nodes plus one
    ! xein          - initial moles of xenon in the rod
    ! fuelTD        - material theoretical density (g/cc)
    !
    ! Output
    !
    ! acmfg         - cumulative fission gas release (gram-moles)
    ! acmn2         - cumulative nitrogen release (gram-moles)
    ! acmhe         - cumulative helium release (gram-moles)
    ! acmh2         - cumulative hydrogen release (gram-moles)
    ! acmh2o        - cumulative water vapor release (gram-moles)
    ! amffg         - absolute mole fraction of fission gas (gram-moles)
    ! amfhe         - absolute mole fraction of helium
    ! amfkry        - absolute mole fraction of krypton
    ! amfn2         - absolute mole fraction of nitrogen
    ! amfxe         - absolute mole fraction of xenon
    ! amgpt         - cumulative fission gas production (gm-moles)
    ! angt          - nitrogen consentration (gm-moles)
    ! gasmo         - gram-moles of gas in rod
    ! hmgpt         - cumulative helium production (gm-moles)
    ! tfgfr         - cumulative fission gas fraction release
    ! thefr         - cumulative helium fraction release
    ! th2ofr        - cumulative water vapor fraction release
    ! tn2fr         - cumulative nitrogen fraction release
    !
    INTEGER(ipk) :: il, jj, lt, i
    REAL(r8k) :: factor, sprod, frac, volume, prod, fgr, fhr, amfair, fuelTD, temp, angt
    REAL(r8k), DIMENSION(2) :: amgp, amgph, amgr, amgrh, amgrn2, amgrh2o, fjj, fjjh
    !
    fuelTD = MatProp ('FUEL', 'TDENSITY', temp)
    !
    il = MAX(it - 1, 1)
    IF (nread == 1) THEN
        il = it
        IF (it <= 3) THEN
            amgp(1) = 0.0_r8k
            amgph(1) = 0.0_r8k
            fjjh(1) = 0.0_r8k
            fjj(1) = 0.0_r8k
            amgrn2(1) = 0.0_r8k
            amgrh2o(1) = 0.0_r8k
        END IF
    END IF
    ! **********
    ! ans-5.4 modifications
    ! factor = conversion from mwd to moles fission gas
    ! frac   = fraction gas release at each axial node-cumulative
    ! prod   = moles fission gas produced at each axial node-cumulative
    ! volume = volume of fuel in axial node
    factor = sgapf / 100.0_r8k *  4.477e-3_r8k
    sprod = 0.0_r8k
    DO jj = jmin, jmax
        frac = fmgr(jj-1,2) / fgmgp(jj-1,2)
        volume = pi * ((dp(jj-1) / 2.0_r8k) ** 2 * (deltaz(jj-1) * fttoin)) * in3tocm3
        prod = volume * den / 100.0_r8k * fuelTD * 0.88_r8k / 1.0e6_r8k * EOSNodeburnup(jj-1) * factor
        sprod = sprod + prod
        fmgr(jj-1,2) = frac * prod
    END DO
    ! **********
    DO lt = il, it
        i = 2 + lt - it
        amgr(i) = 0.0_r8k
        amgrh(i) = 0.0_r8k
        amgrn2(i) = 0.0_r8k
        amgrh2o(i) = 0.0_r8k
        angt = 0.0_r8k
        amgp(i) = 0.0_r8k
        amgph(i) = 0.0_r8k
        DO jj = jmin, jmax
            amgr(i) = amgr(i) + fmgr(jj-1,i)
            amgrh(i) = amgrh(i) + hmgr(jj-1,i)
            amgp(i) = amgp(i) + fgmgp(jj-1,i)
            amgph(i) = amgph(i) + hemgp(jj-1,i)
            angt = angt + ang(jj-1,2)
            amgrn2(i) = amgrn2(i) + angr(jj-1,i)
            amgrh2o(i) = amgrh2o(i) + ah2ogr(jj-1,i)
        END DO
        fjj(i) = amgr(i) / amgp(i)
        fjjh(i) = amgrh(i) / amgph(i)
    END DO
    IF (it == 1) THEN
        amgpt(1) = amgp(2)
        hmgpt(1) = amgph(2)
        acmfg(it) = amgr(2)
        acmhe(it) = amgrh(2)
        acmn2(it) = amgrn2(2)
        acmH2O(it) = amgrh2o(2)
    ELSE
        amgpt(it) = amgpt(it-1) + amgp(2) - amgp(1)
        amgpt(it) = sprod
        hmgpt(it) = hmgpt(it-1) + amgph(2) - amgph(1)
        ! Calculation of cumulative gas releases in gram-moles
        ! Fission gas release
        fgr = 2.0_r8k * (fjj(2) * amgp(2) - fjj(1) * amgp(1)) / (amgp(2) * (1.0_r8k - fjj(2)) + amgp(1) * (1.0_r8k - fjj(1)))
        acmfg(it) = amgr(2)
        IF (acmfg(it) > amgpt(it)) acmfg(it) = amgpt(it)
        ! Helium release
        fhr = 2.0_r8k * (fjjh(2) * amgph(2) - fjjh(1) * amgph(1)) / (amgph(2) * (1.0_r8k - fjjh(2)) + &
          &   amgph(1) * (1.0_r8k - fjjh(1)))
        IF (it == irefab + 1) acmhe(it-1) = 0.0_r8k
        acmhe(it) = acmhe(it-1) + (hmgpt(it-1) + hmgpt(it) - 2.0_r8k * acmhe(it-1)) * fhr / (fhr + 2.0_r8k)
        IF (acmhe(it) > hmgpt(it)) acmhe(it) = hmgpt(it)
        ! Nitrogen release
        acmn2(it) = MIN((acmn2(it-1) + amgrn2(2) - amgrn2(1)), angi)
        ! Water release
        acmH2O(it) = MIN((acmH2O(it-1) + amgrh2o(2) - amgrh2o(1)), h2omi)
    END IF
    ! calculation of total moles of gas in gap
    he_ifba(it) = SUM(ifbarel(jmin-1:jmax-1))
    ! Account for additional gmles of gas supplied by user, assuming that the additional moles of gas are Helium
    IF (it > 1) hein = hein + addgmles(it) - addgmles(it-1)
    ! Calculate total # of gas moles in rod (sum of as-fabricated and released)
    gmlesAsFab = hein + argin + fgin + h2in + an2in + airin + h2oin
    gmlesReleased = acmfg(it) + acmhe(it) + acmn2(it) + acmh2(it) + acmh2o(it) + he_ifba(it)
    gasmo(it-1) = gmlesAsFab + gmlesReleased
    ! Calculation of mole fractions of gap gases
    amffg = (acmfg(it) + fgin) / gasmo(it-1)
    amfair = airin / gasmo(it-1)
    amfn2 = (acmn2(it) + an2in) / gasmo(it-1)
    amfh2 = h2in / gasmo(it-1)
    gases(1) = (acmhe(it) + hein + he_ifba(it)) / gasmo(it-1)
    gases(2) = argin / gasmo(it-1)
    SELECT CASE (imox)
    CASE (0) ! Xe/Kr ratio for UO2 is 5.67
        gases(3) = (0.15_r8k * acmfg(it) + kryin) / gasmo(it-1)
        gases(4) = (0.85_r8k * acmfg(it) + xein) / gasmo(it-1)
    CASE (1, 2) ! Xe/Kr ratio for MOX is 16.0
        gases(3) = (0.0588_r8k * acmfg(it) + kryin) / gasmo(it-1)
        gases(4) = (0.9412_r8k * acmfg(it) + xein) / gasmo(it-1)
    END SELECT
    gases(5) = amfh2
    gases(6) = amfn2
    gases(7) = amfair
    gases(8) = (acmH2O(it) + h2oin) / gasmo(it-1)
    amfhe = gases(1)
    amfarg = gases(2)
    amfxe = gases(4)
    amfkry = gases(3)
    ! calculation of cumulative fraction releases
    ! Fission Gas
    IF (ABS(amgpt(it)) > 0.0_r8k) tfgfr = acmfg(it) / amgpt(it)
    ! Helium
    IF (ABS(hmgpt(it)) > 0.0_r8k) thefr = acmhe(it)/ hmgpt(it)
    ! Helium from B-10 reaction
    ! Note: It is assumed that all helium produced from Rxn is released
    ! Nitrogen in the fuel
    IF (ABS(angi) > 0.0_r8k) tn2fr = acmn2(it) / angi
    ! Water in the fuel
    IF (ABS(h2omi) > 0.0_r8k) th2ofr = acmH2O(it) / h2omi
    !
    END SUBROUTINE totgas
    !
    !
    !
    SUBROUTINE scangas (gp, gg, gb, grs, rls, ansr)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : j
    IMPLICIT NONE
    !>@brief
    !>
    !>@author
    !> IRSN
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> 2014
    !
    ! Input
    !
    ! gp  - concentration of produced fission gas
    ! rls - concentration of gas released
    !
    INTEGER(ipk) :: irad, jaxi
    REAL(r8k), DIMENSION(ngasr) :: tmpgas
    REAL(r8k), DIMENSION(:), INTENT(IN) :: ansr
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: gp, rls
    REAL(r8k), DIMENSION(:,:,:), INTENT(IN) :: gg, gb, grs
    !
    DO irad = 1, ngasr
        DO jaxi = 1, (na - 1)
            conGas(irad,jaxi) = gp(irad,jaxi) - rls(irad,jaxi)
            proGas(irad,jaxi) = gp(irad,jaxi)
            relGas(irad,jaxi) = rls(irad,jaxi)
            intraGas(irad,jaxi) = gg(irad,jaxi,3)
            interGas(irad,jaxi) = gb(irad,jaxi,3)
            resolGas(irad,jaxi) = grs(irad,jaxi,3)
        END DO
        rdGas(irad,j-1) = ansr(ngasr-irad+1)
        tmpgas(irad) = ansr(ngasr-irad+1)
    END DO
    rdGas(1,j-1) = rc(j-1)
    DO irad = 2, (ngasr + 1)
        rdGas(irad,j-1) = SQRT(2.0_r8k * tmpgas(irad-1) ** 2 - rdGas(irad-1,j-1) ** 2)
    END DO
    !
    END SUBROUTINE scangas
    !
    !
    !
    SUBROUTINE ans54_2011 (brnup, rv, dv)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, jst, qmpy, dt, prdct, ansd, den, ounit, crad, nr, im, &
                          qaxnorm, ngasr, na, rc, tfuelr, dp, releas, it, j, ir1, dcoBOL, &
                          prdct, totl, deltaz, na, RB_axial, RB_rod, Power
    IMPLICIT NONE
    !>@brief
    !> The subroutine ans54_2011 implements the ANS 5.4 2011 standard to calculate fission gas release.
    !> The source term with isotopic concentrations for the "gap activity" is provided in the output file.
    !>@author
    !> Dr. Gianluca Longoni, PNNL
    !> Updated by Ian Porter, NRC
    !>@date
    !> July 2015
    !> 5/19/2016
    !
    INTEGER(ipk) :: i1, ir, n
    INTEGER(ipk), PARAMETER :: nshortnuclides = 11
    INTEGER(ipk), PARAMETER :: nlongnuclides = 4
    INTEGER(ipk), PARAMETER :: nnuclides = nshortnuclides + nlongnuclides
    REAL(r8k), INTENT(IN) :: brnup
    REAL(r8k), PARAMETER :: pi2 = pi ** 2
    REAL(r8k), PARAMETER :: pi4 = pi2 ** 2
    REAL(r8k) :: dvoid, ansda, routr, routr2, ansdia, tempf, tempk, Tlink, lhgr, S_V, &
      &          fdot, D, Pave, Paxial, flxtot, Pring, Vnode, Vrod, Vring, F_scaling
    REAL(r8k), DIMENSION(nr) :: rv, dv
    REAL(r8k), DIMENSION(ngasr) :: flxfac
    REAL(r8k), DIMENSION(nnuclides) :: xrdot
    REAL(r8k), DIMENSION(:), POINTER :: ansr
    ! Precursor coefficients for radioactive nuclides
    ! Short-lived nuclides half-life < 6h
    ! 1 = Xe-135m
    ! 2 = Xe-137
    ! 3 = Xe-138
    ! 4 = Xe-139
    ! 5 = Kr-85m
    ! 6 = Kr-87
    ! 7 = Kr-88
    ! 8 = Kr-89
    ! 9 = Kr-90
    ! 10 = I-132
    ! 11 = I-134
    REAL(r8k), DIMENSION(nshortnuclides), PARAMETER :: alpha_s = [ 23.5_r8k, 1.07_r8k, 1.00_r8k, 1.00_r8k, &
      &                                                            1.31_r8k, 1.25_r8k, 1.03_r8k, 1.21_r8k, &
      &                                                            1.11_r8k, 137.0_r8k, 4.40_r8k ]
    REAL(r8k), DIMENSION(nshortnuclides), PARAMETER :: lambda_s = [ 7.55e-4_r8k, 3.02e-3_r8k, 8.19e-4_r8k, 1.75e-2_r8k, &
      &                                                             4.30e-5_r8k, 1.52e-4_r8k, 6.78e-5_r8k, 3.35e-3_r8k, &
      &                                                             2.15e-2_r8k, 8.44e-5_r8k, 2.20e-4_r8k ]
    ! Long-lived nuclides half-life > 6h < 60 days
    ! 1 = Xe-133
    ! 2 = Xe-135
    ! 3 = I-131
    ! 4 = I-133 (missing I-135 and Xe-131m)
    REAL(r8k), DIMENSION(nlongnuclides), PARAMETER :: alpha_l = [ 1.25_r8k, 1.85_r8k, 1.0_r8k, 1.21_r8k ]
    ! Decay constant per each nuclide in sec^-1
    REAL(r8k), DIMENSION(nlongnuclides), PARAMETER :: lambda_l = [ 1.53e-6_r8k, 2.12e-5_r8k, 9.98e-7_r8k, 9.26e-6_r8k ]
    ! Short and long-lived
    REAL(r8k), DIMENSION(nnuclides), PARAMETER :: alpha = [ alpha_s(1:nshortnuclides), alpha_l(1:nlongnuclides) ]
    REAL(r8k), DIMENSION(nnuclides), PARAMETER :: lambda = [ lambda_s(1:nshortnuclides), lambda_l(1:nlongnuclides) ]

    
    ! When it eq 1 time is assumed to be zero so skip gas release
    IF (it == 1) RETURN
    ! Assign pointer
    ansr => FGRData%ansr
    ! Check to see if case is within subroutine's limits
    IF (na < 11) WRITE (ounit,350) (na-1)
    !
    ! Rod average (Pave) and node average (Paxial) LHGR in W/cm at each time step
    Pave = (qmpy(it) * BTUhtoW) * (dcoBOL * intoft / ftocm) * pi
    Paxial = Pave * qaxnorm(j-1,jst(it))
    !
    dvoid = 2.0_r8k * rc(j-1)
    ansda = (dp(j-1) ** 2 - dvoid ** 2) / 4.0_r8k / REAL(ngasr)
    routr = dp(j-1) / 2.0_r8k
    ! calculate relative production rates in each ngasr fuel region
    ! all the radial regions formed here have equal areas, not equal widths.
    ! ansr = radius term
    ! ansdia = diameter term
    ! ansda = area term
    ! routr = radius term
    ! routr2 = area term
    flxtot = 0.0_r8k
    DO i1 = 1, ngasr
        routr2 = routr ** 2
        ansr(i1) = SQRT(routr2 - ansda / 2.0_r8k)
        ansdia = ansr(i1) * 2.0_r8k
        flxfac(i1) = terp(ansdia,dv,rv,nr)
        IF (i1 /= ngasr) routr = SQRT(routr2 - ansda)
        flxtot = flxtot + flxfac(i1)
    END DO
    !
    ! Note: IP 7/13/2015
    !
    ! This does not need to be re-calculated every timestep becuse dp is as-fabricated
    ! Also, this does not take into account the dish or chamfer
    ! Why not use values calculated in Initialization, lines ~ 700 - 750?
    !
    ! Calculate fuel volumes (cm^3)
    Vring = (deltaz(j-1) * fttoin) * pi * ansda * in3tocm3
    Vnode = (deltaz(j-1) * fttoin) * pi * (dp(j-1) / 2.0_r8k) ** 2 * in3tocm3
    Vrod = (totl * fttoin) * (pi / 4.0_r8k) * dp(j-1) ** 2 * in3tocm3
    !
    ! End of note
    !
    ! Normalize the production rate so their sum equals 1
    flxfac(1:ngasr) = flxfac(1:ngasr) / flxtot
    xrdot = 0.0_r8k
    DO ir = 1, ngasr
        ! Ring fuel temperature (K)
        tempf = terp(ansr(ir),crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        
        ! Ring node average LHGR in W/cm
        prdct(ir,j-1,it) = flxfac(ir) * REAL(ngasr)
        Pring = prdct(ir,j-1,it) * Paxial
        
        ! Temperature (K) at which bubbles become interlinked to grain boundaries
        IF (brnup * 1.0e-3_r8k <= 1.0e-2_r8k) THEN
            Tlink = 5000.0_r8k
        ELSE IF (brnup * 1.0e-3_r8k <= 18.2_r8k .AND. brnup * 1.0e-3_r8k > 1.0e-2_r8k) THEN
            Tlink = 9800.0_r8k / LOG(176.0_r8k * brnup * 1.0e-3_r8k) + 273.0_r8k
        ELSE IF (brnup * 1.0e-3_r8k > 18.2_r8k) THEN
            Tlink = 1434.0_r8k - 12.85_r8k * brnup * 1.0e-3_r8k + 273.0_r8k
        END IF
        
        ! Surface area to volume ratio (S_V, units = cm^-1)
        IF (tempk <= Tlink) THEN
            S_V = 120.0_r8k ! cm^-1
        ELSE
            S_V = 650.0_r8k ! cm^-1
        END IF
        
        ! Fission rate (Fdot) fissions / (cm^3 * sec)
        !fdot = 4.0e10_r8k * Paxial / ((dp(j-1) / 2.0_r8k * intocm) ** 2 - (dvoid * intocm) ** 2)
        Fdot = 4.0e10_r8k * Paxial / ((dp(j-1) * intocm) ** 2 - (dvoid * intocm) ** 2)
        ! Diffusion coefficient (D)
        D = 7.6e-7_r8k * EXP(-35000.0_r8k / tempk) + 1.41e-18_r8k * SQRT(Fdot) * EXP(-13800.0_r8k / tempk) + &
          & 2.0e-30_r8k * Fdot ! cm^2/sec
        DO n = 1, nnuclides
            IF (n <= nshortnuclides) THEN
                ! Short-lived nuclides half-life < 6h
                xrdot(n) = xrdot(n) + ((Pring * Vring) / (Paxial * Vnode)) * S_V * SQRT(alpha(n) * D / lambda(n))
            ELSE
                ! Long-lived nuclides half-life > 6h < 60 days
                F_scaling = ((alpha(n) * lambda(5)) / (lambda(n) * alpha(5))) ** 0.25_r8k
                xrdot(n) = xrdot(n) + ((Pring * Vring) / (Paxial * Vnode)) * F_scaling * S_V * &
                 &         SQRT(alpha(5) * D / lambda(5))
            ENDIF
        END DO
    END DO
    
    !
    RB_axial(:,j-1) = ((Paxial * Vnode) / (Pave * Vrod)) * xrdot
    
    DO n = 1, nnuclides
        RB_rod(n,it) = SUM(RB_axial(n,:))
    END DO
!    rdot = SUM(RB_rod(:,it))
350 FORMAT (1x,100('*')/5x,'Problem may not satisfy conditions for ans-5.4 fission gas release standard'/5x, &
      &                    'number of axial regions only',i3/1x,100('*'))
    END SUBROUTINE ans54_2011
    !
    !
    !
    SUBROUTINE gasplt
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, ounit, releas, it
    IMPLICIT NONE
    !>@brief
    !> gasplt should be used to output the fraction of shortlived radioactive gases released as predicted
    !> by the ANS-5.4 gas release model
    !>@author
    !> coded by w n rausch of battelle-northwest
    !
    ! Input
    !
    ! releas          - release array
    ! it              - current time step number + 1
    ! ProblemTime(it) - current time in seconds
    !
    ! Output
    !
    ! h      - half-life array
    ! td     - current time in days
    !
    INTEGER(ipk) :: i, j, k
    REAL(r8k) :: deltim, rtime, td, dif, p
    REAL(r8k), DIMENSION(11) :: h
    CHARACTER(LEN=5), DIMENSION(11) :: s, r
    !
    deltim = 0.5_r8k
    DO i = 1, 11
        rtime = 1.0_r8k + (i - 1) * deltim
        h(i) = 10.0_r8k ** rtime
    END DO
    td = ProblemTime(it) * sectoday
    WRITE (ounit,180) (it-1), td
    WRITE (ounit,260)
    ! output table of half-lives vs fraction released
    WRITE (ounit,190) (h(i),i=1,11),(releas(i),i=1,11)
    WRITE (ounit,200)
    ! initialize plot to blanks
    ! r will be used to save values of s(symbol)
    dif = 1.0_r8k / 40.0_r8k
    DO i = 2, 10
        s(i) = '     '
        r(i) = s(i)
    END DO
    s(1) = '.    '
    s(11) = '.    '
    r(1) = s(1)
    r(11) = s(11)
    ! begin plot calculations one line at a time
    DO i = 1, 39
        p = 1.0_r8k - i * dif
        DO j = 1, 11
            IF (releas(j) >= p) s(j) = '*    '
        END DO
        IF (i /= 20) THEN
            WRITE (ounit,210) (s(k),k=1,11)
        ELSE
            ! print label for vertical axis
            WRITE (ounit,220) (s(k),k=1,11)
        END IF
        DO j = 1, 11
            s(j) = r(j)
        END DO
    END DO
    DO j = 1, 10
        ! print bottom axis of plot
        s(j) = '*....'
    END DO
    s(11) = '*    '
    WRITE (ounit,230) (s(j),j=1,11)
    ! print label for horizontal axis
    WRITE (ounit,240) (h(j),j=1,11,2)
    WRITE (ounit,250)
    !
180 FORMAT (/50x,'fraction radioactive gases released'//55x,'time step = ', &
      &     i3/55x,'time at end of step =',f6.1,' days'/)
190 FORMAT (/5x,'half-life(sec)',5x,11(1pe10.1),/5x,'fraction'/5x,'released',11x,11(1pe10.2))
200 FORMAT (/135('*')//40x,'1.0.',51('.'))
210 FORMAT (44x,11a5)
220 FORMAT (16x,'fraction released',7x,'0.5.',11a5)
230 FORMAT (40x,'0.0.',11a5)
240 FORMAT (44x,6('.',9x)/37x,3f10.0,2x,3f10.0)
250 FORMAT (/62x,'half-life(sec)'////)
260 FORMAT (29x,'release fraction - fraction of non-decayed inventory that resides in the gap'//135('*'))
    !
    END SUBROUTINE gasplt
    !
END MODULE FissionGas