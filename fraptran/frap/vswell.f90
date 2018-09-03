MODULE Volume
    USE Kinds
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE vswell (PelletRad, GapThick, AxialNodLen, naxn, Kswell, GapTemp, VolAveGasTemp, RodLength)
    USE Conversions, ONLY : pi
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes volume associated with node at which swelling is occurring
    !
    ! Input
    !
    ! PelletRad(k)   - Outside radius of fuel pellets at axial node k (ft)
    ! GapThick(k)    - Gas gap thickness at axial node k (ft)
    ! AxialNodLen(k) - Length assosiated with axial node k (ft)
    ! naxn           - Number of axial nodes
    ! Kswell         - Axial node at which cladding swelling is occurring
    ! GapTemp(k)     - Gap temperature at axial node k (F)
    ! RodLength      - Fuel rod (fuel pellet) length  (ft)
    !
    ! Output
    !
    ! vswll          - Volume occupied by gas at axial node Kswell (ft**3)
    ! VolAveGasTemp  - Volume averaged temperature of gas in gas gap (F)
    !
    INTEGER(ipk) :: k
    INTEGER(ipk), INTENT(IN) :: naxn
    INTEGER(ipk), INTENT(INOUT) :: Kswell
    REAL(r8k) :: vswll, GapThickMax, za, zb, tsum, z1, z2, z, r1, r2, zm
    REAL(r8k), INTENT(IN) :: RodLength
    REAL(r8k), INTENT(OUT) :: VolAveGasTemp
    REAL(r8k), PARAMETER :: em10 = 1.0e-10_r8k
    REAL(r8k), PARAMETER :: GapThickMin = 0.8333e-03_r8k
    REAL(r8k), DIMENSION(:), INTENT(IN) :: PelletRad, GapThick, AxialNodLen, GapTemp
    !
    Kswell = naxn + 1
    vswll = 0.0_r8k
    za = 0.0_r8k
    GapThickMax = 0.0_r8k
    zb = 0.0_r8k
    tsum = 0.0_r8k
    DO k = 1, naxn
        r1 = PelletRad(k)
        r2 = r1 + GapThick(k)
        IF (GapThick(k) > GapThickMax) GapThickMax = GapThick(k)
        vswll = vswll + pi * (r2 ** 2 - r1 ** 2) * AxialNodLen(k)
        zb = zb + AxialNodLen(k)
        za = za + (RodLength - zb) * pi * (r2 ** 2 - r1 ** 2) * AxialNodLen(k)
        tsum = tsum + GapTemp(k) * pi * (r2 ** 2 - r1 ** 2) * AxialNodLen(k)
    ENDDO
    IF (vswll < em10 .OR. GapThickMax < GapThickMin) THEN
        VolAveGasTemp = GapTemp(Kswell)
    ELSE
        zm = RodLength - za / vswll
        VolAveGasTemp = tsum / vswll
        ! Find node closest to center of gravity of gas volume
        z = 0.0_r8k
        DO k = 1, naxn
            z1 = z
            z2 = z + AxialNodLen(k)
            z = z + AxialNodLen(k)
            IF (z2 >= zm .AND. z1 < zm) Kswell = k
        ENDDO
        IF (Kswell <= 1 .AND. naxn > 2) Kswell = 2
        IF (Kswell >= naxn .AND. naxn > 1) Kswell = naxn
    ENDIF
    !
    END SUBROUTINE vswell
    !
END MODULE Volume