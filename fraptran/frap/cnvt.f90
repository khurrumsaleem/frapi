MODULE cnvt
    USE Kinds
    USE Conversions
    USE frapc
    IMPLICIT NONE
    !>@brief
    !> This module contains the convert subroutines that are used to convert between SI and British values when linked with T/H CODE
    !> This option is used when ncool = 7 and the flag convert_units = .TRUE.
    !>@Author
    !>Modified by Ian Porter, NRC
    !>@date
    !> 3/23/2014
    !
    CONTAINS
    !
    SUBROUTINE cnvt12
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine converts t/h code input in Module frapc from SI to British units
    !
    INTEGER(ipk) :: k, nrad, naxn, l
    REAL(r8k) :: z, p, tp, r, tmp
    !
    nrad = ncorlp
    naxn = kmxrlp
    tp = tprlp
    tprlp = tkf(tp)
    !
    DO k = 1, naxn
        z = ElevatThermHydr(k)
        ElevatThermHydr(k) = z / ftom
        p = pclrlp(k)
        pclrlp(k) = p * psinm2
        DO l = 1, nrad
            tmp = tmprlp(l,k)
            tmprlp(l,k) = tkf(tmp)
        ENDDO
    ENDDO
    !
    DO l = 1, nrad
        r = rmrlp(l)
        rmrlp(l) = r / ftom
    ENDDO
    !
    END SUBROUTINE cnvt12
    !
    !
    !
    SUBROUTINE cnvt21
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine converts frap code output in Module frapc from British to SI units
    !
    INTEGER(ipk) :: k
    REAL(r8k) :: z, hgap, douter, pgap
    !
    DO k = 1, kmxfrp
        z = ElevatFrap(k)
        ElevatFrap(k) = ftom * z
        hgap = hgpfrp(k)
        hgpfrp(k) = hgpcnv * hgap
        douter = drdfrp(k)
        drdfrp(k) = ftom * douter
        pgap = pgpfrp(k)
        pgpfrp(k) = pgap / psinm2
    ENDDO
    !
    END SUBROUTINE cnvt21
    !
END MODULE cnvt