MODULE resti_h_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !
    TYPE resti_var
        INTEGER(ipk) :: indxmd
        INTEGER(ipk) :: kflgb
        INTEGER(ipk) :: ldir
        INTEGER(ipk) :: lengmd
        INTEGER(ipk) :: ncladi
        INTEGER(ipk) :: nmesh
        INTEGER(ipk) :: nodpln
        INTEGER(ipk) :: ntstep
        INTEGER(ipk) :: Ifcmi
        INTEGER(ipk) :: igpnod
        INTEGER(ipk) :: indx
        INTEGER(ipk) :: indxkc
        INTEGER(ipk) :: indxkf
        INTEGER(ipk) :: inpfil
        INTEGER(ipk) :: la1req
        INTEGER(ipk) :: lchf
        INTEGER(ipk) :: length
        INTEGER(ipk) :: lhtc
        INTEGER(ipk) :: luout
        INTEGER(ipk) :: modfd
        INTEGER(ipk) :: modmw
        INTEGER(ipk) :: mpdcay
        INTEGER(ipk) :: naazp
        INTEGER(ipk) :: naazpd
        INTEGER(ipk) :: naxn
        INTEGER(ipk) :: nkf
        INTEGER(ipk) :: nchn
        INTEGER(ipk) :: nchfmd
        INTEGER(ipk) :: ndtmax
        INTEGER(ipk) :: nedtsw
        INTEGER(ipk) :: nerr
        INTEGER(ipk) :: nfastf
        INTEGER(ipk) :: ngbh
        INTEGER(ipk) :: nhbh
        INTEGER(ipk) :: nhinta
        INTEGER(ipk) :: nhtca
        INTEGER(ipk) :: nhtclv
        INTEGER(ipk) :: nhtcz
        INTEGER(ipk) :: nhupta
        INTEGER(ipk) :: nitmin
        INTEGER(ipk) :: nkc
        INTEGER(ipk) :: npaxp
        INTEGER(ipk) :: npaxp1
        INTEGER(ipk) :: npbh
        INTEGER(ipk) :: npid
        INTEGER(ipk) :: nplnt
        INTEGER(ipk) :: nptha
        INTEGER(ipk) :: nptha1
        INTEGER(ipk) :: nvoid
        INTEGER(ipk) :: ixazmn
        INTEGER(ipk) :: nswinw
        INTEGER(ipk) :: NFrapconInitialization
        INTEGER(ipk) :: nrazp
        INTEGER(ipk) :: nrazpd
        INTEGER(ipk) :: ngaspr
        INTEGER(ipk) :: iStoicGrad
        INTEGER(ipk) :: itdmx
        INTEGER(ipk) :: kbot
        INTEGER(ipk) :: knonue
        INTEGER(ipk) :: IndexInitTemp
        INTEGER(ipk) :: nitdt
        INTEGER(ipk) :: NSteadyTrans
        INTEGER(ipk) :: numdta
        INTEGER(ipk) :: ndtred
        INTEGER(ipk) :: nPelRadDeviat
        INTEGER(ipk) :: nqchn
        REAL(r8k) :: xtime
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ngastmp
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: modfal
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nswpm
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: irest2
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ncs
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: irest3
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: indxjk
        INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: nrc
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasFraction
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtmaxa
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: arest1    !Used for restart. Length = lresr1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: azpang
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: fluxz
        REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: tplna
    END TYPE resti_var
    !
    INTEGER(ipk) :: indxmd
    INTEGER(ipk) :: kflgb
    INTEGER(ipk) :: ldir
    INTEGER(ipk) :: lengmd
    INTEGER(ipk) :: ncladi
    INTEGER(ipk) :: nmesh
    INTEGER(ipk) :: nodpln
    INTEGER(ipk) :: ntstep
    INTEGER(ipk) :: Ifcmi
    INTEGER(ipk) :: igpnod
    INTEGER(ipk) :: indx
    INTEGER(ipk) :: indxkc
    INTEGER(ipk) :: indxkf
    INTEGER(ipk) :: inpfil
    INTEGER(ipk) :: la1req
    INTEGER(ipk) :: lchf
    INTEGER(ipk) :: length
    INTEGER(ipk) :: lhtc
    INTEGER(ipk) :: luout
    INTEGER(ipk) :: modfd
    INTEGER(ipk) :: modmw
    INTEGER(ipk) :: mpdcay
    INTEGER(ipk) :: naazp
    INTEGER(ipk) :: naazpd
    INTEGER(ipk) :: naxn
    INTEGER(ipk) :: nkf
    INTEGER(ipk) :: nchn
    INTEGER(ipk) :: nchfmd
    INTEGER(ipk) :: ndtmax
    INTEGER(ipk) :: nedtsw
    INTEGER(ipk) :: nerr
    INTEGER(ipk) :: nfastf
    INTEGER(ipk) :: ngbh
    INTEGER(ipk) :: nhbh
    INTEGER(ipk) :: nhinta
    INTEGER(ipk) :: nhtca
    INTEGER(ipk) :: nhtclv
    INTEGER(ipk) :: nhtcz
    INTEGER(ipk) :: nhupta
    INTEGER(ipk) :: nitmin
    INTEGER(ipk) :: nkc
    INTEGER(ipk) :: npaxp
    INTEGER(ipk) :: npaxp1
    INTEGER(ipk) :: npbh
    INTEGER(ipk) :: npid
    INTEGER(ipk) :: nplnt
    INTEGER(ipk) :: nptha
    INTEGER(ipk) :: nptha1
    INTEGER(ipk) :: nvoid
    INTEGER(ipk) :: ixazmn
    INTEGER(ipk) :: nswinw
    INTEGER(ipk) :: NFrapconInitialization
    INTEGER(ipk) :: nrazp
    INTEGER(ipk) :: nrazpd
    INTEGER(ipk) :: ngaspr
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ngastmp
    INTEGER(ipk) :: iStoicGrad
    INTEGER(ipk) :: itdmx
    INTEGER(ipk) :: kbot
    INTEGER(ipk) :: knonue
    INTEGER(ipk) :: IndexInitTemp
    INTEGER(ipk) :: nitdt
    INTEGER(ipk) :: NSteadyTrans
    INTEGER(ipk) :: numdta
    INTEGER(ipk) :: ndtred
    INTEGER(ipk) :: nPelRadDeviat
    INTEGER(ipk) :: nqchn
    REAL(r8k) :: xtime
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: modfal
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nswpm
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: irest2
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ncs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: irest3
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: indxjk
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: nrc
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasFraction
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtmaxa
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: arest1    !Used for restart. Length = lresr1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: azpang
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fluxz
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: tplna
    !
    END MODULE resti_h_fraptran













