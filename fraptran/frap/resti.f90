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
    INTEGER(ipk) , target :: indxmd
    INTEGER(ipk) , target :: kflgb
    INTEGER(ipk) , target :: ldir
    INTEGER(ipk) , target :: lengmd
    INTEGER(ipk) , target :: ncladi
    INTEGER(ipk) , target :: nmesh
    INTEGER(ipk) , target :: nodpln
    INTEGER(ipk) , target :: ntstep
    INTEGER(ipk) , target :: Ifcmi
    INTEGER(ipk) , target :: igpnod
    INTEGER(ipk) , target :: indx
    INTEGER(ipk) , target :: indxkc
    INTEGER(ipk) , target :: indxkf
    INTEGER(ipk) , target :: inpfil
    INTEGER(ipk) , target :: la1req
    INTEGER(ipk) , target :: lchf
    INTEGER(ipk) , target :: length
    INTEGER(ipk) , target :: lhtc
    INTEGER(ipk) , target :: luout
    INTEGER(ipk) , target :: modfd
    INTEGER(ipk) , target :: modmw
    INTEGER(ipk) , target :: mpdcay
    INTEGER(ipk) , target :: naazp
    INTEGER(ipk) , target :: naazpd
    INTEGER(ipk) , target :: naxn
    INTEGER(ipk) , target :: nkf
    INTEGER(ipk) , target :: nchn
    INTEGER(ipk) , target :: nchfmd
    INTEGER(ipk) , target :: ndtmax
    INTEGER(ipk) , target :: nedtsw
    INTEGER(ipk) , target :: nerr
    INTEGER(ipk) , target :: nfastf
    INTEGER(ipk) , target :: ngbh
    INTEGER(ipk) , target :: nhbh
    INTEGER(ipk) , target :: nhinta
    INTEGER(ipk) , target :: nhtca
    INTEGER(ipk) , target :: nhtclv
    INTEGER(ipk) , target :: nhtcz
    INTEGER(ipk) , target :: nhupta
    INTEGER(ipk) , target :: nitmin
    INTEGER(ipk) , target :: nkc
    INTEGER(ipk) , target :: npaxp
    INTEGER(ipk) , target :: npaxp1
    INTEGER(ipk) , target :: npbh
    INTEGER(ipk) , target :: npid
    INTEGER(ipk) , target :: nplnt
    INTEGER(ipk) , target :: nptha
    INTEGER(ipk) , target :: nptha1
    INTEGER(ipk) , target :: nvoid
    INTEGER(ipk) , target :: ixazmn
    INTEGER(ipk) , target :: nswinw
    INTEGER(ipk) , target :: NFrapconInitialization
    INTEGER(ipk) , target :: nrazp
    INTEGER(ipk) , target :: nrazpd
    INTEGER(ipk) , target :: ngaspr
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: ngastmp
    INTEGER(ipk) , target :: iStoicGrad
    INTEGER(ipk) , target :: itdmx
    INTEGER(ipk) , target :: kbot
    INTEGER(ipk) , target :: knonue
    INTEGER(ipk) , target :: IndexInitTemp
    INTEGER(ipk) , target :: nitdt
    INTEGER(ipk) , target :: NSteadyTrans
    INTEGER(ipk) , target :: numdta
    INTEGER(ipk) , target :: ndtred
    INTEGER(ipk) , target :: nPelRadDeviat
    INTEGER(ipk) , target :: nqchn
    REAL(r8k) , target :: xtime
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: modfal
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: nswpm
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: irest2
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: ncs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: irest3
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: indxjk
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE , target :: nrc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GasFraction
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dtmaxa
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: arest1    !Used for restart. Length = lresr1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: azpang
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: fluxz
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE , target :: tplna
    !
    END MODULE resti_h_fraptran













