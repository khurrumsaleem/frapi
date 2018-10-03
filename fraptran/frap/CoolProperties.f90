MODULE CoolantProperties_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !
    TYPE Coolant_Properties
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Prop
        !
        INTEGER(ipk) :: nsrad3
        !
        INTEGER(ipk) :: nelrad
        !
        INTEGER(ipk) :: nvol           ! I believe this is used to represent the # of axial nodes
        !
        INTEGER(ipk) :: ithymx
        !
        INTEGER(ipk) :: ixazim
        !
        INTEGER(ipk) :: ncall
        !
        REAL(r8k) :: tc1               ! time 1
        REAL(r8k) :: tc2               ! time 2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tz2           ! Coolant temperature at time t2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: z1            ! Coolant height at time t1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: z2            ! Coolant height at time t2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gz1           ! Coolant flux at time t1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gz2           ! Coolant flux at time t2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hz1           ! Coolant HTC at time t1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hz2           ! Coolant HTC at time t2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pz1           ! Coolant pressure at time t1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pz2           ! Coolant pressure at time t2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tz1           ! Used for restart. Length = lthyd = 1516 = 151*10 + 6
        INTEGER(ipk) :: nbrtmp           !
        INTEGER(ipk) :: nbrfdr           !
        INTEGER(ipk) :: nbrfht           !
        INTEGER(ipk) :: kaxnlo           !
        INTEGER(ipk) :: liqnod           !
        INTEGER(ipk) :: izadfg           !
        INTEGER(ipk) :: irup             !
        INTEGER(ipk) :: nbrpst           !
        INTEGER(ipk) :: nflec            !
        INTEGER(ipk) :: nbrliq           !
        INTEGER(ipk) :: npaxpf           !
        INTEGER(ipk) :: mzq1             !
        INTEGER(ipk) :: mflt             !
        INTEGER(ipk) :: mbdl             !
        INTEGER(ipk) :: ntempf           !
        REAL(r8k) :: hliq                !
        REAL(r8k) :: qmax                !
        REAL(r8k) :: empytm              ! User specified problem time for start of adiabatic heatup, s
        REAL(r8k) :: hrad                ! User specified radiation heat transfer coefficient
        REAL(r8k) :: fldrte              ! flood rate, in/s
        REAL(r8k) :: zqch                !
        REAL(r8k) :: oldtim              !
        REAL(r8k) :: tflood              ! Time since start of reflood, s
        REAL(r8k) :: crf                 ! Carry out rate fraction (reflood)
        REAL(r8k) :: templo              ! coolant temp of next lower axial node, F
        REAL(r8k) :: rhostm              ! specific density of steam from sth2x
        REAL(r8k) :: cpstem              ! heat capacity of steam from sth2x
        REAL(r8k) :: tsatt               ! Saturation temperature
        REAL(r8k) :: pressr              ! system pressure, psia
        REAL(r8k) :: pressi              ! system pressure, SI units
        REAL(r8k) :: cpmult              ! heat capacity multiplier, unitless
        REAL(r8k) :: gflow               ! outlet mass flow rate
        REAL(r8k) :: temphi              ! new coolant bulk temperature
        REAL(r8k) :: ruplev              ! rupture elevation, ft
        REAL(r8k) :: pavg                ! Average rod power, kw/ft
        REAL(r8k) :: refdtm              ! Problem time for initiation of reflood, s
        REAL(r8k) :: hydiam              ! Channel hydraulic diameter, ft
        REAL(r8k) :: flxsec              ! flow channel cross sectional area, ft^2
        REAL(r8k) :: tsub                ! coolant subcooling (tsat - tcoolant), F
        REAL(r8k) :: pdeint              ! 
        REAL(r8k) :: flowbk              ! Flow blockage, %
        REAL(r8k) :: tempmx              ! Max clad surface temperature, F
        REAL(r8k) :: pfflec              ! flect axial power peaking factor
        REAL(r8k) :: tfoldf              ! Time (maybe for flooding), s
        REAL(r8k) :: pdecy               ! 
        REAL(r8k) :: drflt               ! 
        REAL(r8k) :: pfnuc               !
        REAL(r8k) :: toldfc              !
        REAL(r8k) :: zqflt               !
        REAL(r8k) :: qaxpk               !
        REAL(r8k) :: zpkfc               !
        REAL(r8k) :: fltgap              !
        REAL(r8k) :: pavgft              !
        REAL(r8k) :: rcpar               !
        ! User specified adiabatic heat-up prameter for FLECHT-SEASET
        REAL(r8k) :: zad
        ! User specified adiabatic heat-up prameter for FLECHT-SEASET
        REAL(r8k) :: zs
        !
        REAL(r8k) :: trodfc
        !
        REAL(r8k) :: nu1
        !
        REAL(r8k) :: nu2
        !
        REAL(r8k) :: nu3
        ! Rupture flag
        CHARACTER(LEN=8) :: rupflg
        !
        CHARACTER(LEN=8) :: lodmrk
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: flthit
        ! Axial power profile
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: faxzq
        ! Related to power
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: qaxzq
        ! tempfc dimension is 2 times maximum allowed number of axial nodes. Indices in tempfc start at 3 and GOTO (2*naxn+2)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tempfc
        ! Used for restart only.
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: aflcht
        ! Reactor vessel pressure, paired with time
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: prestm
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hlqclp
        ! User specified inlet temperature of flooding water vs time
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: temptm
        ! User specified reflood rate vs time
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: fldrat
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: aasth
        ! Arrays
        ! 151. # of HTC pairs at each axial node
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nhprs
        ! 151. # of temperature pairs at each axial node
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ntprs
        ! 151. # of vapor pairs at each axial node
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nvprs
        ! 151. # of pressure pairs at each axial node
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: npprs
        ! 1. Only used for restart, not sure if real or integer
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: acoold
        ! 5. Not sure if integer or real
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad1
        ! 5. Not sure if integer or real
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad2
        ! 5. Not sure if integer or real
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad3
        ! 5. Not sure if integer or real
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: elvrad
        ! 151
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: htclev 
        ! 2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gbh
        ! 2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hbh
        ! 2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hinta
        ! 2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hupta
        ! 2000. Coolant pressure
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh
        ! 2000. This is in multiple common blocks
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tshrda
        ! 2000,151. This specifies the HTC to the coolant at (time, axial node)
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: htca
        ! 2000,151. This specifies the temperature of the coolant at (time, axial node)
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tblka
        ! 20,5. Not sure if integer or real
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad1
        ! 20,5. Not sure if integer or real
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad2
        ! 20,5. Not sure if integer or real
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad3
    END TYPE Coolant_Properties
    !
    INTEGER(ipk) , target :: nvol
    INTEGER(ipk) , target :: ithymx
    INTEGER(ipk) , target :: ixazim
    INTEGER(ipk) , target :: ncall
    INTEGER(ipk) , target :: nbrtmp           !
    INTEGER(ipk) , target :: nbrfdr           !
    INTEGER(ipk) , target :: nbrfht           !
    INTEGER(ipk) , target :: kaxnlo           !
    INTEGER(ipk) , target :: liqnod           !
    INTEGER(ipk) , target :: izadfg           !
    INTEGER(ipk) , target :: irup             !
    INTEGER(ipk) , target :: nbrpst           !
    INTEGER(ipk) , target :: nflec            !
    INTEGER(ipk) , target :: nbrliq           !
    INTEGER(ipk) , target :: npaxpf           !
    INTEGER(ipk) , target :: mzq1             !
    INTEGER(ipk) , target :: mflt             !
    INTEGER(ipk) , target :: mbdl             !
    INTEGER(ipk) , target :: ntempf           !
    INTEGER(ipk) , target :: nsrad3
    INTEGER(ipk) , target :: nelrad
    REAL(r8k), POINTER :: tt
    REAL(r8k), POINTER :: CoolantPress
    REAL(r8k), POINTER :: v
    REAL(r8k), POINTER :: ubar
    REAL(r8k), POINTER :: hbar
    REAL(r8k), POINTER :: beta
    REAL(r8k), POINTER :: kappa
    REAL(r8k), POINTER :: csubp
    REAL(r8k), POINTER :: x
    REAL(r8k), POINTER :: psat
    REAL(r8k), POINTER :: vsubf
    REAL(r8k), POINTER :: vsubg
    REAL(r8k), POINTER :: usubf
    REAL(r8k), POINTER :: usubg
    REAL(r8k), POINTER :: hsubf
    REAL(r8k), POINTER :: hsubg
    REAL(r8k), POINTER :: betaf
    REAL(r8k), POINTER :: betag
    REAL(r8k), POINTER :: kappaf
    REAL(r8k), POINTER :: kappag
    REAL(r8k), POINTER :: csubpf
    REAL(r8k), POINTER :: csubpg
    REAL(r8k) , target :: tc1
    REAL(r8k) , target :: tc2
    REAL(r8k) , target :: hliq                !
    REAL(r8k) , target :: qmax                !
    REAL(r8k) , target :: empytm              ! User specified problem time for start of adiabatic heatup, s
    REAL(r8k) , target :: hrad                ! User specified radiation heat transfer coefficient
    REAL(r8k) , target :: fldrte              ! flood rate, in/s
    REAL(r8k) , target :: zqch                !
    REAL(r8k) , target :: oldtim              !
    REAL(r8k) , target :: tflood              ! Time since start of reflood, s
    REAL(r8k) , target :: crf                 ! Carry out rate fraction (reflood)
    REAL(r8k) , target :: templo              ! coolant temp of next lower axial node, F
    REAL(r8k) , target :: rhostm              ! specific density of steam from sth2x
    REAL(r8k) , target :: cpstem              ! heat capacity of steam from sth2x
    REAL(r8k) , target :: tsatt               ! Saturation temperature
    REAL(r8k) , target :: pressr              ! system pressure, psia
    REAL(r8k) , target :: pressi              ! system pressure, SI units
    REAL(r8k) , target :: cpmult              ! heat capacity multiplier, unitless
    REAL(r8k) , target :: gflow               ! outlet mass flow rate
    REAL(r8k) , target :: temphi              ! new coolant bulk temperature
    REAL(r8k) , target :: ruplev              ! rupture elevation, ft
    REAL(r8k) , target :: pavg                ! Average rod power, kw/ft
    REAL(r8k) , target :: refdtm              ! Problem time for initiation of reflood, s
    REAL(r8k) , target :: hydiam              ! Channel hydraulic diameter, ft
    REAL(r8k) , target :: flxsec              ! flow channel cross sectional area, ft^2
    REAL(r8k) , target :: tsub                ! coolant subcooling (tsat - tcoolant), F
    REAL(r8k) , target :: pdeint              ! 
    REAL(r8k) , target :: flowbk              ! Flow blockage, %
    REAL(r8k) , target :: tempmx              ! Max clad surface temperature, F
    REAL(r8k) , target :: pfflec              ! flect axial power peaking factor
    REAL(r8k) , target :: tfoldf              ! Time (maybe for flooding), s
    REAL(r8k) , target :: pdecy               ! 
    REAL(r8k) , target :: drflt               ! 
    REAL(r8k) , target :: pfnuc               !
    REAL(r8k) , target :: toldfc              !
    REAL(r8k) , target :: zqflt               !
    REAL(r8k) , target :: qaxpk               !
    REAL(r8k) , target :: zpkfc               !
    REAL(r8k) , target :: fltgap              !
    REAL(r8k) , target :: pavgft              !
    REAL(r8k) , target :: rcpar               !
    REAL(r8k) , target :: zad
    REAL(r8k) , target :: zs
    REAL(r8k) , target :: trodfc
    REAL(r8k) , target :: nu1
    REAL(r8k) , target :: nu2
    REAL(r8k) , target :: nu3
    CHARACTER(LEN=8) , target :: rupflg
    CHARACTER(LEN=8) , target :: lodmrk
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: nhprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: ntprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: nvprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: npprs
    REAL(r8k), DIMENSION(:), ALLOCATABLE,  target :: Prop
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: z1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: z2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: flthit
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: faxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qaxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tempfc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: aflcht
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: prestm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hlqclp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: temptm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: fldrat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: aasth
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: acoold
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad3
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: elvrad
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: htclev
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hinta
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hupta
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tshrda
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: htca
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: tblka
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad2
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad3
    !
    !
    !
    END MODULE CoolantProperties_fraptran














