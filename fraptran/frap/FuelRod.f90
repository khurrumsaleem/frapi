MODULE FuelRod_Data
    USE Kinds
    USE Variables
    USE OutputFile
    USE Uncertainty_Vals
    USE Uncertainties, ONLY : Uncert_fuelref
    IMPLICIT NONE
    !>@brief
    !>
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> May 16, 2016
    
    ! Make everything private by default
    PRIVATE
    ! Explicitly state what is being exposed to code outside this module
    PUBLIC :: Allocate_Rods, FRAPTRAN_Vars, FRAPTRAN_Rod
    
    TYPE FRAPTRAN_Vars
        TYPE (oxidation_var) :: oxidation
        TYPE (Code_var) :: code
        TYPE (phypro_var) :: phypro
        TYPE (powcom_var) :: powcom
        TYPE (debug_var) :: debug
        TYPE (inpdat_var) :: inpdat
        TYPE (modcom_var) :: modcom
        TYPE (bcdcom_var) :: bcdcom
        TYPE (dialb_var) :: dialb
        TYPE (carcom_var) :: carcom
        TYPE (waterprop_var) :: waterprop
        TYPE (scalr_var) :: scalr
        TYPE (prntb_var) :: prntb
        TYPE (presb_var) :: presb
        TYPE (powrd_var) :: powrd
        TYPE (numcom_var) :: numcom
        TYPE (bloon_var) :: bloon
        TYPE (collct_var) :: collct
        TYPE (Coolant_Properties) :: Coolant
        TYPE (dyna_var) :: dyna
        TYPE (excb_var) :: excb
        TYPE (FissionGasRelease_var) :: FGR
        TYPE (HeatConduction_var) :: HeatConduction
        TYPE (htcb_var) :: htcb
        TYPE (intcom_var) :: intcom
        TYPE (iocom_var) :: iocom
        TYPE (resti_var) :: resti
        TYPE (material_var) :: materials
        TYPE (Variables_var) :: Variables
        TYPE (store6_var) :: store6vars
        TYPE (thcntl_var) :: thcntl
        TYPE (frapc_var) :: frapc
        TYPE (Output_var) :: Output
        TYPE (Uncertainty_var) :: Uncertainty
        integer(IPK), dimension(:), allocatable :: nm
    CONTAINS
        PROCEDURE :: Update
        PROCEDURE :: Remember
    END TYPE FRAPTRAN_Vars
    
    TYPE (FRAPTRAN_Vars), DIMENSION(:), ALLOCATABLE, SAVE :: FRAPTRAN_Rod
    
    CONTAINS
    
        SUBROUTINE Allocate_Rods (numrods)
        USE Kinds
        IMPLICIT NONE
        !>@brief
        !> This subroutine allocates the data type fraptran for the # of rods that are being modeled
        !> FRAPTRAN holds all of the common variable that are stored in every module for a given rod.
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/16/2014
        INTEGER(ipk), INTENT(IN) :: numrods
        !
        IF (.NOT. ALLOCATED(FRAPTRAN_Rod)) ALLOCATE (FRAPTRAN_Rod(1:numrods))
        !
        END SUBROUTINE Allocate_rods
    
        SUBROUTINE Update (FuelRod)
        USE Kinds
        IMPLICIT NONE
        !>@brief
        !> This subroutine updates all of the FRAPTRAN variables for a given fuel rod
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> May 16, 2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod

        CALL Update_VariableSizes (FuelRod)
        CALL Update_Variables (FuelRod)
        CALL Update_Oxidation (FuelRod)
        CALL Update_Code (FuelRod)
        CALL Update_Phypro (FuelRod)
        CALL Update_Powcom (FuelRod)
        CALL Update_Debug (FuelRod)
        CALL Update_Inpdat (FuelRod)
        CALL Update_Modcom (FuelRod)
        CALL Update_Bcdcom (FuelRod)
        CALL Update_Dialb (FuelRod)
        CALL Update_Carcom (FuelRod)
        CALL Update_WaterProp (FuelRod)
        CALL Update_Scalr (FuelRod)
        CALL Update_Prntb (FuelRod)
        CALL Update_Presb (FuelRod)
        CALL Update_Powrd (FuelRod)
        CALL Update_Numcom (FuelRod)
        CALL Update_Bloon (FuelRod)
        CALL Update_Collct (FuelRod)
        CALL Update_CoolantProperties (FuelRod)
        CALL Update_Dyna (FuelRod)
        CALL Update_Excb (FuelRod)
        CALL Update_FGR (FuelRod)
        CALL Update_Heatconduction (FuelRod)
        CALL Update_Htcb (FuelRod)
        CALL Update_Intcom (FuelRod)
        CALL Update_Iocom (FuelRod)
        CALL Update_Resti (FuelRod)
        CALL Update_Materials (FuelRod)
        CALL Update_store6vars (FuelRod)
        CALL Update_Iocom (FuelRod)
        CALL Update_Thcntl (FuelRod)
        CALL Update_Frapc (FuelRod)
        CALL Update_Gas (FuelRod)
        CALL Update_Output (FuelRod)
        CALL Update_Uncertainty (FuelRod)
        
        END SUBROUTINE Update
        !
        !
        !
        SUBROUTINE Remember (FuelRod)
        USE Kinds, ONLY : ipk, r8k
        IMPLICIT NONE
        !>@brief
        !> This subroutine remembers all of the FRAPTRAN variables for a given fuel rod
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> May 16, 2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        CALL Remember_VariableSizes (FuelRod)
        CALL Remember_Variables (FuelRod)
        CALL Remember_Oxidation (FuelRod)
        CALL Remember_Code (FuelRod)
        CALL Remember_Phypro (FuelRod)
        CALL Remember_Powcom (FuelRod)
        CALL Remember_Debug (FuelRod)
        CALL Remember_Inpdat (FuelRod)
        CALL Remember_Modcom (FuelRod)
        CALL Remember_Bcdcom (FuelRod)
        CALL Remember_Dialb (FuelRod)
        CALL Remember_Carcom (FuelRod)
        CALL Remember_WaterProp (FuelRod)
        CALL Remember_Scalr (FuelRod)
        CALL Remember_Prntb (FuelRod)
        CALL Remember_Presb (FuelRod)
        CALL Remember_Powrd (FuelRod)
        CALL Remember_Numcom (FuelRod)
        CALL Remember_Bloon (FuelRod)
        CALL Remember_Collct (FuelRod)
        CALL Remember_CoolantProperties (FuelRod)
        CALL Remember_Dyna (FuelRod)
        CALL Remember_Excb (FuelRod)
        CALL Remember_FGR (FuelRod)
        CALL Remember_Heatconduction (FuelRod)
        CALL Remember_Htcb (FuelRod)
        CALL Remember_Intcom (FuelRod)
        CALL Remember_Iocom (FuelRod)
        CALL Remember_Resti (FuelRod)
        CALL Remember_Materials (FuelRod)
        CALL Remember_store6vars (FuelRod)
        CALL Remember_Iocom (FuelRod)
        CALL Remember_Thcntl (FuelRod)
        CALL Remember_Frapc (FuelRod)
        CALL Remember_Gas (FuelRod)
        CALL Remember_Output (FuelRod)
        CALL Remember_Uncertainty (FuelRod)
        
        END SUBROUTINE Remember
        !
        ! Update Subroutines
        !
        SUBROUTINE Update_VariableSizes (FuelRod)
        !>@brief
        !> This subroutine updates the sizes used to define dimensions for dynamically allocated arrays
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/14/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ntimesteps = FuelRod%Variables%ntimesteps
        naxialnodes = FuelRod%Variables%naxialnodes
        nradialnodes = FuelRod%Variables%nradialnodes
        
        END SUBROUTINE Update_VariableSizes
        !
        SUBROUTINE Update_Variables (FuelRod)
        !>@brief
        !> This subroutine updates the Variable module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/14/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        iplant = FuelRod%Variables%iplant
        gammait = FuelRod%Variables%gammait
        maxidx = FuelRod%Variables%maxidx
        nthermex = FuelRod%Variables%nthermex
        e = FuelRod%Variables%e
        pois = FuelRod%Variables%pois
        modheat = FuelRod%Variables%modheat
        DefaultTemp = FuelRod%Variables%DefaultTemp
        dtmpcl = FuelRod%Variables%dtmpcl
        tref = FuelRod%Variables%tref
        printrodburst = FuelRod%Variables%printrodburst
        printballoon = FuelRod%Variables%printballoon
        ModHeatModel = FuelRod%Variables%ModHeatModel
        nm = FuelRod%Variables%nm
        htco = FuelRod%Variables%htco
        tem = FuelRod%Variables%tem
        pbh1 = FuelRod%Variables%pbh1
        pbh2 = FuelRod%Variables%pbh2
        hlqcl = FuelRod%Variables%hlqcl
        ts = FuelRod%Variables%ts
        gadoln = FuelRod%Variables%gadoln
        oldepeff = FuelRod%Variables%oldepeff
        oldeps = FuelRod%Variables%oldeps
        afrap = FuelRod%Variables%afrap
        amatpc = FuelRod%Variables%amatpc
        ispace = FuelRod%Variables%ispace
        emflag = FuelRod%Variables%emflag
        iok = FuelRod%Variables%iok
        iokold = FuelRod%Variables%iokold
        FrapconTemp = FuelRod%Variables%FrapconTemp
        qcold = FuelRod%Variables%qcold
        
        END SUBROUTINE Update_Variables
        !
        SUBROUTINE Update_Oxidation (FuelRod)
        !>@brief
        !> This subroutine updates the oxidation variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/8/2014

        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        CladType = FuelRod%Oxidation%CladType
        ProtectiveOxide = FuelRod%Oxidation%ProtectiveOxide
        npair = FuelRod%Oxidation%npair
        nIDoxide = FuelRod%Oxidation%nIDoxide
        cexh2l = FuelRod%Oxidation%cexh2l
        explenumv = FuelRod%Oxidation%explenumv
        cexh2a = FuelRod%Oxidation%cexh2a
        deltox = FuelRod%Oxidation%deltox
        explenumt = FuelRod%Oxidation%explenumt
        
        END SUBROUTINE Update_Oxidation
        !
        SUBROUTINE Update_Code (FuelRod)
        !>@brief
        !> This subroutine updates the code information
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ntimes = FuelRod%code%ntimes
        tend = FuelRod%code%tend
        gfint = FuelRod%code%gfint
        edint = FuelRod%code%edint
        title = FuelRod%code%title
        
        END SUBROUTINE Update_Code
        !
        SUBROUTINE Update_Phypro (FuelRod)
        !>@brief
        !> This subroutine updates the phypro module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        imox = FuelRod%phypro%imox
        ftmelt = FuelRod%phypro%ftmelt
        fhefus = FuelRod%phypro%fhefus
        ctmelt = FuelRod%phypro%ctmelt
        chefus = FuelRod%phypro%chefus
        fdelta = FuelRod%phypro%fdelta
        bumtp = FuelRod%phypro%bumtp
        compmt = FuelRod%phypro%compmt
        deloxy = FuelRod%phypro%deloxy
        
        END SUBROUTINE Update_Phypro
        !
        SUBROUTINE Update_Powcom (FuelRod)
        !>@brief
        !> This subroutine updates the powcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        nprad = FuelRod%powcom%nprad
        CladdingPower = FuelRod%powcom%CladdingPower
        RadPowProfile = FuelRod%powcom%RadPowProfile
        
        END SUBROUTINE Update_Powcom
        !
        SUBROUTINE Update_Debug (FuelRod)
        !>@brief
        !> This subroutine updates the debug module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        time = FuelRod%debug%time
        DebugTime = FuelRod%debug%DebugTime
        DebugTimeStop = FuelRod%debug%DebugTimeStop
        
        END SUBROUTINE Update_Debug
        !
        SUBROUTINE Update_Inpdat (FuelRod)
        !>@brief
        !> This subroutine updates the inpdat module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        maxit = FuelRod%inpdat%maxit
        IndexFinTemp = FuelRod%inpdat%IndexFinTemp
        indxpr = FuelRod%inpdat%indxpr
        IndexGeom = FuelRod%inpdat%IndexGeom
        IndexThermCon = FuelRod%inpdat%IndexThermCon
        IndexThermConAdv = FuelRod%inpdat%IndexThermConAdv
        IndexBC = FuelRod%inpdat%IndexBC
        noiter = FuelRod%inpdat%noiter
        Iflago = FuelRod%inpdat%Iflago
        Iflagn = FuelRod%inpdat%Iflagn
        
        END SUBROUTINE Update_Inpdat
        !
        SUBROUTINE Update_Modcom (FuelRod)
        !>@brief
        !> This subroutine updates the modcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/11/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        mwork = FuelRod%modcom%mwork
        mtabl1 = FuelRod%modcom%mtabl1
        mtabl2 = FuelRod%modcom%mtabl2
        mtabl3 = FuelRod%modcom%mtabl3
        mgaspr = FuelRod%modcom%mgaspr
        trise = FuelRod%modcom%trise
        
        END SUBROUTINE Update_Modcom
        !
        SUBROUTINE Update_Bcdcom (FuelRod)
        !>@brief
        !> This subroutine updates the bcdcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        jchf = FuelRod%bcdcom%jchf
        mbowr = FuelRod%bcdcom%mbowr
        mgbh = FuelRod%bcdcom%mgbh
        mhbh = FuelRod%bcdcom%mhbh
        mhinta = FuelRod%bcdcom%mhinta
        mhlqc1 = FuelRod%bcdcom%mhlqc1
        mhlqcp = FuelRod%bcdcom%mhlqcp
        mhupta = FuelRod%bcdcom%mhupta
        mndchf = FuelRod%bcdcom%mndchf
        mpbh = FuelRod%bcdcom%mpbh
        mprest = FuelRod%bcdcom%mprest
        mtempt = FuelRod%bcdcom%mtempt
        nbowr = FuelRod%bcdcom%nbowr
        ncooli = FuelRod%bcdcom%ncooli
        nhtc = FuelRod%bcdcom%nhtc
        nbhtc = FuelRod%bcdcom%nbhtc
        jtr = FuelRod%bcdcom%jtr
        achn = FuelRod%bcdcom%achn
        dhe = FuelRod%bcdcom%dhe
        dhy = FuelRod%bcdcom%dhy
        jhtc = FuelRod%bcdcom%jhtc
        Radiation = FuelRod%bcdcom%Radiation
        ndchfi = FuelRod%bcdcom%ndchfi
        bowrat = FuelRod%bcdcom%bowrat
        techfi = FuelRod%bcdcom%techfi
        tschfi = FuelRod%bcdcom%tschfi
        hlqcl1 = FuelRod%bcdcom%hlqcl1
        
        END SUBROUTINE Update_Bcdcom
        !
        SUBROUTINE Update_Dialb (FuelRod)
        !>@brief
        !> This subroutine updates the dialb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        fdial = FuelRod%dialb%fdial
        
        END SUBROUTINE Update_Dialb
        !
        SUBROUTINE Update_Carcom (FuelRod)
        !>@brief
        !> This subroutine updates the Carcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ierr = FuelRod%carcom%ierr 
        ibnopt = FuelRod%carcom%ibnopt 
        nopt = FuelRod%carcom%nopt 
        nunopt = FuelRod%carcom%nunopt 
        
        END SUBROUTINE Update_Carcom
        !
        SUBROUTINE Update_WaterProp (FuelRod)
        !>@brief
        !> This subroutine updates the WaterProp module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        nt = FuelRod%WaterProp%nt 
        np = FuelRod%WaterProp%np 
        ns = FuelRod%WaterProp%ns 
        ns2 = FuelRod%WaterProp%ns2 
        klp = FuelRod%WaterProp%klp 
        klp2 = FuelRod%WaterProp%klp2 
        llp = FuelRod%WaterProp%llp 
        nt5 = FuelRod%WaterProp%nt5 
        jpl = FuelRod%WaterProp%jpl 
        
        END SUBROUTINE Update_WaterProp
        !
        SUBROUTINE Update_Scalr (FuelRod)
        !>@brief
        !> This subroutine updates the Scalr module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        Ifaila = FuelRod%scalr%Ifaila 
        iagap = FuelRod%scalr%iagap 
        iagap0 = FuelRod%scalr%iagap0 
        m5fc2 = FuelRod%scalr%m5fc2 
        m6fc2 = FuelRod%scalr%m6fc2 
        IndexFC2Print = FuelRod%scalr%IndexFC2Print 
        mechan = FuelRod%scalr%mechan 
        irupt = FuelRod%scalr%irupt 
        irefine = FuelRod%scalr%irefine 
        Ifailue = FuelRod%scalr%Ifailue 
        nradq = FuelRod%scalr%nradq 
        nbuq = FuelRod%scalr%nbuq 
        atop = FuelRod%scalr%atop 
        dcldh = FuelRod%scalr%dcldh 
        delth = FuelRod%scalr%delth 
        gpthk0 = FuelRod%scalr%gpthk0 
        cnfsol = FuelRod%scalr%cnfsol 
        cnfliq = FuelRod%scalr%cnfliq 
        tmax = FuelRod%scalr%tmax 
        trecrd = FuelRod%scalr%trecrd 
        t0 = FuelRod%scalr%t0 
        dtenfb = FuelRod%scalr%dtenfb 
        dtenfo = FuelRod%scalr%dtenfo 
        frcoef = FuelRod%scalr%frcoef 
        ruptstrain = FuelRod%scalr%ruptstrain 
        refine = FuelRod%scalr%refine 
        cfluxa = FuelRod%scalr%cfluxa 
        coldw = FuelRod%scalr%coldw 
        dishd = FuelRod%scalr%dishd 
        dishv0 = FuelRod%scalr%dishv0 
        bup = FuelRod%scalr%bup 
        beta1 = FuelRod%scalr%beta1 
        fqcrit = FuelRod%scalr%fqcrit 
        frden = FuelRod%scalr%frden 
        frpo2 = FuelRod%scalr%frpo2 
        drod = FuelRod%scalr%drod 
        dtpo = FuelRod%scalr%dtpo 
        dtss = FuelRod%scalr%dtss 
        fpdcay = FuelRod%scalr%fpdcay 
        TotalGasMoles = FuelRod%scalr%TotalGasMoles 
        pelh = FuelRod%scalr%pelh 
        pfail = FuelRod%scalr%pfail 
        powop = FuelRod%scalr%powop 
        PlenumGasMoles = FuelRod%scalr%PlenumGasMoles 
        prsacc = FuelRod%scalr%prsacc 
        zvoid1 = FuelRod%scalr%zvoid1 
        rhof = FuelRod%scalr%rhof 
        rl = FuelRod%scalr%rl 
        rshd = FuelRod%scalr%rshd 
        rhoc = FuelRod%scalr%rhoc 
        rvoid = FuelRod%scalr%rvoid 
        srcv2 = FuelRod%scalr%srcv2 
        src1 = FuelRod%scalr%src1 
        src2 = FuelRod%scalr%src2 
        tdbugd = FuelRod%scalr%tdbugd 
        tempcs = FuelRod%scalr%tempcs 
        tflux = FuelRod%scalr%tflux 
        tgas0 = FuelRod%scalr%tgas0 
        timop = FuelRod%scalr%timop 
        tmaxc = FuelRod%scalr%tmaxc 
        tmaxf = FuelRod%scalr%tmaxf 
        tmpac1 = FuelRod%scalr%tmpac1
        tpowf = FuelRod%scalr%tpowf 
        trest = FuelRod%scalr%trest 
        t0c = FuelRod%scalr%t0c 
        t0f = FuelRod%scalr%t0f 
        zvoid2 = FuelRod%scalr%zvoid2 
        OpenPorosityFraction = FuelRod%scalr%OpenPorosityFraction 
        spl = FuelRod%scalr%spl 
        scd = FuelRod%scalr%scd 
        swd = FuelRod%scalr%swd 
        dcldh0 = FuelRod%scalr%dcldh0 
        delth0 = FuelRod%scalr%delth0
        dtheta = FuelRod%scalr%dtheta 
        dtold = FuelRod%scalr%dtold 
        fr = FuelRod%scalr%fr 
        frchdt = FuelRod%scalr%frchdt 
        frchmx = FuelRod%scalr%frchmx 
        fuelrd = FuelRod%scalr%fuelrd 
        time0 = FuelRod%scalr%time0 
        tpo = FuelRod%scalr%tpo 
        zbot = FuelRod%scalr%zbot 
        zmesh = FuelRod%scalr%zmesh 
        zro = FuelRod%scalr%zro 
        dppowi = FuelRod%scalr%dppowi 
        powimx = FuelRod%scalr%powimx 
        powict = FuelRod%scalr%powict 
        aidtot = FuelRod%scalr%aidtot 
        aidlng = FuelRod%scalr%aidlng 
        aidsht = FuelRod%scalr%aidsht 
        aidsur = FuelRod%scalr%aidsur
        relocmodel = FuelRod%scalr%relocmodel
        idumr1 = FuelRod%scalr%idumr1 
        vplen = FuelRod%scalr%vplen 
        apln0 = FuelRod%scalr%apln0 
        bpln0 = FuelRod%scalr%bpln0 
        vsn = FuelRod%scalr%vsn 
        bu = FuelRod%scalr%bu 
        gappr0 = FuelRod%scalr%gappr0 
        scd = FuelRod%scalr%scd 
        spl = FuelRod%scalr%spl 
        swd = FuelRod%scalr%swd 
        qpln = FuelRod%scalr%qpln 
        qpln0 = FuelRod%scalr%qpln0 
        flwblk = FuelRod%scalr%flwblk 
        tp = FuelRod%scalr%tp 
        powave = FuelRod%scalr%powave 
        dvdt = FuelRod%scalr%dvdt 
        apln = FuelRod%scalr%apln 
        bpln = FuelRod%scalr%bpln 
        dvdt0 = FuelRod%scalr%dvdt0 
        hfusn = FuelRod%scalr%hfusn 
        tmelt = FuelRod%scalr%tmelt 
        ascal3 = FuelRod%scalr%ascal3 
        ascal2 = FuelRod%scalr%ascal2 
        ascal1 = FuelRod%scalr%ascal1 
        gadolin = FuelRod%scalr%gadolin 
        pchn = FuelRod%scalr%pchn 
        burad = FuelRod%scalr%burad 
        radpow = FuelRod%scalr%radpow 
        radsrc = FuelRod%scalr%radsrc 
        radsrco = FuelRod%scalr%radsrco
        IF (ALLOCATED(FuelRod%scalr%radtemp)) radtemp = FuelRod%scalr%radtemp
        IF (ALLOCATED(FuelRod%scalr%fuelrad)) fuelrad = FuelRod%scalr%fuelrad
        
        END SUBROUTINE Update_Scalr
        !
        SUBROUTINE Update_Prntb (FuelRod)
        !>@brief
        !> This subroutine updates the Prntb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        itcntd = FuelRod%prntb%itcntd
        crfpr = FuelRod%prntb%crfpr
        zqchpr = FuelRod%prntb%zqchpr
        reflpr = FuelRod%prntb%reflpr
        fldrpr = FuelRod%prntb%fldrpr
        TimeIncrement = FuelRod%prntb%TimeIncrement
        tplenb = FuelRod%prntb%tplenb
        aprntb = FuelRod%prntb%aprntb
    
        END SUBROUTINE Update_Prntb
        !
        SUBROUTINE Update_Presb (FuelRod)
        !>@brief
        !> This subroutine updates the Presb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        Kswell = FuelRod%presb%Kswell
        Ifail = FuelRod%presb%Ifail
        vplenc = FuelRod%presb%vplenc
        tplen = FuelRod%presb%tplen
        VolAveGasTemp = FuelRod%presb%VolAveGasTemp
        pctop = FuelRod%presb%pctop
        dvdtp = FuelRod%presb%dvdtp
        vplenb = FuelRod%presb%vplenb
        gfloa1 = FuelRod%presb%gfloa1
        roughf = FuelRod%presb%roughf
        roughc = FuelRod%presb%roughc
        swllfr = FuelRod%presb%swllfr
        TotalVoidVol = FuelRod%presb%TotalVoidVol
        ies = FuelRod%presb%ies
        pswll0 = FuelRod%presb%pswll0
        roi = FuelRod%presb%roi
        vs0 = FuelRod%presb%vs0
        flowg = FuelRod%presb%flowg
        GasAx = FuelRod%presb%GasAx
    
        END SUBROUTINE Update_Presb
        !
        SUBROUTINE Update_Powrd (FuelRod)
        !>@brief
        !> This subroutine updates the Powrd module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        nprofile = FuelRod%powrd%nprofile
        NumAxProfiles = FuelRod%powrd%NumAxProfiles
        apowrd = FuelRod%powrd%apowrd
        ProfileStartTime = FuelRod%powrd%ProfileStartTime
        RodAvePower = FuelRod%powrd%RodAvePower
        AxPowProfile = FuelRod%powrd%AxPowProfile
        pazp = FuelRod%powrd%pazp
        
        END SUBROUTINE Update_Powrd
        !
        SUBROUTINE Update_Numcom (FuelRod)
        !>@brief
        !> This subroutine updates the Numcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        idebug = FuelRod%numcom%idebug
        ncmesh = FuelRod%numcom%ncmesh
        nfmesh = FuelRod%numcom%nfmesh
        nsc = FuelRod%numcom%nsc
        nsf = FuelRod%numcom%nsf
        cmesh = FuelRod%numcom%cmesh
        fmesh = FuelRod%numcom%fmesh
        zelev = FuelRod%numcom%zelev
        
        END SUBROUTINE Update_Numcom
        !
        SUBROUTINE Update_Htcb (FuelRod)
        !>@brief
        !> This subroutine updates the Htcb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        kaxhtc = FuelRod%htcb%kaxhtc
        nfhtc = FuelRod%htcb%nfhtc
        nradsh = FuelRod%htcb%nradsh
        nqbow = FuelRod%htcb%nqbow
        tshtc = FuelRod%htcb%tshtc
        fhtc = FuelRod%htcb%fhtc
        rshrd = FuelRod%htcb%rshrd
        zroug1 = FuelRod%htcb%zroug1
        zroug2 = FuelRod%htcb%zroug2
        ffchf = FuelRod%htcb%ffchf
        htflxa = FuelRod%htcb%htflxa
        bowthr = FuelRod%htcb%bowthr
        
        END SUBROUTINE Update_Htcb
        !
        SUBROUTINE Update_HeatConduction (FuelRod)
        !>@brief
        !> This subroutine updates the HeatConduction module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        Nchan = FuelRod%HeatConduction%Nchan
        areao = FuelRod%HeatConduction%areao
        arean = FuelRod%HeatConduction%arean
        epsht1 = FuelRod%HeatConduction%epsht1
        GapConductivity = FuelRod%HeatConduction%GapConductivity
        BoundaryCondition = FuelRod%HeatConduction%BoundaryCondition
        ThermalConductAdv = FuelRod%HeatConduction%ThermalConductAdv
        FinalTemp = FuelRod%HeatConduction%FinalTemp
        PrevTemp = FuelRod%HeatConduction%PrevTemp
        ArrayE = FuelRod%HeatConduction%ArrayE
        ArrayF = FuelRod%HeatConduction%ArrayF
        VolumeWeightL = FuelRod%HeatConduction%VolumeWeightL
        VolumeWeightR = FuelRod%HeatConduction%VolumeWeightR
        AreaWeight = FuelRod%HeatConduction%AreaWeight
        ThermalConductivity = FuelRod%HeatConduction%ThermalConductivity
        acond = FuelRod%HeatConduction%acond
        RhoCp = FuelRod%HeatConduction%RhoCp
        RhoCp0 = FuelRod%HeatConduction%RhoCp0
        PrevIterateTemp = FuelRod%HeatConduction%PrevIterateTemp
        
        END SUBROUTINE Update_HeatConduction
        !
        SUBROUTINE Update_FGR (FuelRod)
        !>@brief
        !> This subroutine updates the FissionGasRelease module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        TranSwell = FuelRod%FGR%TranSwell
        presfgr = FuelRod%FGR%presfgr
        nFuelSwellPairs = FuelRod%FGR%nFuelSwellPairs
        nFGRpairs = FuelRod%FGR%nFGRpairs
        GasMoles0 = FuelRod%FGR%GasMoles0
        prodxe = FuelRod%FGR%prodxe
        prodkr = FuelRod%FGR%prodkr
        relfract = FuelRod%FGR%relfract
        TranFuelSwell = FuelRod%FGR%TranFuelSwell
        relfraca = FuelRod%FGR%relfraca
        FuelGasSwell = FuelRod%FGR%FuelGasSwell
        ngasr = FuelRod%FGR%ngasr
        ansr = FuelRod%FGR%ansr
        fmgp = FuelRod%FGR%fmgp
        gasavail1 = FuelRod%FGR%gasavail1
        gasavail2 = FuelRod%FGR%gasavail2
        
        END SUBROUTINE Update_FGR
        !
        SUBROUTINE Update_Bloon (FuelRod)
        !>@brief
        !> This subroutine updates the Bloon module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ifbaln = FuelRod%bloon%ifbaln
        jmnbal = FuelRod%bloon%jmnbal
        kntbal = FuelRod%bloon%kntbal
        nbncal = FuelRod%bloon%nbncal
        nodprm = FuelRod%bloon%nodprm
        kbaln = FuelRod%bloon%kbaln
        modbal = FuelRod%bloon%modbal
        nprntb = FuelRod%bloon%nprntb
        chstrs = FuelRod%bloon%chstrs
        frbal = FuelRod%bloon%frbal
        pmxbal = FuelRod%bloon%pmxbal
        r8bal = FuelRod%bloon%r8bal
        tcebal = FuelRod%bloon%tcebal
        pdrato = FuelRod%bloon%pdrato
        rnbnt = FuelRod%bloon%rnbnt
        totnb = FuelRod%bloon%totnb
        trabal = FuelRod%bloon%trabal
        taxbal = FuelRod%bloon%taxbal
        dtbal = FuelRod%bloon%dtbal
        dtobal = FuelRod%bloon%dtobal
        emwbal = FuelRod%bloon%emwbal
        fabal = FuelRod%bloon%fabal
        flxbal = FuelRod%bloon%flxbal
        htcbal = FuelRod%bloon%htcbal
        h0bal = FuelRod%bloon%h0bal
        pcbal = FuelRod%bloon%pcbal
        psbal = FuelRod%bloon%psbal
        qbal = FuelRod%bloon%qbal
        qlbal = FuelRod%bloon%qlbal
        rfbal = FuelRod%bloon%rfbal
        rmpbal = FuelRod%bloon%rmpbal
        r0bal = FuelRod%bloon%r0bal
        tbkbal = FuelRod%bloon%tbkbal
        tc0bal = FuelRod%bloon%tc0bal
        tf0bal = FuelRod%bloon%tf0bal
        tgbal = FuelRod%bloon%tgbal
        timbal = FuelRod%bloon%timbal
        tm1bal = FuelRod%bloon%tm1bal
        tp1bal = FuelRod%bloon%tp1bal
        ztmax = FuelRod%bloon%ztmax
        zm1bal = FuelRod%bloon%zm1bal
        zp1bal = FuelRod%bloon%zp1bal
        zndbal = FuelRod%bloon%zndbal
        htcgba = FuelRod%bloon%htcgba
        tfavba = FuelRod%bloon%tfavba
        zbaln = FuelRod%bloon%zbaln
        farbal = FuelRod%bloon%farbal
        sdfar = FuelRod%bloon%sdfar
        zfarbl = FuelRod%bloon%zfarbl
        ablona = FuelRod%bloon%ablona
        
        END SUBROUTINE Update_Bloon
        !
        SUBROUTINE Update_Dyna (FuelRod)
        !>@brief
        !> This subroutine updates the Dyna module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/11/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        IndexTempConverg = FuelRod%dyna%IndexTempConverg
        RadialPower = FuelRod%dyna%RadialPower
        RadialBound = FuelRod%dyna%RadialBound
        qmaxmelt = FuelRod%dyna%qmaxmelt
        qmaxmeltp1 = FuelRod%dyna%qmaxmeltp1
        rmassflux = FuelRod%dyna%rmassflux
        coolqual = FuelRod%dyna%coolqual
        AAHT1 = FuelRod%dyna%AAHT1
        BBHT1 = FuelRod%dyna%BBHT1
        GasPress0 = FuelRod%dyna%GasPress0
        CoolPress = FuelRod%dyna%CoolPress
        GasMolesAx = FuelRod%dyna%GasMolesAx
        GasMolesAx0 = FuelRod%dyna%GasMolesAx0
        GasPress = FuelRod%dyna%GasPress
        FuelSurfT = FuelRod%dyna%FuelSurfT
        FuelCenterT = FuelRod%dyna%FuelCenterT
        CrackTemp = FuelRod%dyna%CrackTemp
        VolOpenPor = FuelRod%dyna%VolOpenPor
        GapThick = FuelRod%dyna%GapThick
        GapThick0 = FuelRod%dyna%GapThick0
        GapTemp = FuelRod%dyna%GapTemp
        PelletRad = FuelRod%dyna%PelletRad
        PelletRad0 = FuelRod%dyna%PelletRad0
        HeatFlux = FuelRod%dyna%HeatFlux
        HeatFlux0 = FuelRod%dyna%HeatFlux0
        SurfHtFlux = FuelRod%dyna%SurfHtFlux
        CladMaxT = FuelRod%dyna%CladMaxT
        Vreloc = FuelRod%dyna%Vreloc
        Vreloc0 = FuelRod%dyna%Vreloc0
        PelSrfDispl = FuelRod%dyna%PelSrfDispl
        OldPelDis = FuelRod%dyna%OldPelDis
        OldPelDis0 = FuelRod%dyna%OldPelDis0
        OxiThk1 = FuelRod%dyna%OxiThk1
        OxiThk2 = FuelRod%dyna%OxiThk2
        DishVperL = FuelRod%dyna%DishVperL
        FastFlux = FuelRod%dyna%FastFlux
        AlphaThk1 = FuelRod%dyna%AlphaThk1
        AlphaThk2 = FuelRod%dyna%AlphaThk2
        AlphaThk11 = FuelRod%dyna%AlphaThk11
        AlphaThk22 = FuelRod%dyna%AlphaThk22
        oxideid = FuelRod%dyna%oxideid
        EffStrain = FuelRod%dyna%EffStrain
        EffStrain0 = FuelRod%dyna%EffStrain0
        oxideod = FuelRod%dyna%oxideod
        CldPermStrn = FuelRod%dyna%CldPermStrn
        CldPermStrn0 = FuelRod%dyna%CldPermStrn0
        SSHgap = FuelRod%dyna%SSHgap
        OldCoolPrs = FuelRod%dyna%OldCoolPrs
        OldCoolPrs0 = FuelRod%dyna%OldCoolPrs0
        CladAveTemp = FuelRod%dyna%CladAveTemp
        OldGasPrs = FuelRod%dyna%OldGasPrs
        OldGasPrs0 = FuelRod%dyna%OldGasPrs0
        HtFlxFac = FuelRod%dyna%HtFlxFac
        OldCladT = FuelRod%dyna%OldCladT
        OldFuelAxStrn = FuelRod%dyna%OldFuelAxStrn
        RodOD = FuelRod%dyna%RodOD
        OldCldAxStrn = FuelRod%dyna%OldCldAxStrn
        OldCldAxStrn0 = FuelRod%dyna%OldCldAxStrn0
        RodOD0 = FuelRod%dyna%RodOD0
        OldCladT0 = FuelRod%dyna%OldCladT0
        OldFuelAxStrn0 = FuelRod%dyna%OldFuelAxStrn0
        TimeofGBSep = FuelRod%dyna%TimeofGBSep
        SwellDispl = FuelRod%dyna%SwellDispl
        HmaxInitial = FuelRod%dyna%HmaxInitial
        CoolEnthalpy = FuelRod%dyna%CoolEnthalpy
        RInterfacPrs = FuelRod%dyna%RInterfacPrs
        RInterfacPrs0 = FuelRod%dyna%RInterfacPrs0
        HGapAv = FuelRod%dyna%HGapAv
        FilmCoeffAv = FuelRod%dyna%FilmCoeffAv
        AxialPowr = FuelRod%dyna%AxialPowr
        AxialPowr0 = FuelRod%dyna%AxialPowr0
        CritHtFlux = FuelRod%dyna%CritHtFlux
        FuelSrfStrRat = FuelRod%dyna%FuelSrfStrRat
        WatrMetlEnrgy = FuelRod%dyna%WatrMetlEnrgy
        RInterfacGap = FuelRod%dyna%RInterfacGap
        CladSurfT = FuelRod%dyna%CladSurfT
        EDotFZ = FuelRod%dyna%EDotFZ
        PelSrfStrn0 = FuelRod%dyna%PelSrfStrn0
        FuelSrfStrRat0 = FuelRod%dyna%FuelSrfStrRat0
        EDotFZ0 = FuelRod%dyna%EDotFZ0
        EnergyPerL = FuelRod%dyna%EnergyPerL
        HFXSum = FuelRod%dyna%HFXSum
        CladEnrgPerL = FuelRod%dyna%CladEnrgPerL
        CoolEnthalpy0 = FuelRod%dyna%CoolEnthalpy0
        CoolMassFlx = FuelRod%dyna%CoolMassFlx
        Enthl = FuelRod%dyna%Enthl
        CoolDensity = FuelRod%dyna%CoolDensity
        CoolDensity0 = FuelRod%dyna%CoolDensity0
        PelRadDeviat = FuelRod%dyna%PelRadDeviat
        CldPermAxStrn = FuelRod%dyna%CldPermAxStrn
        VoidVolumeRatio = FuelRod%dyna%VoidVolumeRatio
        CldPermHoopStrn = FuelRod%dyna%CldPermHoopStrn
        ECR = FuelRod%dyna%ECR
        OxUptakeID1 = FuelRod%dyna%OxUptakeID1
        OxUpTakeID2 = FuelRod%dyna%OxUpTakeID2
        SEDPNNL = FuelRod%dyna%SEDPNNL
        EffStrainPNNL = FuelRod%dyna%EffStrainPNNL
        coefk = FuelRod%dyna%coefk
        coefn = FuelRod%dyna%coefn
        coefm = FuelRod%dyna%coefm
        Emodulus = FuelRod%dyna%Emodulus
        strainrateterm = FuelRod%dyna%strainrateterm
        SEDEPRI = FuelRod%dyna%SEDEPRI
        EffFastFluStrnHardExp = FuelRod%dyna%EffFastFluStrnHardExp
        BetaThickness = FuelRod%dyna%BetaThickness
        EffColdWkStrnHardExp = FuelRod%dyna%EffColdWkStrnHardExp
        OxygenConcenAve = FuelRod%dyna%OxygenConcenAve
        EffColdWkStrenCoef = FuelRod%dyna%EffColdWkStrenCoef
        OxygenUptake = FuelRod%dyna%OxygenUptake
        OxStAlphaThkRemain = FuelRod%dyna%OxStAlphaThkRemain
        OxStAlphaThkNearFuel = FuelRod%dyna%OxStAlphaThkNearFuel
        EffFastFluStrenCoef = FuelRod%dyna%EffFastFluStrenCoef
        OxiPowerGen = FuelRod%dyna%OxiPowerGen
        PrcntSatBeta = FuelRod%dyna%PrcntSatBeta
        OxConcenAAO = FuelRod%dyna%OxConcenAAO
        OxConcenABO = FuelRod%dyna%OxConcenABO
        OxConcenACO = FuelRod%dyna%OxConcenACO
        OxConcenADO = FuelRod%dyna%OxConcenADO
        OxConcenAEO = FuelRod%dyna%OxConcenAEO
        OxConcenAFO = FuelRod%dyna%OxConcenAFO
        OxConcenAGO = FuelRod%dyna%OxConcenAGO
        OxConcenAHO = FuelRod%dyna%OxConcenAHO
        OxConcenAIO = FuelRod%dyna%OxConcenAIO
        OxConcenAAI = FuelRod%dyna%OxConcenAAI
        OxConcenABI = FuelRod%dyna%OxConcenABI
        OxConcenACI = FuelRod%dyna%OxConcenACI
        OxConcenADI = FuelRod%dyna%OxConcenADI
        OxConcenAEI = FuelRod%dyna%OxConcenAEI
        OxConcenAFI = FuelRod%dyna%OxConcenAFI
        OxConcenAGI = FuelRod%dyna%OxConcenAGI
        OxConcenAHI = FuelRod%dyna%OxConcenAHI
        OxConcenAII = FuelRod%dyna%OxConcenAII
        CrackVolume = FuelRod%dyna%CrackVolume
        OpenPorTemp = FuelRod%dyna%OpenPorTemp
        AveDishTemp = FuelRod%dyna%AveDishTemp
        CentVoidVol = FuelRod%dyna%CentVoidVol
        ExtentOfBow = FuelRod%dyna%ExtentOfBow
        AxialNodLen = FuelRod%dyna%AxialNodLen
        TerfacePres = FuelRod%dyna%TerfacePres
        CrackWidth = FuelRod%dyna%CrackWidth
        EinstabilStrain = FuelRod%dyna%EinstabilStrain
        AxBurnup = FuelRod%dyna%AxBurnup
        BOSOxideThick = FuelRod%dyna%BOSOxideThick
        EOSOxideThick = FuelRod%dyna%EOSOxideThick
        OpenPorVol = FuelRod%dyna%OpenPorVol
        OpenPorosity = FuelRod%dyna%OpenPorosity
        CladEffStress = FuelRod%dyna%CladEffStress
        BulkCoolTemp = FuelRod%dyna%BulkCoolTemp
        CladYieldStress = FuelRod%dyna%CladYieldStress
        StressAtInstStrain = FuelRod%dyna%StressAtInstStrain
        techf = FuelRod%dyna%techf
        CesiumContent = FuelRod%dyna%CesiumContent
        HydrogenContent = FuelRod%dyna%HydrogenContent
        tschf = FuelRod%dyna%tschf
        WorkSpaceGAPI = FuelRod%dyna%WorkSpaceGAPI
        WorkSpaceEP1 = FuelRod%dyna%WorkSpaceEP1
        WorkSpaceURC = FuelRod%dyna%WorkSpaceURC
        WorkSpaceTCMx = FuelRod%dyna%WorkSpaceTCMx
        WorkSpaceGAP = FuelRod%dyna%WorkSpaceGAP
        nodchf = FuelRod%dyna%nodchf
        BOSTemp = FuelRod%dyna%BOSTemp
        EOSTemp = FuelRod%dyna%EOSTemp
        EOSRad = FuelRod%dyna%EOSRad
        BOSRad = FuelRod%dyna%BOSRad
        EnrgyMeltP1 = FuelRod%dyna%EnrgyMeltP1
        EnrgyMeltZ = FuelRod%dyna%EnrgyMeltZ
        EnrgyMelt = FuelRod%dyna%EnrgyMelt
        EnrgyMeltZp1 = FuelRod%dyna%EnrgyMeltZp1
        RadialBoundO = FuelRod%dyna%RadialBoundO
        DeformedRadiusOfMesh = FuelRod%dyna%DeformedRadiusOfMesh
        WorkSpaceEPP1 = FuelRod%dyna%WorkSpaceEPP1
        gapmin = FuelRod%dyna%gapmin
        WorkSpacePINT = FuelRod%dyna%WorkSpacePINT
        WorkSpaceRCI = FuelRod%dyna%WorkSpaceRCI
        WorkSpaceRCO = FuelRod%dyna%WorkSpaceRCO
        WorkSpaceReloc = FuelRod%dyna%WorkSpaceReloc
        AxNodElevat = FuelRod%dyna%AxNodElevat
        ureloc = FuelRod%dyna%ureloc
        a1 = FuelRod%dyna%a1
        CldElStrn = FuelRod%dyna%CldElStrn
        CldPlasStrn = FuelRod%dyna%CldPlasStrn
        CldPlasStrn0 = FuelRod%dyna%CldPlasStrn0
        CldStrn = FuelRod%dyna%CldStrn
        CldStrnRat = FuelRod%dyna%CldStrnRat
        CldStrnRat0 = FuelRod%dyna%CldStrnRat0
        HydrostatPress = FuelRod%dyna%HydrostatPress
        FuelResidStrn = FuelRod%dyna%FuelResidStrn
        FuelResidStrn0 = FuelRod%dyna%FuelResidStrn0
        Baln2Twall = FuelRod%dyna%Baln2Twall
        CldresidStrn = FuelRod%dyna%CldresidStrn
        CldResidStrn0 = FuelRod%dyna%CldResidStrn0
        IodineContent = FuelRod%dyna%IodineContent
        PelSrfStrn = FuelRod%dyna%PelSrfStrn
        CldStress = FuelRod%dyna%CldStress
        CldThermStrn = FuelRod%dyna%CldThermStrn
        GRsv = FuelRod%dyna%GRsv
        Ifchk = FuelRod%dyna%Ifchk
        nce = FuelRod%dyna%nce
        IndexPCMI = FuelRod%dyna%IndexPCMI
        IndexPCMIOnce = FuelRod%dyna%IndexPCMIOnce
        NumAzmuthNod = FuelRod%dyna%NumAzmuthNod
        Ichf = FuelRod%dyna%Ichf
        Ih = FuelRod%dyna%Ih
        Ihtreg = FuelRod%dyna%Ihtreg
        GapIndex = FuelRod%dyna%GapIndex
        BOSGapIndex = FuelRod%dyna%BOSGapIndex
        BOSGapIndex0 = FuelRod%dyna%BOSGapIndex0
        RuptFailIndex = FuelRod%dyna%RuptFailIndex
        CladCollapseIndex = FuelRod%dyna%CladCollapseIndex
        CladCollIndx0 = FuelRod%dyna%CladCollIndx0
        OldGapIndex = FuelRod%dyna%OldGapIndex
        NodeSinterTemp = FuelRod%dyna%NodeSinterTemp
        RodFailIndex = FuelRod%dyna%RodFailIndex
        IF (ALLOCATED(FuelRod%dyna%buradv)) buradv = FuelRod%dyna%buradv
        IF (ALLOCATED(FuelRod%dyna%bufrad)) bufrad = FuelRod%dyna%bufrad
        
        END SUBROUTINE Update_Dyna
        !
        SUBROUTINE Update_Collct (FuelRod)
        !>@brief
        !> This subroutine updates the Collct module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ntapou = FuelRod%collct%ntapou
        la1max = FuelRod%collct%la1max
        MaximumIterations = FuelRod%collct%MaximumIterations
        n1 = FuelRod%collct%n1
        la1fil = FuelRod%collct%la1fil
        ntplot = FuelRod%collct%ntplot
        ntabl1 = FuelRod%collct%ntabl1
        ntabl2 = FuelRod%collct%ntabl2
        ntabl3 = FuelRod%collct%ntabl3
        lexcb = FuelRod%collct%lexcb
        lprntb = FuelRod%collct%lprntb
        lflect = FuelRod%collct%lflect
        ldialb = FuelRod%collct%ldialb
        lhtcb = FuelRod%collct%lhtcb
        lresr1 = FuelRod%collct%lresr1
        lresr2 = FuelRod%collct%lresr2
        lsclr1 = FuelRod%collct%lsclr1
        lsclr2 = FuelRod%collct%lsclr2
        lsclr3 = FuelRod%collct%lsclr3
        lcolct = FuelRod%collct%lcolct
        lcoold = FuelRod%collct%lcoold
        lcombk = FuelRod%collct%lcombk
        lresi2 = FuelRod%collct%lresi2
        lresi3 = FuelRod%collct%lresi3
        lafrap = FuelRod%collct%lafrap
        npramp = FuelRod%collct%npramp
        itswt = FuelRod%collct%itswt
        lmatpc = FuelRod%collct%lmatpc
        nswmd = FuelRod%collct%nswmd
        n3 = FuelRod%collct%n3
        ndap1 = FuelRod%collct%ndap1
        IterationCount = FuelRod%collct%IterationCount
        ntco = FuelRod%collct%ntco
        kdbug = FuelRod%collct%kdbug
        lblona = FuelRod%collct%lblona
        lphypr = FuelRod%collct%lphypr
        lthyd = FuelRod%collct%lthyd
        prestmp = FuelRod%collct%prestmp
        dtp = FuelRod%collct%dtp
        z = FuelRod%collct%z
        gum = FuelRod%collct%gum
        Ifstor = FuelRod%collct%Ifstor
        ihData = FuelRod%collct%ihData
        icglob = FuelRod%collct%icglob
        acolct = FuelRod%collct%acolct
        gasphs = FuelRod%collct%gasphs
        gasths = FuelRod%collct%gasths
        gbse = FuelRod%collct%gbse
        pelprm = FuelRod%collct%pelprm
        pinput = FuelRod%collct%pinput
        potput = FuelRod%collct%potput
        tpln = FuelRod%collct%tpln
        tplbt1 = FuelRod%collct%tplbt1
        
        END SUBROUTINE Update_Collct
        !
        SUBROUTINE Update_Excb (FuelRod)
        !>@brief
        !> This subroutine updates the Excb variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ndim = FuelRod%excb%ndim
        nzmesh = FuelRod%excb%nzmesh
        n2 = FuelRod%excb%n2
        n4 = FuelRod%excb%n4
        nsymm = FuelRod%excb%nsymm
        nconsw = FuelRod%excb%nconsw
        naz = FuelRod%excb%naz
        nnaz = FuelRod%excb%nnaz
        npdtpo = FuelRod%excb%npdtpo
        nbalsw = FuelRod%excb%nbalsw
        nrefld = FuelRod%excb%nrefld
        nlac = FuelRod%excb%nlac
        nprsw = FuelRod%excb%nprsw
        modkf = FuelRod%excb%modkf
        nbotpl = FuelRod%excb%nbotpl
        ncolbp = FuelRod%excb%ncolbp
        dzmesh = FuelRod%excb%dzmesh
        timmd = FuelRod%excb%timmd
        dofset = FuelRod%excb%dofset
        dofang = FuelRod%excb%dofang
        pitch = FuelRod%excb%pitch
        fgrns = FuelRod%excb%fgrns
        fotmtl = FuelRod%excb%fotmtl
        rsntr = FuelRod%excb%rsntr
        tsntrk = FuelRod%excb%tsntrk
        volbp = FuelRod%excb%volbp
        splbp = FuelRod%excb%splbp
        coldbp = FuelRod%excb%coldbp
        spdbp = FuelRod%excb%spdbp
        BottomPlenumGasMoles = FuelRod%excb%BottomPlenumGasMoles
        cldwdc = FuelRod%excb%cldwdc
        aexcb = FuelRod%excb%aexcb
        dtpoa = FuelRod%excb%dtpoa
        tplbot = FuelRod%excb%tplbot
        
        END SUBROUTINE Update_Excb
        !
        SUBROUTINE Update_CoolantProperties (FuelRod)
        !>@brief
        !> This subroutine updates the CoolProperties module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        Prop = FuelRod%Coolant%Prop
        tt => prop(1)
        CoolantPress => prop(2)
        v => prop(3)
        ubar => prop(4)
        hbar => prop(5)
        beta => prop(6)
        kappa => prop(7)
        csubp => prop(8)
        x => prop(9)
        psat => prop(10)
        vsubf => prop(11)
        vsubg => prop(12)
        usubf => prop(13)
        usubg => prop(14)
        hsubf => prop(15)
        hsubg => prop(16)
        betaf => prop(17)
        betag => prop(18)
        kappaf => prop(19)
        kappag => prop(20)
        csubpf => prop(21)
        csubpg => prop(22)
        nsrad3 = FuelRod%Coolant%nsrad3
        nelrad = FuelRod%Coolant%nelrad
        nhprs = FuelRod%Coolant%nhprs
        ntprs = FuelRod%Coolant%ntprs
        nvprs = FuelRod%Coolant%nvprs
        npprs = FuelRod%Coolant%npprs
        acoold = FuelRod%Coolant%acoold
        vfrad1 = FuelRod%Coolant%vfrad1
        vfrad2 = FuelRod%Coolant%vfrad2
        vfrad3 = FuelRod%Coolant%vfrad3
        elvrad = FuelRod%Coolant%elvrad
        htclev = FuelRod%Coolant%htclev
        gbh = FuelRod%Coolant%gbh
        hbh = FuelRod%Coolant%hbh
        hinta = FuelRod%Coolant%hinta
        hupta = FuelRod%Coolant%hupta
        pbh = FuelRod%Coolant%pbh
        tshrda = FuelRod%Coolant%tshrda
        htca = FuelRod%Coolant%htca
        tblka = FuelRod%Coolant%tblka
        trad1 = FuelRod%Coolant%trad1
        trad2 = FuelRod%Coolant%trad2
        trad3 = FuelRod%Coolant%trad3
        nbrtmp = FuelRod%Coolant%nbrtmp
        nbrfdr = FuelRod%Coolant%nbrfdr
        nbrfht = FuelRod%Coolant%nbrfht
        kaxnlo = FuelRod%Coolant%kaxnlo
        liqnod = FuelRod%Coolant%liqnod
        izadfg = FuelRod%Coolant%izadfg
        irup = FuelRod%Coolant%irup
        nbrpst = FuelRod%Coolant%nbrpst
        nflec = FuelRod%Coolant%nflec
        nbrliq = FuelRod%Coolant%nbrliq
        npaxpf = FuelRod%Coolant%npaxpf
        mzq1 = FuelRod%Coolant%mzq1
        mflt = FuelRod%Coolant%mflt
        mbdl = FuelRod%Coolant%mbdl
        ntempf = FuelRod%Coolant%ntempf
        hliq = FuelRod%Coolant%hliq
        qmax = FuelRod%Coolant%qmax
        empytm = FuelRod%Coolant%empytm
        hrad = FuelRod%Coolant%hrad
        fldrte = FuelRod%Coolant%fldrte
        zqch = FuelRod%Coolant%zqch
        oldtim = FuelRod%Coolant%oldtim
        tflood = FuelRod%Coolant%tflood
        crf = FuelRod%Coolant%crf
        templo = FuelRod%Coolant%templo
        rhostm = FuelRod%Coolant%rhostm
        cpstem = FuelRod%Coolant%cpstem
        tsatt = FuelRod%Coolant%tsatt
        pressr = FuelRod%Coolant%pressr
        pressi = FuelRod%Coolant%pressi
        cpmult = FuelRod%Coolant%cpmult
        gflow = FuelRod%Coolant%gflow
        temphi = FuelRod%Coolant%temphi
        ruplev = FuelRod%Coolant%ruplev
        pavg = FuelRod%Coolant%pavg
        refdtm = FuelRod%Coolant%refdtm
        hydiam = FuelRod%Coolant%hydiam
        flxsec = FuelRod%Coolant%flxsec
        tsub = FuelRod%Coolant%tsub
        pdeint = FuelRod%Coolant%pdeint
        flowbk = FuelRod%Coolant%flowbk
        tempmx = FuelRod%Coolant%tempmx
        pfflec = FuelRod%Coolant%pfflec
        tfoldf = FuelRod%Coolant%tfoldf
        pdecy = FuelRod%Coolant%pdecy
        drflt = FuelRod%Coolant%drflt
        pfnuc = FuelRod%Coolant%pfnuc
        toldfc = FuelRod%Coolant%toldfc
        zqflt = FuelRod%Coolant%zqflt
        qaxpk = FuelRod%Coolant%qaxpk
        zpkfc = FuelRod%Coolant%zpkfc
        fltgap = FuelRod%Coolant%fltgap
        pavgft = FuelRod%Coolant%pavgft
        rcpar = FuelRod%Coolant%rcpar
        zad = FuelRod%Coolant%zad
        zs = FuelRod%Coolant%zs
        trodfc = FuelRod%Coolant%trodfc
        nu1 = FuelRod%Coolant%nu1
        nu2 = FuelRod%Coolant%nu2
        nu3 = FuelRod%Coolant%nu3
        rupflg = FuelRod%Coolant%rupflg
        lodmrk = FuelRod%Coolant%lodmrk
        flthit = FuelRod%Coolant%flthit
        faxzq = FuelRod%Coolant%faxzq
        qaxzq = FuelRod%Coolant%qaxzq
        tempfc = FuelRod%Coolant%tempfc
        aflcht = FuelRod%Coolant%aflcht
        prestm = FuelRod%Coolant%prestm
        hlqclp = FuelRod%Coolant%hlqclp
        temptm = FuelRod%Coolant%temptm
        fldrat = FuelRod%Coolant%fldrat
        nvol = FuelRod%Coolant%nvol 
        ithymx = FuelRod%Coolant%ithymx 
        ixazim = FuelRod%Coolant%ixazim 
        ncall = FuelRod%Coolant%ncall 
        tc1 = FuelRod%Coolant%tc1 
        tc2 = FuelRod%Coolant%tc2 
        tz2 = FuelRod%Coolant%tz2 
        z1 = FuelRod%Coolant%z1 
        z2 = FuelRod%Coolant%z2 
        gz1 = FuelRod%Coolant%gz1 
        gz2 = FuelRod%Coolant%gz2 
        hz1 = FuelRod%Coolant%hz1 
        hz2 = FuelRod%Coolant%hz2 
        pz1 = FuelRod%Coolant%pz1 
        pz2 = FuelRod%Coolant%pz2 
        tz1 = FuelRod%Coolant%tz1
        aasth = FuelRod%Coolant%aasth
        
        END SUBROUTINE Update_CoolantProperties
        !
        SUBROUTINE Update_Intcom (FuelRod)
        !>@brief
        !> This subroutine updates the Intcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        nepp0 = FuelRod%intcom%nepp0
        cladid = FuelRod%intcom%cladid
        cladod = FuelRod%intcom%cladod
        cladtk = FuelRod%intcom%cladtk
        rf = FuelRod%intcom%rf
        FuelPelDiam = FuelRod%intcom%FuelPelDiam
        eppinp = FuelRod%intcom%eppinp
        radpel = FuelRod%intcom%radpel
        
        END SUBROUTINE Update_Intcom
        !
        SUBROUTINE Update_Iocom (FuelRod)
        !>@brief
        !> This subroutine updates the Iocom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        npltn = FuelRod%iocom%npltn
        tplot = FuelRod%iocom%tplot
        dtplta = FuelRod%iocom%dtplta
        
        END SUBROUTINE Update_Iocom
        !
        SUBROUTINE Update_Resti (FuelRod)
        !>@brief
        !> This subroutine updates the Resti module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        indxmd = FuelRod%resti%indxmd
        kflgb = FuelRod%resti%kflgb
        ldir = FuelRod%resti%ldir
        lengmd = FuelRod%resti%lengmd
        ncladi = FuelRod%resti%ncladi
        nmesh = FuelRod%resti%nmesh
        nodpln = FuelRod%resti%nodpln
        ntstep = FuelRod%resti%ntstep
        Ifcmi = FuelRod%resti%Ifcmi
        igpnod = FuelRod%resti%igpnod
        indx = FuelRod%resti%indx
        indxkc = FuelRod%resti%indxkc
        indxkf = FuelRod%resti%indxkf
        inpfil = FuelRod%resti%inpfil
        la1req = FuelRod%resti%la1req
        lchf = FuelRod%resti%lchf
        length = FuelRod%resti%length
        lhtc = FuelRod%resti%lhtc
        luout = FuelRod%resti%luout
        modfd = FuelRod%resti%modfd
        modmw = FuelRod%resti%modmw
        mpdcay = FuelRod%resti%mpdcay
        naazp = FuelRod%resti%naazp
        naazpd = FuelRod%resti%naazpd
        naxn = FuelRod%resti%naxn
        nkf = FuelRod%resti%nkf
        nchn = FuelRod%resti%nchn
        nchfmd = FuelRod%resti%nchfmd
        ndtmax = FuelRod%resti%ndtmax
        nedtsw = FuelRod%resti%nedtsw
        nerr = FuelRod%resti%nerr
        nfastf = FuelRod%resti%nfastf
        ngbh = FuelRod%resti%ngbh
        nhbh = FuelRod%resti%nhbh
        nhinta = FuelRod%resti%nhinta
        nhtca = FuelRod%resti%nhtca
        nhtclv = FuelRod%resti%nhtclv
        nhtcz = FuelRod%resti%nhtcz
        nhupta = FuelRod%resti%nhupta
        nitmin = FuelRod%resti%nitmin
        nkc = FuelRod%resti%nkc
        npaxp = FuelRod%resti%npaxp
        npaxp1 = FuelRod%resti%npaxp1
        npbh = FuelRod%resti%npbh
        npid = FuelRod%resti%npid
        nplnt = FuelRod%resti%nplnt
        nptha = FuelRod%resti%nptha
        nptha1 = FuelRod%resti%nptha1
        nvoid = FuelRod%resti%nvoid
        ixazmn = FuelRod%resti%ixazmn
        nswinw = FuelRod%resti%nswinw
        NFrapconInitialization = FuelRod%resti%NFrapconInitialization
        nrazp = FuelRod%resti%nrazp
        nrazpd = FuelRod%resti%nrazpd
        ngaspr = FuelRod%resti%ngaspr
        ngastmp = FuelRod%resti%ngastmp
        iStoicGrad = FuelRod%resti%iStoicGrad
        itdmx = FuelRod%resti%itdmx
        kbot = FuelRod%resti%kbot
        knonue = FuelRod%resti%knonue
        IndexInitTemp = FuelRod%resti%IndexInitTemp
        nitdt = FuelRod%resti%nitdt
        NSteadyTrans = FuelRod%resti%NSteadyTrans
        numdta = FuelRod%resti%numdta
        ndtred = FuelRod%resti%ndtred
        nPelRadDeviat = FuelRod%resti%nPelRadDeviat
        nqchn = FuelRod%resti%nqchn
        xtime = FuelRod%resti%xtime
        modfal = FuelRod%resti%modfal
        nswpm = FuelRod%resti%nswpm
        irest2 = FuelRod%resti%irest2
        ncs = FuelRod%resti%ncs
        irest3 = FuelRod%resti%irest3
        indxjk = FuelRod%resti%indxjk
        nrc = FuelRod%resti%nrc
        dtmaxa = FuelRod%resti%dtmaxa
        arest1 = FuelRod%resti%arest1
        azpang = FuelRod%resti%azpang
        fluxz = FuelRod%resti%fluxz
        tplna = FuelRod%resti%tplna
        
        END SUBROUTINE Update_Resti
        !
        SUBROUTINE Update_Materials (FuelRod)
        !>@brief
        !> This subroutine updates the Material module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        nomat = FuelRod%materials%nomat
        imaterials = FuelRod%materials%imaterials
        imatflag = FuelRod%materials%imatflag
        imatflag1 = FuelRod%materials%imatflag1
        iDatapairs = FuelRod%materials%iDatapairs
        iheattablen = FuelRod%materials%iheattablen
        !
        END SUBROUTINE Update_Materials
        !
        SUBROUTINE Update_Store6vars (FuelRod)
        !>@brief
        !> This subroutine updates the variables in the store6 subroutine
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/15/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        SEDPNNLold = FuelRod%store6vars%SEDPNNLold
        oldEffStrainPNNL = FuelRod%store6vars%oldEffStrainPNNL
        SEDEPRIold = FuelRod%store6vars%SEDEPRIold
        EPStrain1old = FuelRod%store6vars%EPStrain1old
        EPStrain2old = FuelRod%store6vars%EPStrain2old
        EPStrain3old = FuelRod%store6vars%EPStrain3old
        StressHoopOld = FuelRod%store6vars%StressHoopOld
        StressRadialOld = FuelRod%store6vars%StressRadialOld
        StressAxialOld = FuelRod%store6vars%StressAxialOld
        CladEffStressOld = FuelRod%store6vars%CladEffStressOld
        hsolold = FuelRod%store6vars%hsolold
        OldCldPlasStrn = FuelRod%store6vars%OldCldPlasStrn
        
        END SUBROUTINE Update_Store6vars
        !
        SUBROUTINE Update_Thcntl (FuelRod)
        !>@brief
        !> This subroutine updates the Thcntl variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/19/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        ncards = FuelRod%thcntl%ncards
        NRestart = FuelRod%thcntl%NRestart
        defsize = FuelRod%thcntl%defsize
        ncool = FuelRod%thcntl%ncool
        ndtad = FuelRod%thcntl%ndtad
        t1 = FuelRod%thcntl%t1
        t2 = FuelRod%thcntl%t2
        unit = FuelRod%thcntl%unit
        
        END SUBROUTINE Update_Thcntl
        !
        SUBROUTINE Update_Frapc (FuelRod)
        !>@brief
        !> This subroutine updates the frapc variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/16/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        idx2 = FuelRod%frapc%idx2
        ncorlp = FuelRod%frapc%ncorlp
        ncirlp = FuelRod%frapc%ncirlp
        nforlp = FuelRod%frapc%nforlp
        kmxrlp = FuelRod%frapc%kmxrlp
        kmxfrp = FuelRod%frapc%kmxfrp
        ncool2 = FuelRod%frapc%ncool2
        ndtadv = FuelRod%frapc%ndtadv
        ncard2 = FuelRod%frapc%ncard2
        nrest2 = FuelRod%frapc%nrest2
        maxidx = FuelRod%Variables%maxidx
        nqchn  = FuelRod%resti%nqchn
        ncool  = FuelRod%thcntl%ncool
        tprlp = FuelRod%frapc%tprlp
        Allocate_Arrays_FT = FuelRod%frapc%Allocate_Arrays_FT
        convert_units = FuelRod%frapc%convert_units
        setunits = FuelRod%frapc%setunits
        first_pass = FuelRod%frapc%first_pass
        FrapTranFN = FuelRod%frapc%FraptranFN
        Iffrp = FuelRod%frapc%Iffrp
        tmprlp = FuelRod%frapc%tmprlp
        rmrlp = FuelRod%frapc%rmrlp
        hgpfrp = FuelRod%frapc%hgpfrp
        pclrlp = FuelRod%frapc%pclrlp
        ElevatThermHydr = FuelRod%frapc%ElevatThermHydr
        ElevatFrap = FuelRod%frapc%ElevatFrap
        drdfrp = FuelRod%frapc%drdfrp
        vrlfrp = FuelRod%frapc%vrlfrp
        pgpfrp = FuelRod%frapc%pgpfrp
        bufrp = FuelRod%frapc%bufrp
        tc1 = t12
        tc2 = MAX(t22, 1.0e-8_r8k)
        nvol = FuelRod%frapc%kmxrlp
        
        END SUBROUTINE Update_Frapc
        !
        SUBROUTINE Update_Gas (FuelRod)
        !>@brief
        !> This subroutine updates the Gas variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        GasFraction = FuelRod%resti%GasFraction
        gsmol0 = FuelRod%FGR%gsmol0
        
        END SUBROUTINE Update_Gas
        !
        SUBROUTINE Update_Output (FuelRod)
        !>@brief
        !> This subroutine updates the Output variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        efuelref = FuelRod%Output%efuelref
        eout0 = FuelRod%Output%eout0
        ein0 = FuelRod%Output%ein0
        efuel0 = FuelRod%Output%efuel0
        eclad0 = FuelRod%Output%eclad0
        
        END SUBROUTINE Update_Output
        !
        SUBROUTINE Update_Uncertainty (FuelRod)
        !>@brief
        !> This subroutine updates the Output variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        sigfuelthermcond = FuelRod%Uncertainty%sigfuelthermcond
        sigfuelthermexp = FuelRod%Uncertainty%sigfuelthermexp
        sigfuelheatcapa = FuelRod%Uncertainty%sigfuelheatcapa
        sigcladthermcond = FuelRod%Uncertainty%sigcladthermcond
        sigcladthermexp = FuelRod%Uncertainty%sigcladthermexp
        sigcladyieldstr = FuelRod%Uncertainty%sigcladyieldstr
        sigsurfhtc = FuelRod%Uncertainty%sigsurfhtc
        tdkt = FuelRod%Uncertainty%tdkt
        dtdkta = FuelRod%Uncertainty%dtdkta
        dktouts = FuelRod%Uncertainty%dktouts
        dakota = FuelRod%Uncertainty%dakota
        ndktn = FuelRod%Uncertainty%ndktn
        ndktparams = FuelRod%Uncertainty%ndktparams
        dktoutcounter = FuelRod%Uncertainty%dktoutcounter
        Uncert_fuelref = FuelRod%Uncertainty%Uncert_fuelref
        IF (ALLOCATED(FuelRod%Uncertainty%Uncert_fuelref)) Uncert_fuelref = FuelRod%Uncertainty%Uncert_fuelref
        
        END SUBROUTINE Update_Uncertainty
        !
        ! Remember Subroutines
        !
        SUBROUTINE Remember_VariableSizes (FuelRod)
        !>@brief
        !> This subroutine remembers the sizes used to define dimensions for dynamically allocated arrays
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/14/2014
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        ! Set the POINTERs
        FuelRod%Variables%ntimesteps = ntimesteps
        FuelRod%Variables%naxialnodes = naxialnodes
        FuelRod%Variables%nradialnodes = nradialnodes
        
        END SUBROUTINE Remember_VariableSizes
        
        SUBROUTINE Remember_Variables (FuelRod)
        !>@brief
        !> This subroutine remembers the Variable module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/14/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%Variables%iplant = iplant
        FuelRod%Variables%gammait = gammait
        FuelRod%Variables%maxidx = maxidx
        FuelRod%Variables%nthermex = nthermex
        FuelRod%Variables%e = e
        FuelRod%Variables%pois = pois
        FuelRod%Variables%modheat = modheat
        FuelRod%Variables%DefaultTemp = DefaultTemp
        FuelRod%Variables%dtmpcl = dtmpcl
        FuelRod%Variables%tref = tref
        FuelRod%Variables%printrodburst = printrodburst
        FuelRod%Variables%printballoon = printballoon
        FuelRod%Variables%ModHeatModel = ModHeatModel
        FuelRod%Variables%nm = nm
        FuelRod%Variables%htco = htco
        FuelRod%Variables%tem = tem
        FuelRod%Variables%pbh1 = pbh1
        FuelRod%Variables%pbh2 = pbh2
        FuelRod%Variables%hlqcl = hlqcl
        FuelRod%Variables%ts = ts
        FuelRod%Variables%gadoln = gadoln
        FuelRod%Variables%oldepeff = oldepeff
        FuelRod%Variables%oldeps = oldeps
        FuelRod%Variables%afrap = afrap
        FuelRod%Variables%amatpc = amatpc
        FuelRod%Variables%ispace = ispace
        FuelRod%Variables%emflag = emflag
        FuelRod%Variables%iok = iok
        FuelRod%Variables%iokold = iokold
        FuelRod%Variables%FrapconTemp = FrapconTemp
        FuelRod%Variables%qcold = qcold
        
        END SUBROUTINE Remember_Variables
        !
        SUBROUTINE Remember_Oxidation (FuelRod)
        !>@brief
        !> This subroutine remembers the oxidation variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/8/2014

        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%Oxidation%CladType = CladType
        FuelRod%Oxidation%ProtectiveOxide = ProtectiveOxide
        FuelRod%Oxidation%npair = npair
        FuelRod%Oxidation%nIDoxide = nIDoxide
        FuelRod%Oxidation%cexh2l = cexh2l
        FuelRod%Oxidation%explenumv = explenumv
        FuelRod%Oxidation%cexh2a = cexh2a
        FuelRod%Oxidation%deltox = deltox
        FuelRod%Oxidation%explenumt = explenumt
        
        END SUBROUTINE Remember_Oxidation
        !
        SUBROUTINE Remember_Code (FuelRod)
        !>@brief
        !> This subroutine remembers the code information
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%code%ntimes = ntimes
        FuelRod%code%tEnd = tend
        FuelRod%code%gfInt = gfint
        FuelRod%code%edInt = edint
        FuelRod%code%title = title
        
        END SUBROUTINE Remember_Code
        !
        SUBROUTINE Remember_Phypro (FuelRod)
        !>@brief
        !> This subroutine remembers the phypro module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%phypro%imox = imox
        FuelRod%phypro%ftmelt = ftmelt
        FuelRod%phypro%fhefus = fhefus
        FuelRod%phypro%ctmelt = ctmelt
        FuelRod%phypro%chefus = chefus
        FuelRod%phypro%fdelta = fdelta
        FuelRod%phypro%bumtp = bumtp
        FuelRod%phypro%compmt = compmt
        FuelRod%phypro%deloxy = deloxy
        
        END SUBROUTINE Remember_Phypro
        !
        SUBROUTINE Remember_Powcom (FuelRod)
        !>@brief
        !> This subroutine remembers the powcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%powcom%nprad = nprad
        FuelRod%powcom%CladdingPower = CladdingPower
        FuelRod%powcom%RadPowProfile = RadPowProfile
        
        END SUBROUTINE Remember_Powcom
        !
        SUBROUTINE Remember_Debug (FuelRod)
        !>@brief
        !> This subroutine remembers the debug module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%debug%time = time
        FuelRod%debug%DebugTime = DebugTime
        FuelRod%debug%DebugTimeStop = DebugTimeStop
        
        END SUBROUTINE Remember_Debug
        !
        SUBROUTINE Remember_Inpdat (FuelRod)
        !>@brief
        !> This subroutine remembers the inpdat module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/9/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%inpdat%maxit = maxit
        FuelRod%inpdat%IndexFinTemp = IndexFinTemp
        FuelRod%inpdat%indxpr = indxpr
        FuelRod%inpdat%IndexGeom = IndexGeom
        FuelRod%inpdat%IndexThermCon = IndexThermCon
        FuelRod%inpdat%IndexThermConAdv = IndexThermConAdv
        FuelRod%inpdat%IndexBC = IndexBC
        FuelRod%inpdat%noiter = noiter
        FuelRod%inpdat%Iflago = Iflago
        FuelRod%inpdat%Iflagn = Iflagn
        
        END SUBROUTINE Remember_Inpdat
        !
        SUBROUTINE Remember_Modcom (FuelRod)
        !>@brief
        !> This subroutine remembers the modcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/11/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%modcom%mwork = mwork
        FuelRod%modcom%mtabl1 = mtabl1
        FuelRod%modcom%mtabl2 = mtabl2
        FuelRod%modcom%mtabl3 = mtabl3
        FuelRod%modcom%mgaspr = mgaspr
        FuelRod%modcom%trise = trise
        
        END SUBROUTINE Remember_Modcom
        !
        SUBROUTINE Remember_Bcdcom (FuelRod)
        !>@brief
        !> This subroutine remembers the bcdcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%bcdcom%jchf = jchf
        FuelRod%bcdcom%mbowr = mbowr
        FuelRod%bcdcom%mgbh = mgbh
        FuelRod%bcdcom%mhbh = mhbh
        FuelRod%bcdcom%mhinta = mhinta
        FuelRod%bcdcom%mhlqc1 = mhlqc1
        FuelRod%bcdcom%mhlqcp = mhlqcp
        FuelRod%bcdcom%mhupta = mhupta
        FuelRod%bcdcom%mndchf = mndchf
        FuelRod%bcdcom%mpbh = mpbh
        FuelRod%bcdcom%mprest = mprest
        FuelRod%bcdcom%mtempt = mtempt
        FuelRod%bcdcom%nbowr = nbowr
        FuelRod%bcdcom%ncooli = ncooli
        FuelRod%bcdcom%nhtc = nhtc
        FuelRod%bcdcom%nbhtc = nbhtc
        FuelRod%bcdcom%jtr = jtr
        FuelRod%bcdcom%achn = achn
        FuelRod%bcdcom%dhe = dhe
        FuelRod%bcdcom%dhy = dhy
        FuelRod%bcdcom%jhtc = jhtc
        FuelRod%bcdcom%Radiation = Radiation
        FuelRod%bcdcom%ndchfi = ndchfi
        FuelRod%bcdcom%bowrat = bowrat
        FuelRod%bcdcom%techfi = techfi
        FuelRod%bcdcom%tschfi = tschfi
        FuelRod%bcdcom%hlqcl1 = hlqcl1
        
        END SUBROUTINE Remember_Bcdcom
        !
        SUBROUTINE Remember_Dialb (FuelRod)
        !>@brief
        !> This subroutine remembers the dialb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%dialb%fdial = fdial
        
        END SUBROUTINE Remember_Dialb
        !
        SUBROUTINE Remember_Carcom (FuelRod)
        !>@brief
        !> This subroutine remembers the Carcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%carcom%ierr = ierr
        FuelRod%carcom%ibnopt = ibnopt
        FuelRod%carcom%nopt = nopt
        FuelRod%carcom%nunopt = nunopt
        
        END SUBROUTINE Remember_Carcom
        !
        SUBROUTINE Remember_WaterProp (FuelRod)
        !>@brief
        !> This subroutine remembers the WaterProp module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%WaterProp%nt = nt
        FuelRod%WaterProp%np = np
        FuelRod%WaterProp%ns = ns
        FuelRod%WaterProp%ns2 = ns2
        FuelRod%WaterProp%klp = klp
        FuelRod%WaterProp%klp2 = klp2
        FuelRod%WaterProp%llp = llp
        FuelRod%WaterProp%nt5 = nt5
        FuelRod%WaterProp%jpl = jpl
        
        END SUBROUTINE Remember_WaterProp
        !
        SUBROUTINE Remember_Scalr (FuelRod)
        !>@brief
        !> This subroutine remembers the Scalr module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%scalr%Ifaila = Ifaila
        FuelRod%scalr%iagap = iagap
        FuelRod%scalr%iagap0 = iagap0
        FuelRod%scalr%m5fc2 = m5fc2
        FuelRod%scalr%m6fc2 = m6fc2
        FuelRod%scalr%IndexFC2Print = IndexFC2Print
        FuelRod%scalr%mechan = mechan
        FuelRod%scalr%irupt = irupt
        FuelRod%scalr%irefine = irefine
        FuelRod%scalr%Ifailue = Ifailue
        FuelRod%scalr%nradq = nradq
        FuelRod%scalr%nbuq = nbuq
        FuelRod%scalr%atop = atop
        FuelRod%scalr%dcldh = dcldh
        FuelRod%scalr%delth = delth
        FuelRod%scalr%gpthk0 = gpthk0
        FuelRod%scalr%cnfsol = cnfsol
        FuelRod%scalr%cnfliq = cnfliq
        FuelRod%scalr%tmax = tmax
        FuelRod%scalr%trecrd = trecrd
        FuelRod%scalr%t0 = t0
        FuelRod%scalr%dtenfb = dtenfb
        FuelRod%scalr%dtenfo = dtenfo
        FuelRod%scalr%frcoef = frcoef
        FuelRod%scalr%ruptstrain = ruptstrain
        FuelRod%scalr%refine = refine
        FuelRod%scalr%cfluxa = cfluxa
        FuelRod%scalr%coldw = coldw
        FuelRod%scalr%dishd = dishd
        FuelRod%scalr%dishv0 = dishv0
        FuelRod%scalr%bup = bup
        FuelRod%scalr%beta1 = beta1
        FuelRod%scalr%fqcrit = fqcrit
        FuelRod%scalr%frden = frden
        FuelRod%scalr%frpo2 = frpo2
        FuelRod%scalr%drod = drod
        FuelRod%scalr%dtpo = dtpo
        FuelRod%scalr%dtss = dtss
        FuelRod%scalr%fpdcay = fpdcay
        FuelRod%scalr%TotalGasMoles = TotalGasMoles
        FuelRod%scalr%pelh = pelh
        FuelRod%scalr%pfail = pfail
        FuelRod%scalr%powop = powop
        FuelRod%scalr%PlenumGasMoles = PlenumGasMoles
        FuelRod%scalr%prsacc = prsacc
        FuelRod%scalr%zvoid1 = zvoid1
        FuelRod%scalr%rhof = rhof
        FuelRod%scalr%rl = rl
        FuelRod%scalr%rshd = rshd
        FuelRod%scalr%rhoc = rhoc
        FuelRod%scalr%rvoid = rvoid
        FuelRod%scalr%srcv2 = srcv2
        FuelRod%scalr%src1 = src1
        FuelRod%scalr%src2 = src2
        FuelRod%scalr%tdbugd = tdbugd
        FuelRod%scalr%tempcs = tempcs
        FuelRod%scalr%tflux = tflux
        FuelRod%scalr%tgas0 = tgas0
        FuelRod%scalr%timop = timop
        FuelRod%scalr%tmaxc = tmaxc
        FuelRod%scalr%tmaxf = tmaxf
        FuelRod%scalr%tmpac1 = tmpac1
        FuelRod%scalr%tpowf = tpowf
        FuelRod%scalr%trest = trest
        FuelRod%scalr%t0c = t0c
        FuelRod%scalr%t0f = t0f
        FuelRod%scalr%zvoid2 = zvoid2
        FuelRod%scalr%OpenPorosityFraction = OpenPorosityFraction
        FuelRod%scalr%spl = spl
        FuelRod%scalr%scd = scd
        FuelRod%scalr%swd = swd
        FuelRod%scalr%dcldh0 = dcldh0
        FuelRod%scalr%delth0 = delth0
        FuelRod%scalr%dtheta = dtheta
        FuelRod%scalr%dtold = dtold
        FuelRod%scalr%fr = fr
        FuelRod%scalr%frchdt = frchdt
        FuelRod%scalr%frchmx = frchmx
        FuelRod%scalr%fuelrd = fuelrd
        FuelRod%scalr%time0 = time0
        FuelRod%scalr%tpo = tpo
        FuelRod%scalr%zbot = zbot
        FuelRod%scalr%zmesh = zmesh
        FuelRod%scalr%zro = zro
        FuelRod%scalr%dppowi = dppowi
        FuelRod%scalr%powimx = powimx
        FuelRod%scalr%powict = powict
        FuelRod%scalr%aidtot = aidtot
        FuelRod%scalr%aidlng = aidlng
        FuelRod%scalr%aidsht = aidsht
        FuelRod%scalr%aidsur = aidsur
        FuelRod%scalr%relocmodel = relocmodel
        FuelRod%scalr%idumr1 = idumr1
        FuelRod%scalr%vplen = vplen
        FuelRod%scalr%apln0 = apln0
        FuelRod%scalr%bpln0 = bpln0
        FuelRod%scalr%vsn = vsn
        FuelRod%scalr%bu = bu
        FuelRod%scalr%gappr0 = gappr0
        FuelRod%scalr%scd = scd
        FuelRod%scalr%spl = spl
        FuelRod%scalr%swd = swd
        FuelRod%scalr%qpln = qpln
        FuelRod%scalr%qpln0 = qpln0
        FuelRod%scalr%flwblk = flwblk
        FuelRod%scalr%tp = tp
        FuelRod%scalr%powave = powave
        FuelRod%scalr%dvdt = dvdt
        FuelRod%scalr%apln = apln
        FuelRod%scalr%bpln = bpln
        FuelRod%scalr%dvdt0 = dvdt0
        FuelRod%scalr%hfusn = hfusn
        FuelRod%scalr%tmelt = tmelt
        FuelRod%scalr%ascal3 = ascal3
        FuelRod%scalr%ascal2 = ascal2
        FuelRod%scalr%ascal1 = ascal1
        FuelRod%scalr%gadolin = gadolin
        FuelRod%scalr%pchn = pchn
        FuelRod%scalr%burad = burad
        FuelRod%scalr%radpow = radpow
        FuelRod%scalr%radsrc = radsrc
        FuelRod%scalr%radsrco = radsrco
        IF (ALLOCATED(radtemp)) FuelRod%scalr%radtemp = radtemp
        IF (ALLOCATED(fuelrad)) FuelRod%scalr%fuelrad = fuelrad
        
        END SUBROUTINE Remember_Scalr
        !
        SUBROUTINE Remember_Prntb (FuelRod)
        !>@brief
        !> This subroutine remembers the Prntb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%prntb%itcntd = itcntd
        FuelRod%prntb%crfpr = crfpr
        FuelRod%prntb%zqchpr = zqchpr
        FuelRod%prntb%reflpr = reflpr
        FuelRod%prntb%fldrpr = fldrpr
        FuelRod%prntb%TimeIncrement = TimeIncrement
        FuelRod%prntb%tplenb = tplenb
        FuelRod%prntb%aprntb = aprntb
        
        END SUBROUTINE Remember_Prntb
        !
        SUBROUTINE Remember_Presb (FuelRod)
        !>@brief
        !> This subroutine remembers the Presb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%presb%Kswell = Kswell
        FuelRod%presb%Ifail = Ifail
        FuelRod%presb%vplenc = vplenc
        FuelRod%presb%tplen = tplen
        FuelRod%presb%VolAveGasTemp = VolAveGasTemp
        FuelRod%presb%pctop = pctop
        FuelRod%presb%dvdtp = dvdtp
        FuelRod%presb%vplenb = vplenb
        FuelRod%presb%gfloa1 = gfloa1
        FuelRod%presb%roughf = roughf
        FuelRod%presb%roughc = roughc
        FuelRod%presb%swllfr = swllfr
        FuelRod%presb%TotalVoidVol = TotalVoidVol
        FuelRod%presb%ies = ies
        FuelRod%presb%pswll0 = pswll0
        FuelRod%presb%roi = roi
        FuelRod%presb%vs0 = vs0
        FuelRod%presb%flowg = flowg
        FuelRod%presb%GasAx = GasAx
        
        END SUBROUTINE Remember_Presb
        !
        SUBROUTINE Remember_Powrd (FuelRod)
        !>@brief
        !> This subroutine remembers the Powrd module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%powrd%nprofile = nprofile
        FuelRod%powrd%NumAxProfiles = NumAxProfiles
        FuelRod%powrd%apowrd = apowrd
        FuelRod%powrd%ProfileStartTime = ProfileStartTime
        FuelRod%powrd%RodAvePower = RodAvePower
        FuelRod%powrd%AxPowProfile = AxPowProfile
        FuelRod%powrd%pazp = pazp
        
        END SUBROUTINE Remember_Powrd
        !
        SUBROUTINE Remember_Numcom (FuelRod)
        !>@brief
        !> This subroutine remembers the Numcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%numcom%idebug = idebug
        FuelRod%numcom%ncmesh = ncmesh
        FuelRod%numcom%nfmesh = nfmesh
        FuelRod%numcom%nsc = nsc
        FuelRod%numcom%nsf = nsf
        FuelRod%numcom%cmesh = cmesh
        FuelRod%numcom%fmesh = fmesh
        FuelRod%numcom%zelev = zelev
        
        END SUBROUTINE Remember_Numcom
        !
        SUBROUTINE Remember_Htcb (FuelRod)
        !>@brief
        !> This subroutine remembers the Htcb module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%htcb%kaxhtc = kaxhtc
        FuelRod%htcb%nfhtc = nfhtc
        FuelRod%htcb%nradsh = nradsh
        FuelRod%htcb%nqbow = nqbow
        FuelRod%htcb%tshtc = tshtc
        FuelRod%htcb%fhtc = fhtc
        FuelRod%htcb%rshrd = rshrd
        FuelRod%htcb%zroug1 = zroug1
        FuelRod%htcb%zroug2 = zroug2
        FuelRod%htcb%ffchf = ffchf
        FuelRod%htcb%htflxa = htflxa
        FuelRod%htcb%bowthr = bowthr
        
        END SUBROUTINE Remember_Htcb
        !
        SUBROUTINE Remember_HeatConduction (FuelRod)
        !>@brief
        !> This subroutine remembers the HeatConduction module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%HeatConduction%Nchan = Nchan
        FuelRod%HeatConduction%areao = areao
        FuelRod%HeatConduction%arean = arean
        FuelRod%HeatConduction%epsht1 = epsht1
        FuelRod%HeatConduction%GapConductivity = GapConductivity
        FuelRod%HeatConduction%BoundaryCondition = BoundaryCondition
        FuelRod%HeatConduction%ThermalConductAdv = ThermalConductAdv
        FuelRod%HeatConduction%FinalTemp = FinalTemp
        FuelRod%HeatConduction%PrevTemp = PrevTemp
        FuelRod%HeatConduction%ArrayE = ArrayE
        FuelRod%HeatConduction%ArrayF = ArrayF
        FuelRod%HeatConduction%VolumeWeightL = VolumeWeightL
        FuelRod%HeatConduction%VolumeWeightR = VolumeWeightR
        FuelRod%HeatConduction%AreaWeight = AreaWeight
        FuelRod%HeatConduction%ThermalConductivity = ThermalConductivity
        FuelRod%HeatConduction%acond = acond
        FuelRod%HeatConduction%RhoCp = RhoCp
        FuelRod%HeatConduction%RhoCp0 = RhoCp0
        FuelRod%HeatConduction%PrevIterateTemp = PrevIterateTemp
        
        END SUBROUTINE Remember_HeatConduction
        !
        SUBROUTINE Remember_FGR (FuelRod)
        !>@brief
        !> This subroutine remembers the FissionGasRelease module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%FGR%TranSwell = TranSwell
        FuelRod%FGR%presfgr = presfgr
        FuelRod%FGR%nFuelSwellPairs = nFuelSwellPairs
        FuelRod%FGR%nFGRpairs = nFGRpairs
        FuelRod%FGR%GasMoles0 = GasMoles0
        FuelRod%FGR%prodxe = prodxe
        FuelRod%FGR%prodkr = prodkr
        FuelRod%FGR%relfract = relfract
        FuelRod%FGR%TranFuelSwell = TranFuelSwell
        FuelRod%FGR%relfraca = relfraca
        FuelRod%FGR%FuelGasSwell = FuelGasSwell
        FuelRod%FGR%ngasr = ngasr
        FuelRod%FGR%ansr = ansr
        FuelRod%FGR%fmgp = fmgp
        FuelRod%FGR%gasavail1 = gasavail1
        FuelRod%FGR%gasavail2 = gasavail2
        
        END SUBROUTINE Remember_FGR
        !
        SUBROUTINE Remember_Bloon (FuelRod)
        !>@brief
        !> This subroutine remembers the Bloon module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%bloon%ifbaln = ifbaln
        FuelRod%bloon%jmnbal = jmnbal
        FuelRod%bloon%kntbal = kntbal
        FuelRod%bloon%nbncal = nbncal
        FuelRod%bloon%nodprm = nodprm
        FuelRod%bloon%kbaln = kbaln
        FuelRod%bloon%modbal = modbal
        FuelRod%bloon%nprntb = nprntb
        FuelRod%bloon%chstrs = chstrs
        FuelRod%bloon%frbal = frbal
        FuelRod%bloon%pmxbal = pmxbal
        FuelRod%bloon%r8bal = r8bal
        FuelRod%bloon%tcebal = tcebal
        FuelRod%bloon%pdrato = pdrato
        FuelRod%bloon%rnbnt = rnbnt
        FuelRod%bloon%totnb = totnb
        FuelRod%bloon%trabal = trabal
        FuelRod%bloon%taxbal = taxbal
        FuelRod%bloon%dtbal = dtbal
        FuelRod%bloon%dtobal = dtobal
        FuelRod%bloon%emwbal = emwbal
        FuelRod%bloon%fabal = fabal
        FuelRod%bloon%flxbal = flxbal
        FuelRod%bloon%htcbal = htcbal
        FuelRod%bloon%h0bal = h0bal
        FuelRod%bloon%pcbal = pcbal
        FuelRod%bloon%psbal = psbal
        FuelRod%bloon%qbal = qbal
        FuelRod%bloon%qlbal = qlbal
        FuelRod%bloon%rfbal = rfbal
        FuelRod%bloon%rmpbal = rmpbal
        FuelRod%bloon%r0bal = r0bal
        FuelRod%bloon%tbkbal = tbkbal
        FuelRod%bloon%tc0bal = tc0bal
        FuelRod%bloon%tf0bal = tf0bal
        FuelRod%bloon%tgbal = tgbal
        FuelRod%bloon%timbal = timbal
        FuelRod%bloon%tm1bal = tm1bal
        FuelRod%bloon%tp1bal = tp1bal
        FuelRod%bloon%ztmax = ztmax
        FuelRod%bloon%zm1bal = zm1bal
        FuelRod%bloon%zp1bal = zp1bal
        FuelRod%bloon%zndbal = zndbal
        FuelRod%bloon%htcgba = htcgba
        FuelRod%bloon%tfavba = tfavba
        FuelRod%bloon%zbaln = zbaln
        FuelRod%bloon%farbal = farbal
        FuelRod%bloon%sdfar = sdfar
        FuelRod%bloon%zfarbl = zfarbl
        FuelRod%bloon%ablona = ablona
        
        END SUBROUTINE Remember_Bloon
        !
        SUBROUTINE Remember_Dyna (FuelRod)
        !>@brief
        !> This subroutine remembers the Dyna module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/11/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%dyna%IndexTempConverg = IndexTempConverg
        FuelRod%dyna%RadialPower = RadialPower
        FuelRod%dyna%RadialBound = RadialBound
        FuelRod%dyna%qmaxmelt = qmaxmelt
        FuelRod%dyna%qmaxmeltp1 = qmaxmeltp1
        FuelRod%dyna%rmassflux = rmassflux
        FuelRod%dyna%coolqual = coolqual
        FuelRod%dyna%AAHT1 = AAHT1
        FuelRod%dyna%BBHT1 = BBHT1
        FuelRod%dyna%GasPress0 = GasPress0
        FuelRod%dyna%CoolPress = CoolPress
        FuelRod%dyna%GasMolesAx = GasMolesAx
        FuelRod%dyna%GasMolesAx0 = GasMolesAx0
        FuelRod%dyna%GasPress = GasPress
        FuelRod%dyna%FuelSurfT = FuelSurfT
        FuelRod%dyna%FuelCenterT = FuelCenterT
        FuelRod%dyna%CrackTemp = CrackTemp
        FuelRod%dyna%VolOpenPor = VolOpenPor
        FuelRod%dyna%GapThick = GapThick
        FuelRod%dyna%GapThick0 = GapThick0
        FuelRod%dyna%GapTemp = GapTemp
        FuelRod%dyna%PelletRad = PelletRad
        FuelRod%dyna%PelletRad0 = PelletRad0
        FuelRod%dyna%HeatFlux = HeatFlux
        FuelRod%dyna%HeatFlux0 = HeatFlux0
        FuelRod%dyna%SurfHtFlux = SurfHtFlux
        FuelRod%dyna%CladMaxT = CladMaxT
        FuelRod%dyna%Vreloc = Vreloc
        FuelRod%dyna%Vreloc0 = Vreloc0
        FuelRod%dyna%PelSrfDispl = PelSrfDispl
        FuelRod%dyna%OldPelDis = OldPelDis
        FuelRod%dyna%OldPelDis0 = OldPelDis0
        FuelRod%dyna%OxiThk1 = OxiThk1
        FuelRod%dyna%OxiThk2 = OxiThk2
        FuelRod%dyna%DishVperL = DishVperL
        FuelRod%dyna%FastFlux = FastFlux
        FuelRod%dyna%AlphaThk1 = AlphaThk1
        FuelRod%dyna%AlphaThk2 = AlphaThk2
        FuelRod%dyna%AlphaThk11 = AlphaThk11
        FuelRod%dyna%AlphaThk22 = AlphaThk22
        FuelRod%dyna%oxideid = oxideid
        FuelRod%dyna%EffStrain = EffStrain
        FuelRod%dyna%EffStrain0 = EffStrain0
        FuelRod%dyna%oxideod = oxideod
        FuelRod%dyna%CldPermStrn = CldPermStrn
        FuelRod%dyna%CldPermStrn0 = CldPermStrn0
        FuelRod%dyna%SSHgap = SSHgap
        FuelRod%dyna%OldCoolPrs = OldCoolPrs
        FuelRod%dyna%OldCoolPrs0 = OldCoolPrs0
        FuelRod%dyna%CladAveTemp = CladAveTemp
        FuelRod%dyna%OldGasPrs = OldGasPrs
        FuelRod%dyna%OldGasPrs0 = OldGasPrs0
        FuelRod%dyna%HtFlxFac = HtFlxFac
        FuelRod%dyna%OldCladT = OldCladT
        FuelRod%dyna%OldFuelAxStrn = OldFuelAxStrn
        FuelRod%dyna%RodOD = RodOD
        FuelRod%dyna%OldCldAxStrn = OldCldAxStrn
        FuelRod%dyna%OldCldAxStrn0 = OldCldAxStrn0
        FuelRod%dyna%RodOD0 = RodOD0
        FuelRod%dyna%OldCladT0 = OldCladT0
        FuelRod%dyna%OldFuelAxStrn0 = OldFuelAxStrn0
        FuelRod%dyna%TimeofGBSep = TimeofGBSep
        FuelRod%dyna%SwellDispl = SwellDispl
        FuelRod%dyna%HmaxInitial = HmaxInitial
        FuelRod%dyna%CoolEnthalpy = CoolEnthalpy
        FuelRod%dyna%RInterfacPrs = RInterfacPrs
        FuelRod%dyna%RInterfacPrs0 = RInterfacPrs0
        FuelRod%dyna%HGapAv = HGapAv
        FuelRod%dyna%FilmCoeffAv = FilmCoeffAv
        FuelRod%dyna%AxialPowr = AxialPowr
        FuelRod%dyna%AxialPowr0 = AxialPowr0
        FuelRod%dyna%CritHtFlux = CritHtFlux
        FuelRod%dyna%FuelSrfStrRat = FuelSrfStrRat
        FuelRod%dyna%WatrMetlEnrgy = WatrMetlEnrgy
        FuelRod%dyna%RInterfacGap = RInterfacGap
        FuelRod%dyna%CladSurfT = CladSurfT
        FuelRod%dyna%EDotFZ = EDotFZ
        FuelRod%dyna%PelSrfStrn0 = PelSrfStrn0
        FuelRod%dyna%FuelSrfStrRat0 = FuelSrfStrRat0
        FuelRod%dyna%EDotFZ0 = EDotFZ0
        FuelRod%dyna%EnergyPerL = EnergyPerL
        FuelRod%dyna%HFXSum = HFXSum
        FuelRod%dyna%CladEnrgPerL = CladEnrgPerL
        FuelRod%dyna%CoolEnthalpy0 = CoolEnthalpy0
        FuelRod%dyna%CoolMassFlx = CoolMassFlx
        FuelRod%dyna%Enthl = Enthl
        FuelRod%dyna%CoolDensity = CoolDensity
        FuelRod%dyna%CoolDensity0 = CoolDensity0
        FuelRod%dyna%PelRadDeviat = PelRadDeviat
        FuelRod%dyna%CldPermAxStrn = CldPermAxStrn
        FuelRod%dyna%VoidVolumeRatio = VoidVolumeRatio
        FuelRod%dyna%CldPermHoopStrn = CldPermHoopStrn
        FuelRod%dyna%ECR = ECR
        FuelRod%dyna%OxUptakeID1 = OxUptakeID1
        FuelRod%dyna%OxUpTakeID2 = OxUpTakeID2
        FuelRod%dyna%SEDPNNL = SEDPNNL
        FuelRod%dyna%EffStrainPNNL = EffStrainPNNL
        FuelRod%dyna%coefk = coefk
        FuelRod%dyna%coefn = coefn
        FuelRod%dyna%coefm = coefm
        FuelRod%dyna%Emodulus = Emodulus
        FuelRod%dyna%strainrateterm = strainrateterm
        FuelRod%dyna%SEDEPRI = SEDEPRI
        FuelRod%dyna%EffFastFluStrnHardExp = EffFastFluStrnHardExp
        FuelRod%dyna%BetaThickness = BetaThickness
        FuelRod%dyna%EffColdWkStrnHardExp = EffColdWkStrnHardExp
        FuelRod%dyna%OxygenConcenAve = OxygenConcenAve
        FuelRod%dyna%EffColdWkStrenCoef = EffColdWkStrenCoef
        FuelRod%dyna%OxygenUptake = OxygenUptake
        FuelRod%dyna%OxStAlphaThkRemain = OxStAlphaThkRemain
        FuelRod%dyna%OxStAlphaThkNearFuel = OxStAlphaThkNearFuel
        FuelRod%dyna%EffFastFluStrenCoef = EffFastFluStrenCoef
        FuelRod%dyna%OxiPowerGen = OxiPowerGen
        FuelRod%dyna%PrcntSatBeta = PrcntSatBeta
        FuelRod%dyna%OxConcenAAO = OxConcenAAO
        FuelRod%dyna%OxConcenABO = OxConcenABO
        FuelRod%dyna%OxConcenACO = OxConcenACO
        FuelRod%dyna%OxConcenADO = OxConcenADO
        FuelRod%dyna%OxConcenAEO = OxConcenAEO
        FuelRod%dyna%OxConcenAFO = OxConcenAFO
        FuelRod%dyna%OxConcenAGO = OxConcenAGO
        FuelRod%dyna%OxConcenAHO = OxConcenAHO
        FuelRod%dyna%OxConcenAIO = OxConcenAIO
        FuelRod%dyna%OxConcenAAI = OxConcenAAI
        FuelRod%dyna%OxConcenABI = OxConcenABI
        FuelRod%dyna%OxConcenACI = OxConcenACI
        FuelRod%dyna%OxConcenADI = OxConcenADI
        FuelRod%dyna%OxConcenAEI = OxConcenAEI
        FuelRod%dyna%OxConcenAFI = OxConcenAFI
        FuelRod%dyna%OxConcenAGI = OxConcenAGI
        FuelRod%dyna%OxConcenAHI = OxConcenAHI
        FuelRod%dyna%OxConcenAII = OxConcenAII
        FuelRod%dyna%CrackVolume = CrackVolume
        FuelRod%dyna%OpenPorTemp = OpenPorTemp
        FuelRod%dyna%AveDishTemp = AveDishTemp
        FuelRod%dyna%CentVoidVol = CentVoidVol
        FuelRod%dyna%ExtentOfBow = ExtentOfBow
        FuelRod%dyna%AxialNodLen = AxialNodLen
        FuelRod%dyna%TerfacePres = TerfacePres
        FuelRod%dyna%CrackWidth = CrackWidth
        FuelRod%dyna%EinstabilStrain = EinstabilStrain
        FuelRod%dyna%AxBurnup = AxBurnup
        FuelRod%dyna%BOSOxideThick = BOSOxideThick
        FuelRod%dyna%EOSOxideThick = EOSOxideThick
        FuelRod%dyna%OpenPorVol = OpenPorVol
        FuelRod%dyna%OpenPorosity = OpenPorosity
        FuelRod%dyna%CladEffStress = CladEffStress
        FuelRod%dyna%BulkCoolTemp = BulkCoolTemp
        FuelRod%dyna%CladYieldStress = CladYieldStress
        FuelRod%dyna%StressAtInstStrain = StressAtInstStrain
        FuelRod%dyna%techf = techf
        FuelRod%dyna%CesiumContent = CesiumContent
        FuelRod%dyna%HydrogenContent = HydrogenContent
        FuelRod%dyna%tschf = tschf
        FuelRod%dyna%WorkSpaceGAPI = WorkSpaceGAPI
        FuelRod%dyna%WorkSpaceEP1 = WorkSpaceEP1
        FuelRod%dyna%WorkSpaceURC = WorkSpaceURC
        FuelRod%dyna%WorkSpaceTCMx = WorkSpaceTCMx
        FuelRod%dyna%WorkSpaceGAP = WorkSpaceGAP
        FuelRod%dyna%nodchf = nodchf
        FuelRod%dyna%BOSTemp = BOSTemp
        FuelRod%dyna%EOSTemp = EOSTemp
        FuelRod%dyna%EOSRad = EOSRad
        FuelRod%dyna%BOSRad = BOSRad
        FuelRod%dyna%EnrgyMeltP1 = EnrgyMeltP1
        FuelRod%dyna%EnrgyMeltZ = EnrgyMeltZ
        FuelRod%dyna%EnrgyMelt = EnrgyMelt
        FuelRod%dyna%EnrgyMeltZp1 = EnrgyMeltZp1
        FuelRod%dyna%RadialBoundO = RadialBoundO
        FuelRod%dyna%DeformedRadiusOfMesh = DeformedRadiusOfMesh
        FuelRod%dyna%WorkSpaceEPP1 = WorkSpaceEPP1
        FuelRod%dyna%gapmin = gapmin
        FuelRod%dyna%WorkSpacePINT = WorkSpacePINT
        FuelRod%dyna%WorkSpaceRCI = WorkSpaceRCI
        FuelRod%dyna%WorkSpaceRCO = WorkSpaceRCO
        FuelRod%dyna%WorkSpaceReloc = WorkSpaceReloc
        FuelRod%dyna%AxNodElevat = AxNodElevat
        FuelRod%dyna%ureloc = ureloc
        FuelRod%dyna%a1 = a1
        FuelRod%dyna%CldElStrn = CldElStrn
        FuelRod%dyna%CldPlasStrn = CldPlasStrn
        FuelRod%dyna%CldPlasStrn0 = CldPlasStrn0
        FuelRod%dyna%CldStrn = CldStrn
        FuelRod%dyna%CldStrnRat = CldStrnRat
        FuelRod%dyna%CldStrnRat0 = CldStrnRat0
        FuelRod%dyna%HydrostatPress = HydrostatPress
        FuelRod%dyna%FuelResidStrn = FuelResidStrn
        FuelRod%dyna%FuelResidStrn0 = FuelResidStrn0
        FuelRod%dyna%Baln2Twall = Baln2Twall
        FuelRod%dyna%CldresidStrn = CldresidStrn
        FuelRod%dyna%CldResidStrn0 = CldResidStrn0
        FuelRod%dyna%IodineContent = IodineContent
        FuelRod%dyna%PelSrfStrn = PelSrfStrn
        FuelRod%dyna%CldStress = CldStress
        FuelRod%dyna%CldThermStrn = CldThermStrn
        FuelRod%dyna%GRsv = GRsv
        FuelRod%dyna%Ifchk = Ifchk
        FuelRod%dyna%nce = nce
        FuelRod%dyna%IndexPCMI = IndexPCMI
        FuelRod%dyna%IndexPCMIOnce = IndexPCMIOnce
        FuelRod%dyna%NumAzmuthNod = NumAzmuthNod
        FuelRod%dyna%Ichf = Ichf
        FuelRod%dyna%Ih = Ih
        FuelRod%dyna%Ihtreg = Ihtreg
        FuelRod%dyna%GapIndex = GapIndex
        FuelRod%dyna%BOSGapIndex = BOSGapIndex
        FuelRod%dyna%BOSGapIndex0 = BOSGapIndex0
        FuelRod%dyna%RuptFailIndex = RuptFailIndex
        FuelRod%dyna%CladCollapseIndex = CladCollapseIndex
        FuelRod%dyna%CladCollIndx0 = CladCollIndx0
        FuelRod%dyna%OldGapIndex = OldGapIndex
        FuelRod%dyna%NodeSinterTemp = NodeSinterTemp
        FuelRod%dyna%RodFailIndex = RodFailIndex
        IF (ALLOCATED(buradv)) FuelRod%dyna%buradv = buradv
        IF (ALLOCATED(bufrad)) FuelRod%dyna%bufrad = bufrad
        
        END SUBROUTINE Remember_Dyna
        !
        SUBROUTINE Remember_Collct (FuelRod)
        !>@brief
        !> This subroutine remembers the Collct module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%collct%ntapou = ntapou
        FuelRod%collct%la1max = la1max
        FuelRod%collct%MaximumIterations = MaximumIterations
        FuelRod%collct%n1 = n1
        FuelRod%collct%la1fil = la1fil
        FuelRod%collct%ntplot = ntplot
        FuelRod%collct%ntabl1 = ntabl1
        FuelRod%collct%ntabl2 = ntabl2
        FuelRod%collct%ntabl3 = ntabl3
        FuelRod%collct%lexcb = lexcb
        FuelRod%collct%lprntb = lprntb
        FuelRod%collct%lflect = lflect
        FuelRod%collct%ldialb = ldialb
        FuelRod%collct%lhtcb = lhtcb
        FuelRod%collct%lresr1 = lresr1
        FuelRod%collct%lresr2 = lresr2
        FuelRod%collct%lsclr1 = lsclr1
        FuelRod%collct%lsclr2 = lsclr2
        FuelRod%collct%lsclr3 = lsclr3
        FuelRod%collct%lcolct = lcolct
        FuelRod%collct%lcoold = lcoold
        FuelRod%collct%lcombk = lcombk
        FuelRod%collct%lresi2 = lresi2
        FuelRod%collct%lresi3 = lresi3
        FuelRod%collct%lafrap = lafrap
        FuelRod%collct%npramp = npramp
        FuelRod%collct%itswt = itswt
        FuelRod%collct%lmatpc = lmatpc
        FuelRod%collct%nswmd = nswmd
        FuelRod%collct%n3 = n3
        FuelRod%collct%ndap1 = ndap1
        FuelRod%collct%IterationCount = IterationCount
        FuelRod%collct%ntco = ntco
        FuelRod%collct%kdbug = kdbug
        FuelRod%collct%lblona = lblona
        FuelRod%collct%lphypr = lphypr
        FuelRod%collct%lthyd = lthyd
        FuelRod%collct%prestmp = prestmp
        FuelRod%collct%dtp = dtp
        FuelRod%collct%z = z
        FuelRod%collct%gum = gum
        FuelRod%collct%Ifstor = Ifstor
        FuelRod%collct%ihData = ihData
        FuelRod%collct%icglob = icglob
        FuelRod%collct%acolct = acolct
        FuelRod%collct%gasphs = gasphs
        FuelRod%collct%gasths = gasths
        FuelRod%collct%gbse = gbse
        FuelRod%collct%pelprm = pelprm
        FuelRod%collct%pinput = pinput
        FuelRod%collct%potput = potput
        FuelRod%collct%tpln = tpln
        FuelRod%collct%tplbt1 = tplbt1
        
        END SUBROUTINE Remember_Collct
        !
        SUBROUTINE Remember_Excb (FuelRod)
        !>@brief
        !> This subroutine remembers the Excb variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%excb%ndim = ndim
        FuelRod%excb%nzmesh = nzmesh
        FuelRod%excb%n2 = n2
        FuelRod%excb%n4 = n4
        FuelRod%excb%nsymm = nsymm
        FuelRod%excb%nconsw = nconsw
        FuelRod%excb%naz = naz
        FuelRod%excb%nnaz = nnaz
        FuelRod%excb%npdtpo = npdtpo
        FuelRod%excb%nbalsw = nbalsw
        FuelRod%excb%nrefld = nrefld
        FuelRod%excb%nlac = nlac
        FuelRod%excb%nprsw = nprsw
        FuelRod%excb%modkf = modkf
        FuelRod%excb%nbotpl = nbotpl
        FuelRod%excb%ncolbp = ncolbp
        FuelRod%excb%dzmesh = dzmesh
        FuelRod%excb%timmd = timmd
        FuelRod%excb%dofset = dofset
        FuelRod%excb%dofang = dofang
        FuelRod%excb%pitch = pitch
        FuelRod%excb%fgrns = fgrns
        FuelRod%excb%fotmtl = fotmtl
        FuelRod%excb%rsntr = rsntr
        FuelRod%excb%tsntrk = tsntrk
        FuelRod%excb%volbp = volbp
        FuelRod%excb%splbp = splbp
        FuelRod%excb%coldbp = coldbp
        FuelRod%excb%spdbp = spdbp
        FuelRod%excb%BottomPlenumGasMoles = BottomPlenumGasMoles
        FuelRod%excb%cldwdc = cldwdc
        FuelRod%excb%aexcb = aexcb
        FuelRod%excb%dtpoa = dtpoa
        FuelRod%excb%tplbot = tplbot
        
        END SUBROUTINE Remember_Excb
        !
        SUBROUTINE Remember_CoolantProperties (FuelRod)
        !>@brief
        !> This subroutine remembers the CoolProperties module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%Coolant%Prop = Prop
        tt => NULL()
        CoolantPress => NULL()
        v => NULL()
        ubar => NULL()
        hbar => NULL()
        beta => NULL()
        kappa => NULL()
        csubp => NULL()
        x => NULL()
        psat => NULL()
        vsubf => NULL()
        vsubg => NULL()
        usubf => NULL()
        usubg => NULL()
        hsubf => NULL()
        hsubg => NULL()
        betaf => NULL()
        betag => NULL()
        kappaf => NULL()
        kappag => NULL()
        csubpf => NULL()
        csubpg => NULL()
        FuelRod%Coolant%nsrad3 = nsrad3
        FuelRod%Coolant%nelrad = nelrad
        FuelRod%Coolant%nhprs = nhprs
        FuelRod%Coolant%ntprs = ntprs
        FuelRod%Coolant%nvprs = nvprs
        FuelRod%Coolant%npprs = npprs
        FuelRod%Coolant%acoold = acoold
        FuelRod%Coolant%vfrad1 = vfrad1
        FuelRod%Coolant%vfrad2 = vfrad2
        FuelRod%Coolant%vfrad3 = vfrad3
        FuelRod%Coolant%elvrad = elvrad
        FuelRod%Coolant%htclev = htclev
        FuelRod%Coolant%gbh = gbh
        FuelRod%Coolant%hbh = hbh
        FuelRod%Coolant%hinta = hinta
        FuelRod%Coolant%hupta = hupta
        FuelRod%Coolant%pbh = pbh
        FuelRod%Coolant%tshrda = tshrda
        FuelRod%Coolant%htca = htca
        FuelRod%Coolant%tblka = tblka
        FuelRod%Coolant%trad1 = trad1
        FuelRod%Coolant%trad2 = trad2
        FuelRod%Coolant%trad3 = trad3
        FuelRod%Coolant%nbrtmp = nbrtmp
        FuelRod%Coolant%nbrfdr = nbrfdr
        FuelRod%Coolant%nbrfht = nbrfht
        FuelRod%Coolant%kaxnlo = kaxnlo
        FuelRod%Coolant%liqnod = liqnod
        FuelRod%Coolant%izadfg = izadfg
        FuelRod%Coolant%irup = irup
        FuelRod%Coolant%nbrpst = nbrpst
        FuelRod%Coolant%nflec = nflec
        FuelRod%Coolant%nbrliq = nbrliq
        FuelRod%Coolant%npaxpf = npaxpf
        FuelRod%Coolant%mzq1 = mzq1
        FuelRod%Coolant%mflt = mflt
        FuelRod%Coolant%mbdl = mbdl
        FuelRod%Coolant%ntempf = ntempf
        FuelRod%Coolant%hliq = hliq
        FuelRod%Coolant%qmax = qmax
        FuelRod%Coolant%empytm = empytm
        FuelRod%Coolant%hrad = hrad
        FuelRod%Coolant%fldrte = fldrte
        FuelRod%Coolant%zqch = zqch
        FuelRod%Coolant%oldtim = oldtim
        FuelRod%Coolant%tflood = tflood
        FuelRod%Coolant%crf = crf
        FuelRod%Coolant%templo = templo
        FuelRod%Coolant%rhostm = rhostm
        FuelRod%Coolant%cpstem = cpstem
        FuelRod%Coolant%tsatt = tsatt
        FuelRod%Coolant%pressr = pressr
        FuelRod%Coolant%pressi = pressi
        FuelRod%Coolant%cpmult = cpmult
        FuelRod%Coolant%gflow = gflow
        FuelRod%Coolant%temphi = temphi
        FuelRod%Coolant%ruplev = ruplev
        FuelRod%Coolant%pavg = pavg
        FuelRod%Coolant%refdtm = refdtm
        FuelRod%Coolant%hydiam = hydiam
        FuelRod%Coolant%flxsec = flxsec
        FuelRod%Coolant%tsub = tsub
        FuelRod%Coolant%pdeint = pdeint
        FuelRod%Coolant%flowbk = flowbk
        FuelRod%Coolant%tempmx = tempmx
        FuelRod%Coolant%pfflec = pfflec
        FuelRod%Coolant%tfoldf = tfoldf
        FuelRod%Coolant%pdecy = pdecy
        FuelRod%Coolant%drflt = drflt
        FuelRod%Coolant%pfnuc = pfnuc
        FuelRod%Coolant%toldfc = toldfc
        FuelRod%Coolant%zqflt = zqflt
        FuelRod%Coolant%qaxpk = qaxpk
        FuelRod%Coolant%zpkfc = zpkfc
        FuelRod%Coolant%fltgap = fltgap
        FuelRod%Coolant%pavgft = pavgft
        FuelRod%Coolant%rcpar = rcpar
        FuelRod%Coolant%zad = zad
        FuelRod%Coolant%zs = zs
        FuelRod%Coolant%trodfc = trodfc
        FuelRod%Coolant%nu1 = nu1
        FuelRod%Coolant%nu2 = nu2
        FuelRod%Coolant%nu3 = nu3
        FuelRod%Coolant%rupflg = rupflg
        FuelRod%Coolant%lodmrk = lodmrk
        FuelRod%Coolant%flthit = flthit
        FuelRod%Coolant%faxzq = faxzq
        FuelRod%Coolant%qaxzq = qaxzq
        FuelRod%Coolant%tempfc = tempfc
        FuelRod%Coolant%aflcht = aflcht
        FuelRod%Coolant%prestm = prestm
        FuelRod%Coolant%hlqclp = hlqclp
        FuelRod%Coolant%temptm = temptm
        FuelRod%Coolant%fldrat = fldrat
        FuelRod%Coolant%nvol = nvol
        FuelRod%Coolant%ithymx = ithymx
        FuelRod%Coolant%ixazim = ixazim
        FuelRod%Coolant%ncall = ncall
        FuelRod%Coolant%tc1 = tc1
        FuelRod%Coolant%tc2 = tc2
        FuelRod%Coolant%tz2 = tz2
        FuelRod%Coolant%z1 = z1
        FuelRod%Coolant%z2 = z2
        FuelRod%Coolant%gz1 = gz1
        FuelRod%Coolant%gz2 = gz2
        FuelRod%Coolant%hz1 = hz1
        FuelRod%Coolant%hz2 = hz2
        FuelRod%Coolant%pz1 = pz1
        FuelRod%Coolant%pz2 = pz2
        FuelRod%Coolant%tz1 = tz1
        FuelRod%Coolant%aasth = aasth
        
        END SUBROUTINE Remember_CoolantProperties
        !
        SUBROUTINE Remember_Intcom (FuelRod)
        !>@brief
        !> This subroutine remembers the Intcom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%intcom%nepp0 = nepp0
        FuelRod%intcom%cladid = cladid
        FuelRod%intcom%cladod = cladod
        FuelRod%intcom%cladtk = cladtk
        FuelRod%intcom%rf = rf
        FuelRod%intcom%FuelPelDiam = FuelPelDiam
        FuelRod%intcom%eppinp = eppinp
        FuelRod%intcom%radpel = radpel
        
        END SUBROUTINE Remember_Intcom
        !
        SUBROUTINE Remember_Iocom (FuelRod)
        !>@brief
        !> This subroutine remembers the Iocom module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%iocom%npltn = npltn
        FuelRod%iocom%tplot = tplot
        FuelRod%iocom%dtplta = dtplta
        
        END SUBROUTINE Remember_Iocom
        !
        SUBROUTINE Remember_Resti (FuelRod)
        !>@brief
        !> This subroutine remembers the Resti module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/18/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%resti%indxmd = indxmd
        FuelRod%resti%kflgb = kflgb
        FuelRod%resti%ldir = ldir
        FuelRod%resti%lengmd = lengmd
        FuelRod%resti%ncladi = ncladi
        FuelRod%resti%nmesh = nmesh
        FuelRod%resti%nodpln = nodpln
        FuelRod%resti%ntstep = ntstep
        FuelRod%resti%Ifcmi = Ifcmi
        FuelRod%resti%igpnod = igpnod
        FuelRod%resti%indx = indx
        FuelRod%resti%indxkc = indxkc
        FuelRod%resti%indxkf = indxkf
        FuelRod%resti%inpfil = inpfil
        FuelRod%resti%la1req = la1req
        FuelRod%resti%lchf = lchf
        FuelRod%resti%length = length
        FuelRod%resti%lhtc = lhtc
        FuelRod%resti%luout = luout
        FuelRod%resti%modfd = modfd
        FuelRod%resti%modmw = modmw
        FuelRod%resti%mpdcay = mpdcay
        FuelRod%resti%naazp = naazp
        FuelRod%resti%naazpd = naazpd
        FuelRod%resti%naxn = naxn
        FuelRod%resti%nkf = nkf
        FuelRod%resti%nchn = nchn
        FuelRod%resti%nchfmd = nchfmd
        FuelRod%resti%ndtmax = ndtmax
        FuelRod%resti%nedtsw = nedtsw
        FuelRod%resti%nerr = nerr
        FuelRod%resti%nfastf = nfastf
        FuelRod%resti%ngbh = ngbh
        FuelRod%resti%nhbh = nhbh
        FuelRod%resti%nhinta = nhinta
        FuelRod%resti%nhtca = nhtca
        FuelRod%resti%nhtclv = nhtclv
        FuelRod%resti%nhtcz = nhtcz
        FuelRod%resti%nhupta = nhupta
        FuelRod%resti%nitmin = nitmin
        FuelRod%resti%nkc = nkc
        FuelRod%resti%npaxp = npaxp
        FuelRod%resti%npaxp1 = npaxp1
        FuelRod%resti%npbh = npbh
        FuelRod%resti%npid = npid
        FuelRod%resti%nplnt = nplnt
        FuelRod%resti%nptha = nptha
        FuelRod%resti%nptha1 = nptha1
        FuelRod%resti%nvoid = nvoid
        FuelRod%resti%ixazmn = ixazmn
        FuelRod%resti%nswinw = nswinw
        FuelRod%resti%NFrapconInitialization = NFrapconInitialization
        FuelRod%resti%nrazp = nrazp
        FuelRod%resti%nrazpd = nrazpd
        FuelRod%resti%ngaspr = ngaspr
        FuelRod%resti%ngastmp = ngastmp
        FuelRod%resti%iStoicGrad = iStoicGrad
        FuelRod%resti%itdmx = itdmx
        FuelRod%resti%kbot = kbot
        FuelRod%resti%knonue = knonue
        FuelRod%resti%IndexInitTemp = IndexInitTemp
        FuelRod%resti%nitdt = nitdt
        FuelRod%resti%NSteadyTrans = NSteadyTrans
        FuelRod%resti%numdta = numdta
        FuelRod%resti%ndtred = ndtred
        FuelRod%resti%nPelRadDeviat = nPelRadDeviat
        FuelRod%resti%nqchn = nqchn
        FuelRod%resti%xtime = xtime
        FuelRod%resti%modfal = modfal
        FuelRod%resti%nswpm = nswpm
        FuelRod%resti%irest2 = irest2
        FuelRod%resti%ncs = ncs
        FuelRod%resti%irest3 = irest3
        FuelRod%resti%indxjk = indxjk
        FuelRod%resti%nrc = nrc
        FuelRod%resti%dtmaxa = dtmaxa
        FuelRod%resti%arest1 = arest1
        FuelRod%resti%azpang = azpang
        FuelRod%resti%fluxz = fluxz
        FuelRod%resti%tplna = tplna
        
        END SUBROUTINE Remember_Resti
        !
        SUBROUTINE Remember_Materials (FuelRod)
        !>@brief
        !> This subroutine remembers the Material module
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/12/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%materials%nomat = nomat
        FuelRod%materials%imaterials = imaterials
        FuelRod%materials%imatflag = imatflag
        FuelRod%materials%imatflag1 = imatflag1
        FuelRod%materials%iDatapairs = iDatapairs
        FuelRod%materials%iheattablen = iheattablen
        
        END SUBROUTINE Remember_Materials
        !
        SUBROUTINE Remember_Store6vars (FuelRod)
        !>@brief
        !> This subroutine remembers the variables in the store6 subroutine
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 5/15/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%store6vars%SEDPNNLold = SEDPNNLold
        FuelRod%store6vars%oldEffStrainPNNL = oldEffStrainPNNL
        FuelRod%store6vars%SEDEPRIold = SEDEPRIold
        FuelRod%store6vars%EPStrain1old = EPStrain1old
        FuelRod%store6vars%EPStrain2old = EPStrain2old
        FuelRod%store6vars%EPStrain3old = EPStrain3old
        FuelRod%store6vars%StressHoopOld = StressHoopOld
        FuelRod%store6vars%StressRadialOld = StressRadialOld
        FuelRod%store6vars%StressAxialOld = StressAxialOld
        FuelRod%store6vars%CladEffStressOld = CladEffStressOld
        FuelRod%store6vars%hsolold = hsolold
        FuelRod%store6vars%OldCldPlasStrn = OldCldPlasStrn
        
        END SUBROUTINE Remember_Store6vars
        !
        SUBROUTINE Remember_Thcntl (FuelRod)
        !>@brief
        !> This subroutine remembers the Thcntl variables
        !>@author
        !> Ian Porter, NRC  May 2014
        !>@date
        !> 5/19/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%thcntl%ncards = ncards
        FuelRod%thcntl%NRestart = NRestart
        FuelRod%thcntl%defsize = defsize
        FuelRod%thcntl%ncool = ncool
        FuelRod%thcntl%ndtad = ndtad
        FuelRod%thcntl%t1 = t1
        FuelRod%thcntl%t2 = t2
        FuelRod%thcntl%unit = unit
        
        END SUBROUTINE Remember_Thcntl
        !
        SUBROUTINE Remember_Frapc (FuelRod)
        !>@brief
        !> This subroutine remembers the frapc variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/16/2014
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%frapc%idx2 = idx2
        FuelRod%frapc%ncorlp = ncorlp
        FuelRod%frapc%ncirlp = ncirlp
        FuelRod%frapc%nforlp = nforlp
        FuelRod%frapc%kmxrlp = kmxrlp
        FuelRod%frapc%kmxfrp = kmxfrp
        FuelRod%frapc%ncool2 = ncool2
        FuelRod%frapc%ndtadv = ndtadv
        FuelRod%frapc%ncard2 = ncard2
        FuelRod%frapc%nrest2 = nrest2
        FuelRod%Variables%maxidx = maxidx
        FuelRod%resti%nqchn = nqchn
        FuelRod%thcntl%ncool = ncool
        FuelRod%frapc%tprlp = tprlp
        FuelRod%frapc%Allocate_Arrays_FT = Allocate_Arrays_FT
        FuelRod%frapc%convert_units = convert_units
        FuelRod%frapc%setunits = setunits
        FuelRod%frapc%first_pass = first_pass
        FuelRod%frapc%FraptranFN = FrapTranFN
        FuelRod%frapc%Iffrp = Iffrp
        FuelRod%frapc%tmprlp = tmprlp
        FuelRod%frapc%rmrlp = rmrlp
        FuelRod%frapc%hgpfrp = hgpfrp
        FuelRod%frapc%pclrlp = pclrlp
        FuelRod%frapc%ElevatThermHydr = ElevatThermHydr
        FuelRod%frapc%ElevatFrap = ElevatFrap
        FuelRod%frapc%drdfrp = drdfrp
        FuelRod%frapc%vrlfrp = vrlfrp
        FuelRod%frapc%pgpfrp = pgpfrp
        FuelRod%frapc%bufrp = bufrp
        FuelRod%frapc%kmxrlp = nvol
        
        END SUBROUTINE Remember_Frapc
        !
        SUBROUTINE Remember_Gas (FuelRod)
        !>@brief
        !> This subroutine remembers the Gas variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%resti%GasFraction = GasFraction
        FuelRod%FGR%gsmol0 = gsmol0
        
        END SUBROUTINE Remember_Gas
        !
        SUBROUTINE Remember_Output (FuelRod)
        !>@brief
        !> This subroutine remembers the Output variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%Output%efuelref = efuelref
        FuelRod%Output%eout0 = eout0
        FuelRod%Output%ein0 = ein0
        FuelRod%Output%efuel0 = efuel0
        FuelRod%Output%eclad0 = eclad0
        
        END SUBROUTINE Remember_Output
        !
        SUBROUTINE Remember_Uncertainty (FuelRod)
        !>@brief
        !> This subroutine remembers the Output variables
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 05/16/2016
        
        CLASS (FRAPTRAN_Vars), INTENT(INOUT) :: FuelRod
        
        FuelRod%Uncertainty%sigfuelthermcond = sigfuelthermcond
        FuelRod%Uncertainty%sigfuelthermexp = sigfuelthermexp
        FuelRod%Uncertainty%sigfuelheatcapa = sigfuelheatcapa
        FuelRod%Uncertainty%sigcladthermcond = sigcladthermcond
        FuelRod%Uncertainty%sigcladthermexp = sigcladthermexp
        FuelRod%Uncertainty%sigcladyieldstr = sigcladyieldstr
        FuelRod%Uncertainty%sigsurfhtc = sigsurfhtc
        FuelRod%Uncertainty%tdkt = tdkt
        FuelRod%Uncertainty%dtdkta = dtdkta
        FuelRod%Uncertainty%dktouts = dktouts
        FuelRod%Uncertainty%dakota = dakota
        FuelRod%Uncertainty%ndktn = ndktn
        FuelRod%Uncertainty%ndktparams = ndktparams
        FuelRod%Uncertainty%dktoutcounter = dktoutcounter
        IF (ALLOCATED(Uncert_fuelref)) FuelRod%Uncertainty%Uncert_fuelref = Uncert_fuelref
        
        END SUBROUTINE Remember_Uncertainty
    
END MODULE FuelRod_Data