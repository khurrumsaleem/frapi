module m_problem

    use conversions_fraptran
    use frapi, only : t_fuelrod
    use fpc_reader
    use fpt_reader
    use m_interp1d, only : interp1d
    use m_round, only : round
    use odfile, only : t_odfile

    implicit none

    character(len=128), dimension(88) :: varname, vartype
    real(8), allocatable :: timesteps(:,:)

    type, public :: t_problem
        type (t_odfile)  :: ofile
        type (t_fuelrod) :: frod
        integer :: nsteps
        real(8) :: finishtime, timestep
        contains
        procedure :: make_fraptran => p_make_fraptran
        procedure :: update_fraptran => p_update_fraptran
        procedure :: timestep_fraptran => p_timestep_fraptran
        procedure :: save_in_file_fraptran =>  p_save_in_file_fraptran
        procedure :: finalize => p_finalize
    end type t_problem

    contains

    subroutine p_make_fraptran (this, ifilename, rfilename, ofilename, frapmode)

        implicit none

        class (t_problem), intent(inout) :: this

        character(len=256) :: ifilename, rfilename, ofilename, frapmode

        integer :: i, j, itime, starttime, n

        real(8) :: qtot, dt

        include "fi_fpt_outvars_h.f90"

        call this % ofile % open(ofilename)

        ! Read input data from fraptran's input file
        call read_fraptran_file(ifilename)

        this % finishtime = ProblemEndTime

        call this % frod % make(nr=nfmesh, na=naxn, nce=ncmesh, verbose=.false., frapmode=frapmode)

        call this % frod % set_ch_0 ('restart file', rfilename   )
        call this % frod % set_ch_0 ('coolant'     , coolant     )
        call this % frod % set_ch_0 ('bheat'       , bheat       )
        call this % frod % set_ch_0 ('mheat'       , mheat       )
        call this % frod % set_ch_0 ('reflood'     , reflood     )
        call this % frod % set_ch_0 ('internal'    , internal    )
        call this % frod % set_ch_0 ('metal'       , metal       )
        call this % frod % set_ch_0 ('deformation' , deformation )
        call this % frod % set_ch_0 ('inst'        , inst        )
        call this % frod % set_ch_0 ('relocmodel'  , relocmodel  )
        call this % frod % set_i4_0 ("geomet"      , geomet      )
        call this % frod % set_i4_0 ("nvol1"       , nvol1       )
        call this % frod % set_i4_0 ("lowpl"       , lowpl       )
        call this % frod % set_i4_0 ("pressu"      , pressu      )
        call this % frod % set_i4_0 ("massfl"      , massfl      )
        call this % frod % set_i4_0 ("coreav"      , coreav      )
        call this % frod % set_i4_0 ("chf"         , chf         )
        call this % frod % set_i4_0 ("filmbo"      , filmbo      )
        call this % frod % set_i4_0 ("coldwa"      , coldwa      )
        call this % frod % set_i4_0 ("axpow"       , axpow       )
        call this % frod % set_i4_0 ("bowing"      , bowing      )
        call this % frod % set_i4_0 ("spefbz"      , spefbz      )
        call this % frod % set_i4_0 ("geometry"    , geometry    )
        call this % frod % set_i4_0 ("nbundl"      , nbundl      )
        call this % frod % set_i4_0 ("refloodtime" , time        )
        call this % frod % set_i4_0 ("radiat"      , radiat      )
        call this % frod % set_i4_0 ("ruptur"      , ruptur      )
        call this % frod % set_i4_0 ("liquid"      , liquid      )
        call this % frod % set_i4_0 ("inlet"       , inlet       )
        call this % frod % set_i4_0 ("reflo"       , reflo       )
        call this % frod % set_i4_0 ("pressure"    , pressure    )
        call this % frod % set_i4_0 ("collaps"     , collaps     )
        call this % frod % set_i4_0 ("frapt4"      , frapt4      )
        call this % frod % set_i4_0 ("geom"        , geom        )
        call this % frod % set_i4_0 ("temp"        , temp        )
        call this % frod % set_i4_0 ("tape2"       , tape2       )
        call this % frod % set_i4_0 ("nvol2"       , nvol2       )
        call this % frod % set_i4_0 ("zone"        , zone        )
        call this % frod % set_i4_0 ("upppl"       , upppl       )
        call this % frod % set_i4_0 ("jfb"         , jfb         )
        call this % frod % set_i4_0 ("nucbo"       , nucbo       )
        call this % frod % set_i4_0 ("unitin"      , unitin      )
        call this % frod % set_i4_0 ("unitout"     , unitout     )
        call this % frod % set_i4_0 ("res"         , res         )
        call this % frod % set_i4_0 ("pow"         , pow         )
        call this % frod % set_i4_0 ("gasflo"      , gasflo      )
        call this % frod % set_i4_0 ("idoxid"      , idoxid      )
        call this % frod % set_i4_0 ("cathca"      , cathca      )
        call this % frod % set_i4_0 ("baker"       , baker       )
        call this % frod % set_i4_0 ("noball"      , noball      )
        call this % frod % set_i4_0 ("cenvoi"      , cenvoi      )
        call this % frod % set_i4_0 ("soltyp"      , soltyp      )
        call this % frod % set_r8_0 ('splbp'       , splbp       )
        call this % frod % set_r8_0 ('tpowf', tpowf)
        call this % frod % set_r8_0 ('ruptstrain', ruptstrain)
        call this % frod % set_r8_0 ('frcoef', frcoef)
        call this % frod % set_r8_0 ('epsht1', epsht1)
        call this % frod % set_r8_0 ('CladPower', CladPower)
        call this % frod % set_i4_0 ('azang', azang)
        call this % frod % set_r8_0 ('pitch', pitch)
        call this % frod % set_r8_0 ('bowthr', bowthr)
        call this % frod % set_i4_0 ('iStoicGrad', iStoicGrad)
        call this % frod % set_i4_0 ('prestmp', prestmp)
        call this % frod % set_r8_0 ('dofang', dofang)
        call this % frod % set_r8_0 ('coldbp', coldbp)
        call this % frod % set_i4_0 ('nbhtc', nbhtc)
        call this % frod % set_i4_0 ('rtheta', rtheta)
        call this % frod % set_r8_0 ('frden', frden)
        call this % frod % set_r8_0 ('RodDiameter', RodDiameter)
        call this % frod % set_i4_0 ('prescri', prescri)
        call this % frod % set_i4_0 ('mechan', mechan)
        call this % frod % set_i4_0 ('nfmesh', nfmesh)
        call this % frod % set_r8_0 ('refdtm', refdtm)
        call this % frod % set_r8_0 ('totnb', totnb)
        call this % frod % set_r8_0 ('powop', powop)
        call this % frod % set_r8_0 ('flxsec', flxsec)
        call this % frod % set_i4_0 ('NumAxProfiles', NumAxProfiles)
        call this % frod % set_i4_0 ('radiat', radiat)
        call this % frod % set_r8_0 ('ffch', ffch)
        call this % frod % set_r8_0 ('fpdcay', fpdcay)
        call this % frod % set_r8_0 ('roughc', roughc)
        call this % frod % set_r8_0 ('roughf', roughf)
        call this % frod % set_r8_0 ('prsacc', prsacc)
        call this % frod % set_r8_0 ('fpowr', fpowr)
        call this % frod % set_i4_0 ('maxit', maxit)
        call this % frod % set_r8_0 ('tref', tref)
        call this % frod % set_i4_0 ('tape1', tape1)
        call this % frod % set_i4_0 ('jtr', jtr)
        call this % frod % set_r8_0 ('pelh', pelh)
        call this % frod % set_r8_0 ('pdrato', pdrato)
        call this % frod % set_i4_0 ('NRestart', NRestart)
        call this % frod % set_i4_0 ('irupt', irupt)
        call this % frod % set_r8_0 ('tgas0', tgas0)
        call this % frod % set_r8_0 ('tsntrk', tsntrk)
        call this % frod % set_i4_0 ('CladType', CladType)
        call this % frod % set_r8_0 ('spdbp', spdbp)
        call this % frod % set_i4_0 ('odoxid', odoxid)
        call this % frod % set_r8_0 ('achn', achn)
        call this % frod % set_i4_0 ('nchn', nchn)
        call this % frod % set_r8_0 ('tflux', tflux)
        call this % frod % set_i4_0 ('TranSwell', TranSwell)
        call this % frod % set_r8_0 ('RodLength', RodLength)
        call this % frod % set_i4_0 ('noiter', noiter)
        call this % frod % set_i4_0 ('nthermex', nthermex)
        call this % frod % set_r8_0 ('OpenPorosityFraction', OpenPorosityFraction)
        call this % frod % set_r8_0 ('zad', zad)
        call this % frod % set_r8_0 ('rshrd', rshrd)
        call this % frod % set_i4_0 ('IndexFC2Print', IndexFC2Print)
        call this % frod % set_r8_0 ('doffst', doffst)
        call this % frod % set_i4_0 ('irefine', irefine)
        call this % frod % set_r8_0 ('emptm', emptm)
        call this % frod % set_i4_0 ('ProtectiveOxide', ProtectiveOxide)
        call this % frod % set_r8_0 ('trise', trise)
        call this % frod % set_r8_0 ('fltgap2', fltgap2)
        call this % frod % set_r8_0 ('hydiam', hydiam)
        call this % frod % set_r8_0 ('dishd', dishd)
        call this % frod % set_r8_0 ('ph', ph)
        call this % frod % set_r8_0 ('hrad', hrad)
        call this % frod % set_r8_0 ('dtss', dtss)
        call this % frod % set_r8_0 ('bup', bup)
        call this % frod % set_r8_0 ('cldwdc', cldwdc)
        call this % frod % set_r8_0 ('timop', timop)
        call this % frod % set_i4_0 ('nIDoxide', nIDoxide)
        call this % frod % set_i4_0 ('IndexGrainBndSep', IndexGrainBndSep)
        call this % frod % set_r8_0 ('cfluxa', cfluxa)
        call this % frod % set_r8_0 ('rvoid', rvoid)
        call this % frod % set_r8_0 ('dofset', dofset)
        call this % frod % set_i4_0 ('grass', grass)
        call this % frod % set_r8_0 ('pl', pl)
        call this % frod % set_i4_0 ('ncolbp', ncolbp)
        call this % frod % set_r8_0 ('fltgap', fltgap)
        call this % frod % set_i4_0 ('presfgr', presfgr)
        call this % frod % set_r8_0 ('frpo2', frpo2)
        call this % frod % set_r8_0 ('trest', trest)
        call this % frod % set_i4_0 ('inp', inp)
        call this % frod % set_r8_0 ('fgrns', fgrns)
        call this % frod % set_r8_0 ('refine', refine)
        call this % frod % set_r8_0 ('modheat', modheat)
        call this % frod % set_r8_0 ('tmpac1', tmpac1)
        call this % frod % set_r8_0 ('coldw', coldw)
        call this % frod % set_r8_0 ('dhe', dhe)
        call this % frod % set_r8_0 ('explenumv', explenumv)
        call this % frod % set_r8_0 ('dhy', dhy)
        call this % frod % set_i4_0 ('naz', naz)
        call this % frod % set_i4_0 ('jchf', jchf)
        call this % frod % set_r8_0 ('volbp', volbp)
        call this % frod % set_r8_0 ('rshd', rshd)
        call this % frod % set_i4_0 ('profile', profile)
        call this % frod % set_r8_0 ('fotmtl', fotmtl)
        call this % frod % set_r8_0 ('gsms', gsms)
        call this % frod % set_r8_0 ('dishv0', dishv0)           
        call this % frod % set_i4_0 ('nsym', nsym)
        call this % frod % set_r8_0 ('rnbnt', rnbnt)
        call this % frod % set_r8_0 ('zvoid2', zvoid2)
        call this % frod % set_r8_0 ('gapthk', gapthk)
        call this % frod % set_r8_0 ('zvoid1', zvoid1)
        call this % frod % set_r8_0 ('zs', zs)
        call this % frod % set_r8_0 ('FuelPelDiam', FuelPelDiam)
        call this % frod % set_r8_1 ('scd', scd)
        call this % frod % set_r8_1 ('azpang', azpang)
        call this % frod % set_r8_1 ('htclev', htclev)
        call this % frod % set_r8_1 ('ExtentOfBow', ExtentOfBow)
        call this % frod % set_r8_1 ('vplen', vplen)
        call this % frod % set_r8_1 ('gadoln', gadoln)
        call this % frod % set_r8_1 ('gfrac', gfrac)
        call this % frod % set_r8_1 ('gbse', gbse)
        call this % frod % set_r8_1 ('fluxz', fluxz)
        call this % frod % set_r8_1 ('nodchf', nodchf)
        call this % frod % set_i4_1 ('ngastmp', ngastmp)
        call this % frod % set_r8_1 ('swd', swd)
        call this % frod % set_r8_1 ('oxideod', oxideod)
        call this % frod % set_r8_1 ('cexh2a', cexh2a)
        call this % frod % set_r8_2 ('pazp', pazp)
        call this % frod % set_r8_1 ('radpel', radpel)
        call this % frod % set_r8_1 ('gappr0', gappr0)
        call this % frod % set_r8_1 ('butemp', butemp)
        call this % frod % set_r8_1 ('oxideid', oxideid)
        call this % frod % set_r8_1 ('spl', spl)
        call this % frod % set_r8_1 ('eppinp', eppinp)
        call this % frod % set_r8_1 ('techf', techf)
        call this % frod % set_i4_1 ('ncs', ncs)
        call this % frod % set_r8_1 ('tschf', tschf)
        call this % frod % set_r8_1 ('zelev', zelev)
        call this % frod % set_r8_1 ('fmesh', fmesh)
        call this % frod % set_r8_1 ('cmesh', cmesh)

        do i = 1, lsize(rodavepower) * 2
            RodAvePower(2*i-1) = RodAvePower(2*i-1) * fpowr
        enddo

        i = 1
        do while ((dtmaxa(2*i) < dtmaxa(2*i+2)).and.(2*i+2 < size(dtmaxa)))
            i = i + 1
        enddo
        n = i
        allocate(timesteps(n,2))

        do i = 1, n-1
            timesteps(i,1) = dtmaxa(2*i+2)
            timesteps(i,2) = dtmaxa(2*i-1)
        enddo
        timesteps(n,1) = problemendtime
        timesteps(n,2) = dtmaxa(2*n-1)

    end subroutine p_make_fraptran

    subroutine p_update_fraptran (this, time)

        class (t_problem), intent(inout) :: this

        integer :: n_roda, n_relf, n_expl
        integer :: i, j
        real(8) :: time

        call this % frod % set_r8_1('htca',   (/( interp1d( (/(   htca(i,j), i = 1, 2*htco(j) )/), time ), j = 1, zone)/) ) ! htco(j)
        call this % frod % set_r8_1('tblka',  (/( interp1d( (/(  tblka(i,j), i = 1, 2*tem(j)  )/), time ), j = 1, zone)/) ) ! tem(j)
        !call this % frod % set_r8_1('gasths', (/( interp1d( (/( gasths(i,j), i = 1, ntimesteps )/), time ), j = 1, 2          )/) )
        
        call this % frod % set_r8_1('axpowprofile', (/( AxPowProfile(j,1), j = 1, 2*naxialnodes )/) )
        
        !call this % frod % set_r8_1('radtemp',      (/( interp1d( radpowprofile(2*j+1 + 2*naxialnodes*(i+1)), j = 1, naxialnodes )/) )
        !call this % frod % set_r8_1('fuelrad',      (/( interp1d( radpowprofile(2*j   + 2*naxialnodes*(i+1)), j = 1, naxialnodes )/) )

        n_roda = lsize(rodavepower)
        n_relf = lsize(relfraca)
        n_expl = lsize(explenumt)

        if (   coreav > 1) call this % frod % set_r8_0('hbh',          interp1d(         hbh(1:   coreav * 2), time) )
        if (    upppl > 1) call this % frod % set_r8_0('hupta',        interp1d(       hupta(1:    upppl * 2), time) )
        if (    lowpl > 1) call this % frod % set_r8_0('hinta',        interp1d(       hinta(1:    lowpl * 2), time) )
        if (   massfl > 1) call this % frod % set_r8_0('gbh',          interp1d(         gbh(1:   massfl * 2), time) )
        if (TranSwell > 1) call this % frod % set_r8_0('FuelGasSwell', interp1d(FuelGasSwell(1:TranSwell * 2), time) )
        if (    inlet > 1) call this % frod % set_r8_0('temptm',       interp1d(      temptm(1:    inlet * 2), time) )
        if ( pressure > 1) call this % frod % set_r8_0('prestm',       interp1d(      prestm(1: pressure * 2), time) )
        if (    reflo > 1) call this % frod % set_r8_0('fldrat',       interp1d(      fldrat(1:    reflo * 2), time) )
        if (  prescri > 1) call this % frod % set_r8_0('gasphs',       interp1d(      gasphs(1:  prescri * 2), time) )
        if (  collaps > 1) call this % frod % set_r8_0('hlqcl',        interp1d(       hlqcl(1:  collaps * 2), time) )
        if (   n_roda > 1) call this % frod % set_r8_0('RodAvePower',  interp1d( RodAvePower(1:   n_roda * 2), time) )
        if (   n_relf > 1) call this % frod % set_r8_0('relfraca',     interp1d(    relfraca(1:   n_relf * 2), time) )
        if (   n_expl > 1) call this % frod % set_r8_0('explenumt',    interp1d(   explenumt(1:   n_expl * 2), time) )

        if (coolant == 'on') call this % frod % set_r8_0('pbh', pbh1(1) )
        if (  mheat == 'on') call this % frod % set_r8_0('pbh', pbh2(1) )

    end subroutine p_update_fraptran

    subroutine p_save_in_file_fraptran (this)
        implicit none
        class (t_problem), intent(inout) :: this
        integer :: i
        integer :: var_i4_0, var_i4_1(naxn)
        real(8) :: var_r8_0, var_r8_1(naxn), var_r8_2(nradialnodes, naxn)
        real(8) :: tmp0(naxn)
        !na = naxn + 25
        !nr = nfmesh + ncmesh + 1
        do i = 1, size(varname)
            select case (vartype(i))
            case ('i4_0')
                call this % frod  % get_i4_0  (varname(i), var_i4_0)
                call this % ofile % write_i4_0(varname(i), var_i4_0)
            case ('i4_1')
                call this % frod  % get_i4_1  (varname(i), var_i4_1)
                call this % ofile % write_i4_1(varname(i), var_i4_1)
            case ('r8_0')
                call this % frod  % get_r8_0  (varname(i), var_r8_0)
                call this % ofile % write_r8_0(varname(i), var_r8_0)
            case ('r8_1')
                call this % frod  % get_r8_1  (varname(i), var_r8_1)
                call this % ofile % write_r8_1(varname(i), var_r8_1)
            case ('r8_2')
                !call this % frod  % get_r8_2  (varname(i), var_r8_2)
                !call this % ofile % write_r8_2(varname(i), var_r8_2)
            end select
        enddo
    end subroutine p_save_in_file_fraptran

    function p_timestep_fraptran(this, time) result (dt)
        implicit none
        class (t_problem), intent(inout) :: this
        real(8) :: time, dt
        integer :: i, n(2)

        n(:) = shape(timesteps)

        do i = n(1), 1, -1
            if (time < timesteps(i,1)) dt = timesteps(i,2)
        enddo

    end function p_timestep_fraptran

    subroutine p_finalize (this)
        implicit none
        class (t_problem), intent(inout) :: this
        call this % frod % destroy ()
        call this % ofile % close()
    end subroutine p_finalize


    integer function lsize(a)
        implicit none
        real(8) :: a(:)
        integer :: n, i
        n = size(a)
        i = 1
        do while ((a(2*i) < a(2*i+2)).and.(2*i < n))
            i = i + 1
        enddo
        lsize = i
    end function lsize

end module m_problem