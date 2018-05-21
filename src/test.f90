program test
    use fpn4rastk, only : init, next, get, set

    implicit none

    character(len = 32) :: iname
    character(len = 32) :: oname

    integer :: i_input_file = 42
    integer :: i_output_file = 43

    integer(4) :: n_fuel_rad_in
    integer(4) :: na_r
    integer(4) :: na_in
    integer(4) :: n_bu
    integer(4) :: run_media
    integer(4) :: iz
    integer(4) :: i_bu_step

    real(8) :: init_enrich
    real(8) :: pitch_in
    real(8) :: gap_rad
    real(8) :: clad_rad
    real(8) :: fuel_rad
    real(8) :: inlet_temp_in
    real(8) :: mass_flow_rate_in
    real(8) :: init_den
    real(8) :: h_bu_step
    real(8) :: p2
    real(8) :: tw
    real(8) :: go(1)
    real(8) :: qmpy

    integer(4), allocatable :: z_meshes(:)

    real(8), allocatable  :: pow_hist(:,:)
    real(8), allocatable  :: line_pow_hist(:,:)    
    real(8), allocatable  :: thickness_RASTK(:)
    real(8), allocatable  :: thickness_FRPCN(:)
    real(8), allocatable  :: height_FRPCN(:)
    real(8), allocatable  :: height_RASTK(:)
    real(8), allocatable  :: tmp_time(:)
    real(8), allocatable  :: pow_hist_loc(:,:)
    real(8), allocatable  :: power_dist_in(:)
    real(8), allocatable  :: ctf_coo_pres(:,:)
    real(8), allocatable  :: ctf_coo_temp(:,:)
    real(8), allocatable  :: ctf_clad_temp(:,:)

    real(8), allocatable   ::  fue_avg_temp(:)
    real(8), allocatable   ::  coo_avg_temp(:)
    real(8), allocatable   ::  fue_dyn_hgap(:)
    real(8), allocatable   ::  t_oxidelayer(:)
    real(8), allocatable   ::  t_fuecladgap(:)
    real(8), allocatable   ::  gap_pressure(:)
    real(8), allocatable   ::  contact_pres(:)
    real(8), allocatable   ::  hoop_strain (:)
    real(8), allocatable   ::  hoop_stress (:)

    ! ARRAYS NEEDED FOR SPLINE INTERPOLATION
    real(8) :: ispline
    real(8), allocatable  :: ix1(:), iy1(:), b1(:), c1(:), d1(:)
    real(8), allocatable  :: ix2(:), iy2(:), b2(:), c2(:), d2(:)

    ! ARRAYS FOR FRAPCON
    real(8), allocatable  :: qf_FRPCN(:)
    real(8), allocatable  :: tcool_FRPCN(:)
    real(8), allocatable  :: pcool_FRPCN(:)

    ! ITERATIONAL VARIABLES
    integer :: i

    ! READING INPUT FILE

    call get_command_argument(1, iname)
    call get_command_argument(2, oname)

    open(i_input_file, file=iname, status='old')

    read(i_input_file,*) n_fuel_rad_in
    read(i_input_file,*) init_enrich
    read(i_input_file,*) pitch_in
    read(i_input_file,*) fuel_rad
    read(i_input_file,*) gap_rad
    read(i_input_file,*) clad_rad

    read(i_input_file,*) inlet_temp_in   
    read(i_input_file,*) mass_flow_rate_in
    read(i_input_file,*) na_in, na_r

    allocate(z_meshes(1:na_in))
    allocate(thickness_RASTK(1:na_r))
    allocate(thickness_FRPCN(1:na_in))

    read(i_input_file,*) z_meshes(1:na_in)
    read(i_input_file,*) thickness_RASTK(1:na_r)
    read(i_input_file,*) init_den
    read(i_input_file,*) n_bu
    read(i_input_file,*) run_media

    allocate(height_RASTK(1:na_r +1))
    allocate(height_FRPCN(1:na_in+1))
    allocate(tmp_time(1:n_bu))
    allocate(pow_hist(1:na_in,1:n_bu))
    allocate(pow_hist_loc(1:na_in,1:n_bu))
    allocate(line_pow_hist(1:na_r,1:n_bu))
    allocate(power_dist_in(1:na_in))
    allocate(ctf_coo_pres(1:na_in,1:n_bu))
    allocate(ctf_coo_temp(1:na_in,1:n_bu))
    allocate(ctf_clad_temp(1:na_in,1:n_bu))

    read(i_input_file,*) tmp_time(1:n_bu)  

    select case (run_media)
        case(1)
            read(i_input_file,*) (ctf_coo_temp(iz,:), iz = 1, na_in)
            read(i_input_file,*) (ctf_coo_pres(iz,:), iz = 1, na_in)
            read(i_input_file,*) (line_pow_hist(iz,:), iz = 1, na_r)
        case(2)
            read(i_input_file,*) (ctf_clad_temp(iz,:), iz = 1, na_in)
        case default
            read(i_input_file,*) (line_pow_hist(iz,:), iz = 1, na_r)
    end select

    close(i_input_file)

    allocate(qf_FRPCN(1:na_in+1))
    allocate(tcool_FRPCN(1:na_in+1))
    allocate(pcool_FRPCN(1:na_in+1))
    allocate(ix1(1:na_r))
    allocate(iy1(1:na_r))
    allocate(b1(1:na_r))
    allocate(c1(1:na_r))
    allocate(d1(1:na_r))
    allocate(ix2(1:na_in))
    allocate(iy2(1:na_in))
    allocate(b2(1:na_in))
    allocate(c2(1:na_in))
    allocate(d2(1:na_in))

    allocate(fue_avg_temp(1:na_in))
    allocate(coo_avg_temp(1:na_in))
    allocate(fue_dyn_hgap(1:na_in))
    allocate(t_oxidelayer(1:na_in))
    allocate(t_fuecladgap(1:na_in))
    allocate(gap_pressure(1:na_in))
    allocate(hoop_strain (1:na_in))
    allocate(hoop_stress (1:na_in))

    ! DATA PROCESSING
    ! WTF ???
    if(tmp_time(1) .lt. 0.5) tmp_time(1) = 0.5d0

    ! HEIGHTS FOR RASTK and FRAPCON, LIKE (0.5, 1.0, 1.5, 2.0, ...)
    height_RASTK(1)  = 0.
    height_RASTK(2:) = (/( sum(thickness_RASTK(1:i)) , i = 1, na_r )/)
    height_FRPCN(1)  = 0.
    height_FRPCN(2:) = (/( height_RASTK(sum(z_meshes(1:i))+1), i = 1, na_in )/)
    thickness_FRPCN(:) = height_FRPCN(2:na_in+1) - height_FRPCN(1:na_in)

    ! FRAPCON INITIALIZATION
    p2 = ctf_coo_pres(1,1)
    tw = ctf_coo_temp(1,1)
    go(1) = mass_flow_rate_in
    qmpy = sum(line_pow_hist(:,1))
    call init(n_fuel_rad_in, na_in, height_FRPCN, fuel_rad, gap_rad, &
    clad_rad, pitch_in, init_den, init_enrich, p2, tw, go(1), qmpy)

    ! FRAPCON RUN

    h_bu_step = 10.

    do i_bu_step = 1, n_bu

        ! SPLINE INTERPOLATION OF THE RASP-K LINEAR POWER RATIO
        ix1(:) = height_RASTK(1:na_r) +  0.5 * thickness_RASTK(:)
        iy1(:) = line_pow_hist(:,i_bu_step)
        call spline(ix1, iy1, b1, c1, d1, na_r)
        qf_FRPCN(:) = (/( ispline(height_FRPCN(i), ix1, iy1, b1, c1, d1, na_r), i = 1, na_in+1 )/)

        ! SPLINE INTERPOLATION OF THE RASP-K COOLANT TEMPERATURE
        ix2(:) = height_FRPCN(1:na_in) +  0.5 * thickness_FRPCN(:)
        iy2(:) = ctf_coo_temp(:,i_bu_step)
        call spline(ix2, iy2, b2, c2, d2, na_in)
        tcool_FRPCN(:) = (/( ispline(height_FRPCN(i), ix2, iy2, b2, c2, d2, na_in), i = 1, na_in+1 )/)

        ! SPLINE INTERPOLATION OF THE RASP-K COOLANT PRESSURE
        ix2(:) = height_FRPCN(1:na_in) +  0.5 * thickness_FRPCN(:)
        iy2(:) = ctf_coo_pres(:,i_bu_step)
        call spline(ix2, iy2, b2, c2, d2, na_in)
        pcool_FRPCN(:) = (/( ispline(height_FRPCN(i), ix2, iy2, b2, c2, d2, na_in), i = 1, na_in+1 )/)

        ! SETUP THE UPDATED VARIABLES
        call set("linear power", qf_FRPCN)
        call set("coolant temperature", tcool_FRPCN)
        call set("coolant pressure", pcool_FRPCN)
        call set("coolant mass flux", go)
        call next(h_bu_step)

    enddo

    call get('axial fuel temperature', fue_avg_temp)
    call get('bulk coolant temperature', coo_avg_temp)
    call get('gap conductance', fue_dyn_hgap)
    call get('oxide thickness', t_oxidelayer)
    call get('gap thickness', t_fuecladgap)
    call get('gap pressure', gap_pressure)
    call get('hoop strain', hoop_strain)
    call get('hoop stress', hoop_stress)

    open(i_output_file, file=oname, status='unknown')

    do i = 1, na_in
        write(i_output_file,'(100es13.5)') fue_avg_temp(i), coo_avg_temp(i), &
        fue_dyn_hgap(i), t_oxidelayer(i) , t_fuecladgap(i), gap_pressure(i), &
        hoop_strain(i), hoop_stress(i)
    enddo

    write(*,*) 'done!'

end program test