program test
    use fpn4rastk, only : init, next, set_linear_power_ratio

    implicit none

    character(len = 32) :: iname
    character(len = 32) :: oname

    integer :: i_input_file = 42

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
    real(8) :: pin_power    
    real(8) :: h_bu_step

    integer(4), allocatable :: z_meshes(:)

    real(8), allocatable  :: pow_hist(:,:)
    real(8), allocatable  :: line_pow_hist(:,:)    
    real(8), allocatable  :: thickness_RASTK(:)
    real(8), allocatable  :: height_FRPCN(:)
    real(8), allocatable  :: height_RASTK(:)
    real(8), allocatable  :: bong_rad(:)  
    real(8), allocatable  :: tmp_time(:)
    real(8), allocatable  :: pow_hist_loc(:,:)
    real(8), allocatable  :: power_dist_in(:)
    real(8), allocatable  :: height_FPN(:)
    real(8), allocatable  :: ctf_coo_pres(:,:)
    real(8), allocatable  :: ctf_coo_temp(:,:)
    real(8), allocatable  :: ctf_clad_temp(:,:)

    ! ARRAYS NEEDED FOR SPLINE INTERPOLATION
    real(8) :: ispline
    real(8), allocatable  :: ix(:), iy(:), b(:), c(:), d(:)


    ! LINEAR POWER RATIO FOR FRAPCON
    real(8), allocatable  :: qf(:)

    ! ADDITIONAL VARIABLES
    integer :: i, j, k

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
    allocate(qf(1:na_in+1))
    allocate(ix(1:na_r))
    allocate(iy(1:na_r))
    allocate(b(1:na_r))
    allocate(c(1:na_r))
    allocate(d(1:na_r))

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

    ! DATA PROCESSING
    ! WTF ???
    if(tmp_time(1) .lt. 0.5) tmp_time(1) = 0.5d0

    ! HEIGHTS FOR RASTK and FRAPCON, LIKE (0.5, 1.0, 1.5, 2.0, ...)
    height_RASTK(1)  = 0.
    height_RASTK(2:) = (/ (sum(thickness_RASTK(1:i)) , i = 1, na_r)  /)
    height_FRPCN(1)  = 0.
    height_FRPCN(2:) = (/ (height_RASTK(sum(z_meshes(1:i))), i = 1, na_in ) /)

    ! FRAPCON INITIALIZATION 
    call init(n_fuel_rad_in, na_in, height_FRPCN)

    ! FRAPCON RUN

    h_bu_step = 10.

    do i_bu_step = 1, n_bu

        ! SPLINE INTERPOLATION OF THE RASP-K LINEAR POWER RATIO
        ix(:) = height_RASTK(1:na_r) +  0.5 * thickness_RASTK(:)
        iy(:) = line_pow_hist(:,i_bu_step)
        call spline(ix, iy, b, c, d, na_r)
        qf(:) = (/( ispline(height_FRPCN(i), ix, iy, b, c, d, na_r), i = 1, na_in+1 )/)

        ! SETUP THE UPDATED VARIABLES
        call set_linear_power_ratio(qf)
        call next(h_bu_step)

    enddo

    close(i_input_file)

    write(*,*) 'done!'

end program test