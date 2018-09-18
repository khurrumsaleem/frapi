MODULE conversions_fraptran
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This file contains the subroutines used to perform unit conversions and holds the conversion factors used in the code.
    !> These conversions are identical to those used in FRAPCON-4.0.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/13/2016
    !
    ! ** Constants **
    !
    ! Avogadro's Number
    REAL(r8K), PARAMETER :: Avogadro = 6.0221413E23_r8k              ! (molecules / mol)
    ! Boltzmann's Constant
    REAL(r8k), PARAMETER :: Boltzmann = 1.3806505E-23_r8k            ! (J / K)
    ! Gravitational Acceleration
    REAL(r8K), PARAMETER :: Gravity_SI = 9.80665_r8k                 ! (m / s)
    ! Ideal Gas Constant
    REAL(r8K), PARAMETER :: R_JmolK = 8.3144621_r8k                  ! (J / mol - K)
    ! Ideal Gas Constant
    REAL(r8k), PARAMETER :: R_in3psiRlbmol = 10.73159_r8k            ! (in^3*psi)/(Rankine*lb-mol)
    ! Pi
    REAL(r8k), PARAMETER :: pi = 3.1415926535897932_r8k              ! Value for pi
    ! Speed of Light
    REAL(r8K), PARAMETER :: speedlight = 2.99792458E8_r8k            ! (m / s)
    

    REAL(r8k), PARAMETER :: ftin = 12.0_r8k              ! ( inch / ft )
    REAL(r8k), PARAMETER :: ftom = 0.3048_r8k            ! ( ft / meter )
    REAL(r8k), PARAMETER :: ftmetr = 3.28084_r8k         ! ( m / ft )
    REAL(r8k), PARAMETER :: sechr = 3600.0_r8k           ! ( second / hour )
    REAL(r8k), PARAMETER :: btuhkw = 3412.874_r8k        ! ( btu/hour / kilowatt )
    REAL(r8k), PARAMETER :: powcnv = 0.9478_r8k          ! ( Btu/second / kilowatt) !.94781712
    REAL(r8k), PARAMETER :: psinm2 = 1.4503774e-4_r8k    ! ( psi / n/m2 )
    REAL(r8k), PARAMETER :: psift2 = 144.0_r8k           ! ( psi / lb/ft^2 )
    REAL(r8k), PARAMETER :: gtolb = 453.6_r8k            ! ( grams / lb )
    REAL(r8k), PARAMETER :: hgpcnv = 5.67826_r8k         ! Convert gap HTC (Btu/hr*ft^2*F to W/m^2*K)
    !
    !
    ! ** Energy **
    !
    ! Convert BTU/lb to J/kg
    REAL(r8k), PARAMETER :: BTUlbtoJkg = 2326.000292_r8k             ! (J/kg / BTU/lb)
    ! Convert J/kg to BTU/lb
    REAL(r8k), PARAMETER :: JkgtoBTUlb = 1.0_r8k / BTUlbtoJkg        ! (BTU/lb / J/kg)
    !
    ! ** Heat Conductance (Thermal Conductivity) **
    !
    ! Convert Btu/(hr*ft*F) to W/(m*K)
    REAL(r8k), PARAMETER :: BhftFtoWmK = 1.73073467_r8k              ! Convert gap HTC (W/m*K / Btu/hr*ft*F)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBhftF = 1.0_r8k / BhftFtoWmK        ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBsftF = WmKtoBhftF / 3600.0_r8k     ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    !
    ! ** Heat Capacity **
    !
    ! Convert Btu/(lb*F) to J/(kg*K)
    REAL(r8k), PARAMETER :: BtulbFtoJkgK = 4183.995381_r8k           ! (J/kg*K / Btu/lb*F) or (J/kg*C)
    ! Convert J/(kg*K) to Btu/(lb*F)
    REAL(r8k), PARAMETER :: JkgKtoBtulbF = 1.0_r8k / BtulbFtoJkgK    ! (Btu/lb*F / J/kg*K) or (J/kg*C)
    !
    ! ** Pressure **
    !
    ! Convert atm to psi
    REAL(r8k), PARAMETER :: ATMtoPSI = 14.6959488_r8k                ! (psi / atm)
    ! Convert psi to atm
    REAL(r8k), PARAMETER :: PSItoATM = 1.0_r8k / ATMtoPSI            ! (atm / psi)
    ! Convert psi to lb/ft^2
    REAL(r8k), PARAMETER :: PSItolbft2 = 144.0_r8k                   ! (lb/in^2 / lb/ft^2)
    ! Convert kg/cm^2 to psi
    REAL(r8k), PARAMETER :: kgcm2toPSI = 14.2233433_r8k              ! (psi / kg-Force/cm^2)
    ! Convert psi to kg/cm^2
    REAL(r8k), PARAMETER :: PSItokgcm2 = 1.0_r8k / kgcm2toPSI        ! (kg-Force/cm^2 / psi)
    ! Convert psi to kPa
    REAL(r8k), PARAMETER :: PSItokPa = 6.89475728_r8k                ! (kPa / psi)
    ! Convert psi to Pa
    REAL(r8k), PARAMETER :: PSItoPa = 1.0E3_r8k * PSItokPa           ! (Pa / psi)
    ! Convert psi to MPa
    REAL(r8k), PARAMETER :: PSItoMPa = 1.0E-3_r8k * PSItokPa         ! (MPa / psi)
    ! Convert Pa to psi
    REAL(r8k), PARAMETER :: PatoPSI = 1.0_r8k / PSItoPa              ! (Pa / psi)
    ! Convert MPa to psi
    REAL(r8k), PARAMETER :: MPatoPSI = 1.0_r8k / PSItoMPa            ! (Pa / psi)

    !
    CONTAINS
    !
    PURE FUNCTION TO_UPPERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes lowercase text in a string to uppercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(LOWL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = CAPL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_UPPERCASE
    !
    !
    !
    PURE FUNCTION TO_LOWERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes uppercase text in a string to lowercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(CAPL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = LOWL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_LOWERCASE
    !
    ! ** Temperature Conversion Functions **
    !
    PURE ELEMENTAL FUNCTION tkf (T_Kelvin) RESULT (T_Fahrenheit)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: K to F
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/14/2016
    REAL(r8k), INTENT(IN) :: T_Kelvin
    REAL(r8k) :: T_Fahrenheit
    
    T_Fahrenheit = (T_Kelvin * 1.8_r8k) - 459.67_r8k
    
    END FUNCTION tkf
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfr (T_Fahrenheit) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  R
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Rankine
    
    T_Rankine = T_Fahrenheit + 459.67_r8k
    
    END FUNCTION tfr
    !
    !
    !
    PURE ELEMENTAL FUNCTION tkc (T_Kelvin) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: Kelvin to Celcius
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/23/2016
    REAL(r8k), INTENT(IN) :: T_Kelvin
    REAL(r8k) :: T_Celcius
    
    T_Celcius = T_Kelvin - 273.15_r8k
    
    END FUNCTION tkc
    !
    !
    !
    PURE ELEMENTAL FUNCTION trc (T_Rankine) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  R  to  C
    REAL(r8k), INTENT(IN) :: T_Rankine
    REAL(r8k) :: T_Celcius
    
    T_Celcius = (T_Rankine / 1.8_r8k) - 273.15_r8k
    
    END FUNCTION trc
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfk (T_Fahrenheit) RESULT (T_Kelvin)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: F to K
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/14/2016
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Kelvin
    
    T_Kelvin = (T_Fahrenheit + 459.67_r8k) / 1.8_r8k
    
    END FUNCTION tfk
    !
    !
    !
    PURE ELEMENTAL FUNCTION tcr (T_Celcius) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  R
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Rankine
    
    T_Rankine = ((T_Celcius * 1.8_r8k) + 32.0_r8k) + 459.67_r8k
    
    END FUNCTION tcr
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfc (T_Fahrenheit) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  C
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Celcius
    
    T_Celcius = (T_Fahrenheit - 32.0_r8k) / 1.8_r8k
    
    END FUNCTION tfc
    !
    !
    !
    PURE ELEMENTAL FUNCTION tcf (T_Celcius) RESULT (T_Fahrenheit)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  F
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Fahrenheit
    
    T_Fahrenheit = (T_Celcius * 1.8_r8k) + 32.0_r8k
    
    END FUNCTION tcf
    !
    !
    !
    SUBROUTINE coneu (xin, xout, itype)
    USE Kinds, ONLY : ipk, r8k
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> coneu - convert engineering units.
    !> This routine converts engineering units using the itype argument to specify the type of conversion needed.
    !> For units with itypes less than 11, a multiplication and addition factor is needed.
    !> For itypes greater than or equal to 11, only a multiplication factor is needed.  (y = (x * a) + b)
    !>@author
    !> h.d. stewart     07/15/80
    !
    ! Input
    !
    ! p1 - Number to be converted
    ! p3 - Type of conversion
    !
    ! Output
    !
    ! p2 - Converted number
    !
    INTEGER(ipk), INTENT(IN) :: itype
    INTEGER(ipk), PARAMETER :: itotal = 28
    REAL(r8k), INTENT(IN) :: xin
    REAL(r8k), INTENT(OUT) :: xout
    REAL(r8k) :: a, x
    REAL(r8k), DIMENSION(itotal), PARAMETER :: contbl = &
      &             [ 0.555555556_r8k,   255.37222_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                     1.2e4_r8k,    11348.93_r8k, &
      &                    5.6785_r8k,      0.3048_r8k, &
      &                     25.40_r8k,      0.0254_r8k, &
      &              1.6387064e+4_r8k,  6.89476e-3_r8k, &
      &                   3.28084_r8k,  1.35623e-3_r8k, &
      &                  2.326e+3_r8k,       304.8_r8k, &
      &                    1728.0_r8k,        12.0_r8k, &
      &                   1055.87_r8k,    11348.93_r8k, &
      &                16.0184625_r8k,   0.4535924_r8k ]
    !
    !    The conversion code is as follows:
    !
    !    TYPE   from                to
    !
    !     1     K                   F
    !    11     ft                  mils
    !    12     btu/(s.ft2)         W/m2
    !    13     btu/((hr)(ft2)(F))  W/((m2)(K))
    !    14     ft                  m
    !    15     in                  mm
    !    16     mils                mm
    !    17     in3                 mm3
    !    18     psia                mpa
    !    19     kW/ft               kW/m
    !    20     lb/((hr)(ft2))      kg/((s)(m2))
    !    21     btu/lb              j/kg
    !    22     ft                  mm
    !    23     ft3                 in3
    !    24     ft                  in
    !    25     btu                 j
    !    26     btu/((s)(ft2))      W/m2
    !    27     lbm/ft3             kg/m3
    !    28     lbm                 kg
    !
    xout = xin
    !
    IF (itype <= 0 .OR. itype > itotal) THEN
        WRITE(ounit,10) itype
10      FORMAT(/'itype = ',i6,' is not in expected range of 1 to itotal')
        STOP
    ENDIF
    !
    x = contbl(itype)
    a = 0.0_r8k
    IF (itype <= 9) a = contbl(itype+1)
    !
    xout = (xin * x) + a
    !
    END SUBROUTINE coneu
    !
END MODULE conversions_fraptran












