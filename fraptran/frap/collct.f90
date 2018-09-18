Module collct_h
    USE Kinds
    IMPLICIT NONE
    !
    TYPE collct_var
        ! No longer used. Replaced by frtrunit. I.P.
        INTEGER(ipk) :: ntapou
        ! Total length for heat conuction space
        INTEGER(ipk) :: la1max
        ! Maximum # of iterations. Set at 120.
        INTEGER(ipk) :: MaximumIterations
        ! Maximum # of radial nodes arrays are dimen-sioned for in main
        INTEGER(ipk) :: n1
        ! length required by heat-1 arrays
        INTEGER(ipk) :: la1fil
        ! Plot initialization
        INTEGER(ipk) :: ntplot
        ! Not Used
        INTEGER(ipk) :: ntabl1
        ! Not Used
        INTEGER(ipk) :: ntabl2
        ! Not Used
        INTEGER(ipk) :: ntabl3
        ! Length for restart common block
        INTEGER(ipk) :: lexcb
        ! Length for restart common block
        INTEGER(ipk) :: lprntb
        ! Length for restart common block
        INTEGER(ipk) :: lflect
        ! Length for restart common block
        INTEGER(ipk) :: ldialb
        ! Length for restart common block
        INTEGER(ipk) :: lhtcb
        ! Length for restart common block
        INTEGER(ipk) :: lresr1
        ! Not Used
        INTEGER(ipk) :: lresr2
        ! Length for restart common block
        INTEGER(ipk) :: lsclr1
        ! Length for restart common block
        INTEGER(ipk) :: lsclr2
        ! Length for restart common block
        INTEGER(ipk) :: lsclr3
        ! Length for restart common block
        INTEGER(ipk) :: lcolct
        ! Length for restart common block
        INTEGER(ipk) :: lcoold
        ! Total # of words in restart data block
        INTEGER(ipk) :: lcombk
        ! Length for restart common block
        INTEGER(ipk) :: lresi2
        ! Length for restart common block
        INTEGER(ipk) :: lresi3
        ! Total length of afrap array
        INTEGER(ipk) :: lafrap
        ! # of the power step being performed
        INTEGER(ipk) :: npramp
        !
        INTEGER(ipk) :: itswt
        ! Length for restart common block
        INTEGER(ipk) :: lmatpc
        ! multi-heat conduction indicator. 0=not done previously 1=done previously
        INTEGER(ipk) :: nswmd
        ! two times maximum permitted number of radial nodes
        INTEGER(ipk) :: n3
        ! Assigned a value of 21
        INTEGER(ipk) :: ndap1
        ! # of iterations
        INTEGER(ipk) :: IterationCount
        ! Fortran logical unit number for data set with coolant conditions (used only If nqchn = 2)
        INTEGER(ipk) :: ntco
        ! Debug indicator
        INTEGER(ipk) :: kdbug
        ! Length for restart common block ablona. Length = 73
        INTEGER(ipk) :: lblona
        ! Length for restart common block aphypr. Length = 1
        INTEGER(ipk) :: lphypr
        ! Length for restart common block tz1. Length = 226
        INTEGER(ipk) :: lthyd
        ! User specifies the plenum temperature vs time (0-No (Default), 1-Yes)
        INTEGER(ipk) :: prestmp
        ! Maximum permissible time step prescribed in input (sec)
        REAL(r8k) :: dtp
        ! Fuel rod elevation (ft)
        REAL(r8k) :: z
        ! Unmodified mass flux (not given absolute value) (lbm/ft2-hr)
        REAL(r8k) :: gum
        ! Vector storing fixed point variables that define node numbers at boundaries or are switches
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ifstor
        ! Stores array of     INTEGER(ipk), DIMENSION(:), ALLOCATABLEs
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ihData
        ! Global index in acndaz for 1,nconn pairing
        INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: icglob
        ! Used for restart. Length = lcolc2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: acolct
        ! Gas pressure history. This needs to be double checked because the name is used twice for different reasons
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gasphs
        ! Plenum temperature history. gasths(x,1) is for the upper plenum and gasths(x,2) is for the lower plenum
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasths
        ! Grain boundary separation effects
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gbse
        ! Array of pellet parameters
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pelprm
        ! Input gas pressure
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pinput
        ! Gas pressure
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: potput
        ! Plenum temperature
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tpln
        ! Plenum temperature
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tplbt1
    END TYPE collct_var
    !
    INTEGER(ipk) :: ntapou
    INTEGER(ipk) :: la1max
    INTEGER(ipk) :: MaximumIterations
    INTEGER(ipk) :: n1
    INTEGER(ipk) :: la1fil
    INTEGER(ipk) :: ntplot
    INTEGER(ipk) :: ntabl1
    INTEGER(ipk) :: ntabl2
    INTEGER(ipk) :: ntabl3
    INTEGER(ipk) :: lexcb
    INTEGER(ipk) :: lprntb
    INTEGER(ipk) :: lflect
    INTEGER(ipk) :: ldialb
    INTEGER(ipk) :: lhtcb
    INTEGER(ipk) :: lresr1
    INTEGER(ipk) :: lresr2
    INTEGER(ipk) :: lsclr1
    INTEGER(ipk) :: lsclr2
    INTEGER(ipk) :: lsclr3
    INTEGER(ipk) :: lcolct
    INTEGER(ipk) :: lcoold
    INTEGER(ipk) :: lcombk
    INTEGER(ipk) :: lresi2
    INTEGER(ipk) :: lresi3
    INTEGER(ipk) :: lafrap
    INTEGER(ipk) :: npramp
    INTEGER(ipk) :: itswt
    INTEGER(ipk) :: lmatpc
    INTEGER(ipk) :: nswmd
    INTEGER(ipk) :: n3
    INTEGER(ipk) :: ndap1
    INTEGER(ipk) :: IterationCount
    INTEGER(ipk) :: ntco
    INTEGER(ipk) :: kdbug
    INTEGER(ipk) :: lblona
    INTEGER(ipk) :: lphypr
    INTEGER(ipk) :: lthyd
    INTEGER(ipk) :: prestmp
    REAL(r8k) :: dtp
    REAL(r8k) :: z
    REAL(r8k) :: gum
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ifstor
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ihData
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: icglob
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acolct
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: table1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: table2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: table3
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gasphs
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasths
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gbse
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pelprm
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pinput
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: potput
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tpln
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tplbt1
    !
    END MODULE collct_h












