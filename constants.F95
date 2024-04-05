module constants
! Written by S. Chitale for fingerprint analysis purpose
! Feb 26, 2023

    USE ISO_FORTRAN_ENV
 
    integer, parameter :: DP=selected_real_kind(15,307)
    integer, parameter :: QP=selected_real_kind(33, 4931)
    integer, parameter :: HP=kind(1.d0)
 
    real(kind=DP), parameter :: NULL=0.0_DP, HALF=0.5_DP ! , ONE=1.0_DP
    integer, parameter :: twice=2 

    integer, parameter :: zero=0, one=1, two=2, three=3, four=4, five=5,  high=1, low=0
 
 end module constants
