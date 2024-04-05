module binarize
! Written by S. Chitale for fingerprint analysis purpose
! Feb 26, 2023

    use constants

    contains
    
    subroutine otsu(dim,img_in,img_out)

    integer, intent(in) :: dim
    integer, dimension(dim,dim), intent(in) :: img_in
    integer, dimension(dim,dim), intent(out) :: img_out
    real(kind=DP), dimension(dim,dim) :: img_norm

    integer :: i, j, k
    integer :: graylim, nbins, win
    real(kind=DP) :: min, max
    integer, dimension(:), allocatable :: count
    real(kind=DP), dimension(:), allocatable :: count_p
    real(kind=DP) :: count_sum
    integer :: temp, temp1, temp2
    logical :: found

    real(kind=DP) :: sigma_max, t1
    real(kind=DP) :: w0, w1, u0, u1, sigma
    real(kind=DP), dimension(:), allocatable :: a, b
    integer :: t2

    open(13,file='hist.dat')

    ! Initialize
    graylim = 256
    win = 1
    img_out = 0
    temp = 0
    t1 = 0.0_DP
    t2 = 0
    w0 = 0.0_DP
    w1 = 0.0_DP
    u0 = 0.0_DP
    u1 = 0.0_DP
    sigma_max = 0.0_DP
    sigma = 0.0_DP
    

    nbins = graylim/win
    
    allocate(count(nbins))
    allocate(count_p(nbins))
    count = 0
    count_p = 0.0_DP
    count_sum = 0.0_DP

    ! Calculate min and max of img_in
    min = minval(img_in)
    max = maxval(img_in)

    ! Determine optimized starting point
    temp = floor(min/win - 1) 
    temp1 = temp
    temp2 = floor(max/win - 1) + 1

    write(13,'(A16,i5)') 'Starting point: ', temp
    write(13,'(A16,i5)') 'End point: ', temp2

    ! Histogram of input image
    do i=1,dim 
      do j=1,dim
          found = .false.
          do while (found .neqv. .true.)
             if (img_in(i,j) .ge. temp*win .and. img_in(i,j) < temp*win + win) then
                count(temp) = count(temp) + 1
                found = .true.  
                temp = temp1   
             else      
                temp = temp + 1    
             end if
          end do
       end do
    end do

    count_sum = sum(count)
    count_p = count/count_sum

    write(13,'(A11,f10.2)') 'Count sum: ', count_sum

    do k=1,nbins
       write(13,'(2i10,f10.2)') k, count(k), count_p(k)
    end do

    do i=2,graylim-1
       w0 = sum(count_p(1:i)) ! probability of class 1 (separated by threshold)
       w1 = sum(count_p(i+1:256)) ! probability of class 2 (separated by threshold)

       allocate(a(0:i-1))
       allocate(b(i:graylim))
       do j=0,i-1
          a(j) = j
       end do
       do k=i,graylim-1
          b(k) = k
       end do

       if(w0 > 0.0_DP) then ! Check for division by 0.0
          u0 = (dot_product(a,count_p(1:i)))/w0 ! class 1 mean
       else if(w1 > 0.0_DP) then
          u1 = (dot_product(b,count_p(i+1:graylim)))/w1 ! class 2 mean
       end if

       sigma = w0 * w1 * ((u1-u0) * (u1-u0)) ! compute variance between classes

       if(sigma > sigma_max) then
          sigma_max = sigma
          t2 = i-1
       end if 

       deallocate(a,b)

    end do

    t1 = (real(t2,kind=DP)/real((graylim-1),kind=DP))

    write(13,'(A26,i5)') 'Threshold intensity is: ', t2 
    write(13,'(A14,f10.2)') 'Threshold is: ', t1 

    ! normalize img_in wrt graylim
    img_norm = real(img_in,kind=DP)/real(graylim,kind=DP)

    do i=1,dim
       do j=1,dim
          if(img_norm(i,j) < t1) then
             img_norm(i,j) = 1.0_DP  ! 0.0 on Feb 20, 2023
          else
             img_norm(i,j) = 0.0_DP  ! 1.0 on Feb 20, 2023
          end if
       end do
    end do

    img_out = int(img_norm)

    close(13)

 end subroutine

end module binarize
