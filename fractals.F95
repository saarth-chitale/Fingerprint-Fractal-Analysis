module fractals

    use constants
    use store

    implicit none

    integer :: numcat
    logical :: found

    contains

    subroutine randomwalk(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale
        real(kind=DP) :: r1
        integer :: x, y, numanchors

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 8
        numcat = numanchors + one
        subscale = scale/two
        
        allocate(a(numanchors))

        call store_init(nmax)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                call random_number(r1)
                x = floor(dim*r1)
                call random_number(r1)
                y = floor(dim*r1)

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y
                a(2)%x = x
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y
                a(4)%x = x
                a(4)%y = y-scale
                a(5)%x = x+subscale
                a(5)%y = y+subscale
                a(6)%x = x-subscale
                a(6)%y = y+subscale
                a(7)%x = x-subscale
                a(7)%y = y-subscale
                a(8)%x = x+subscale
                a(8)%y = y-subscale

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                else
                    valid = .true.
                end if
            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) &
                + img_in(a(5)%x,a(5)%y) + img_in(a(6)%x,a(6)%y) + img_in(a(7)%x,a(7)%y) + img_in(a(8)%x,a(8)%y) == (j-1)) then
                      
                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                end if

            end do

        end do

    end subroutine

    subroutine randomwalk_1(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale
        real(kind=DP) :: r1
        integer :: x, y, numanchors

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 4
        numcat = numanchors + one
        subscale = scale/two
        
        allocate(a(numanchors))

        call store_init(nmax)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                call random_number(r1)
                x = floor(dim*r1)
                call random_number(r1)
                y = floor(dim*r1)

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y+scale
                a(2)%x = x-scale
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y-scale
                a(4)%x = x+scale
                a(4)%y = y-scale
        

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                else
                    valid = .true.
                end if

            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) == (j-1)) then         
                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                end if

            end do

        end do

    end subroutine

    subroutine chaos(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale, count_invalid
        real(kind=DP) :: r1, r2
        integer :: x, y, numanchors, x1, y1, d

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 8
        numcat = numanchors + one
        subscale = scale/two
        count_invalid = 0
        
        allocate(a(numanchors))

        call store_init(nmax)

        call random_number(r1)
        x1 = floor(dim*r1)
        call random_number(r1)
        y1 = floor(dim*r1)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                ! Choose direction
1               call random_number(r2)
                d = ceiling(4*r2)

                if(d == one) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP)) !-real(1,kind=DP)
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                else if(d == two) then
                    x = X1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                    ! write(16,'(2i5)') x, y
                else if(d == three) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else if(d == four) then
                    x = x1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else
                    goto 1
                end if
                ! write(16,'(A30)') 'Chaos subrountine direction'
                ! write(16,'(A10,i5)') 'Direction: ', d
               

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y
                a(2)%x = x
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y
                a(4)%x = x
                a(4)%y = y-scale
                a(5)%x = x+subscale
                a(5)%y = y+subscale
                a(6)%x = x-subscale
                a(6)%y = y+subscale
                a(7)%x = x-subscale
                a(7)%y = y-subscale
                a(8)%x = x+subscale
                a(8)%y = y-subscale

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                    ! write(16,'(A30)') 'Invalid counter in chaos subrountine'
                    ! write(16,'(A10,i5)') 'Invalid: ', count_invalid
                else
                    valid = .true.
                end if
            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) &
                    + img_in(a(5)%x,a(5)%y) + img_in(a(6)%x,a(6)%y) + img_in(a(7)%x,a(7)%y) + img_in(a(8)%x,a(8)%y) == (j-1)) then

                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                 end if

            end do

            ! Change starting point
            x1 = x
            y1 = y

        end do

    end subroutine

    subroutine chaos_1(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale, count_invalid
        real(kind=DP) :: r1, r2
        integer :: x, y, numanchors, x1, y1, d

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 4
        numcat = numanchors + one
        subscale = scale/two
        count_invalid = 0
        
        allocate(a(numanchors))

        call store_init(nmax)

        call random_number(r1)
        x1 = floor(dim*r1)
        call random_number(r1)
        y1 = floor(dim*r1)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                ! Choose direction
1               call random_number(r2)
                d = ceiling(4*r2)

                if(d == one) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP)) !-real(1,kind=DP)
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                else if(d == two) then
                    x = X1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                    ! write(16,'(2i5)') x, y
                else if(d == three) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else if(d == four) then
                    x = x1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else
                    goto 1
                end if
                ! write(16,'(A30)') 'Chaos subrountine direction'
                ! write(16,'(A10,i5)') 'Direction: ', d
               

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y+scale
                a(2)%x = x-scale
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y-scale
                a(4)%x = x+scale
                a(4)%y = y-scale
          

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                    ! write(16,'(A30)') 'Invalid counter in chaos subrountine'
                    ! write(16,'(A10,i5)') 'Invalid: ', count_invalid
                else
                    valid = .true.
                end if
            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) == (j-1)) then

                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                end if

            end do

            ! Change starting point
            x1 = x
            y1 = y

        end do

    end subroutine

    subroutine chaos_2(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale, count_invalid
        real(kind=DP) :: r1, r2
        integer :: x, y, numanchors, x1, y1, d, d1

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 4
        numcat = numanchors + one
        subscale = scale/two
        count_invalid = 0
        
        allocate(a(numanchors))

        call store_init(nmax)

        call random_number(r1)
        x1 = floor(dim*r1)
        call random_number(r1)
        y1 = floor(dim*r1)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                ! Choose direction
1               call random_number(r2)
                d = ceiling(4*r2)

                if(d == d1) then
                    goto 1
                end if

                if(d == one) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP)) !-real(1,kind=DP)
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                else if(d == two) then
                    x = X1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = floor((real(y1,kind=DP))/real(2,kind=DP))
                    ! write(16,'(2i5)') x, y
                else if(d == three) then
                    x = floor((real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else if(d == four) then
                    x = x1 + floor((real(dim,kind=DP)-real(x1,kind=DP))/real(2,kind=DP))
                    y = y1 + floor((real(dim,kind=DP)-real(y1,kind=DP))/real(2,kind=DP))
                else
                    goto 1
                end if
                ! write(16,'(A30)') 'Chaos subrountine direction'
                ! write(16,'(A10,i5)') 'Direction: ', d
               

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y
                a(2)%x = x
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y
                a(4)%x = x
                a(4)%y = y-scale
                a(5)%x = x+subscale
                a(5)%y = y+subscale
                a(6)%x = x-subscale
                a(6)%y = y+subscale
                a(7)%x = x-subscale
                a(7)%y = y-subscale
                a(8)%x = x+subscale
                a(8)%y = y-subscale
          

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                    ! write(16,'(A30)') 'Invalid counter in chaos subrountine'
                    ! write(16,'(A10,i5)') 'Invalid: ', count_invalid
                else
                    valid = .true.
                end if
            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) &
                + img_in(a(5)%x,a(5)%y) + img_in(a(6)%x,a(6)%y) + img_in(a(7)%x,a(7)%y) + img_in(a(8)%x,a(8)%y) == (j-1)) then

                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                end if

            end do

            ! Change starting point
            x1 = x
            y1 = y
            d1 = d

        end do

    end subroutine

    subroutine chaos_3(dim,img_in,nmax,scale)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(in) :: img_in
        integer, intent(in) :: scale

        integer :: i, j, subscale, count_invalid
        real(kind=DP) :: r1, r2
        integer :: x, y, numanchors, x1, y1, d, d1

        logical :: valid

        type anchors
            integer :: x
            integer :: y
        end type anchors

        type(anchors), allocatable, dimension(:) :: a

        ! Initialize
        numanchors = 4
        numcat = numanchors + one
        subscale = scale/two
        count_invalid = 0
        
        allocate(a(numanchors))

        call store_init(nmax)

        call random_number(r1)
        x1 = floor(dim*r1)
        call random_number(r1)
        y1 = floor(dim*r1)

        do i=1,nmax
            ! Initialize
            valid = .false.
            found = .false.
            ! Choose random pixel
            do while (valid .neqv. .true.)
                ! Choose direction
1               call random_number(r2)
                d = ceiling(4*r2)

                if(d == one) then
                    x = floor(real(2,kind=DP)*(real(x1,kind=DP))/real(3,kind=DP)) 
                    y = floor(real(2,kind=DP)*(real(y1,kind=DP))/real(3,kind=DP))
                else if(d == two) then
                    x = X1 + floor(real(2,kind=DP)*(real(dim,kind=DP)-real(x1,kind=DP))/real(3,kind=DP))
                    y = floor(real(2,kind=DP)*(real(y1,kind=DP))/real(3,kind=DP))
                    ! write(16,'(2i5)') x, y
                else if(d == three) then
                    x = floor(real(2,kind=DP)*(real(x1,kind=DP))/real(3,kind=DP))
                    y = y1 + floor(real(2,kind=DP)*(real(dim,kind=DP)-real(y1,kind=DP))/real(3,kind=DP))
                else if(d == four) then
                    x = x1 + floor(real(2,kind=DP)*(real(dim,kind=DP)-real(x1,kind=DP))/real(3,kind=DP))
                    y = y1 + floor(real(2,kind=DP)*(real(dim,kind=DP)-real(y1,kind=DP))/real(3,kind=DP))
                else
                    goto 1
                end if
                ! write(16,'(A30)') 'Chaos subrountine direction'
                ! write(16,'(A10,i5)') 'Direction: ', d
               

                ! define anchors
                a(1)%x = x+scale
                a(1)%y = y
                a(2)%x = x
                a(2)%y = y+scale
                a(3)%x = x-scale
                a(3)%y = y
                a(4)%x = x
                a(4)%y = y-scale
                a(5)%x = x+subscale
                a(5)%y = y+subscale
                a(6)%x = x-subscale
                a(6)%y = y+subscale
                a(7)%x = x-subscale
                a(7)%y = y-subscale
                a(8)%x = x+subscale
                a(8)%y = y-subscale
          

                if(a(1)%x > dim .or. a(2)%y > dim .or. a(3)%x < one .or. a(4)%y < one) then
                    valid = .false.
                    ! write(16,'(A30)') 'Invalid counter in chaos subrountine'
                    ! write(16,'(A10,i5)') 'Invalid: ', count_invalid
                else
                    valid = .true.
                end if
            end do
               
            ! Categorize pixels with random walk
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
    
                if(img_in(a(1)%x,a(1)%y) + img_in(a(2)%x,a(2)%y) + img_in(a(3)%x,a(3)%y) + img_in(a(4)%x,a(4)%y) &
                + img_in(a(5)%x,a(5)%y) + img_in(a(6)%x,a(6)%y) + img_in(a(7)%x,a(7)%y) + img_in(a(8)%x,a(8)%y) == (j-1)) then

                    call store_write(i,x,y,(j-1))
                    found = .true.
                else
                        j = j + one
                end if

            end do

            ! Change starting point
            x1 = x
            y1 = y

        end do

    end subroutine
    

    subroutine count_frac(nmax,cat_count)

        integer, intent(in) :: nmax

        integer :: cat
        integer, dimension(:), allocatable, intent(out) :: cat_count
        integer :: i, j

        ! allocate with number of categories
        numcat = 9
        allocate(cat_count(numcat))

        ! Initialize
        cat_count = 0

        do i=1,nmax
            found = .false.
            call store_read_cat(i,cat)
            ! do j=1,numcat
            j = zero
            do while (found .neqv. .true.)
                if(cat == (j-1)) then
                    cat_count(j) = cat_count(j) + 1
                    found = .true.
                else
                    j = j + one
                end if
            end do
        end do
            
    end subroutine

    subroutine plot_fractals(dim,nmax,img_out)

        integer, intent(in) :: dim, nmax
        integer, dimension(dim,dim), intent(out) :: img_out

        integer :: i, x, y, cat

        img_out = 0

        do i=1,nmax

            call store_read_all(i,x,y,cat)

            ! img_out(x,y) = 1   
            ! if(cat == 5) then
            !     img_out(x,y) = img_out(x,y) + 1
            ! end if

            img_out(x,y) = img_out(x,y) + 1

        end do


    end subroutine


end module fractals