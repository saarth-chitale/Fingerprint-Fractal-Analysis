module preprocess
! Written by S. Chitale for fingerprint analysis purpose
! Feb 26, 2023

    use constants

    implicit none

    integer :: shift, xgrid, ygrid, minbuffer, count, mincount, gridlim
    integer, dimension(4) :: lim 
    integer, parameter :: left=1, right=2, top=3, bottom=4
    integer, dimension(:,:), allocatable :: bl, br, tl, tr 

    logical :: valid

    contains
    
    subroutine grid_red_1(dim,img_in,buffer,img_red_1,red_dim_1)

        integer, intent(in) :: dim
        integer, dimension(dim,dim), intent(in) :: img_in  
        integer :: buffer
        integer, dimension(:,:), allocatable, intent(out) :: img_red_1
        integer, intent(out) :: red_dim_1


        open(12,file='img_grid.dat')
        write(12,'(A40)') 'Running first grid reduction...'

        ! Initialize
        valid = .false.
        shift = 1
        minbuffer = 10
        count = 0
        gridlim = dim/2
        mincount = 30


        ! Choose area of image to work on
        ! Automate finding a nice area
        ! Check for black pixel in grid starting from outer boundary of image

        ! Starting points

        lim(left) = 1
        lim(right) = dim
        lim(top) = dim
        lim(bottom) = 1


    outer: do while(valid .neqv. .true.)

            if(sum(img_in(lim(left),:)) .le. buffer .or. sum(img_in(lim(right),:)) .le. buffer .or. &  
                sum(img_in(:,lim(top))) .le. buffer .or. sum(img_in(:,lim(bottom))) .le. buffer) then
                lim(left) = lim(left) + shift
                lim(right) = lim(right) - shift
                lim(top) = lim(top) - shift
                lim(bottom) = lim(bottom) + shift
                count = count + one
                ! write(12,'(5i5)') lim(left), lim(right), lim(top), lim(bottom), buffer
            else
                if(buffer > minbuffer .and. count .ge. mincount ) then  
                    buffer = buffer - one
                    lim(left) = lim(left) - shift
                    lim(right) = lim(right) + shift
                    lim(top) = lim(top) + shift
                    lim(bottom) = lim(bottom) - shift
                else
                    valid = .true.
                    write(12,'(A40,4i5)') 'Shifted Grid boundaries are (L,R,B,T): ', lim(left), lim(right), lim(bottom), lim(top)
                end if
            end if

            xgrid = lim(right) - lim(left)
            ygrid = lim(top) - lim(bottom)

            if (xgrid .le. gridlim .or. ygrid .le. gridlim) then
                write(12,'(A30)') 'Error finding workable area'
                exit outer
            end if

        end do outer

        ! Subrountine Output

        allocate(img_red_1(xgrid,ygrid))
        red_dim_1 = xgrid
        img_red_1 = img_in(lim(left):lim(right),lim(bottom):lim(top))

        write(12,'(A25,i5)') 'Grid reduce 1 shifts: ', count

    end subroutine

    subroutine grid_red_2(dim,img_in,side,img_red_2,red_dim_2)
        
        integer, intent(in) :: dim
        integer, dimension(dim,dim), intent(in) :: img_in  
        integer, dimension(:,:), allocatable, intent(out) :: img_red_2
        integer, intent(out) :: red_dim_2
        integer, intent(in) :: side

        integer :: i, j

        ! Initialize
        gridlim = dim/4
        valid = .false.

        allocate(bl(side,side))
        allocate(br(side,side))
        allocate(tl(side,side))
        allocate(tr(side,side))

        br = 0
        bl = 0
        tl = 0
        tr = 0

         ! Starting points

        lim(left) = 1
        lim(right) = dim
        lim(top) = dim
        lim(bottom) = 1

        write(12,'(A40)') 'Running second grid reduction...'

        outer: do while(valid .neqv. .true.)

            do i=0,side-1
                do j=0,side-1
                    bl(i+1,j+1) = img_in(lim(left)+i,lim(bottom)+j)
                    br(i+1,j+1) = img_in(lim(right)-i,lim(bottom)+j)
                    tl(i+1,j+1) = img_in(lim(left)+i,lim(top)-j)
                    tr(i+1,j+1) = img_in(lim(right)-i,lim(top)-j)
                end do
            end do

            ! write(12,'(A20,4i5)') 'Sums of corners: ', sum(bl), sum(br), sum(tl), sum(tr)

            if(sum(bl) == zero .or. sum(br) == zero .or. sum(tl) == zero .or. sum(tr) == zero) then
                lim(left) = lim(left) + side
                lim(right) = lim(right) - side
                lim(top) = lim(top) - side
                lim(bottom) = lim(bottom) + side
                count = count + one
            else
                valid = .true.
                write(12,'(A40,4i5)') 'Shifted Grid boundaries are (L,R,B,T): ', lim(left), lim(right), lim(bottom), lim(top)
            end if

            xgrid = lim(right) - lim(left)
            ygrid = lim(top) - lim(bottom)

            if (xgrid .le. gridlim .or. ygrid .le. gridlim) then
                write(12,'(A30)') 'Cannot dimish area further'
                exit outer
            end if

        end do outer

        ! Subrountine Output

        allocate(img_red_2(xgrid,ygrid))
        red_dim_2 = xgrid
        img_red_2 = img_in(lim(left):lim(right),lim(bottom):lim(top))

        write(12,'(A25,i5)') 'Grid reduce 2 shifts: ', count


    end subroutine  

end module preprocess
