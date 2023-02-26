subroutine filesize(filename,nchars,ierr)

    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(out)         :: nchars
    integer,intent(out)         :: ierr
    character(len=256)          :: message
    integer                     :: lun, ios
    
       nchars=0
       ierr=0
    
       ! open named file in stream mode positioned to append
       open (13,     & ! newunit=lun
       & file=trim(filename), &
      !  & access='stream',     &
      !  & status='old',        &
       & position='append',   &
       & iomsg=message,       &
       iostat=ios)
    
       if(ios.eq.0)then                  ! if file was successfully opened
          ! get file size in bytes and position file to beginning of file
          inquire(13,pos=nchars)   ! get number of bytes in file plus one unit=lun
          nchars=nchars-1                ! opened for append, so subtract one to get current length
       else
          write(*,'("*error*:",a)')message
       endif
    
       ierr=ios

    end subroutine filesize

    subroutine save_pgm(filename,icode,ngray)
        character(len=*), intent(in) :: filename
        character(1), intent(in) :: icode(:,:)
        integer, intent(in) :: ngray
           ! number of gray values
  
        integer :: i, j, unit
  
        open(newunit=unit,file=filename,status='unknown')
  
        ! a description of the format is available at
        !   https://en.wikipedia.org/wiki/Netpbm
  
        ! header part
        write(unit,'(A)') 'P2'
        write(unit,'(I0,1X,I0)') size(icode,1), size(icode,2)
        write(unit,'(I0)') ngray
  
        ! pixel data, gray values
        do j = 1, size(icode,2)
           write(unit,'(*(A1,:,1X))') (icode(i,j), i = 1, size(icode,1))
        end do
  
        close(unit)
  
     end subroutine

    ! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat1) then
!       call store_write(i,x,y,cat1)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat2) then
!         call store_write(i,x,y,cat2)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat3) then
!         call store_write(i,x,y,cat3)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat4) then
!         call store_write(i,x,y,cat4)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat5) then
!         call store_write(i,x,y,cat5)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat6) then
!         call store_write(i,x,y,cat6)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat7) then
!         call store_write(i,x,y,cat7)

! else if(img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) &
!     + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) + img_in(a1(1),a1(2)) == cat8) then
!         call store_write(i,x,y,cat8)