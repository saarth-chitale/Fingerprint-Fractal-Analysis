module store
! Written by S. Chitale for fingerprint analysis purpose
! Feb 26, 2023

    use constants
 
    integer, parameter :: cat1=1, cat2=2, cat3=3, cat4=4, cat5=5, cat6=6, cat7=7, cat8=8, cat9=9
    integer :: nmax

    type store_node
      integer :: x   ! x co-ord of pixel
      integer :: y   ! y co-ord of pixel
      integer :: cat ! category number
    end type store_node

    type(store_node), allocatable, dimension(:) :: storage
 
 
   contains

   subroutine store_init(nmax)
      integer, intent(in) :: nmax
      allocate(storage(nmax))
   end subroutine

   subroutine store_write(n,x,y,cat)

      integer, intent(in) :: n,x,y,cat

      storage(n)%x = x
      storage(n)%y = y
      storage(n)%cat = cat

   end subroutine

   subroutine store_read_cat(n,cat)

      integer, intent(in) :: n
      integer, intent(out) :: cat

      cat = storage(n)%cat

   end subroutine

   subroutine store_read_all(n,x,y,cat)

      integer, intent(in) :: n
      integer, intent(out) :: x, y, cat

      x = storage(n)%x
      y = storage(n)%y
      cat = storage(n)%cat

   end subroutine
 
 
   subroutine store_close()
      deallocate(storage)
   end subroutine store_close
 
 
 end module store
 
