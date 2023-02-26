program main

   use :: iso_fortran_env !, only : FILE_STORAGE_SIZE
   use constants
   use binarize
   use preprocess
   use fractals
   use store

   implicit none

   integer :: i, n, dim, io

   real :: cputime0, cputime1

   character(len=5) :: EXECODE='FFA'
   character(len=6) :: VERSION='v0.1'
   character(len=20) :: hostname
   character(len=40) :: filename, output_file, frac_file

   logical :: yes

   integer :: iargc, buffer, red_dim_1, red_dim_2, side
   character(len=40) :: arg

   integer, dimension(:,:), allocatable :: img, img_bw, img_red_1, img_red_2, fract_plot
   integer :: numscales
   integer, dimension(:), allocatable :: scale, fractal_count
   real(kind=DP), dimension(:), allocatable :: fractal_count_norm
   integer :: ppos


#ifdef DEBUG
   open(11,file='debug.dat')
#endif

   open(16,file='test.dat')

   n = iargc()
   if(n<1) then
      write(0,*) '**ERROR**  No image file specified.'
      stop

   else if(n>0) then
      i=1
      do while (i<n+1)
         call getarg(i,arg)
         select case (trim(arg))
         case('-v', '--version')
            write(6,*) EXECODE, ':  ', VERSION
            stop ''
         case('-h' , '--help')
            write(6,*) ' Usage: FFA [options] image_file'
            write(6,*) ' image_file: fingerprint image 256*256 grayscale'
            write(6,*) ' options:'
            write(6,*) '   [-h | --help]      : show this help'
            write(6,*) '   [-v | --version]   : show the version number'
            stop
         case default
            call getarg(i,arg)
            filename = trim(arg)
            inquire(file=trim(filename),exist=yes)
            if(.not.yes) then
               write(0,*) '**ERROR**  ',trim(filename), ' not found.'
               stop
            end if
         end select
         i=i+1
      end do
   end if


   call get_environment_variable('HOSTNAME',hostname)

   open(9,file='FFA.dat')
   write(9,'(a,a,a,a,a)')  '*** Fingerprint Fractal Analysis (',EXECODE,',',VERSION,') ***'
   write(9,'(/a,a)') 'Begin at ', time_stamp()
   write(9,'(/a,a)') 'Host: ',trim(hostname)

   call cpu_time(cputime0)

   ! generates a random number seed  based on the day and time.
   call get_seed_daytime()

   ! Open image file and read data

   ! Initialize
   dim = 0

   open (1,file=trim(filename)) ! ,access='sequential',form='unformatted'
   do
     read (1,*,end=1)
     dim = dim + 1
   end do

1  write(9,'(A6,i5)') 'Rows: ', dim

   rewind(1)

   open (1,file=trim(filename))
   allocate(img(dim,dim))
   allocate(img_bw(dim,dim))

   ! Initialize
   img = 0

   do i=1,dim
      read(1,*,iostat=io) img(i,:)
   end do

   write(9,'(/a,a,a,a,a)') 'Corner elements of data are: '
   write(9,'(4i5)') img(1,1), img(1,dim), img(dim,1), img(dim,dim)


   if(io < 0) then
   	print *, 'EOF'
   else if(io > 0) then
	   print *, 'Error handling file'
   end if

   ! Call binarize image subroutine
   call otsu(dim,img,img_bw)

   write(9,'(/a,a,a,a,a)') 'Dimensions binarized image are: '
   write(9,'(2i5)') dim, dim

   ! Write binarized image to output file
   open(14,file='binarized.dat')
   write(14,'(i5)') img_bw

   write(9,'(/a,a,a,a,a)') 'Corner elements of binarized image are: '
   write(9,'(4i5)') img_bw(1,1), img_bw(1,dim), img_bw(dim,1), img_bw(dim,dim)

   ! Determine workable image grid
   ! Call first reduction protocol
   buffer = 20  ! minimum number of black pixels in row for contructing grid
   call grid_red_1(dim,img_bw,buffer,img_red_1,red_dim_1)

   ! Call second reduction protocol
   side = 2
   call grid_red_2(red_dim_1,img_red_1,side,img_red_2,red_dim_2)

   ! Fractal Generation output
   ppos = scan(trim(filename),".", BACK= .true.)
   if ( ppos > 0 ) output_file = filename(1:ppos)// 'dat'
   open(15,file=trim(output_file))

   nmax = 1000000
   numscales = 2
   allocate(scale(numscales))
   allocate(fract_plot(red_dim_2,red_dim_2))

   scale(1) = 2!4
   scale(2) = 10!14

   numcat = 9
   allocate(fractal_count(numcat))
   allocate(fractal_count_norm(numcat))

   do i=1,numscales
      ! Call fractal generator
      call chaos_2(red_dim_2,img_red_2,nmax,scale(i))
      ! Count fractals
      call count_frac(nmax,fractal_count)
      write(15,'(A25)') 'Number of fractals is: '
      write(15,'(i8)') fractal_count

      fractal_count_norm = real(fractal_count,kind=DP)/real(nmax,kind=DP)
      write(15,'(A25)') 'Normalized fractals are: '
      write(15,'(f10.6)') fractal_count_norm

      ! Fractal plot
   
      call plot_fractals(red_dim_2,nmax,fract_plot)

      ppos = scan(trim(filename),".", BACK= .true.)
      if ( ppos > 0 ) frac_file = 'scale'//trim(str(scale(i)))//'-'//trim(filename(1:ppos))// 'dat'
      open(17,file=trim(frac_file))

      write(17,'(i5)') fract_plot

      close(17)

      call store_close()

   end do


   call cpu_time(cputime1)
   write(9,'(/a,f10.5)' ) 'cputime=',cputime1-cputime0
   write(9,'(/a,a)') 'Completed at ', time_stamp()
   close(9)

#ifdef DEBUG
   close(11)
#endif

   close(1)
   close(9)
   close(15)
   close(16)


   contains

      character(len=20) function str(k)
      !   Function converts an integer to string
         integer, intent(in) :: k
         write (str, *) k
         str = adjustl(str)
      end function str

      function time_stamp()
         !---------------------------------------
         ! this returns current time in a string
         !---------------------------------------
         implicit none

         character(18) :: time_stamp
         character(8)  :: date
         character(10) :: time

         call date_and_time(date,time)

         time_stamp = date(5:6) // '/' // date(7:8) // '/' //  date(1:4) &
                              // '  ' // time(1:2) // ':' // time(3:4)

      end function time_stamp

      subroutine get_seed_daytime()

         integer :: nmax
         integer, dimension(8) :: val
         integer, dimension(64) :: f
         integer, dimension(:), allocatable :: seed

         call date_and_time(VALUES=val)

         call random_seed(SIZE=nmax)
         if(nmax>64) then
            write(9,'(/a)') '** Error [get_seed_daytime] Seed size is too large:', nmax
            stop
         end if
         allocate(seed(nmax))

         ! generate 64 integers based on current date and time
         call date_and_time(VALUES=val)
         f = (val(8)+val(7)*1000+val(6)*60000)

         call random_seed(GET=seed(1:nmax))

         seed(1:nmax) = seed(1:nmax)*f(1:nmax)

         call random_seed(PUT=seed(1:nmax))

         open(31,file='random_seed.dat')
         write(31, '(i12)') seed
         close(31)

         deallocate(seed)

         write(9,'(/a)') '** Random seed generated'

   end subroutine get_seed_daytime


end program
