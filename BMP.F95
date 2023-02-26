Program BMP

    	implicit none
    
    	character*4000 str
    	character*24 skra,skraL,skraR
    	integer*4 lskra,lskraL,lskraR
    
    	integer*4 l,k,i,n,j,m,k1,k2,k3,ik1,ik2,ik3,mm
    !	integer*2 r(256),g(256),b(256),s(256)
    	byte r(256),g(256),b(256),s(256)
    	integer*2 ir,ig,ib,is
    	integer*4 leng,lengU
    	real*8 T
    
    	byte string(4000)
    
    	byte C,D,E
    	integer*2 iC
    	byte X(1000,500)
    	integer*4 ix,iz
    	integer*4 sizeA,sizeB,sizeZ
    
    	structure /BitmapFileheader/
    	   integer*2 bfType
    	   integer*4 bfSize
    	   integer*2 bfReserved1
    	   integer*2 bfReserved2
    	   integer*4 bfOffsetBits
    	end structure
    	record /BitmapFileheader/ bmfH
    
    	structure /BitmapInfoheader/
    	   integer*4 biSize
    	   integer*4 biWidth
    	   integer*4 biHeight
    	   integer*2 biPlanes
    	   integer*2 biBitCount
    	   integer*4 biCompression
    	   integer*4 biSizeImage
    	   integer*4 biXPixelPerMeter
    	   integer*4 biYPixelPerMeter
    	   integer*4 biClrUsed
    	   integer*4 biClrImportant
    	end structure
    	record /BitmapInfoheader/ bmiH
    
    	structure /RGBQuad/
    	   byte rgbBlue
    	   byte rgbGreen
    	   byte rgbRed
    	   byte rgbReserved
    	end structure
    c	record /RGBQuad/ bmiC(1)
    	record /RGBQuad/ bmiC(256)
    
    c	structure /BitmapInfo/
    c	   record /BitmapInfoheader/ bmiH
    c	   record /RGBQuad/ bmiC(1)
    c	end structure
    c	record /BitmapInfo/ bmpI
    
    	write(*,'(1x,a,$)') 'BMP-file: '
    	read(*,'(a)') skra
    	k = index(skra,'.')
    	if(k.eq.0) then
    	   str = skra
    	   l = len_trim(str)
    	   skra = str(1:l)//'.BMP'
    	   skraL = str(1:l)//'.LGO'
    	   skraR = str(1:l)//'.LGR'
    	else
    	   skraL = skra(1:(k-1))//'.LGO'
    	   skraR = skra(1:(k-1))//'.LGR'
    	endif
    	lskra = len_trim(skra)
    	lskraL = len_trim(skraL)
    	lskraR = len_trim(skraR)
    
    	open(unit=1,file=skra(1:lskra),form='binary',status='old')
    
    	read(1) bmfH
    	read(1) bmiH
    
    	if(bmfH.bfType.ne.'BM') then			! 'BM' = 19778
    	   write(*,'(1x,a)') 'Not a BMP-file'
    	   goto 99
    	endif
    
    !	skraL = '222.txt'
    !	lskraL = len_trim(skraL)
    
    	open(unit=2,file='111.txt',status='unknown')
    	open(unit=3,file=skraL(1:lskraL),status='unknown')
    	open(unit=4,file='333.txt',status='unknown')
    	open(unit=5,file='444.txt',status='unknown')
    	open(unit=6,file=skraR(1:lskraR),status='unknown')
    
    	write(*,'(1x)')
    	write(2,'(1x,a,a)') 'File: ',skra
    
    	write(*,'(1x,a,i10)') 'Type:           ',bmfH.bfType
    	write(*,'(1x,a,i10)') 'Size:           ',bmfH.bfSize
    	write(*,'(1x,a,i10)') 'Reserved1:      ',bmfH.bfReserved1
    	write(*,'(1x,a,i10)') 'Reserved2:      ',bmfH.bfReserved2
    	write(*,'(1x,a,i10)') 'OffsetBits:     ',bmfH.bfOffsetBits
    	write(*,'(1x)')
    
    	write(*,'(1x,a,i10)') 'Size:           ',bmiH.biSize
    	write(*,'(1x,a,i10)') 'Width:          ',bmiH.biWidth
    	write(*,'(1x,a,i10)') 'Height:         ',bmiH.biHeight
    	write(*,'(1x,a,i10)') 'Planes:         ',bmiH.biPlanes
    	write(*,'(1x,a,i10)') 'BitCount:       ',bmiH.biBitCount
    	write(*,'(1x,a,i10)') 'Compression:    ',bmiH.biCompression
    	write(*,'(1x,a,i10)') 'SizeImage:      ',bmiH.biSizeImage
    	write(*,'(1x,a,i10)') 'XPixelPerMeter: ',bmiH.biXPixelPerMeter
    	write(*,'(1x,a,i10)') 'YPixelPerMeter: ',bmiH.biYPixelPerMeter
    	write(*,'(1x,a,i10)') 'ClrUsed:        ',bmiH.biClrUsed
    	write(*,'(1x,a,i10)') 'ClrImportant:   ',bmiH.biClrImportant
    	write(*,'(1x)')
    
    	write(2,'(1x)')
    
    	write(2,'(1x,a,i10)') 'Type:           ',bmfH.bfType
    	write(2,'(1x,a,i10)') 'Size:           ',bmfH.bfSize
    	write(2,'(1x,a,i10)') 'Reserved1:      ',bmfH.bfReserved1
    	write(2,'(1x,a,i10)') 'Reserved2:      ',bmfH.bfReserved2
    	write(2,'(1x,a,i10)') 'OffsetBits:     ',bmfH.bfOffsetBits
    	write(2,'(1x)')
    
    	write(2,'(1x,a,i10)') 'Size:           ',bmiH.biSize
    	write(2,'(1x,a,i10)') 'Width:          ',bmiH.biWidth
    	write(2,'(1x,a,i10)') 'Height:         ',bmiH.biHeight
    	write(2,'(1x,a,i10)') 'Planes:         ',bmiH.biPlanes
    	write(2,'(1x,a,i10)') 'BitCount:       ',bmiH.biBitCount
    	write(2,'(1x,a,i10)') 'Compression:    ',bmiH.biCompression
    	write(2,'(1x,a,i10)') 'SizeImage:      ',bmiH.biSizeImage
    	write(2,'(1x,a,i10)') 'XPixelPerMeter: ',bmiH.biXPixelPerMeter
    	write(2,'(1x,a,i10)') 'YPixelPerMeter: ',bmiH.biYPixelPerMeter
    	write(2,'(1x,a,i10)') 'ClrUsed:        ',bmiH.biClrUsed
    	write(2,'(1x,a,i10)') 'ClrImportant:   ',bmiH.biClrImportant
    	write(2,'(1x)')
    
    	if(bmiH.biBitCount.eq.24) goto 50	! 24
    
    	n = 2**bmiH.biBitCount			! 1, 4 ,8, 24
    
    	write(*,'(1x,a,i10)') 'n:              ',n
    	write(*,'(1x)')
    
    	write(2,'(1x,a,i10)') 'n:              ',n
    	write(2,'(1x)')
    
    !!	pause
    
    	do i=1,n
    	   read(1) bmiC(i)
    	   b(i) = bmiC(i).rgbBlue
    	   g(i) = bmiC(i).rgbGreen
    	   r(i) = bmiC(i).rgbRed
    	   s(i) = bmiC(i).rgbReserved
    	   ib = #ff .and. b(i)
    	   ig = #ff .and. g(i)
    	   ir = #ff .and. r(i)
    	   is = #ff .and. s(i)
    	   write(*,201) 'RGB:  ',ir,ig,ib,is,r(i),g(i),b(i),s(i)
    	   write(2,201) 'RGB:  ',ir,ig,ib,is,r(i),g(i),b(i),s(i)
    201	   format(1x,a,4i5,3x,4(2x,z))
    	enddo
    
    	write(2,'(1x)')
    
    50	continue
    
    
    	if(bmiH.biBitCount.eq.1) then
    !!	   ix = bmiH.biWidth /32
    !!	   T = real(bmiH.biWidth)/32.
    !!	   if(ix.lt.T) ix=ix+1
    !!	   ix = 4*ix
    	   ix = bmiH.biSizeImage / bmiH.biHeight
    	   do j=1,bmiH.biHeight
    	      do i=1,ix
    	         read(1) C
    	         X(i,j) = C
    	         iC = C .and. #ff
    	      enddo
    	   enddo
    !	   iz = (16*(bmiH.biWidth/16))/8
    	   iz = 4*(bmiH.biWidth/4)/8
    	   sizeA = bmiH.biWidth * bmiH.biHeight
    	   sizeB = bmiH.biSizeImage
    	   sizeZ = (2*iz+2)*bmiH.biHeight + 3
    !	   write(3,'(3(i9,a))') bmiH.biBitCount
    !	   write(3,'(3(i9,a))') bmiH.biWidth,',',bmiH.biHeight,',',sizeA
    !	   write(3,'(3(i9,a))') 8*ix,',',bmiH.biHeight,',',sizeB
    !	   write(3,'(3(i9,a))') 8*iz,',',bmiH.biHeight,',',sizeZ
    	   write(3,'(a,i6)') '   << /Type /XObject'
    	   write(3,'(a,i6)') '      /Subtype /Image'
    	   write(3,'(a,i6)') '      /Width ', 8*iz
    	   write(3,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(3,'(a,i6)') '      /ColorSpace /DeviceGray'
    	   write(3,'(a,i3)') '      /BitsPerComponent', bmiH.biBitCount
    	   write(3,'(a,i8)') '      /Length', sizeZ
    	   write(3,'(a,i6)') '      /Filter /ASCIIHexDecode'
    	   write(3,'(a,i6)') '   >>'
    	   write(3,'(a,i6)') 'stream'
    	   lengU = 0
    	   do j = bmiH.biHeight,1,-1
    	      write(3,'(4000Z)') (X(m,j),m=1,iz)
    	      mm = 0
    	      do m=1,iz
    	         mm = mm+1
    	         string(mm) = X(m,j)
    	      enddo
    	      leng = 0
    	      call RunLengthEncode(0,iz,leng,lengU,string)
    	   enddo
    	   write(3,'(a)') '>'
    	   write(3,'(a)') 'endstream'
    	   leng = 0
    	   call RunLengthEncode(1,iz,leng,lengU,string)
    	   lengU = lengU + 3
    	   close(5)
    	   write(6,'(a,i6)') '   << /Type /XObject'
    	   write(6,'(a,i6)') '      /Subtype /Image'
    	   write(6,'(a,i6)') '      /Width ', 8*iz
    	   write(6,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(6,'(a,i6)') '      /ColorSpace /DeviceGray'
    	   write(6,'(a,i3)') '      /BitsPerComponent', bmiH.biBitCount
    	   write(6,'(a,i8)') '      /Length', lengU
    	   write(6,'(6x,a)') '/Filter [/ASCIIHexDecode /RunLengthDecode]'
    	   write(6,'(a,i6)') '   >>'
    	   write(6,'(a,i6)') 'stream'
    	   open(unit=5,file='444.txt',status='unknown')
    	      do while(.true.)
    	         read (5,'(a)',end=22) str
    	         write(6,'(a)') str(1:len_trim(str))
    	      enddo
    22	   continue
    	   close(5)
    	   write(6,'(a)') '>'
    	   write(6,'(a)') 'endstream'
    	   close(6)
    	endif
    
    
    	if(bmiH.biBitCount.eq.4) then
    	   ix = bmiH.biSizeImage / bmiH.biHeight
    	   do j=1,bmiH.biHeight
    	      do i=1,ix
    	         read(1) C
    	         k1 = 2*(i-1)+1
    	         k2 = k1+1
    	         X(k1,j) = #0f .and. ishft(C,-4)
    	         X(k2,j) = #0f .and. C
    	         ik1 = X(k1,j)
    	         ik2 = X(k2,j)
    	         write(2,'(2i6,2i4,2x,3(2x,Z),2i3,4x,4(z,2x),2x,4(z,2x))') 
         *                  j,i,k1,k2,C,X(k1,j),X(k2,j),ik1,ik2,
         *                  r(ik1+1),g(ik1+1),b(ik1+1),s(ik1+1),
         *                  r(ik2+1),g(ik2+1),b(ik2+1),s(ik2+1)
    
    	      enddo
    	   enddo
    	   iz = bmiH.biWidth
    	   sizeA = bmiH.biWidth * bmiH.biHeight
    	   sizeB = bmiH.biSizeImage
    	   sizeZ = (6*iz+2)*bmiH.biHeight + 3
    !	   write(3,'(3(i9,a))') bmiH.biBitCount
    !	   write(3,'(3(i9,a))') bmiH.biWidth,',',bmiH.biHeight,',',sizeA
    !	   write(3,'(3(i9,a))') 2*ix,',',bmiH.biHeight,',',sizeB
    !	   write(3,'(3(i9,a))') iz,',',bmiH.biHeight,',',sizeZ
    	   write(3,'(a,i6)') '   << /Type /XObject'
    	   write(3,'(a,i6)') '      /Subtype /Image'
    	   write(3,'(a,i6)') '      /Width ', iz
    	   write(3,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(3,'(a,i6)') '      /ColorSpace /DeviceRGB'
    	   write(3,'(a,i3)') '      /BitsPerComponent', 8
    	   write(3,'(a,i8)') '      /Length', sizeZ
    	   write(3,'(a,i6)') '      /Filter /ASCIIHexDecode'
    	   write(3,'(a,i6)') '   >>'
    	   write(3,'(a,i6)') 'stream'
    	   lengU = 0
    	   do j = bmiH.biHeight,1,-1
    	      write(3,'(4000Z)') (r(x(m,j)+1),g(x(m,j)+1),b(x(m,j)+1)
         *                           ,m=1,iz)
    	      mm = 0
    	      do m=1,iz
    	         mm = mm+1
    	         string(mm) = r(x(m,j)+1)
    	         mm = mm+1
    	         string(mm) = g(x(m,j)+1)
    	         mm = mm+1
    	         string(mm) = b(x(m,j)+1)
    	      enddo
    !!	      write(3,'(4000Z)') (string(m),m=1,3*iz)
    	      leng = 0
    	      call RunLengthEncode(0,3*iz,leng,lengU,string)
    	   enddo
    	   write(3,'(a)') '>'
    	   write(3,'(a)') 'endstream'
    	   leng = 0
    	   call RunLengthEncode(1,3*iz,leng,lengU,string)
    	   lengU = lengU + 3
    	   close(5)
    	   write(6,'(a,i6)') '   << /Type /XObject'
    	   write(6,'(a,i6)') '      /Subtype /Image'
    	   write(6,'(a,i6)') '      /Width ', iz
    	   write(6,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(6,'(a,i6)') '      /ColorSpace /DeviceRGB'
    	   write(6,'(a,i3)') '      /BitsPerComponent', 8
    	   write(6,'(a,i8)') '      /Length', lengU
    	   write(6,'(6x,a)') '/Filter [/ASCIIHexDecode /RunLengthDecode]'
    	   write(6,'(a,i6)') '   >>'
    	   write(6,'(a,i6)') 'stream'
    	   open(unit=5,file='444.txt',status='unknown')
    	      do while(.true.)
    	         read (5,'(a)',end=33) str
    	         write(6,'(a)') str(1:len_trim(str))
    	      enddo
    33	   continue
    	   close(5)
    	   write(6,'(a)') '>'
    	   write(6,'(a)') 'endstream'
    	   close(6)
    	endif
    
    
    	if(bmiH.biBitCount.eq.24) then
    	   iz = bmiH.biWidth
    	   ix = (8*bmiH.biWidth)/32
    	   T = real(8*bmiH.biWidth)/32.
    	   if(ix.lt.T) ix=ix+1
    !	   ix = 4*ix - iz - 2
    	   ix = 4*ix - iz
    	   do j=1,bmiH.biHeight
    	      do i=1,iz
    	         read(1) C,D,E
    	         k1 = 3*(i-1)+1
    	         k2 = k1+1
    	         k3 = k1+2
    	         X(k1,j) = E
    	         X(k2,j) = D
    	         X(k3,j) = C
    	         ik1 = #ff .and. X(k1,j)
    	         ik2 = #ff .and. X(k2,j)
    	         ik3 = #ff .and. X(k3,j)
    	         write(2,'(2i6,2x,3i4,4x,3(z,2x),3i4)') 
         *                 j,i,k1,k2,k3,X(k1,j),X(k2,j),X(k3,j),ik1,ik2,ik3
     	      enddo
    	      if(ix.gt.0) then
    	         do i=1,ix
    	            read(1) C
    	            ik1 = #ff .and. C
    	         write(2,'(2i6,2x,12x,4x,z,10x,i4)') j,(i+iz),C,ik1
    	         enddo
    	      endif
    	   enddo
    
    	   iz = bmiH.biWidth
    	   sizeA = bmiH.biWidth * bmiH.biHeight
    	   sizeB = bmiH.biSizeImage
    	   sizeZ = (6*iz+2)*bmiH.biHeight + 3
    !	   write(3,'(3(i9,a))') bmiH.biBitCount
    !	   write(3,'(3(i9,a))') bmiH.biWidth,',',bmiH.biHeight,',',sizeA
    !	   write(3,'(3(i9,a))') 2*ix,',',bmiH.biHeight,',',sizeB
    !	   write(3,'(3(i9,a))') iz,',',bmiH.biHeight,',',sizeZ
    	   write(3,'(a,i6)') '   << /Type /XObject'
    	   write(3,'(a,i6)') '      /Subtype /Image'
    	   write(3,'(a,i6)') '      /Width ', iz
    	   write(3,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(3,'(a,i6)') '      /ColorSpace /DeviceRGB'
    	   write(3,'(a,i3)') '      /BitsPerComponent', 8
    	   write(3,'(a,i8)') '      /Length', sizeZ
    	   write(3,'(a,i6)') '      /Filter /ASCIIHexDecode'
    	   write(3,'(a,i6)') '   >>'
    	   write(3,'(a,i6)') 'stream'
    	   lengU = 0
    	   do j = bmiH.biHeight,1,-1
    !	      write(3,'(4000Z)') x((3*(m-1)+1),j),x((3*(m-1)+2),j),
    !     *                           x((3*(m-1)+3),j),m=1,iz)
    	      mm = 0
    	      do m=1,iz
    	         k1 = 3*(m-1)+1
    	         k2 = k1+1
    	         k3 = k1+2
    	         mm = mm+1
    	         string(mm) = x(k1,j)
    	         mm = mm+1
    	         string(mm) = x(k2,j)
    	         mm = mm+1
    	         string(mm) = x(k3,j)
    	      enddo
    	      write(3,'(4000Z)') (string(m),m=1,3*iz)
    	      leng = 0
    	      call RunLengthEncode(0,3*iz,leng,lengU,string)
    	   enddo
    	   write(3,'(a)') '>'
    	   write(3,'(a)') 'endstream'
    	   leng = 0
    	   call RunLengthEncode(1,3*iz,leng,lengU,string)
    	   lengU = lengU + 3
    	   close(5)
    	   write(6,'(a,i6)') '   << /Type /XObject'
    	   write(6,'(a,i6)') '      /Subtype /Image'
    	   write(6,'(a,i6)') '      /Width ', iz
    	   write(6,'(a,i6)') '      /Height', bmiH.biHeight
    	   write(6,'(a,i6)') '      /ColorSpace /DeviceRGB'
    	   write(6,'(a,i3)') '      /BitsPerComponent', 8
    	   write(6,'(a,i8)') '      /Length', lengU
    	   write(6,'(6x,a)') '/Filter [/ASCIIHexDecode /RunLengthDecode]'
    	   write(6,'(a,i6)') '   >>'
    	   write(6,'(a,i6)') 'stream'
    	   open(unit=5,file='444.txt',status='unknown')
    	      do while(.true.)
    	         read (5,'(a)',end=44) str
    	         write(6,'(a)') str(1:len_trim(str))
    	      enddo
    44	   continue
    	   close(5)
    	   write(6,'(a)') '>'
    	   write(6,'(a)') 'endstream'
    	   close(6)
    	endif
    
    99	continue
    
    	end
    
    !	-------------------------------------------------------------
    
    	subroutine RunLengthEncode(E,len,leng,lengU,string)
    
    	implicit none
    
    	byte string(4000)
    	byte RLenc(4000)
    	byte a,b,EOF
    !	byte a1,b2,c1,a2,b2,c2
    	integer*4 E,len,i,n,m,mm
    	byte nf
    	integer*4 leng,lengU
    
    	if(E.eq.1) then
    	   EOF = 128
    	   write(4,'(10x,2x,Z)') EOF
    	   write(5,'(Z)') EOF
    	   leng = 2 + 2
    	   lengU = lengU + leng
    	   write(4,'(i10)') lengU
    	   goto 99
    	endif
    
    !	write(4,'(4000Z)') (string(i),i=1,len)
    
    	a = string(1)
    	n = 1
    
    	mm = 0
    
    	do i=2,len
    	   b = string(i)
    	   if(a.eq.b) then
    	      n = n+1
    	   else
    !	      write(4,'(i10,2x,Z)') n,a
    	      if(n.gt.128) then
    	         m = 128
    	         do while(n.gt.m)
    	            nf = 129
    	            write(4,'(i10,2x,Z,2x,Z)') m,nf,a
    	            n = n-m
    	            mm = mm+1
    	            RLenc(mm) = nf
    	            mm = mm+1
    	            RLenc(mm) = a
    	         enddo
    	      endif
    	      nf = 257-n
    	      if(n.eq.1) nf = 0
    	      write(4,'(i10,2x,Z,2x,Z)') n,nf,a
    	      mm = mm+1
    	      RLenc(mm) = nf
    	      mm = mm+1
    	      RLenc(mm) = a
    	      a = b
    	      n = 1
    	   endif
    	enddo
    
    	if(n.gt.128) then
    	   m = 128
    	   do while(n.gt.m)
    	      nf = 129
    	      write(4,'(i10,2x,Z,2x,Z)') m,nf,a
    	      n = n-m
    	      mm = mm+1
    	      RLenc(mm) = nf
    	      mm = mm+1
    	      RLenc(mm) = a
    	   enddo
    	endif
    	nf = 257-n
    	if(n.eq.1) nf = 0
    	write(4,'(i10,2x,Z,2x,Z)') n,nf,a
    	mm = mm+1
    	RLenc(mm) = nf
    	mm = mm+1
    	RLenc(mm) = a
    
    	write(4,'(1x)')
    
    	if(mm.gt.4000) stop
    	write(5,'(4000Z)') (RLenc(i),i=1,mm)
    	leng = 2*mm + 2
    	lengU = lengU + leng
    
    99	continue
    
    	return
    	end
    
    !	------------------------------------------------------------- 