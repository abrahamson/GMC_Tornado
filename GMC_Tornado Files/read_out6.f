c -------------------------------------------------------------------

      subroutine read_logichaz_out6 ( haz, nProb, nattenType, nAtten, nInten, ifile)
      implicit none
      include 'tornado.h'


      real*8 haz(MAX_PROB, MAX_ATTENTYPE, MAX_ATTEN, MAX_INTEN)
      real version
      integer nInten, nProb, nattenType, nAtten(MAX_PROB,MAX_ATTENTYPE)
      integer iProb, jType, iAtten
      integer iProb1, jType1, iAtten1
      integer j, nwr, ifile
      character*80 file1
      nwr = 12
                     
c     Open output file
      read (31,'( a80)') file1
c      write (65,'( i5,2x,a80)') ifile, file1

      write (*,*) 'Opening output6 file from the hazard runs.'
      write (*,*) file1
      open (nwr,file=file1,status='old')

C     Check for version compatibility with hazard code
        read (nwr,*) version
         if (version .ne. 45.2) then
           write (*,*) 'out3 from incompatible version of Haz45, use Haz45.2'
           stop 99
         endif

c currently, only works if nInten is the same for all periods
c  Fix this later

      do iProb=1,nProb
        do jType=1,nattenType
           do iAtten = 1,nAtten(iProb,jType)
               read (nwr,'( 3i5, 100e12.4 )')  iProb1, jType1, iAtten1, 
     1             (Haz(iProb, jType, iAtten, j ), j=1,nInten)
           enddo
         enddo
      enddo

      close (nwr)

      return
  
      end

c -------------------------------------------------------------------------
