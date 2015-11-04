c -------------------------------------------------------------------------

      subroutine RdInput (nInten,  testInten, lgTestInten, nGM_model, nattentype,
     1 attenType, nProb, iPer, wt_GM1, period)

      include 'tornado.h'

      real testInten(MAX_INTEN)
      real segModelWt(1), probAct(1), minlat,maxlat,minlong,maxlong,maxdist
      integer nInten, ntotal, attentype(MAX_FLT)
      integer iii
      integer jCalc(MAX_ATTENTYPE,MAX_ATTEN),  sssCalc(MAX_ATTENTYPE,MAX_ATTEN), scalc(MAX_ATTENTYPE,MAX_ATTEN)
      real sigFix(MAX_ATTENTYPE,MAX_ATTEN)

      character*80 filein, title, fname(1), dummy
      integer nfiles, ix(MAX_FILES)
      integer nWidth(MAX_FLT)

      integer nMagBins, nDistBins, nEpsBins, nXcostBins, soilampflag
      real magBins(MAX_MAG), distBins(MAX_DIST), epsBins(MAX_EPS)
      real XcostBins(MAX_XCOST)
      integer nProb, nattentype, nGM_Model(MAX_PROB,MAX_ATTENTYPE)
      real testwt, checkwt, c1, c2, wtgm(MAX_ATTENTYPE,MAX_ATTEN), wt_gm1(MAX_ATTENTYPE,MAX_ATTEN)

c      pause 'inside read input'


c     Set Data file units
      nwr = 11

      ntotal = 0

c     Read in the number of data files.
c      read (5,*) nfiles
c     Program no longer allowed to read from multiple files.
      nFiles = 1

c     Loop over the number of files.
c      do 111 iii=1,nfiles

c     Open PSHA Run Input File
      read (31,'( a80)') filein
      write (*,'( a80)') filein
      open (20,file=filein,status='old')

c     Open Input PSHA Source/Fault file
      read (20,'( a80)') filein
c      open (10,file=filein,status='old')


c     Read in parameters for background grid.
      read (20,*) minlat,maxlat,minlong,maxlong

Cnjg  Added back in read of single maxdist
      read (20,*) maxdist

c     Input Title (not used) 
      read(20,'( a80)') title

c     Number of Spectral Periods and Number of attenuation relations types
      read(20,*) nProb, nattentype
        write (*,'( 2i5)') nProb, nattentype
      
      do iprob=1,nProb

C       Read period, maxeps dir flag and gm intensities
        read (20,*) specT, sigtrunc, dirflag 
        read (20,*) nInten, (testInten(j), j=1,nInten)
        call CheckDim ( nInten, MAX_INTEN, 'MAX_INTEN' )

C       Read in the suite of attenution models and wts for each attentype
        do j=1,nattentype
          checkwt = 0.0
          read (20,*) nGM_model(iProb,j)
          
c         Check for Max number of attenuation model
          call checkDim ( nGM_model(iProb,j), MAX_ATTEN, 'MAX_ATTEN' )

          do jj=1,nGM_model(iProb,j)
            read (20,*) jcalc(j,jj), c1, c2, wtgm(j,jj), Varadd, iMix
c            if ( jCalc(j,jj) .lt. 0 ) then
c               backspace (20)
c               read (20,*) jcalc(j,jj), c1, c2, wtgm(j,jj), Varadd, sCalc(j,jj), sigfix(j,jj), sssCalc(j,jj)
c            endif
          enddo
        enddo

c       keep the weights for the selected problem only
        if ( iper .eq. iProb) then 
          period = specT
          do j=1,nattentype
            do jj=1,nGM_model(iProb,j)
              Wt_GM1(j,jj) = Wtgm(j,jj)
            enddo
          enddo
        endif

      enddo

      close (20)
       
       return
       end
