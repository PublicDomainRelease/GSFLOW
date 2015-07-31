!  CSV_merge_sagehen.f90 
!
!  FUNCTIONS:
!  CSV_merge - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: CSV_merge
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program CSV_merge

    implicit none
    

    ! Variables
    character*1000 line
    character*60 name(100),mergfile,namfile
    integer i, j, numfiles, numlines,reason
    ! Body of CSV_merge
    mergfile = './sagehen_restart/output/gsflow_merged.csv'
    namfile = './sagehen_restart/linux/names.dat'
    open(88,file=mergfile,status='unknown')
    open(77,file=namfile)
    read(77,*)numfiles
    print *, numfiles
    do i=1,numfiles
    read(77,'(a)')name(i)
    print *, name(i)
    open(i+9,file=name(i))
    end do
!
    numlines = 5000
    do 10 i=1,numfiles
    do 11 j=1,numlines
    read(i+9,98,IOSTAT=reason)line
    IF ( reason<0 ) exit
    IF( j+i==2.OR.j>1 )write(88,98)line
  11 continue 
     print *, i
  10 continue
  98 format(A)
    do i=1,numfiles
    close(i)
    end do
    end program CSV_merge

