! This is the module for import into the main heatflow2D program
module HeatFlow
  implicit none

  
contains


  
  subroutine readInput(input,output,freq) !./myproject input.dat output_freq output.dat
    !This subroutine reads the inputs from the command line and assigns them variable names
    
    character(len=64) :: input, output, freq
    
    call getarg(1,input)
    call getarg(2,freq)
    call getarg(3,output)

    !print *, 'the input file is ', input
    !print *, 'the output frequency is ', freq
    !print *, 'the output file will be ', output
  end subroutine readInput



  
  
  subroutine openFile(input,size_x,size_y,alpha,num_timesteps,holds,num_holds)
    !This subroutine opens the input .dat file and assigns it variables in memory
    character(len=64) :: input
    integer :: size_x, size_y, num_timesteps
    real :: alpha
    character(len=16), dimension(:,:), allocatable :: holds
    integer :: ncol,nrow,k,ios,i,j, num_holds
    
    print *, 'opening file...'    
    open (1,file=input, status='old',action='read')
    read(1,*) size_x, size_y, alpha, num_timesteps
    
    print *, 'x length:', size_x
    print *, 'y_length:', size_y
    print *, 'alpha:', alpha
    print *, 'Timesteps:',num_timesteps
    print *, ''

    ncol = 4
    nrow = size_x
    k=1
    allocate(holds(nrow,ncol))

    !Reading unknown length matrix, from ISP slides
    do
       read(1,*,iostat=ios) holds(k,:)
       if (is_iostat_end(ios)) then
          write (*,'(a,i2)') &
               'End of file reached: row= ', k-1 !-1 because first line is read above
          exit
       else if (ios /= 0) then
          write (*,'(a,i2)') &
               'Problems reading line', k-1
          exit
       endif
       write(*,*) holds(k,:)
       k = k+1
    enddo

    num_holds = k-1
    holds = holds(1:num_holds,:) !I hope this write over holds to make it short
    
    close (1)
    !size_x size_y alpha num_timesteps
    !x_pos y_pos temp hold
    !x_pos y_pos temp hold
    !...

    
  end subroutine openFile



  subroutine readHolds(holds,num_holds)
    !Right now it's a character matrix and I don't like that :( 
    integer :: i,j,num_holds
    character(len=16), dimension(num_holds,4) :: holds

    
    do j = 1,4
       do i = 1,num_holds
          !check if this is a *
          if holds(j,i) == '*' then
          !change to -999
          holds(i,j) = '-999'
          endif
       enddo
    enddo

    !change the character matrix to integer

    !add +1 to all xpos ypos values (they are in C notation)

    
  end subroutine readHolds

  
end module HeatFlow
