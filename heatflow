module HeatFlow
  implicit none

contains
  subroutine readInput()
    integer :: i
    character(len=64) :: input, output, freq
    
    call getarg(1,input)
    call getarg(2,freq)
    call getarg(3,output)
    
    print *, 'first arg is ', input
    print *, 'second arg is ', freq
    print *, 'third arg is ', output
  end subroutine readInput
  
  subroutine openFile()
      open (1,file=input, status="unknown" )
      read (1,*) intread
      read (1,*) realread

      print *, intread
      print *, realread
      
      !size_x size_y alpha num_timesteps
      !x_pos y_pos temp hold
      !x_pos y_pos temp hold
      !...

      !get the number of lines in input file. n-1 is the iterations for constant heat points
    
  end subroutine openFile
  
end module HeatFlow
