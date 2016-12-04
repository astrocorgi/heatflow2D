! this program is the main file of the heatflow 2D project
!  usage: ./myproject input.dat output_freq output.dat

program HeatCalc
  use HeatFlow ! import module
  implicit none

  character(len=64) :: input, output, freq
  character(len=16), dimension(:,:), allocatable :: holds
  integer :: size_x, size_y, num_timesteps
  real :: alpha
  
  call readInput(input,output,freq)

  call openFile(input,size_x,size_y,alpha,num_timesteps,holds)
  
end program HeatCalc

