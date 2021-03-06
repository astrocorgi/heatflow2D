! this program is the main file of the heatflow 2D project
!  usage: ./myproject input.dat output_freq output.dat

program HeatCalc
  use HeatFlow ! import module
  implicit none

  character(len=64) :: input, output, freq
  character(len=16), dimension(:,:), allocatable :: holds
  integer :: size_x, size_y, num_timesteps, num_holds
  real :: alpha
  real, dimension(:,:), allocatable :: holds_real
  real, dimension(:,:,:), allocatable :: heatmat
  
  call readInput(input,output,freq)

  call openFile(input,size_x,size_y,alpha,num_timesteps,holds,num_holds)

  call readHolds(holds,num_holds,holds_real)

  call heatPropagation(output,freq,size_x,size_y,alpha,num_timesteps,holds_real,num_holds,heatmat)
  
end program HeatCalc

