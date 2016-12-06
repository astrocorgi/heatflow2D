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
    read(1,*) size_x, size_y, alpha, num_timesteps !reads first line of input file
    
    !convert sizes considering Fortran indexing 
    size_x = size_x + 1
    size_y = size_y + 1
    
    print *, 'x length:', size_x
    print *, 'y_length:', size_y
    print *, 'alpha:', alpha
    print *, 'Timesteps:',num_timesteps
    print *, ''

    ncol = 4
    nrow = size_y 
    k=1
    allocate(holds(ncol,nrow)) ! (COL,ROW)

    !Reading unknown length matrix, from ISP slides
    do
       read(1,*,IOSTAT=ios) holds(:,k)
       if (is_iostat_end(ios)) then
          write (*,'(a,i2)') &
               'End of file reached: row = ', k-1 !-1 because first line is read above
          exit
       else if (ios /= 0) then
          write (*,'(a,i2)') !&
               !'Problems reading line', k-1
          exit
       endif
       write(*,*) holds(:,k) ! (COL,ROW)
       k = k+1
    enddo

    num_holds = k-1
    !print *, holds
    !print *, '(1,1):', holds(1,1)
    !print *, '(1,2):',holds(1,2)
    !print *, '(2,1):',holds(2,1)
    
    close (1)
    !size_x size_y alpha num_timesteps
    !x_pos y_pos temp hold
    !x_pos y_pos temp hold
    !...
    
  end subroutine openFile


  subroutine readHolds(holds,num_holds,holds_real)
    !Right now it's a character matrix and I don't like that :(
    implicit none
    integer :: i,j,num_holds, chartest,stat
    real, dimension(:,:), allocatable :: holds_real
    character(len=16), dimension(4,num_holds) :: holds !(COL,ROW)

    allocate(holds_real(4,num_holds))
    
    !This loop goes through every element in the input matrix "holds". It looks for *'s, turns them to -999. It converts all characters to integers and adds +1 to the xpos and ypos (since they are in C notation)
    do i = 1,4 !for each column
       do j = 1,num_holds !for each row
          !print *, holds(i,j)
          !check if this is a *
          if (holds(i,j) == '*') then
          !change to -999 
             holds(i,j) = '-999'
             holds_real(i,j) = -999
          else
             !change the character to real
             read(holds(i,j),*,iostat=stat) holds_real(i,j)
             
             !add +1 to all xposition and yposition values that aren't *'s
             if (i == 1 .OR. i == 2) then
                holds_real(i,j) = holds_real(i,j) + 1
             endif
             
          endif
       enddo
    enddo

    print *, 'The matrix of reals:'
    write(*,'(4f10.2)') holds_real
    
  end subroutine readHolds

  
  subroutine heatPropagation(output,freq,size_x,size_y,alpha,num_timesteps,holds_real,num_holds,heatmat)
    !here we apply the heat equation to each element in the given rectangle
    character(len=64) :: output, freq
    integer :: size_x, size_y, num_timesteps, num_holds, y, x, k, t, hold_check
    integer :: interior, top, bottom, ls, rs, tl, tr, bl, br, corner
    real, dimension(4,num_holds) :: holds_real
    real :: alpha
    real, dimension(:,:,:), allocatable :: heatmat

    allocate(heatmat(size_x,size_y,num_timesteps)) !change z to mod(num_timesteps,output) later
    hold_check = 0
    interior = 0
    top = 0
    bottom = 0
    ls = 0
    rs = 0

    !STILL NEED TO INCLUDE *  * WILDCARD HANDLING
    
    do t=1,num_timesteps
       do y = 1,size_y ! for each row
          do x=1,size_x ! for each column
             ! check if this position is a hold
             do k=1,num_holds
                if ((x==holds_real(k,1)) .AND. (y==holds_real(k,2)) .AND. (holds_real(k,4)==1)) then
                   heatmat(y,x,t) = holds_real(k,3)
                   hold_check = 1
                elseif ((y == holds_real(k,2)) .AND. (holds_real(k,1) == -999)) then
                   !it's in a hold column, set to hold value
                   heatmat(y,x,t) = holds_real(k,3)
                elseif ((x == holds_real(k,1)) .AND. (holds_real(k,2) == -999) .AND. (holds_real(k,4)==1)) then
                   !it's in a hold row, set to hold value
                   heatmat(y,x,t) = holds_real(k,3)
                endif
             enddo ! for each hold
             
             !now determine if it is interior, top, LS, RS, or bottom. Flag accordingly.
             if (hold_check==0) then
                if (y == 1) then
                   if (x==1) then
                      tl = 1 !top left
                      corner = 1
                   elseif (x==size_x) then
                      tr = 1 !top right
                      corner = 1
                   else 
                      ls = 1
                   endif
                elseif (y == size_y) then
                   if (x ==1) then
                      bl = 1 ! bottom left
                      corner = 1
                   elseif (x==size_x) then
                      br = 1 ! bottom right
                      corner = 1
                   else
                      rs = 1
                   endif
                elseif (x == 1) then
                   top = 1
                elseif (x == size_x) then
                   bottom = 1
                else
                   interior = 1
                endif !determining location flag
             endif !hold check

             ! now apply the correct heat solution to the element
             ! CORNER ELEMENTS
             if (corner==1) then
                if (tl==1) then
                   !function without x-1, y-1
                   !heatmat(y,x,t) =  u(x,y,t-1)  &
                    !       + u(x+1,y,t-1)+ u(x,y+1,t-1)    &
                     !      +u(x+1,y+1,t-1)-8*u(x,y,t-1))
                elseif (tr==1) then
                   !function without x+1, y-1
                   
                   !heatmat(y,x,t) = u(x,y,t-1)  &
                    !    +u(x-1,y,t-1)+u(x-1,y+1,t-1)+ u(x,y+1,t-1)    &
                     !   -8*u(x,y,t-1))
                elseif  (bl==1) then
                   !function without x-1, y+1
                   
                   !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x,y-1,t-1)+ u(x+1,y-1,t-1) &
                    !    + u(x+1,y,t-1)   &
                     !   -8*u(x,y,t-1))
                elseif (br==1) then
                   !function without y+1, x+1
                   !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x-1,y-1,t-1)+u(x,y-1,t-1) &
                    !    +u(x-1,y,t-1)    &
                     !   -8*u(x,y,t-1))
                endif
             else !(corner /= 1), its an edge or interior

                   if (interior==1) then
                
                      !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x-1,y-1,t-1)+u(x,y-1,t-1)+ u(x+1,y-1,t-1) &
                       !    +u(x-1,y,t-1)+ u(x+1,y,t-1)+u(x-1,y+1,t-1)+ u(x,y+1,t-1)    &
                        !   +u(x+1,y+1,t-1)-8*u(x,y,t-1))
                
                   elseif (ls==1) then
                      !same equation with x-1 terms removed
                      
                      !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x,y-1,t-1)+ u(x+1,y-1,t-1) &
                       !    + u(x+1,y,t-1)+u(x,y+1,t-1)    &
                        !   +u(x+1,y+1,t-1)-8*u(x,y,t-1))
                
                   elseif (rs==1) then
                      !same equation with x+1 terms removed
                      !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x-1,y-1,t-1)+u(x,y-1,t-1) &
                       !    +u(x-1,y,t-1)+u(x-1,y+1,t-1)+ u(x,y+1,t-1)    &
                        !   -8*u(x,y,t-1))

                   elseif (bottom==1) then
                      !same equation with y+1 removed
                   
                      !heatmat(y,x,t) = u(x,y,t-1) + a*(u(x-1,y-1,t-1)+u(x,y-1,t-1)+ u(x+1,y-1,t-1) &
                       !    +u(x-1,y,t-1)+ u(x+1,y,t-1)+u(x-1,y+1,t-1)+ u(x,y+1,t-1)    &
                        !   +u(x+1,y+1,t-1)-8*u(x,y,t-1))

                   elseif (top==1) then
                      !same equation with y-1 removed
                      !heatmat(y,x,t) = u(x,y,t-1)  &
                       !    +u(x-1,y,t-1)+ u(x+1,y,t-1)+u(x-1,y+1,t-1)+ u(x,y+1,t-1)    &
                        !   +u(x+1,y+1,t-1)-8*u(x,y,t-1))
                   endif !interior == 1
                endif
          enddo ! for each column
          hold_check = 0 !reset to zero after every element
          interior = 0
          top = 0
          bottom = 0
          ls = 0
          rs = 0
          tl = 0
          tr = 0
          bl = 0
          br = 0
          corner = 0
       enddo ! for each row
    enddo ! for each time step
    
  end subroutine heatPropagation

  
end module HeatFlow
