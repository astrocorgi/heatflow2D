This folder contains code to compute the 2D heat propagation problem for the final project in ISP Fall 2016. By Cassie Stuurman, Dec 2016.

heatflow.f90 is the module file. To compile, type "gfortran -c heatflow.f90"

heatflow2d.f90 is the main file. To compile, type "gfortran heatflow.f90 heatflow2d.f90"

To run, the input syntax is

./a.out input_file.dat output_frequency output_file.dat

Output_frequency must be an integer. The input file must already exist in the folder. An example input file is provided. The input file format is:


size_x size_y alpha num_timesteps
x_pos y_pos temp hold
x_pos y_pos temp hold
...

The code will output the results at the timestep interval provided, to the file output_file.dat specified.
