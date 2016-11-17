#include <iostream>
#include <cmath>

using namespace std;

// main class 
class heatflow
{
public:
  int width, height;
  void getInputs;
  void createRectangle(int,int);
};

// reading the input file
void heatflow :: getInputs()
{
  cout << "Please enter the desired width:";
  cin >> heatflow :: width;
  cout << "Please enter the desired height:";
  cin >> heatflow :: height;
}

// creating the rectangle for heat flow
void heatflow :: createRectangle(int dx, int dy)
{
  

};


int main(int argc, char *argv[]) // http://www.cprogramming.com/tutorial/lesson14.html
{
  if (argc != 3) // argc should be 3 for correct execution
    cout << "usage: $./heatflow2D input.dat output_freq output.dat\n";
  else
    {
      //open the input file
      ifstream the_file( argv[1] );
      // check to see if the file opening succeeded
      if (!the_file.is_open())
	cout << "Could not open file \n";
      else
	{
	  char x;
	  //the_file.get (x) returns false if the end of the file is reached or an error occurs
	  while (the_file.get (x) )
	    cout << x;
	} //else 2
      
      //the file is implicitly here

    } //else 1
  
  return 0;
  
} //int main
  
