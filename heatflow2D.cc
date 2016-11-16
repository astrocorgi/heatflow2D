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

// prompting the user for inputs
void heatflow :: getInputs()
{
  cout << "Please enter the desired width:"
    cin >> heatflow :: width;
}

// creating the rectangle for heat flow
void heatflow :: createRectangle(int dx, int dy)
{
  

};


int main()
{
  // prompt user for input geometry
  heatflow :: getInputs;
  return 0;
}
  
