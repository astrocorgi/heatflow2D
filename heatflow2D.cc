#include <iostream>
#include <cmath>

using namespace std;

// main class 
class HeatFlow
{
public:
  int width, height;
  void createRectangle(int,int);

} HeatFlow;

// creating the rectangle for heat flow, part of heatflow class
void HeatFlow :: createRectangle(int dx, int dy) //can I pass a class here instead of dx, dy?
{
  // I don't think I need to create a rectangle?
  width = dx;
  height = dy;
  double rectangle[width][height];
};

 class ReadFile
 {
   fstream inputfile;
   void openFile(char);

 }ReadFile;

void ReadFile :: openFile(char file_name) 
{

}

int main()
{
  HeatFlow heatflow;
  heatflow.createRectangle(
  
  return 0;
}
  
