#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

// main class 
class HeatFlow
{
public:
  int width, height;
  void createRectangle(int,int);

};

// creating the rectangle for heat flow, part of heatflow class
void HeatFlow :: createRectangle(int dx, int dy) //can I pass a class here instead of dx, dy?
{
  // I don't think I need to create a rectangle?
  width = dx;
  height = dy;
  double rectangle[width][height];
};

//reading input file
 class ReadFile
 {
 public:
   void openFile(char*);
 };

void ReadFile :: openFile(char *file_name) 
{
  fstream inputfile(file_name,ios::in);
  streampos size;
  char * memblock;
  char STRING;
  
  if (inputfile.is_open()) // from http://www.cplusplus.com/doc/tutorial/files/
  {
    size = inputfile.tellg();
    memblock = new char [size];
    inputfile.seekg (0, ios::beg);
    inputfile.read (memblock, size);
    inputfile.close();

    cout << "the entire file content is in memory\n";
    //parsing the file contents
    while(!inputfile.eof)
      {
	getline(inputfile,STRING); //save the lines in a string
	cout << STRING;
      }
    
    
    delete[] memblock;
  }
  else cout << "Unable to open file\n";
}

int main(int argc, char *argv[]) // argv[0] = program name, argv[1] = input, argv[2] = output frequency, argv[3] = output filename
{
  ReadFile readfile;
  readfile.openFile(argv[1]);
  
  HeatFlow heatflow;
  heatflow.createRectangle(1,1);
  
  return 0;
}
  
