/*
 * 注意：应该将maze_gui.rkt, libcommon.rkt文件放在同一个文件夹
 *
 * PipeStream类使用方法：
 * 用文件名初始化类，将会用该文件创建子进程，并且将建立起与该子进程之间的管道
 * 注意：PipeStream目前只支持写管道，无法从管道中读取数据
 *
 * pout使用方法：
 * pout已被构造好，可直接使用来与maze_gui.rkt程序交互。
 * pout << "命令";
 * 如上语句会将"命令"输入给maze_gui.rkt程序，并由它执行。
 * 目前支持的命令:
 * (set-origin x0 y0)     设置原点坐标
 * (set-size n m)         设置矩阵有nxm个格子
 * (set-grid-size w h)    设置格子的尺寸为wxh
 * (set-colors x y color) 设置格子的颜色, 例如(set-colors 0 0 "red")设置左上角第一个格子的颜色为红色
 * (set-background color)  设置背景的颜色
 * 其实"命令"不限于上述命令，原则而言凡是Racket语言支持的命令都能输入。
 * 举例：
 * pout << "(set-origin 0 0)";  // 使图像移动到窗口最左上角
 *
 */
#include <cstdio>
#include <sstream>
#include <string>
#include <cstring>
using std::stringstream;
using std::string;
using std::strcpy;
using std::strlen;
using std::strcat;

class PipeStream {
private:
  const int size_buf = 4048;
  stringstream ss;
  FILE *pipe;
  char *buffer;

public:

  PipeStream(string filename)
  {
    buffer = new char[size_buf];
    pipe = popen(filename.c_str(), "w");
    if (!pipe) throw "pipe not open!";
  }

  ~PipeStream()
  {
    delete [] buffer;
    pclose(pipe);
  }

  template<class T> PipeStream &operator<<(T var)
  {
    // 将输入值输入stringstream处理后取回
    ss << var;
    string str;

    while ( !ss.eof() ) {
      ss >> str;
      strcpy(buffer, str.c_str());
      strcat(buffer, "\n");
      fwrite(buffer, sizeof(char), strlen(str.c_str()) + 1, pipe);
    }

    return *this;
  }
};

PipeStream pout("./maze_gui.rkt");
