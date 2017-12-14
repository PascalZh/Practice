// lastedit: 2017/11/2
#pragma once
//#pragma once could substitute #ifndef... , and works better.
#include <iostream>
#include <stack>
#include <fstream>
#include <iostream>
#include <string>
#include <queue>

#define MAZE_USE_QUEUE
/* I wrote two function to solve this problem you could choose which function you want to use,
through defining a macro-'MAZE_USE_STACK' or 'MAZE_USE_QUEUE'.
However I prefer the latter.
*/
#ifdef MAZE_USE_STACK
// The graph of the maze is stored in the 2d int array: maze.
// '1' shows wall, '0' shows route, but '2' indicates where you have visited.
// a maze array is like this:
/*		+-------------i
		|
		|
		|
		|
		j
*/



// Every step is recorded in a stack, and the step is described as a structure as follow:
namespace maze {
	struct node {
		int i; int j; //(i,j) shows where you are
		int direction; // direction indicates in which direcition you are going.
		// 0 : left, 1 : up, 2 : right, 3 : down
		node(int para_i=0, int para_j=0) :i(para_i), j(para_j), direction(0) {}
	};
}

void getOutOfMaze(int **maze, maze::node start, maze::node end, const int &i_size,const int &j_size) {
	using namespace maze;

	std::stack<node> path; // path
	path.push(start);
	node cur;
	node next;
	while (!path.empty()) {
		// cur mark where you are
		cur = path.top();
		path.pop();
		maze[cur.i][cur.j] = 2;

		if (cur.i == end.i&&cur.j == end.j) {// Congratulations! You find the outlet, then print it!
			path.push(cur);

			std::cout << "Path:" << std::endl;
			std::stack<node> tmp;//used to inverse the path in order to print path
			while (!path.empty()) {
				tmp.push(path.top());
				path.pop();
			}
			while (!tmp.empty()) {// print path
				std::cout << "(" << (tmp.top().i)+1 << "," << (tmp.top().j)+1 << ") ";
				tmp.pop();
			}
			for (int j = 0; j < j_size; j++) {
				for (int i = 0; i < i_size; i++) {
					if (maze[i][j] == 1)
						cout << "#";
					else if (maze[i][j] == 0)
						cout << " ";
					else
						cout << "*";
				}
				cout << endl;
			}
			break;
		}
			switch (cur.direction) {
			case 0:
				if (cur.i != 0 && maze[cur.i - 1][cur.j] == 0) {
					cur.direction++;//WARNING: Don't forget it!!!
					path.push(cur); //find then push two times
					next.i = cur.i - 1;
					next.j = cur.j;
					path.push(next);
					continue;
				} 
				else {
					cur.direction++;
					path.push(cur); //not find then preserve the direction and push back to the path
					continue;
				}
			case 1:
				if (cur.j != 0 && maze[cur.i][cur.j - 1] == 0) {
					cur.direction++;
					path.push(cur);
					next.i = cur.i;
					next.j = cur.j - 1;
					path.push(next);
					continue;
				}
				else {
					cur.direction++;
					path.push(cur);
					continue;
				}
			case 2:
				if (cur.i != i_size - 1 && maze[cur.i+1][cur.j] == 0) {
					cur.direction++;
					path.push(cur);
					next.i = cur.i + 1;
					next.j = cur.j;
					path.push(next);
					continue;
				}
				else {
					cur.direction++;
					path.push(cur);
					continue;
				}
			case 3:
				if (cur.j != j_size - 1 && maze[cur.i][cur.j + 1] == 0) {
					cur.direction++;
					path.push(cur);
					next.i = cur.i;
					next.j = cur.j + 1;
					path.push(next);
					continue;
				}
				else {
					cur.direction++;
					path.push(cur);
					continue;
				}
			default:
				//not find then not push back cur, and set maze[i][j]=0
			{//add: without braces, break; statement will never be executed!!!
				maze[cur.i][cur.j] = 0;
				continue;
			}
			}
/*
	j2:
		;//without this semicolon, program won't succeed in compiling.
		*/
	}

}


void Maze() {
	using namespace maze;
	using namespace std;
	
	int col, row;

	// using ifstream
	string str;
	ifstream fmaze;
	row = 0; col = 0;
	fmaze.open("maze.txt");
	while (getline(fmaze, str))
	{
		row++;
	}
	col = str.size();
	fmaze.close();
	//std::cout << "Please input col and row:" << std::endl;
	//std::cin >> col >> row;
	int **maze = new int *[col];
	
	for (int a = 0; a < col; a++) {
		maze[a] = new int[row];
	}
	//std::cout << "Please input the maze:" << std::endl;
	/*
	for (int j = 0; j < row; j++) {
		//std::cout << "Please input No." << j + 1 << " row:" << std::endl;

		for (int i = 0; i < col; i++) {
			std::cin >> maze[i][j];
		}
	}
	*/
	fmaze.open("maze.txt");
	for (int y = 0; getline(fmaze, str); y++) {
		for (unsigned x = 0; x < str.size(); x++) {
			maze[x][y] = int(str[x]-'0');
		}
	}
	fmaze.close();


	maze::node start(1, 1);
	maze::node end(col - 2, row - 2);

	getOutOfMaze(maze, start, end, col, row);
	for (int i = 0; i < col; i++) {
		delete [] maze[i];
	}
	delete[] maze;
}
#endif

#ifdef MAZE_USE_QUEUE
/*
Just one important data:map[i][j], used to store the map, like this:
+--------------j
|
|
|
|
i
Wall set to -1(in the maze.txt I use # to present -1), road set to 0(space)

Then we use Dijkstra Algorithm to calculate the shortest path.
map[i][j] may refer to the lenght of path.

map[1][1] is start point.
*/
#define InTheMap top.i > 0 && top.i < row - 1 && top.j>0 && top.j < col - 1
void Maze()
{
	// input maze.txt to the map[i][j]
	using namespace std;
	int row = 0, col = 0; // row refer to how many rows map has.
	ifstream fmaze("maze.txt");
	string str;
	while (getline(fmaze, str)) {
		row++;
	}
	col = str.size();
	int **map = new int *[row];
	fmaze.close();
	fmaze.open("maze.txt");
	for (int i = 0; i < row; i++) {
		map[i] = new int[col];
		getline(fmaze, str);
		for (int j = 0; j < col; j++)
			map[i][j] = (str[j] == '#' ? -1:0);
	}


	struct node {
		int i;
		int j;
		node(int parai = 0, int paraj = 0) :i(parai), j(paraj) {}
	};
	queue<node> path;
	path.push(node(1, 1));
	node top;
	while (!path.empty()) {
		top = path.front();
		path.pop();
		if (InTheMap && map[top.i - 1][top.j] == 0)
		{
			path.push(node(top.i - 1, top.j)); 
			map[top.i - 1][top.j] = map[top.i][top.j] + 1;
		}
		if (InTheMap && map[top.i + 1][top.j] == 0)
		{
			path.push(node(top.i + 1, top.j));
			map[top.i + 1][top.j] = map[top.i][top.j] + 1;
		}
		if (InTheMap && map[top.i][top.j + 1] == 0)
		{
			path.push(node(top.i, top.j + 1));
			map[top.i][top.j + 1] = map[top.i][top.j] + 1;
		}
		if (InTheMap && map[top.i][top.j - 1] == 0)
		{
			path.push(node(top.i, top.j - 1));
			map[top.i][top.j - 1] = map[top.i][top.j] + 1;
		}
		//// print map
		//for (int i = 0; i < row; i++) {
		//	for (int j = 0; j < col; j++)
		//		cout << map[i][j]<<"\t";
		//	cout << endl;
		//}
		//cout << endl;
	}
	map[1][1] = 0;
	
	// print map
	for (int i = 0; i < row; i++) {
		for (int j = 0; j < col; j++) {
			cout.width(3);
			cout << map[i][j];
		}
		cout << endl;
	}
	// I fix the start point, however you could choose your
	// outlet point
	// Then all you need to do is return back to the start point in the numerical sequence.
	// for example:
	// your outlet is marked 23, then you find 22, 21, ...
	// finally, you will find 0, that's the start point
}
#endif


