#pragma once
//#pragma once could substitute #ifndef... , and works better.
#include <iostream>
#include <stack>
#include <fstream>
#include <iostream>
#include <string>

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




