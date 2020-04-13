#include <string>
#include <vector>
#include <sstream>

void split(const std::string& s, std::vector<std::string>& tokens, const std::string& delimiters);
std::vector<std::string> split(const std::string& s, const std::string& delimiters);

std::string join(std::vector<std::string> v, const std::string& delimiters);

int str2int(const std::string& s);

std::string int2str(int i);
