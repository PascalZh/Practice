#include <pybind11/pybind11.h>
namespace py = pybind11;
#include "core.h"
using std::string;

class WordQuery : public WordQuerySimple
{
public:
    py::tuple py_get_last_query()
    {
        py::tuple ret(2);
        py::list word;
        const query_record_t * record = this -> get_last_query();
        ret[0] = record -> pinyin;
        for (const string& c : record -> candidates) {
            word.append<char *>(const_cast<char *>(c.c_str()));
        }
        ret[1] = word;
        return ret;
    }
};

PYBIND11_MODULE(blitz, m) {
    m.doc() = "blitz backend";
    py::class_<WordQuery>(m, "WordQuery")
        .def(py::init())
        .def("query", &WordQuery::query)
        .def("get_last_query", &WordQuery::py_get_last_query, py::return_value_policy::reference);
}
