#include <pybind11/pybind11.h>
namespace py = pybind11;
#include "core.h"
using std::string;

class WordQuery : public WordQuerySimple
{
public:
    py::tuple get_last_query()
    {
        py::tuple ret(2);
        py::list word;
        query_record_t record = records.back();
        ret[0] = record.pinyin;
        for (string& c : record.candidates) {
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
        .def("get_last_query", &WordQuery::get_last_query, py::return_value_policy::reference);
}
