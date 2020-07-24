#include <memory>
#include <chrono>
#include <sstream>
#include "lexicon.hpp"
#include "coder.hpp"
#include "database.hpp"
#include "utils.hpp"
#include "core.hpp"
using std::unique_ptr; using std::cin; using std::cout; using std::endl;
using std::vector; using std::string; using std::stringstream;
namespace chrono = std::chrono;
using namespace blitz;

class CTextBook {
    public:
        char* operator[](size_t p) const { return pText + p; }
    private:
        char* pText;
};

struct Foo {
    int a;
    int b;
    int c;
    Foo(int i, int j) : a(i), b(j) {}
    Foo(const Foo& rhs) {}
};

void test_inputmethod()
{
    //cout << InputMethod::valid_pinyin_dict << endl;
    //cout << InputMethod::valid_pinyin_tokens << endl;
    InputMethod im;
    {
        TimeIt ti("input 1 character");
        im.on_input('n');
        cout << im.get_candidates() << endl;
    }
    {
        TimeIt ti("input 2 character");
        im.on_input('h');
        cout << im.get_candidates() << endl;
    }
    {
        TimeIt ti("input 3 character");
        im.on_input('z');
        cout << im.get_candidates() << endl;
    }
    {
        TimeIt ti("input 4 character");
        im.on_input('z');
        cout << im.get_candidates() << endl;
    }
}

void test_lexicon()
{
    //blitz::Record e;
    //stringstream ss;
    //ss << "a'a'a 啊啊啊 0\nwjeiowojie wjeowj 232\n";
    //ss >> e;
    //cout << bool(ss) << " " << e << endl;
    //ss >> e;
    //cout << bool(ss) << " " << e << endl;
    //ss >> e;
    //cout << bool(ss) << " " << e << endl;
    //auto vec = lexical_cast<vector<blitz::Record>>("a'a'a 啊啊啊 0\nwjeiowojie wjeowj 232\n");
    //cout << "{" <<  lexical_cast<string>(vec) << "}" << endl;

    //blitz::Lexicon l;
    //l.insert("a'a'a", "啊啊啊", 1);
    //auto v = l.find_all("a'a'a");
    //cout << "v.empty()? " << v.empty() << endl;
    //cout << "v:" << endl;
    //for (auto& x : v)
        //cout << x << endl;
    {
        TimeIt it("use insert");
        auto lexicon = Lexicon::create();
        DataLoader dataloader;
        //auto vec = lexical_cast<vector<Record>>(dataloader.read_data());
        stringstream ss(dataloader.read_data());
        for (Record r; ss >> r;)
          lexicon->insert(move(r));
        lexicon->check_data();

        for (auto r : lexicon->find_all("ni'hao"))
          cout << r;
    }
}

void test_coder()
{
    string s = "啊啊啊啊啊啊啊啊啊啊 啊wejowfjoiejfiowf";
    //for (int i = 0; i < 10; ++i) s += s;
    unique_ptr<Coder> c = Coder::create(s);
    auto m = s;
    c->encode(s);

    cout << "original string:         " << s << endl;
    cout << "encoded data:            " << m << endl;
    c->decode(s);
    cout << "after encode and decode: " << m << endl;
    cout << "memory usage of string:       " << s.size() << endl;
    cout << "memory usage of encoded data: " << m.size() << endl;
    cout << "rate:                         " << m.size() / float(s.size()) << endl;
}

void test_dataloader()
{
    DataLoader dataloader;
    auto vec = lexical_cast<vector<Record>>(dataloader.read_data());
    Record empty_record;
    cout << vec.size() << endl;
    for (auto& v : vec) {
        if (v == empty_record)
            cout << "bad data" << endl;
    }
}

int main()
{
    test_inputmethod();
    //test_lexicon();
    //test_coder();
    //test_dataloader();
    TimeIt::show();

    return 0;
}
