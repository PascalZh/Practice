#include <memory>
#include <chrono>
#include <sstream>
#include "lexicon.h"
#include "coder.h"
#include "database.h"
#include "utils.h"
using std::unique_ptr; using std::cin; using std::cout; using std::endl;
using std::vector; using std::string; using std::stringstream;
namespace chrono = std::chrono;

vector<string> valid_pinyin = { "a", "ai", "an", "ang", "ao", "ba", "bai", "ban", "bang", "bao", "bei", "ben", "beng", "bi", "bian", "biao", "bie", "bin", "bing", "bo", "bu", "ca", "cai", "can", "cang", "cao", "ce", "cen", "ceng", "cha", "chai", "chan", "chang", "chao", "che", "chen", "cheng", "chi", "chong", "chou", "chu", "chua", "chuai", "chuan", "chuang", "chui", "chun", "chuo", "ci", "cong", "cou", "cu", "cuan", "cui", "cun", "cuo", "da", "dai", "dan", "dang", "dao", "de", "dei", "deng", "di", "dia", "dian", "diao", "die", "ding", "diu", "dong", "dou", "du", "duan", "dui", "dun", "duo", "e", "ei", "en", "er", "fa", "fan", "fang", "fe", "fei", "fen", "feng", "fiao", "fo", "fou", "fu", "ga", "gai", "gan", "gang", "gao", "ge", "gei", "gen", "geng", "gong", "gou", "gu", "gua", "guai", "guan", "guang", "gui", "gun", "guo", "ha", "hai", "han", "hang", "hao", "he", "hei", "hen", "heng", "hong", "hou", "hu", "hua", "huai", "huan", "huang", "hui", "hun", "huo", "i", "ji", "jia", "jian", "jiang", "jiao", "jie", "jin", "jing", "jiong", "jiu", "ju", "juan", "jue", "jun", "ka", "kai", "kan", "kang", "kao", "ke", "ken", "keng", "kong", "kou", "ku", "kua", "kuai", "kuan", "kuang", "kui", "kun", "kuo", "la", "lai", "lan", "lang", "lao", "le", "lei", "leng", "li", "lia", "lian", "liang", "liao", "lie", "lin", "ling", "liu", "long", "lou", "lu", "luan", "lue", "lun", "luo", "lv", "ma", "mai", "man", "mang", "mao", "me", "mei", "men", "meng", "mi", "mian", "miao", "mie", "min", "ming", "miu", "mo", "mou", "mu", "na", "nai", "nan", "nang", "nao", "ne", "nei", "nen", "neng", "ng", "ni", "nian", "niang", "niao", "nie", "nin", "ning", "niu", "nong", "nou", "nu", "nuan", "nue", "nuo", "nv", "o", "ou", "pa", "pai", "pan", "pang", "pao", "pei", "pen", "peng", "pi", "pian", "piao", "pie", "pin", "ping", "po", "pou", "pu", "qi", "qia", "qian", "qiang", "qiao", "qie", "qin", "qing", "qiong", "qiu", "qu", "quan", "que", "qun", "ran", "rang", "rao", "re", "ren", "reng", "ri", "rong", "rou", "ru", "ruan", "rui", "run", "ruo", "sa", "sai", "san", "sang", "sao", "se", "sen", "seng", "sha", "shai", "shan", "shang", "shao", "she", "shei", "shen", "sheng", "shi", "shou", "shu", "shua", "shuai", "shuan", "shuang", "shui", "shun", "shuo", "si", "song", "sou", "su", "suan", "sui", "sun", "suo", "ta", "tai", "tan", "tang", "tao", "te", "teng", "ti", "tian", "tiao", "tie", "ting", "tong", "tou", "tu", "tuan", "tui", "tun", "tuo", "u", "v", "wa", "wai", "wan", "wang", "wei", "wen", "weng", "wo", "wu", "xi", "xia", "xian", "xiang", "xiao", "xie", "xin", "xing", "xiong", "xiu", "xu", "xuan", "xue", "xun", "ya", "yan", "yang", "yao", "ye", "yi", "yin", "ying", "yo", "yong", "you", "yu", "yuan", "yue", "yun", "za", "zai", "zan", "zang", "zao", "ze", "zei", "zen", "zeng", "zha", "zhai", "zhan", "zhang", "zhao", "zhe", "zhen", "zheng", "zhi", "zhong", "zhou", "zhu", "zhua", "zhuai", "zhuan", "zhuang", "zhui", "zhun", "zhuo", "zi", "zong", "zou", "zu", "zuan", "zui", "zun",  "zuo" };

void test_lexicon()
{
    blitz::Record e;
    stringstream ss;
    ss << "a'a'a 啊啊啊 0\nwjeiowojie wjeowj 232\n";
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    auto vec = lexical_cast<vector<blitz::Record>>("a'a'a 啊啊啊 0\nwjeiowojie wjeowj 232\n");
    cout << "{" <<  lexical_cast<string>(vec) << "}" << endl;

    blitz::LexiconMMU l;
    l.insert("a'a'a", "啊啊啊", 1);
    auto v = l.find_all("a'a'a");
    cout << "v.empty()? " << v.empty() << endl;
    cout << "v:" << endl;
    for (auto& x : v)
        cout << x << endl;
}

void test_coder()
{
    string s = "啊啊啊啊啊啊啊啊啊啊 啊wejowfjoiejfiowf";
    //for (int i = 0; i < 10; ++i) s += s;
    blitz::HuffmanCoder c(s);
    auto m = c.encode(s);

    cout << "original string:         " << s << endl;
    cout << "encoded data:            " << m << endl;
    cout << "after encode and decode: " << c.decode(m) << endl;
    cout << "memory usage of string:       " << s.size() << endl;
    cout << "memory usage of encoded data: " << m.size() << endl;
    cout << "rate:                         " << m.size() / float(s.size()) << endl;
}

void test_dataloader()
{
    blitz::DataLoader dataloader;
    auto vec = dataloader.read_records();
    blitz::Record emtpy_record;
    cout << vec.size() << endl;
    for (auto& v : vec) {
        if (v == emtpy_record)
            cout << "bad data" << endl;
    }
}

int main()
{
    //test_lexicon();
    //test_coder();
    //test_dataloader();

    blitz::LexiconMMU lexicon;
    blitz::DataLoader dataloader;
    lexicon.init_lexicon(dataloader.read_data());
    //auto vec = dataloader.read_records();
    //for (int i = 0; i < vec.size(); ++i) {
        //lexicon.insert(vec[i]);
    //}
    lexicon.show_map_node();

    return 0;
}
