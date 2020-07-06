#include <memory>
#include <chrono>
#include <sstream>
#include "core.h"
#include "lexicon.h"
#include "coder.h"
#include "utils.h"
using std::unique_ptr; using std::cin; using std::cout; using std::endl;
using std::vector; using std::string; using std::stringstream;
namespace chrono = std::chrono;

vector<string> valid_pinyin = {
"a", "ai", "an", "ang", "ao", "ba", "bai", "ban", "bang", "bao", "bei", "ben",
"beng", "bi", "bian", "biao", "bie", "bin", "bing", "bo", "bu", "ca", "cai",
"can", "cang", "cao", "ce", "cen", "ceng", "cha", "chai", "chan", "chang",
"chao", "che", "chen", "cheng", "chi", "chong", "chou", "chu", "chua", "chuai",
"chuan", "chuang", "chui", "chun", "chuo", "ci", "cong", "cou", "cu", "cuan",
"cui", "cun", "cuo", "da", "dai", "dan", "dang", "dao", "de", "dei", "deng", 
"di", "dia", "dian", "diao", "die", "ding", "diu", "dong", "dou", "du", "duan",
"dui", "dun", "duo", "e", "ei", "en", "er", "fa", "fan", "fang", "fe", "fei",
"fen", "feng", "fiao", "fo", "fou", "fu", "ga", "gai", "gan", "gang", "gao",
"ge", "gei", "gen", "geng", "gong", "gou", "gu", "gua", "guai", "guan", "guang",
"gui", "gun", "guo", "ha", "hai", "han", "hang", "hao", "he", "hei", "hen",
"heng", "hong", "hou", "hu", "hua", "huai", "huan", "huang", "hui", "hun",
"huo", "i", "ji", "jia", "jian", "jiang", "jiao", "jie", "jin", "jing", "jiong",
"jiu", "ju", "juan", "jue", "jun", "ka", "kai", "kan", "kang", "kao", "ke",
"ken", "keng", "kong", "kou", "ku", "kua", "kuai", "kuan", "kuang", "kui",
"kun", "kuo", "la", "lai", "lan", "lang", "lao", "le", "lei", "leng", "li",
"lia", "lian", "liang", "liao", "lie", "lin", "ling", "liu", "long", "lou",
"lu", "luan", "lue", "lun", "luo", "lv", "ma", "mai", "man", "mang", "mao",
"me", "mei", "men", "meng", "mi", "mian", "miao", "mie", "min", "ming", "miu",
"mo", "mou", "mu", "na", "nai", "nan", "nang", "nao", "ne", "nei", "nen",
"neng", "ng", "ni", "nian", "niang", "niao", "nie", "nin", "ning", "niu",
"nong", "nou", "nu", "nuan", "nue", "nuo", "nv", "o", "ou", "pa", "pai", "pan",
"pang", "pao", "pei", "pen", "peng", "pi", "pian", "piao", "pie", "pin", "ping",
"po", "pou", "pu", "qi", "qia", "qian", "qiang", "qiao", "qie", "qin", "qing",
"qiong", "qiu", "qu", "quan", "que", "qun", "ran", "rang", "rao", "re", "ren",
"reng", "ri", "rong", "rou", "ru", "ruan", "rui", "run", "ruo", "sa", "sai",
"san", "sang", "sao", "se", "sen", "seng", "sha", "shai", "shan", "shang",
"shao", "she", "shei", "shen", "sheng", "shi", "shou", "shu", "shua", "shuai",
"shuan", "shuang", "shui", "shun", "shuo", "si", "song", "sou", "su", "suan",
"sui", "sun", "suo", "ta", "tai", "tan", "tang", "tao", "te", "teng", "ti",
"tian", "tiao", "tie", "ting", "tong", "tou", "tu", "tuan", "tui", "tun", "tuo",
"u", "v", "wa", "wai", "wan", "wang", "wei", "wen", "weng", "wo", "wu", "xi",
"xia", "xian", "xiang", "xiao", "xie", "xin", "xing", "xiong", "xiu", "xu",
"xuan", "xue", "xun", "ya", "yan", "yang", "yao", "ye", "yi", "yin", "ying",
"yo", "yong", "you", "yu", "yuan", "yue", "yun", "za", "zai", "zan", "zang",
"zao", "ze", "zei", "zen", "zeng", "zha", "zhai", "zhan", "zhang", "zhao",
"zhe", "zhen", "zheng", "zhi", "zhong", "zhou", "zhu", "zhua", "zhuai", "zhuan",
"zhuang", "zhui", "zhun", "zhuo", "zi", "zong", "zou", "zu", "zuan", "zui",
"zun",  "zuo"
};


void test_single_pinyin()
{
    
    WordQuerySimple w;
    w.query("ni", 10);
    const query_record_t * q = w.get_last_query();
    cout << q -> pinyin << endl;
    for (auto& x: q -> candidates)
        cout << x;
    cout << endl;

    size_t n = 1000;
    auto t1 = std::chrono::high_resolution_clock::now();
    for (size_t i=0;i<n;i++)
        for (size_t j=0;j<valid_pinyin.size();j++) {
            w.query(valid_pinyin[j], 20);
        }
    auto t2 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> ms = t2 - t1;
	cout << "test single: "  << ms.count()
        << "ms, " << ms.count() / n / valid_pinyin.size() * 1000 << "us per time" << endl;

}

void test_ciyu()
{
    WordQuerySimple w;
    w.query("ni'hao", 10);
    const query_record_t * q = w.get_last_query();
    cout << q -> pinyin << endl;
    for (auto& x: q -> candidates)
        cout << x;
    cout << endl;

}

void test_tree()
{
    //SearchTree<pinyin_t, word_t> tree("db");
    //auto i = tree.find(pinyin_t("bai'piao"));
    //tree.print(*i);
    //cout << tree.capacity() / 1024 << "KiB"<< endl;
}

void test_lexicon()
{
    blitz::Entry e;
    stringstream ss("a'a'a 啊啊啊 0\nwjeiowojie wjeowj 232 \n");
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    ss >> e;
    cout << bool(ss) << " " << e << endl;
    //LexiconMMU l();
    //l.insert_word("a'a'a", "啊啊啊", 1);
    //auto v = l.search_pinyin("a'a'a");
    //cout << v[0] << endl;
}

void test_coder()
{
    string s = "efjwoppcopzjoifpewhqofpewpppppppppppppp";
    for (int i = 0; i < 10; ++i) s += s;
    blitz::HuffmanCoder c(s);
    auto m = c.encode(s);

    //cout << "original string:         " << s << endl;
    //cout << "after encode and decode: " << c.decode(m) << endl;
    //cout << "encoded data:            ";
    //for (auto& d : m) {
        //cout << to_binary(d) << "'";
    //} cout << endl;
    cout << "memory usage of string:       " << s.size() << endl;
    cout << "memory usage of encoded data: " << sizeof(uint32_t) * m.size() << endl;
    cout << "rate:                         " << sizeof(uint32_t) * m.size() / float(s.size()) << endl;
}

int main()
{
    //cout << p_start << endl;
    
    //test_single_pinyin();
    //test_ciyu();
    //test_tree();
    //test_lexicon_node();
    test_lexicon();
    test_coder();

    //cin.get();
    return 0;
}
