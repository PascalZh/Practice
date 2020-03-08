#include "core.h"
#include <memory>
#include <chrono>
using std::unique_ptr; using std::cin; using std::cout; using std::endl;
using std::vector; using std::string;
namespace chrono = std::chrono;

vector<string> valid_pinyin = {
"a", "ai", "an", "ang", "ao", "ba", "bai", "ban", 
"bang", "bao", "bei", "ben", "beng", "bi", "bian", "biao", 
"bie", "bin", "bing", "bo", "bu", "ca", "cai", "can", 
"cang", "cao", "ce", "cen", "ceng", "cha", "chai", "chan", 
"chang", "chao", "che", "chen", "cheng", "chi", "chong", "chou", 
"chu", "chua", "chuai", "chuan", "chuang", "chui", "chun", "chuo", 
"ci", "cong", "cou", "cu", "cuan", "cui", "cun", "cuo", 
"da", "dai", "dan", "dang", "dao", "de", "dei", "deng", 
"di", "dia", "dian", "diao", "die", "ding", "diu", "dong",
"dou", "du", "duan", "dui", "dun", "duo", "e",
"ei", "en", "er", "fa", "fan", "fang", "fe", "fei",
"fen", "feng", "fiao", "fo", "fou", "fu", "ga", "gai",
"gan", "gang", "gao", "ge", "gei", "gen", "geng", "gong",
"gou", "gu", "gua", "guai", "guan", "guang", "gui", "gun",
"guo", "ha", "hai", "han", "hang", "hao", "he", "hei",
"hen", "heng", "hong", "hou", "hu", "hua", "huai", "huan",
"huang", "hui", "hun", "huo", "i", "ji", "jia", "jian",
"jiang", "jiao", "jie", "jin", "jing", "jiong", "jiu", "ju", "juan",
"jue", "jun", "ka", "kai", "kan", "kang", "kao",
"ke", "ken", "keng", "kong", "kou", "ku", "kua", "kuai", "kuan", "kuang",
"kui", "kun", "kuo", "la", "lai", "lan",
"lang", "lao", "le", "lei", "leng", "li", "lia", "lian", "liang", "liao", "lie",
"lin", "ling", "liu", "long", "lou",
"lu", "luan", "lue", "lun", "luo", "lv", "ma", "mai",
"man", "mang", "mao", "me", "mei", "men", "meng", "mi",
"mian", "miao", "mie", "min", "ming", "miu", "mo", "mou",
"mu", "na", "nai", "nan", "nang", "nao", "ne", "nei",
"nen", "neng", "ng", "ni", "nian", "niang", "niao", "nie",
"nin", "ning", "niu", "nong", "nou", "nu", "nuan", "nue",
"nuo", "nv", "o", "ou", "pa", "pai", "pan", "pang",
"pao", "pei", "pen", "peng", "pi", "pian", "piao", "pie", "pin",
"ping", "po", "pou", "pu", "qi", "qia", "qian",
"qiang", "qiao", "qie", "qin", "qing", "qiong", "qiu", "qu", "quan",
"que", "qun", "ran", "rang", "rao", "re", "ren",
"reng", "ri", "rong", "rou", "ru", "ruan", "rui", "run", "ruo", "sa", "sai",
"san", "sang", "sao", "se", "sen",
"seng", "sha", "shai", "shan", "shang", "shao", "she", "shei", "shen", "sheng",
"shi", "shou", "shu", "shua", "shuai", "shuan",
"shuang", "shui", "shun", "shuo", "si", "song", "sou", "su", "suan",
"sui", "sun", "suo", "ta", "tai", "tan", "tang",
"tao", "te", "teng", "ti", "tian", "tiao", "tie", "ting", "tong", "tou",
"tu", "tuan", "tui", "tun", "tuo", "u",
"v", "wa", "wai", "wan", "wang", "wei", "wen", "weng", "wo", "wu", "xi", "xia",
"xian", "xiang", "xiao", "xie",
"xin", "xing", "xiong", "xiu", "xu", "xuan", "xue", "xun", "ya", "yan", "yang", "yao",
"ye", "yi", "yin", "ying",
"yo", "yong", "you", "yu", "yuan", "yue", "yun", "za",
"zai", "zan", "zang", "zao", "ze", "zei", "zen", "zeng",
"zha", "zhai", "zhan", "zhang", "zhao", "zhe", "zhen", "zheng",
"zhi", "zhong", "zhou", "zhu",
"zhua", "zhuai", "zhuan", "zhuang",
"zhui", "zhun", "zhuo", "zi", "zong", "zou", "zu", "zuan", "zui", "zun",  "zuo"
};

void test_single_pinyin();
void test_ciyu();

int main()
{
    //cout << p_start << endl;
    
    test_single_pinyin();
    test_ciyu();
    cin.get();
    return 0;
}

void test_single_pinyin()
{
    
    WordQueryBase * w = new WordQuerySimple();
    w -> query("ni", 10);
    const query_record_t * q = w -> get_last_query();
    cout << q -> pinyin << endl;
    for (auto& x: q -> candidates)
        cout << x;
    cout << endl;

    int n = 1000;
    auto t1 = std::chrono::high_resolution_clock::now();
    for (int i=0;i<n;i++)
        for (int j=0;j<valid_pinyin.size();j++) {
            w -> query(valid_pinyin[j], 20);
        }
    auto t2 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> ms = t2 - t1;
	cout << "test single: "  << ms.count()
        << "ms, " << ms.count() / n / valid_pinyin.size() * 1000 << "us per time" << endl;
    delete w;
}

void test_ciyu()
{
    WordQueryBase * w = new WordQuerySimple();
    w -> query("ni'hao", 10);
    const query_record_t * q = w -> get_last_query();
    cout << q -> pinyin << endl;
    for (auto& x: q -> candidates)
        cout << x;
    cout << endl;
    delete w;
}
