# Roadmap
1. a database.
2. Markov chain implementation.

# TODO
- [x] embed GBK.txt into our .so file. This could be done with `objcopy` command.
- [ ] 2020/3/9~11: text file based database, enable lazy loading of lexicon. Motivation: now it takes seconds of time to load the `sogou_lexicon.txt` file.

## Build help:
### gbk.o
1. check the architecture:
    objdump -x core.o | grep architecture
    [output]: i386:x86-64

2. build `gbk.o`:
    objcopy -I binary -O elf64-x86-64 -B i386:x86-64 gbk.txt gbk.o

3. check link symbol:`objdump -x gbk.o`, you can find two symbol:`_binary_gbk_txt_start`, `_binary_gbk_txt_end`
4. to use them in our codes:
    extern char _binary_gbk_txt_start;
    extern char _binary_gbk_txt_end;
To be careful `_binary_gbk_txt_end` is not a variable, and you can declare it as any type, in our case, it'js string.

Don't use `_binary_gbk_txt_end` directly! Here is an example:
    char *p = &_binary_gbk_txt_start;
    cout << p << endl;
It will output `gbk.txt` file.

## Database Design
### goals
1. least memory cost.
2. query time complexity, insert time complexity, are not considered. Since the C++ map is super fast(4us) already.
3. text based readable db files, easy to debug.
### implementation
1. 80-20 rule is used to load the most frequent records once initialized.
2. Other records are loaded when needed. Maybe need some policy to load multiple records when an unloaded record is queried.
#### implementation details
* first line of the db file is some config information:
    total_freq
* A record is like this:
    ni 2032:你 322:尼 11:呢
* Most frequent records are kept in the front of the file.
* If the needed record are in the line n, the first n lines will be read. This is because the file can only be read line by line when every line has different size.
* Based on this feature, every time before closing the application will the `SearchTree` reorder the records and rewrite the whole file.
* So we will need a backup system, this could be done later.
