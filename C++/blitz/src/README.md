# Roadmap
1. a database.
2. Markov chain implementation.

# TODO
- [x] embed GBK.txt into our .so file.
This could be done with `objcopy` command.

## Build help:
### gbk.o
1. check the architecture:
    objdump -x core.o | grep architecture
    >> i386:x86-64

2. build `gbk.o`:
    objcopy -I binary -O elf64-x86-64 -B i386:x86-64 gbk.txt gbk.o

3. check link symbol:`objdump -x gbk.o`, you can find two symbol:`_binary_gbk_txt_start`, `_binary_gbk_txt_end`
4. to use them in our codes:
    extern char _binary_gbk_txt_start;
    extern char _binary_gbk_txt_end;
To be careful `_binary_gbk_txt_end` is not a variable, and you can declare it as any type, in our case, it's string.

Don't use `_binary_gbk_txt_end` directly! Here is an example:
    char *p = &_binary_gbk_txt_start;
    cout << p << endl;
It will output `gbk.txt` file.
