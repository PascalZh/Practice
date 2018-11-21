#!/bin/bash
cpp=$(find . -name "*.cpp"\
  -not -path "./MCU/ESP32/*"\
  |xargs cat|grep -v ^$|wc -l)
h=$(find . -name "*.h"\
  -not -path "./MCU/ESP32/*"\
  |xargs cat|grep -v ^$|wc -l)
c=$(find . -name "*.c"\
  -not -path "./MCU/ESP32/*"\
  |xargs cat|grep -v ^$|wc -l)
cpp_code_cnt=`expr $cpp + $h + $c`
if [[ $cpp_code_cnt -gt 0 ]]; then
  echo -e "$cpp_code_cnt\tc++/c"
fi

#js_code_cnt=$(find . -name "*.js"|xargs cat|grep -v ^$|wc -l)
#if [[ $js_code_cnt -gt 0 ]]; then
  #echo -e "$js_code_cnt\tjavascript"
#fi

html=$(find . -name "*.html" -o -name "*.css"|xargs cat|grep -v ^$|wc -l)
if [[ $html -gt 0 ]]; then
  echo -e "$html\thtml or css"
fi

rkt=$(find . -name "*.rkt"|xargs cat|grep -v ^$|wc -l)
if [[ $rkt -gt 0 ]]; then
  echo -e "$rkt\tracket"
fi

py=$(find . -name "*.py"|xargs cat|grep -v ^$|wc -l)
if [[ $py -gt 0 ]]; then
  echo -e "$py\tpython"
fi

sh=$(find . -name "*.sh"|xargs cat|grep -v ^$|wc -l)
if [[ $sh -gt 0 ]]; then
  echo -e "$sh\tshell"
fi

sum=`expr $cpp_code_cnt + $html + $rkt + $py + $sh`
echo -e "\n$sum\ttotal (without blank lines)"
