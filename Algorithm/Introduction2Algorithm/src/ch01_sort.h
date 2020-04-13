#pragma once

#include <vector>
#include <iostream>
#include "test.h"

namespace i2a {

void insert_sort(Array<Key>& A);
void merge_sort(Array<Key>& A);
void merge_sort_(Array<Key>& A, int p, int r);
void merge(Array<Key>& A, int p, int q, int r);

} /* namespace i2a */
