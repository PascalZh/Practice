class Solution
{
public:
  int integerBreak(int n)
  {
    if (n == 2)
      return 1;
    if (n == 3)
      return 2;
    int a = 1;
    while (n > 4) {
      n = n - 3;
      a = a * 3;
    }
    return a * n;
  }

  int pivotIndex(vector<int>& nums)
  {
    int sum = 0;
    int len = nums.size();
    int left_sum = 0;
    for (int i = 0; i < len; ++i)
      sum += nums[i];
    for (int i = 0; i < len; ++i) {
      if (left_sum == sum - left_sum - nums[i])
        return i;
      left_sum += nums[i];
    }
    return -1;
  }
};
