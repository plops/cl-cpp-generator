#include <cstddef>
#include <vector>
int main(int argc, const char **argv) {
  std::vector<int> v;
  static const int max_vec_size = 256;

  for (int i = 0; i < max_vec_size; i += 1) {
    v.push_back(i);
  }

  return 0;
}
