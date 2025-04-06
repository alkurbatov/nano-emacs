#include <iostream>
#include <string>

class A {
public:
  int a = {0};
};

int main(int argc, char **argv)
{
  std::string s = "abc";
  std::cout << s << s.size() << '\n';

  A a;
  std::cout << a.a << '\n';

  return 0;
}
