#include "mycpp.h"

#include <iostream>
#include <cmath>

void simple()
{
    std::cout << "Hello form C++\n";
}

void easy(int a, int b)
{
    std::cout << "a = " << a << " b = " << b << "\n";
}

void no_problem(float a, float b, float* c)
{
    std::cout << "a = " << a << " b = " << b << "\n";
    *c = a + b;
    std::cout << "c = " << *c << "\n";
}

void no_problemas(float a, float b, float& c)
{
    std::cout << "a = " << a << " b = " << b << "\n";
    c = a + b;
    std::cout << "c = " << c << "\n";
}

void many_numbers(float* a, float* b, float* c, int n)
{
    for (auto i=0; i<n; i++)
        c[i] = a[i] + b[i];
}

double myfunc(double x)
{
    return sin(x);
}
