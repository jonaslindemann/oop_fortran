#ifndef MYCPP_H
#define MYCPP_H

extern "C" {
    void simple();
    void easy(int a, int b);
    void no_problem(float a, float b, float* c);
    void no_problemas(float a, float b, float& c);
    void many_numbers(float* a, float* b, float* c, int n);
    double myfunc(double x);
}

#endif // MYCPP_H
