#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <random>
#include <cmath>

//
// dy = WdW
//
float integrate(float t0, float t1, int n, bool stratonovich) {
    std::random_device rd{};
    std::mt19937 gen{rd()};
 
    // values near the mean are the most likely
    // standard deviation affects the dispersion of generated values from the mean
    std::normal_distribution<> d{0,1};

    float dt = (t1-t0)/float(n);
    float I = 0;
    float W = 0;
    for (int i = 0; i < n; ++i) {
        float dW = d(gen)*std::sqrt(dt);
        if (stratonovich) {
            I += (W+0.5*dW)*dW; // Stratonovich integral
        } else {
            I += W*dW; // Ito integral
        }
        W += dW;
    }

    std::cout << "I = " << I << " W = " << W << " W^2/2-0.5 = " << W*W/2 << std::endl;

    return I;
}

int main() {
    float sI = 0.0f;
    float sII = 0.0f;
    int n = 1000;
    for (int i = 0; i < n; ++i) {
        float I = integrate(0.0, 1.0, 1000, false);
        sI += I;
        sII += I*I;
    }

    std::cout << sI/float(n) << ' ' << sII/float(n)-(sI/float(n))*(sI/float(n)) << std::endl;
}
