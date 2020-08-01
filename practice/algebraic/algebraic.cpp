#include <iostream>
#include <utility>
#include <variant>

enum alg_type { LEFT = 1, RIGHT = 2 };

template<typename L, typename R>
using algebraic = std::pair<int, std::variant<L, R>>;

template<typename L, typename R>
static algebraic<L, R> in_L(L var) {
    return std::make_pair(LEFT, var);
}

template<typename L, typename R>
static algebraic<L, R> in_R(R var) {
    return std::make_pair(RIGHT, var);
}

template<typename L, typename R, typename X>
X alg_case(algebraic<L, R> sum, X (*lfunc)(L), X (*rfunc)(R)) {
    switch (sum.first) {
        case LEFT:
            return lfunc(std::get<0>(sum.second));
        case RIGHT:
            return rfunc(std::get<1>(sum.second));
    }
}

bool is_odd(int x) {
    return x % 2;
}

bool is_letter_a(char x) {
    return x == 'a' || x == 'A';
}

int main() {
    auto x = in_L<int, char>(15), y = in_R<int, char>('A');
    std::cout << alg_case<>(x, is_odd, is_letter_a) << std::endl;
    std::cout << alg_case<>(y, is_odd, is_letter_a) << std::endl;

    return 0;
}
