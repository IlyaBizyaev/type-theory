#include <stdio.h>
#include <stdbool.h>

enum alg_type {
    LEFT = 1,
    RIGHT = 2
};

struct algebraic {
    int type;
    union {
        int l; char r;
    };
};

struct algebraic in_L(int v) {
    struct algebraic result = {.l = v, .type = LEFT};
    return result;
}

struct algebraic in_R(char v) {
    struct algebraic result = {.r = v, .type = RIGHT};
    return result;
}

bool alg_case(struct algebraic sum, bool (*lfunc)(int), bool (*rfunc)(char)) {
    switch (sum.type) {
        case LEFT:
            return lfunc(sum.l);
        case RIGHT:
            return rfunc(sum.r);
    }
}

bool is_odd(int x) {
    return x % 2;
}

bool is_letter_a(char x) {
    return x == 'a' || x == 'A';
}

int main() {
    struct algebraic x = in_L(15), y = in_R('A');
    printf("%d\n", alg_case(x, is_odd, is_letter_a));
    printf("%d\n", alg_case(y, is_odd, is_letter_a));

    return 0;
}
