## Syntax and semantics ##
`(define-syntax ...)` ; defines a macro

`(define-syntax-rule (id . pattern) template)` ; defines a macro

`(defmacro id formals body ...+)` ; defines a (non-hygienic) macro id through a procedure that manipulates S-expressions, as opposed to syntax objects.

`(include "file1.h" ...)` => `#include "file1.h" ...`

`(defn int main (int argc char **argv) (...))` => `int main(int argc, char **argv) {...}`

`(def a 3 b 4.0 ...)` => `auto a = 3; auto b = 4.0; ...`

`(decl TYPE VAR [VAL])` => `TYPE VAR[=VAL]` ; declares a variable

`(+ A B C ...)` => `(A + B + C + ...)` (`+ - * / << >> =`)

`(++ A)` => `(++ A)` ; unary operators (`++ -- not compl`)

`(< A B)` => `(A < B)` ; binary operators (`< <= > >= == != % += -= *= /= %= and and_eq bitand bitor not_eq or or_eq xor xor_eq`)

`(return A)` => `return A`

`(? TEST THEN ELSE)` => `(TEST ? THEN : ELSE)`

`(if TEST THEN [ELSE])` => `if (TEST) THEN; [else ELSE]`

`(when TEST THEN ...)` => `if (TEST) {THEN; ...;}`

`(while TEST BODY ...)` => `while (TEST) {BODY; ...;}`

`(for INIT TEST STEP BODY ...)` => `for (INIT; TEST; STEP) {BODY; ...;}`

`(foreach VAR CONTAINER BODY ...)` => `for (auto &VAR : CONTAINER) {BODY; ...;}`

`(do BODY ...)` => `{BODY; ...;}`

`(do/e EXPR ...)` => `(EXPR, ...)`

`(at ARRAY [POSITION])` => `ARRAY[[POSITION]]`

`(break)` => `break` (`break continue`)

`(main BODY ...)` => `int main(int argc, char **argv) {BODY; ...; return 0;}`

`(pr A ...)` => `std::cout << A << ...`

`(prn A ...)` => `std::cout << A << ... << std::endl`

`(label ID)` => `ID:`

`(goto ID)` => `goto ID`

`(switch EXPR BODY ...)` => `switch (EXPR) {BODY; ...;}`

`(case EXPR ...)` => `case EXPR: case ...:`

`(default)` => `default:`

`(fn (int a int b) (return (+ a b)))` => `[&](int a, int b) {return a + b;}`

`(code "CODE")` => `CODE` as-is

`|CODE|` => `CODE` as-is

`(format form ...)` ; compile-time formatting

`(F ARG ...)` => `F(ARG, ...)`

`#\A` => `'A'`

See [the source code](https://bitbucket.org/ktg/l/src) for details.