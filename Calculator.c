#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include<stdlib.h>

#define L 6     //Lenght
#define TL 30   //Term Lenght

const double coeffSin[L] = {0, 1, 0, -1/6, 0, 1/120};
const double coeffSinh[L] = {0, 1, 0, 1/6, 0, 1/120};
const double coeffCos[L] = {1, 0, -1/2, 0, 1/24, 0};
const double coeffCosh[L] = {1, 0, 1/2, 0, 1/24, 0};
const double coeffTan[L] = {0, 1, 0, 1/3, 0, 2/15};
const double coeffTanh[L] = {0, 1, 0, -1/3, 0, 2/15};
const double coeffExp[L] = {1, 1, 1/2, 1/6, 1/24, 1/120};
const double coeffLn[L] = {0, 1, -1/2, 1/3, -1/4, 1/5};

typedef struct {
    double coeff[L];    //3, 1, 0, -5...
    char term[TL];      //y = 3x^2
} polynomial;           //3y^0 + 1y^1 + 0y^2 - 5y^3...


int solveToPoly(polynomial *poly, int *remXDiv);
int findPoly(char *term, double *coeff, int *remXDiv);
void deleteSpaces(char *term);
void multiplyPoly(double base[], double factor[]);



int main() {
    char isInfinite = 0;
    polynomial poly;
    double sol = 0.;
    int remXDiv = 0;        //Remaining x divided for division

    for (int c = 0; c < L; c++)     //Initializing poly as y^1
        poly.coeff[c] = 0.;
    poly.coeff[1] = 1.;

    while (1) {
        printf("\nEnter the problem : ");
        gets(poly.term);

        if (!strcmp("Exit", poly.term) || !strcmp("exit", poly.term)) {
            printf("Exiting");
            break;
        }
        if (!strcmp("Help", poly.term) || !strcmp("help", poly.term)) {
            printf("+ : Addition\n- : Substraction\n* : Multiplication\n/ : Division\n^ : Power\n\n");
            printf("Usable functions :\nSin(x)\nSinh(x)\nCos(x)\nCosh(x)\nTan(x)\nTanh(x)\nExp(x)");
            continue;
        }
        if (solveToPoly(&poly, &remXDiv) == 1)
            continue;

        //Check if there is any x or 1/x factors as they would instantly make solution 0 or infinite 
        for (int c = 0; c < L; c++) {
            if (poly.coeff[c] != 0) {       //Find the smallest a in x^a
                sol = poly.coeff[c];
                if (c < remXDiv)            //Check if the smallest a in x^a is smaller than the smallest b in 1/x^b
                    isInfinite = 1;
                break;
            }
            if (c >= remXDiv) {             //Check if the smallest a is bigger than the smallest b
                sol = 0.;
                break;
            }
            remXDiv = 0;
        }

        if (isInfinite) {
            if (sol < 0)
                printf("Solution : Minus Infinite");
            else
                printf("Solution : Infinite");
            isInfinite = 0;
        }
        else
            printf("Solution : %g\n", sol);
        sol = 0.;
    }

    return 0;
}



int solveToPoly(polynomial *poly, int *remXDiv) {
    double coeffIns[L];                     //Coefficients of inside polinomial
    if (findPoly(poly->term, coeffIns, remXDiv) == 1) 
        return 1;
    double coeffTemp[L] = {0.};
    double coeffRes[L] = {poly->coeff[0]};   //Coefficients of result polynome
    double coeffLast[L] = {0.};              //Coefficients of last used polynome
    for (int c = 0; c < L; c++) {
        coeffTemp[c] = 0.;
        coeffLast[c] = coeffIns[c];
        coeffRes[c] += coeffIns[c];
    }

    for (int t = 2; t < L; t++) {                       //Term counter of first polynome, x^t, 0 gives back {k, 0, 0, 0, 0, 0}, 1 gives back original coefficients
        for (int tc1 = 0; tc1 < L; tc1++) {             //First term counter of second polynome
            for (int tc2 = 0; tc1 + tc2 < L; tc2++) {   //Second term counter of second polynome
                coeffTemp[tc1 + tc2] += poly->coeff[t] * coeffLast[tc1] * coeffIns[tc2];    //Multiply coefficients and find sum of each term based on x^a
            }
        }
        for (int c = 0; c < L; c++) {
            coeffLast[c] = coeffTemp[c];
            coeffRes[c] += coeffTemp[c];
            coeffTemp[c] = 0.;
        }
    }
    for (int c = 0; c < L; c++) {
        poly->coeff[c] = coeffRes[c];
    }
    poly->term[0] = 'x';
    poly->term[1] = '\0';
    return 0;
}



int findPoly(char *term, double *coeff, int *remXDiv) {
    deleteSpaces(term);
    double number = 0.;
    double coeffTemp[L] = {0.};
    double coeffOrg[L] = {0.};
    int power = 0;
    double factor = 1.;
    polynomial poly, polyOld, polyRes;
    for (int c = 0; c < L; c++) {
        poly.coeff[c] = 0.;
        polyRes.coeff[c] = 0.;
    }
    poly.coeff[0] = 1.;
    int brackets = 0;
    int termLenght = 0;
    char storedOp = 0;
    char isNeg = 0;
    int i = 0, j = 0;
    while (i <= TL) {
        termLenght = 0;
        if (isalnum(term[i])) {
            multiplyPoly(polyOld.coeff, poly.coeff);
            for (int c = 0; c < L; c++)
                poly.coeff[c] = 0.;
            poly.coeff[0] = 1.;
            if (isalpha(term[i])) {
                switch (term[i]) {      //Find the expected coefficients of the taylor serie version of the current term
                    case 'x':
                        termLenght = 1;
                        for (int c = 0; c < L; c++)
                            poly.coeff[c] = 0.;
                        poly.coeff[1] = 1.;
                        poly.term[0] = 'x';
                        poly.term[1] = '\0';
                        break;
                    case 's':       //Sin
                        termLenght = 3;
                        if (term[i + termLenght] == 'h') {      //Sinh
                            termLenght++;
                            for(int c = 0; c < L; c++)
                                poly.coeff[c] = coeffSinh[c];
                        }
                        else {
                            for(int c = 0; c < L; c++)
                                poly.coeff[c] = coeffSin[c];
                        }
                        break;

                    case 't':       //Tan
                        termLenght = 3;
                        if (term[i + termLenght] == 'h') {      //Tanh
                            termLenght++;
                            for(int c = 0; c < L; c++)
                                poly.coeff[c] = coeffTanh[c];
                        }
                        else {
                            for(int c = 0; c < L; c++)
                                poly.coeff[c] = coeffTan[c];
                        }
                        break;

                    case 'c':
                        termLenght = 2;
                        if (term[i + termLenght] == 's') {         //Cos
                            termLenght++;
                            if (term[i + termLenght] == 'h') {      //Cosh
                                termLenght++;
                                for(int c = 0; c < L; c++)
                                    poly.coeff[c] = coeffCosh[c];
                            }
                            else {
                                for(int c = 0; c < L; c++)
                                    poly.coeff[c] = coeffCos[c];
                            }
                        }
                        else if (term[i + termLenght] == 't') {    //Cot
                            printf("Does not support cot!\n");
                            return 1;
                        }
                        else {    
                            printf("Error, unclear syntaxe! 1\n");
                            return 1;
                        }
                        break;

                    case 'e':       //Exp
                        termLenght = 3;
                        for(int c = 0; c < L; c++)
                            poly.coeff[c] = coeffSin[c];
                        break;

                    case 'l':       //Ln
                        printf("Does not support ln yet!\n");
                        return 1;
                    
                    default:
                        printf("Error, unclear syntaxe! 2\n");
                        return 1;
                }
                if (termLenght) {
                    if (term[i + termLenght] == '^') {
                        termLenght++;
                        while(isdigit(term[i + termLenght])) {          //Convert digits to double
                            power *= 10;
                            power += term[i + termLenght++] - '0';
                        }
                        for (int c = 0; c < L; c++)
                            coeffOrg[c] = poly.coeff[c];                //Store original coefficients for multiplication
                        for (int p = 1; p < power; p++) {               //P^1(x) is the same polynome so we pass 1
                            for (int tc1 = 0; tc1 < L; tc1++) {
                                for (int tc2 = 0; tc1 + tc2 < L; tc2++)
                                    coeffTemp[tc1 + tc2] += poly.coeff[tc1] * coeffOrg[tc2];    //Multiply P^n(x) with P(x) until n reachs p
                            }
                            for (int c = 0; c < L; c++) {
                                poly.coeff[c] = coeffTemp[c];
                                coeffTemp[c] = 0;
                            }
                        }
                    }
                    if (term[i + termLenght] == '(' && strcmp(poly.term, "x")) {
                        termLenght++;
                        brackets = 1;
                        for (j = 0; term[i + termLenght] != '\0'; j++, termLenght++) {      //Gather the term until current bracket ends
                            if (term[i + termLenght] == '(')
                                brackets++;
                            if (term[i + termLenght] == ')') {
                                brackets--;
                                termLenght++;
                                if (brackets < 1)
                                    break;
                            }
                            poly.term[j] = term[i + termLenght];
                        }
                        poly.term[j] = '\0';

                        if (strcmp(poly.term, "x")) {           //If term different than "x" solve it
                            if (solveToPoly(&poly, remXDiv) == 1)
                                return 1;
                        }
                    }
                    else if (strcmp(poly.term, "x")) {          //If term didn't include brackets or is different than "x" print error
                        printf("Error, unclear syntaxe! 3\n");
                        return 1;
                    }
                }
            }
            else {
                while (isdigit(term[i + termLenght])) {         //Translate the number in string to double
                    number *= 10;
                    number += (double) (term[i + termLenght++] - '0');
                }
                



                if (term[i + termLenght] == '+' || term[i + termLenght] == '-')
                    poly.coeff[0] += number;
                else if (term[i + termLenght] == '*' || term[i + termLenght] == '/')
                    factor = number;
                number = 0;
            }
        }
        else if (term[i] == '+' || term[i] == '-' || term[i] == '*' || term[i] == '/' || term[i] == '\0') {

            switch (storedOp) {
                case 0:
                    storedOp = term[i];
                    polyOld = poly;
                    if (storedOp == '-')
                        isNeg = 1;
                    break;
                
                case '+':
                    if (isNeg) {
                        for (int c = 0; c < L; c++)
                            polyRes.coeff[c] -= polyOld.coeff[c];
                        isNeg = 0;
                    }
                    else {
                        for (int c = 0; c < L; c++) 
                            polyRes.coeff[c] += polyOld.coeff[c];
                    }
                    for (int c = 0; c < L; c++) {
                        polyOld.coeff[c] = factor * poly.coeff[c];
                        poly.coeff[c] = 0.;
                    }
                    poly.coeff[0] = 1.;
                    break;
                
                case '-':
                    if (isNeg) {
                        for (int c = 0; c < L; c++)
                            polyRes.coeff[c] -= polyOld.coeff[c];
                    }
                    else {
                        for (int c = 0; c < L; c++) 
                            polyRes.coeff[c] += polyOld.coeff[c];
                        isNeg = 1;
                    }
                    for (int c = 0; c < L; c++) {
                        polyOld.coeff[c] = factor * poly.coeff[c];
                        poly.coeff[c] = 0.;
                    }
                    poly.coeff[0] = 1.;
                    break;
            
                case '*':
                    for (int tc1 = 0; tc1 < L; tc1++) {
                        for (int tc2 = 0; tc1 + tc2 < L; tc2++)
                            coeffTemp[tc1 + tc2] += polyOld.coeff[tc1] * poly.coeff[tc2];
                    }
                    for (int c = 0; c < L; c++) {
                        polyOld.coeff[c] = coeffTemp[c];
                        coeffTemp[c] = 0;
                    }
                    break;
                
                case '/':
                    for (int c1 = 0; c1 < L; c1++) {
                        if(poly.coeff[c1]) {
                            for (int c2 = 0; c2 < L; c2++)
                                polyOld.coeff[c2] /= poly.coeff[c1];
                            break;
                        }
                        (*remXDiv)++;
                    }
                    break;
                default:
                    break;
            }
            termLenght = 1;
            storedOp = term[i];
            if (storedOp == '\0')
                break;
        }
        else {    
            printf("Error, unclear syntaxe! 4\n");
            return 1;
        }
        if (termLenght)
            i += termLenght;
        else
            i++;
    }
    if (isNeg) {
        for (int c = 0; c < L; c++)
            polyRes.coeff[c] -= polyOld.coeff[c];
    }
    else {
        for (int c = 0; c < L; c++) 
            polyRes.coeff[c] += polyOld.coeff[c];
    }
    for (int c = 0; c < L; c++)
        coeff[c] = polyRes.coeff[c];
    return 0;
}

//For multiplying 2 polynomials
void multiplyPoly(double base[], double factor[]) {
    double temp[L];
    for (int b = 0; b < L; b++) {
        for (int f = 0; f < L; f++) {
            temp[b + f] = base[b] * factor[f];
        }
    }
    for (int c = 0; c < L; c++)
        base[c] = temp[c];
}

void deleteSpaces(char *term) {
    for (int i = 0, j = 0; i < TL; i++) {
        if (term[i] != ' ') {
            term[j++] = term[i];
        }
        if (term[i] == '\0')
            break;
    }
}


