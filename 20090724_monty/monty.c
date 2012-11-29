#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int *simulation(int n)
{
    int win_with_switch_cup = 0;
    int win_with_stay   = 0;

    srand(time(NULL));

    int i = 0;
    for (i = 0; i < n; i++) {
        int cash   = 0;
        int choice = 0;
        cash   = rand()%3;
        choice = rand()%3;
 
        int flipped_over = 0;

        int a = 0;
        for (a = 0; a < 3; a++) {
            if ((a != cash) && (a != choice)) {
                flipped_over = a;
            }
        }

        int switch_cup = 0;
        switch_cup = rand()%(2);
        if (switch_cup) {
            for (a = 0; a < 3; a++) {
                if (a != choice && a != flipped_over) {
                    choice = a;
                    break;
                }
            }
        }


        if (choice == cash) {
            if (switch_cup) {
                win_with_switch_cup++;
            } else {
                win_with_stay++;
            }
        }
    }
    
    int *wins = malloc(2 * sizeof(int));
    wins[0] = win_with_switch_cup; 
    wins[1] = win_with_stay; 

    return wins;
}

int main(void){
    int n = 10000;

    int *w = simulation(n);
    
    printf("(%d, %d)\n", w[0], w[1]);

    free(w);
    return 0;
}

