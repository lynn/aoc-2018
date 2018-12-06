/* https://adventofcode.com/2018/day/6 in rustic, homespun C89. */
#include <stdlib.h>
#include <stdio.h>

typedef struct { int x; int y; } Point;

/* Ha, you thought I was gonna read these with scanf()? */
/* YOu fool. you absolute clown. I don't know how to use scanf() */
const Point points[] = {{242, 164}, {275, 358}, {244, 318}, {301, 335}, {310, 234}, {159, 270}, {82, 142}, {229, 286}, {339, 256}, {305, 358}, {224, 339}, {266, 253}, {67, 53}, {100, 143}, {64, 294}, {336, 303}, {261, 267}, {202, 86}, {273, 43}, {115, 256}, {78, 356}, {91, 234}, {114, 146}, {114, 260}, {353, 346}, {336, 283}, {312, 341}, {234, 119}, {281, 232}, {65, 203}, {95, 85}, {328, 72}, {285, 279}, {61, 123}, {225, 179}, {97, 140}, {329, 305}, {236, 337}, {277, 110}, {321, 335}, {261, 258}, {304, 190}, {41, 95}, {348, 53}, {226, 298}, {263, 187}, {106, 338}, {166, 169}, {310, 295}, {236, 191}};

/* I eyeballed that it's a 1000x1000 grid. */
#define GRID_SIZE 1000
#define NUM_POINTS (sizeof(points) / sizeof(Point))
#define DISTANCE(x1,y1,x2,y2) (abs((x1) - (x2)) + abs((y1) - (y2)))

/* Return the index of the closest point to (x, y), or -1 in case of a tie. */
int closest(int x, int y) {
    int best_index = -1;
    int best_distance = 99999;
    int tie = 0;
    int i;

    for (i = 0; i < NUM_POINTS; i++) {
        int distance = DISTANCE(x, y, points[i].x, points[i].y);
        if (distance == best_distance) {
            tie = 1;
        } else if (distance < best_distance) {
            best_index = i;
            best_distance = distance;
            tie = 0;
        }
    }
    return tie ? -1 : best_index;
}

int main() {
    int is_infinite[NUM_POINTS] = {0};
    int area[NUM_POINTS] = {0};
    int i, k, x, y;

    /* The only clever part: probe a border of points waaay outside the grid. */
    /* All the points with an infinite neighborhood must still show up there! */
    /* Basically, we read along this white border: https://i.imgur.com/IINYpur.png */
    /* (For the Manhattan metric, a GRID_SIZE margin is more than enough.) */
    const int margin = GRID_SIZE;
    const int lo = 0 - margin;
    const int hi = GRID_SIZE + margin;
    for (k = lo; k < hi; k++) {
        i = closest(lo, k); if (i >= 0) is_infinite[i] = 1;
        i = closest(hi, k); if (i >= 0) is_infinite[i] = 1;
        i = closest(k, lo); if (i >= 0) is_infinite[i] = 1;
        i = closest(k, hi); if (i >= 0) is_infinite[i] = 1;
    }

    /* Count areas. */
    for (y = 0; y < GRID_SIZE; y++)
    for (x = 0; x < GRID_SIZE; x++) {
        i = closest(x, y);
        if (i >= 0 && !is_infinite[i])
            area[i]++;
    }

    /* Part A: what's the largest finite area? */
    {
        int largest_area = 0;
        for (i = 0; i < NUM_POINTS; i++)
            if (!is_infinite[i] && area[i] > largest_area)
                largest_area = area[i];
        printf("largest_area: %d\n", largest_area);
    }

    /* Part B: how many grid points are "safe" (sum of distances < 10000)? */
    {
        const int threshold = 10000;
        int safe_area = 0;
        for (y = 0; y < GRID_SIZE; y++)
        for (x = 0; x < GRID_SIZE; x++) {
            int sum_distance = 0;
            for (i = 0; i < NUM_POINTS && sum_distance < threshold; i++)
                sum_distance += DISTANCE(x, y, points[i].x, points[i].y);
            if (sum_distance < threshold)
                safe_area++;
        }
        printf("safe_area: %d\n", safe_area);
    }
}
