#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main (int argc, char *argv[]) {
	char * two = "2";
	char * cspace = " ";
        char * move = "mv ";
	char * makerun2;
        char * tmp;
        char * mvrun2;
        int len;
        if (strcmp(argv[1], "mv") == 0){
           len = strlen(argv[2]);
           if (!(tmp = malloc(len))) {
              printf("malloc error");
           }
           len--;
           strncpy(tmp, argv[2], len);
           tmp[len] = '\0';
           if (!(mvrun2 = malloc (2 * len + 6))){
              printf("malloc error");
           }
           strcpy(mvrun2, argv[1]);
           strcat(mvrun2, cspace);
           strcat(mvrun2, argv[2]);
           strcat(mvrun2, cspace);
           strcat(mvrun2, tmp);
           // printf("mvrun2 branch\n");
           printf("%s\n", mvrun2);
           system(mvrun2);
        } else {
           len = strlen(argv[2]);
           if (!(makerun2 = malloc (len + 7))){
              printf("malloc error");
           }
	   strcpy(makerun2, argv[1]);
	   strcat(makerun2, cspace);
	   strcat(makerun2, argv[2]);
	   strcat(makerun2, two);
	   printf("%s\n", makerun2);
	   system(makerun2);
        }
}
