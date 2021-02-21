#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main (int argc, char *argv[]) {
	char * two = "2";
	char * cspace = " ";
        char * move = "mv ";
	char makerun2[100];
        char * tmp;
        char mvrun2[100];
        int len;
        if (strcmp(argv[1], "mv") == 0){
           len = strlen(argv[2]);
           if (!(tmp = malloc(len))) {
              printf("malloc error");
           }
           len--;
           strncpy(tmp, argv[2], len);
           tmp[len] = '\0';
           strcpy(mvrun2, argv[1]);
           strcat(mvrun2, cspace);
           strcat(mvrun2, argv[2]);
           strcat(mvrun2, cspace);
           strcat(mvrun2, tmp);
           // printf("mvrun2 branch\n");
           printf("%s\n", mvrun2);
           system(mvrun2);
        } else {
	   strcpy(makerun2, argv[1]);
	   strcat(makerun2, cspace);
	   strcat(makerun2, argv[2]);
	   strcat(makerun2, two);
	   printf("%s\n", makerun2);
	   system(makerun2);
        }
}
