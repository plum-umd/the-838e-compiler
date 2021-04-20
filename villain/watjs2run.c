#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
int main (int argc, char *argv[]) {
    int n = strlen(argv[1]);
    char filename[n - 5];
    char cfilename[3 + n];    
    char line1[1000 + n];
    char line2[1000 + n];
    char command1[1000 + n];
    char command2[1000 + n];
    char command3[1000 + n];
    FILE * fp;
    char buf[100];
    if (!getcwd(buf, 99)) {
        return 1;
    }
    strncpy(filename, argv[1], (n - 5));
    filename[n - 5] = '\0';
    sprintf(cfilename, "%s%s", filename, ".c");
    fp = fopen(cfilename, "a+");
    fputs("#include <stdlib.h>\r\n", fp);
    fputs("int main (int argc, char *argv[]) {\r\n", fp);
    sprintf(line1, "%s %s%s %s%s", "\r\n   system(\"wat2wasm ", filename, ".wat -o ", filename, ".wasm\"); \r\n");
    fputs(line1, fp);
    sprintf(line2, "%s %s%s %s%s", "   system(\"node ", buf, "/jsmain.js ", filename, ".wasm\"); \r\n}\r\n");
    fputs(line2, fp);
    fclose(fp);
    sprintf(command1, "%s %s %s %s%s", "gcc ", cfilename, " -o ", filename, ".wrun");
    system(command1);
    sprintf(command2, "%s %s", "rm ", cfilename);
    system(command2);
}