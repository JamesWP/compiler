#include <stdlib.h>
#include <stdio.h>

extern char my_data_bytes_start;
extern char my_data_bytes_end;

int main(int argc, char* argv[]) {

  printf("Data:\n");

  for(char* c = &my_data_bytes_start; c != &my_data_bytes_end; c++) {
    printf("%xc\n", *c);
  }

  return 0;
}
