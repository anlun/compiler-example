# include <stdio.h>

void write(int val) {
  printf("%d ", val);
}

int read() {
  int val;
  printf("> ");
  scanf("%d", &val);
  return val;
}
