#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv)
{
  if (argc < 2)
    return;

  FILE * fin = fopen(argv[1], "r");
  FILE * fout = fopen(argv[2], "w");

  int ax,ay,az,gx,gy,gz;
  while (!feof(fin))
    {
      fscanf(fin, "%d %d %d %d %d %d\n", &ax, &ay, &az, &gx, &gy, &gz);
      fwrite(&ax,1,  sizeof(short), fout);
      fwrite(&ay, sizeof(short), 1,fout);
      fwrite(&az, sizeof(short), 1, fout);
      fwrite(&gx, sizeof(short), 1, fout);
      fwrite(&gy, sizeof(short), 1, fout);
      fwrite(&gz, sizeof(short), 1, fout);
    }

}
