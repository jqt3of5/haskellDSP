#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv)
{
  /* if (argc != 4) */
  /*   return 1; */

  FILE * fin = fopen(argv[1], "r");
  FILE * fout0 = fopen(argv[2], "w");
  FILE * fout1 = fopen(argv[3], "w");

  short gx,gy,gz,ax,ay,az;
  fscanf(fin, "%hd %hd %hd %hd %hd %hd\n", &ax, &ay, &az, &gx, &gy, &gz);
  while (!feof(fin))
    {
      
      fwrite(&gx, sizeof(short), 1, fout0);
      fwrite(&gy, sizeof(short), 1, fout0);
      fwrite(&gz, sizeof(short), 1, fout0);

      fwrite(&ax, sizeof(short), 1, fout1);
      fwrite(&ay, sizeof(short), 1, fout1);
      fwrite(&az, sizeof(short), 1, fout1);
      fscanf(fin, "%hd %hd %hd %hd %hd %hd\n", &ax, &ay, &az, &gx, &gy, &gz);


    }


}
