#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <string.h>

/*******************************************
 *
 * Code written by Graphix. 
 * More information regarding this code,  other
 * hangman source code and words files
 * can be found on:
 *
 * http://www.hangman.symbolwebdesign.nl 
 *
 * Contact: info@symbolwebdesign.nl
 *
 ********************************************/
char fileLoc[500]; // The backup file location
void showLogo() {
  printf("--------------------------------------------\n");
  printf("| #  #   #   #   #  #### #   #   #   #   # |\n");
  printf("| #  #  # #  ##  # #     ## ##  # #  ##  # |\n");
  printf("| #### ##### # # # #  ## # # # ##### # # # |\n");
  printf("| #  # #   # #  ## #   # #   # #   # #  ## |\n");
  printf("| #  # #   # #   #  ###  #   # #   # #   # |\n");
  printf("--------------------------------------------\n\n");
}

void prn_galg(int i) {
  switch (i) {
  case 0 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("\n");
    printf("\n");
    printf("\n");
    printf("\n");
    printf("\n");
    printf("\n");
    printf("____________\n\n");
    break;
  case 1 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 2 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 3 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 4 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |\n");
    printf("  |\n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 5 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |    |\n");
    printf("  |    |\n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 6 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |   \\|\n");
    printf("  |    | \n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 7 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |   \\|/\n");
    printf("  |    | \n");
    printf("  |\n");
    printf("__|_________\n\n");
    break;
  case 8 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |   \\|/\n");
    printf("  |    | \n");
    printf("  |   /\n");
    printf("__|_________\n\n");
    break;
  case 9 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    O \n");
    printf("  |   \\|/\n");
    printf("  |    | \n");
    printf("  |   / \\\n");
    printf("__|_________\n\n");
    break;
  case 10 :
    printf("Amount of wrong letters: %d\n\n", i);
    printf("  _______\n");
    printf("  |/   | \n");
    printf("  |    X \n");
    printf("  |   \\|/\n");
    printf("  |    | \n");
    printf("  |   / \\\n");
    printf("__|_________\n\n");
    break;
  }
}

char randomNumber(int max_number) {
  srand(time(NULL));
  int g = (rand() % (max_number + 1));
  return g;
}

char *getWord() {
  char c[50000];
  int n;
  FILE *file;

  /* Opening words file */
  file = strcmp(fileLoc, "") != 1 ? fopen("words.txt", "r") : fopen(fileLoc, "r");

  /* In case the file cant be opened */
  if (file == NULL) {
    printf("Can't open words file.\n");
    return;
  }

  /* Read words file */
  n = fread(c, 1, 50000, file);
  c[n] = '\0';

  /* Separating the contents, divided by | and declaring variables */
  char *token = strtok(c, "|");
  char *words[200] = {0};
  int f = 0;
  while(token != NULL) {
    /* Allocating memory for the pointer */
    words[f] = malloc(strlen(token)+1);
   
    /* Copying entire string to pointer */
    strcpy(words[f],token);
    
    /* Resetting pointer */
    token = strtok(NULL, "|");

    f++;
  }

  /* Closing the file */
  fclose(file);

  /* Retrieving a random number */
  int wordN = randomNumber(f);
  
  /* Freeing all the memory allocated for the strings */
  int q;
  for (q = 0; q < 200; q++) {
    if (q != wordN) {
      free(words[q]);
    }
  }
   
  /* Returning string */
  return words[wordN];
}

int main(void) {

  /* Declaring the guessWord with the length of dkljafoue */
  char *guessWord = "dkljafoue";

  guessWord = getWord();

  printf("guessWord = %s\n",guessWord);

  /* Calculate the length of the guessWord */
  int wordlength = strlen(guessWord);

  /* Creating the dotword (name: currentWord) */
  char* currentWord = malloc(wordlength);

  int t;
  for (t = 0; t <= wordlength; t++) currentWord[t] = t == wordlength ? '\0' : '.';

  int errors = 0;
  int guessedLetter = 0;
  int i, n = 1;
  char c;

  /* Printing logo */
  showLogo();

  /* Printing introduction message */
  printf("%s\n\n%s\n%s\n%s\n%s\n\n%s%s\n\n",
         "Welcome to the game Hangman!",
         "The objective in this game is to guess the word.",
         "You can enter both uppercase and lowercase letters.",
         "If you think you know the word, you can type it in.",
         "You will lose if you have guessed 10 letters wrong.",
         "This is the word you need to guess: ",
         currentWord);
  printf("%d.     %s", n, "Enter the letter(s) you want to guess: ");

  /* As long as the word hasn't been guessed or the errors are lower than 10: */
  while((strcmp(currentWord, guessWord) != 0) && (errors < 10)) {

    /* Retrieving the user entry */
    scanf("%c", &c);
    c = tolower(c);
    if (c != '\n') {
      if (isalpha(c)) {

      /* Does the letter entered occur in the guessWord */
      for (i = 0; i < wordlength; i++) {
        if (guessWord[i] == c) {
          currentWord[i] = c;
          guessedLetter = 1;
        }
      }

      /* Actions taken when the letter doesn't occur in the guessWord and when it does */
      if (guessedLetter == 0) {
        errors++;
        printf("\nThat letter was incorrect.\n\n");
      } else {
        guessedLetter = 0;
        printf("\nThat letter was correct.\n\n");
      }

      /* Showing the galg and the amount of errors */
      printf("%s%s\n\n", "The word including the letters you guessed: ", currentWord);
      prn_galg(errors);
      n++;

      /* Showing header if the word has not been guessed and the errors are lower than 10 */
      if ((strcmp(currentWord, guessWord) != 0) && (errors < 10)) {
        printf("%d.     %s", n, "Enter the letter(s) you want to guess: ");
      }
    } else {
      printf("Only alphanumeric symbols are allowed (a-z, A-Z), try again:\n");
      }
    }
  }

  /* Showing the results, whether the player won or not  */
  printf("---------------\n");
  printf("--- Results ---\n");
  printf("---------------\n\n");

  if (strcmp(currentWord, guessWord) == 0) {
    printf("Congratulations you guessed the right word!\n\n");
  } else {
    printf("You guessed the wrong word. The word was %s. better luck next time!\n\n", guessWord);
  }
}
