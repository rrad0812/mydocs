# C programiranje 12

## There's One Born Every Minute

Al Stevens, juli '89

It is now time to put the SMALLCOM project into the cask and let it age. Some of you are using it and sending in comments, suggestions, and problems. The project has spanned ten months, and newer readers are finding it necessary to get back issues to catch up. I might update the project in a future column and add more functions to it. For now, though, vacation is upon us, and time has come to reveal our hidden hedonism with a trip to the islands and the tables of chance.

Rodney Dangerfield says that he went to Las Vegas in a $25,000 automobile and came home in a $200,000 bus. I went to Puerto Rico to build an income tax system and came home with a Blackjack simulation program.

San Juan, Puerto Rico has gambling casinos. My pal Fast Eddie Dwyer lives there, deals with the Puerto Rican version of the IRS by day, and pokes around the casinos by night. Armed with a book on how to win at Blackjack, he has planned an early retirement. With luck, however, he will merely have a lot of fun spending (losing) not a lot of money. Notice the operative word -- luck.

I'm no gambler, but I'm drawn to a challenging problem. I decided to test Fast Eddie's Blackjack systems without risking any of my own dough. What better way than with computer simulation in a C program? The program in [Listing 1](#listing-1) is "bj.c", and it models the game and some of the strategies being taught by the self-proclaimed masters. My conclusions about the viability of these strategies can be seen in the results delivered by the program. In the long haul, you cannot win. Not everyone will agree.

There was a time when you could find a Blackjack game at some casinos that used one deck. If you were able to mentally master all the strategies, you could win consistently. The casinos learned this and changed some of the rules, one of them being the number of decks in the play. Now they use six decks, and the strategies, even if you can execute them flawlessly, which no one can, do not work. Fast Eddie still has a day job.

The casinos all have signs that forbid the use of personal computers and other electronic aids. The signs are there to convince you that "systems" can work, to make you want to try, to get you to play, to get your money. You could take all the electronic help you wanted. You'd still lose.

There are three fundamental strategies touted to make you a Blackjack winner. The first strategy involves observing your hand and the dealer's up card. Based on a chart (provided in Fast Eddie's how-to book) you decide whether to hit or stand. If your first two cards are the same denomination, another chart tells you whether you should split the hand. When you get the second card, a third chart tells you if you should double-down the hand. The trick is memorizing all three charts and being able to implement them instantly during the fast pace of the deal. To complicate matters, the charts are different depending on whether you are playing in Nevada, Atlantic City, or the Caribbean.

The second strategy involves counting cards. You keep a running count of two groups of cards. The first group is the good group, and it consists of the face cards, tens, and aces. The second group is bad. It is made up of all the other cards, deuces through nines. You have to watch all the cards that were dealt to you, the dealer, and all the other players. You have to mentally compute a running balance of the two groups to know what ratio of good to bad cards are left in the undealt portion of the deck. The more good cards, the better your chances of winning. The more bad cards, the more likely you are to lose. When the ratio gets to a certain level in your favor, you increase your bet. When the ratio goes far enough against you, you stop playing. The ratio you apply is a function of the total number of cards in play. As play proceeds you add one to a running total each time you see a good card. You subtract one each time you see a bad card. You apply this running total to the total number of cards to figure the ratio of good to bad cards left in the deck. That is the essence of card counting. I no longer have the specifics because Fast Eddie kept the book when I left San Juan. He's still trying.

The third strategy is called money management. You change your bets based on your recent win/loss ratio. I never understood this one, and could not simulate it properly. Fast Eddie and I have a pal who believes in it fervently. The last time he was in San Juan, he borrowed money to get home.

I simulated both of the first two strategies for a while. Only the first is included in bj.c, however, because I neglected to bring the card-counting algorithm back into the States. No matter, card-counting had no measurable impact on the model's outcome unless the deal was a one-deck game, something you cannot find any more in the big gambling houses. If you have a card-counting algorithm that you think might work, you can add it to the program. But if you let all your simulated players count cards, they all drop out when the ratio gets too unfavorable, and the game grinds to a halt.

Bear in mind that the techniques practiced by the gamblers and simulated by bj.c require perfect concentration and a near-photographic memory. Those who hawk the methods insist that with practice the necessary skills can be mastered, and that might be true. But when a student fails to make money, the teachers usually blame the student's inability to correctly administer the procedures rather than the methods. The truth seems to be that the methods do not work. The computer does not make any of those mistakes (unless, of course, the mistakes are in the code), and even though it plays flawlessly, the computer cannot win either.

## The BJ.C Program

The bj.c program is written mostly in generic ANSI C. I compiled it with Turbo C 2.0. The program uses two macros that assume the ANSI.SYS driver is installed in a PC. These two macros are clr_scrn and cursor. If you are using a different system you will need to change these macros to clear the screen and position the cursor with the protocols of your system.

There are two global variables you can change to modify the game. One is PLAYERS, which defines how many players are in the game. The other is DECKS, which defines how many decks the game uses. I used a much more complicated version of the program in San Juan, and you can add the features it had if you wish. It let me select the deck size and player count at run time. I could choose a chair and play along, too.

The program uses the IBM PC graphics character set to display the cards on the screen. If your terminal cannot support these characters, remove the #define IBMPC statement, and the program will use ASCII characters to represent the cards.

Between deals in a Blackjack game, the cards are divided between the "shoe," a box that holds the undealt cards and the "discards," a stack of the cards that have already been used. When the shoe runs out of cards, the dealer re-shuffles the cards and puts them back in the shoe. You will see both terms throughout the program.

The program uses a typedef for the CARD, which contains a value (ace, deuce, trey ... king) and a suit. The suit is not relevant to the simulation but I included it to make the program more realistic and more interesting. There is an array of CARDs called shoe and one called discards.

The PLAYER typedef contains everything about a player including an array of the CARDs in the current hand, the amount of money in the player's bank, the amount the player is betting, a pointer to the player's hit/stand strategy function, and some other operational variables that tell the program what the player is doing at a given place in the deal. There is an array of PLAYERs named players. The array has two PLAYER entries for each actual player. The second entry is set aside for that time when a player decides to split the hand. The last PLAYER entry is the dealer.

When you run the program, it displays each hand being dealt and the outcome. Each player's bank is displayed. You can run the program so that it stops after each hand to let you look at the results, or you can have it run continuously without intervention. You can run the program to bypass the displays, showing the banks only. This runs the simulation a lot faster and gets the same results.

Each player and the dealer start with a bank value of zero. When a player wins, the player's bank is increased by the amount of the bet and the dealer's bank is decreased by the same amount. When the player loses, the reverse occurs. A negative bank means the player is losing. A positive bank means the player is winning.

A player can "double-down" a hand after the second card. This means the player doubles the bet but can get no more than one additional card. A player can split a hand if the first two cards are the same denomination. For example, if you get two fives, you can play them as two independent hands with the same bet on each. You can double-down on either or both of the split hands. You can win one of the hands, both of them, or neither.

If a player's hand ties with the dealer, the player wins, but not the full amount. In theory, you win half of what you bet. But because the chips do not come in denominations evenly divisible by two, a tie is called a "push." Your bet stays up, and to keep it, you have to win the next hand, too.

There is something in Blackjack called "insurance" that I never understood. It is not in the bj.c program. Detractors of my conclusions can point to this deficiency if it helps their argument.

The shuffle is done by computing a random subscript into the discards, moving the card at the offset into the shoe, and moving all the following discards down one position. This procedure continues until all the cards are in the shoe. When the program begins, all the cards are in the discards ready for the first shuffle. Each time the deck is shuffled, the first card is "buried," that is, moved to the discard pile.

The dealer's hit/stand strategy is a simple one. The rules say the dealer must hit if the hand value is 16 or below and stand if the hand value is 17 or above. Dealers cannot split a hand. The dstrategy function implements the dealer's strategy.

Players do not have the restrictions imposed on dealers, and the essence of the simulation is in the two functions named split and pstrategy. These functions decide what a player is going to do with the hand and are designed to use the strategies of the three charts given in Fast Eddie's book for Caribbean casinos.

The split decision is based on what your identical cards are and what the dealer's up card is. The switch statement in the split function makes the decision.

The pstrategy function decides whether to double-down or not and whether to take a hit or not. It looks at the value of the player's hand and the dealer's up card. If either of the first two cards is an ace, the third card strategy is different than otherwise. These switch statements are based on the charts.

You can run the bj.c program and draw your own conclusions. Often a player will have a long run of luck. Often you will see the dealer losing for an extended time. But if you let the program run long enough, the dealer wins big and the players lose big.

This month's column was fun but it might raise some controversy. It has concluded with computer simulation that you cannot consistently win at Blackjack. Some people do not want to believe that. There is a dominant trait of character common in most gamblers. They do not want to believe that winning is a matter of luck, losing is a function of odds, and the systems do not work. Anyone who doubts these arguments is welcome to find the flaws in my model and correct them. But if, on the other hand, they are that sure that I am wrong, they are too busy winning money to mess with a trifling computer program.

Those who write or send a CompuServe message to disagree with these conclusions are requested to restrict their criticisms to the accuracy of the code and the degree to which they think the simulation reflects the real world. I am not interested in tales of success at the tables or the specifications for Blackjack systems that are guaranteed to work. Books and seminars on how to beat the Blackjack tables abound. They are almost as plentiful as the books, home study cassette courses, and seminars on how to get rich in real estate. It makes you wonder where the real money is. If the systems work, one might ask, how come their promoters are in the seminar and book business?

The code in bj.c is tossed out for those of you who would care to try it and maybe tweak it. I doubt that you'll ever get it to win. If you do, I doubt that you'll ever be able to take its skill to the tables yourself. The program convinced me to stay away from the Blackjack tables. I watched it consistently lose simulated money for its simulated players, and I stood in casinos and watched real people lose real money. If the program can convince you the same way it convinced me, then it is a public service and has served its purpose.

Spend your money instead going to see Wayne Newton.

## Availability

All source code for articles in this issue is available on a single disk. To order, send $14.95 (Calif. residents add sales tax) to Dr. Dobb's Journal, 501 Galveston Dr., Redwood City, CA 94063, or call 800-356-2002 (from inside Calif.) or 800-533-4372 (from outside Calif.). Please specify the issue number and format (MS-DOS, Macintosh, Kaypro).

## LISTING 1

```c
/* ------------- bj.c ---------------- */
/*
 * A Blackjack Simulation
 */
#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

#define TRUE 1
#define FALSE 0
#define IBMPC

/* ------- ANSI.SYS screen controls --------- */
#define clr_scrn() puts("\033[2J")
#define cursor(x,y) printf("\033[%02d;%02dH",y+1,x+1)

/* --------- for display purposes ------------- */
#define CARDWIDTH 5
#define CARDHEIGHT 4
#define PLAYERS 7    /* number of players (not incl dealer) */
#define DECKS   6    /* number of decks to play             */
#define CARDS (52*DECKS)

/* -------- the card display characters -------- */
#ifdef IBMPC
#define UL 218      /* IBM Graphics upper left box corner  */
#define HZ 196      /*              box horizontal line    */
#define UR 191     /*              upper right box corner */
#define VT 179     /*              box vertical line      */
#define LL 192     /*              lower left box corner  */
#define LR 217     /*              lower right box corner */
#define HEARTS   3
#define DIAMONDS 4
#define CLUBS    5
#define SPADES   6
#else
#define UL '.'      /*    ASCII     upper left box corner  */
#define HZ '-'      /*              box horizontal line    */
#define UR '.'     /*              upper right box corner */
#define VT '|'     /*              box vertical line      */
#define LL '\''    /*              lower left box corner  */
#define LR '\''    /*              lower right box corner */
#define HEARTS   'h'
#define DIAMONDS 'd'
#define CLUBS    'c'
#define SPADES   's'
#endif

/* --------- the card display values ---------- */
char topline[] = {UL, HZ,  HZ,  HZ,  UR, 0};
char midline[] = {VT, ' ', ' ', ' ', VT, 0};
char botline[] = {LL, HZ,  HZ,  HZ,  LR, 0};
int suits[] = {HEARTS,DIAMONDS,CLUBS,SPADES};
char *vals[] = {"A","2","3","4","5","6","7","8","9","10",
                "J","Q","K"};
/* --------- a card ----------- */
typedef struct crd {
    int value;
   int suit;
} CARD;
/* ---------- a player -------- */
typedef struct ply {
   CARD phand[10];       /* the player's hand            */
   int playing;       /* true if the player is playing */
   int cards;          /* the card count in the hand    */
   int bank;          /* player's bank account        */
   int bet;          /* player's bet amount           */
   int mode;          /* D = double-down              */
   int pushing;       /* true = player tied last hand  */
   int (*strat)(int);    /* pointer to hit/pass strategy  */
} PLAYER;

/* ----- 1 per player, 1 per split, 1 dealer ------ */
#define NBRPLAYERS (PLAYERS*2+1)
int hands;
#define dealer (NBRPLAYERS-1)
PLAYER players [NBRPLAYERS+1];   /* the players  */
CARD shoe[CARDS];            /* the shoe     */
CARD discards[CARDS];         /* the discards */
int nextshoe;      /* shoe subscript      */
int inshoe;         /* # cards in the shoe */
int nextdisc;      /* discard subscript   */
int display=TRUE;   /* true if hands are being displayed   */
int stopping=TRUE;   /* true if display stops at every hand */
int stopper;      /* the key that restarts the display   */

/* ---------- prototypes ----------- */
void shuffle(void);
void bury(void);
void hand(CARD *lst, int x, int y);
void card(int sut, int val, int x, int y);
void cardframe(int x, int y);
void play(void);
void playahand(void);
int split(int p);
void discs(void);
int pstrategy(int p);
void doubledown(int p);
int bj_bust(int p);
int dstrategy(int p);
void winlose(int p);
void won(int p);
void lost(int p);
void push(int p);
void post(int p, char *s);
void nextcard(CARD *cd);
void shoes(void);
void showhand(int pl);
void cleargame(void);
void nohand(CARD *lst, int x, int y);
void nocardframe(int x, int y);
int handvalue(int pl);
void stat(int p, char *s);

void main(void)
{
   int i, s, v;
   clr_scrn();
   /* --- build the decks of cards in the discard pile --- */
    for (i = 0; i < DECKS; i++)
      for (s = 0; s < 4; s++)
         for (v = 0; v < 13; v++)   {
            discards[nextdisc].value = v;
            discards[nextdisc++].suit = suits[s];
         }
   /* ----- initialize the players ----- */
   for (i = 0; i < NBRPLAYERS-1; i++)   {
      players[i].bet = 1;
      players[i].strat = pstrategy;
      /* --- every other player is a split --- */
      players[i].playing = !(i & 1);
   }
   players[dealer].playing = TRUE;
   play();
   clr_scrn();
}
/* --------- begin play of the game --------- */
void play(void)
{
   int c;
   while (TRUE)   {
      if (stopping || kbhit())   {
         if (!stopping || NBRPLAYERS == 1)
            c = getch();
         else   {
            c = stopper;
            stopper = FALSE;
         }
         if (c == 27)
            break;
         if (tolower(c) == 'd')
            display ^= TRUE;
         if (c == 's')
            stopping ^= TRUE;
      }
      playahand();
   }
}
/* --------- play one hand of blackjack ------------- */
void playahand(void)
{
   int p, i, bnk;
   CARD *cd;
   /* -------- deal everyone one card ----------- */
   for (p = 0; p < NBRPLAYERS; p++)   {
      if (players[p].playing == TRUE)   {
         nextcard(players[p].phand);
         players[p].cards++;
           showhand(p);
      }
   }
   /* ----- deal each of the players a second card ----- */
   for (p = 0; p < dealer; p++)   {
      if (players[p].playing == TRUE)   {
         nextcard(players[p].phand+1);
         players[p].cards++;
         /* -- test to see if this player should split -- */
         if (split(p))   {
            /* ----- split the hand ------- */
            players[p].cards = 1;
            players[p].phand[1].suit =
               players[p].phand[1].value = 0;
            bnk = players[p+1].bank;
            players[p+1] = players[p];
            players[p+1].bank = bnk;
            stat(p, "SPLT");
         }
         showhand(p);
      }
   }
   /*  deal the rest of the hand for each player in turn  */
   for (p = 0; p < NBRPLAYERS-1; p++)   {
      if (players[p].playing == TRUE)   {
         while (!bj_bust(p) && (*players[p].strat)(p))
            nextcard(players[p].phand+(players[p].cards++));
         showhand(p);
      }
   }
   /* ------ see if all the players went bust ------- */
   for (p = 0; p < NBRPLAYERS-1; p++)
      if (players[p].playing == TRUE && handvalue(p) <= 21)
         break;
   /* ----- if so, the dealer doesn't have to play ---- */
   if (p < NBRPLAYERS-1)   {
      /* ------ deal the rest of the dealer's hand ------- */
      while (!bj_bust(dealer) && dstrategy(dealer))
         nextcard(players[dealer].phand+
            (players[dealer].cards++));
      showhand(dealer);
   }
   /* ------ post players' wins and losses --------- */
   for (p = 0; p < NBRPLAYERS-1; p++)   {
      if (players[p].playing == TRUE)
         winlose(p);
      players[p].mode = FALSE;
   }
    post(dealer, "DEAL");
   cursor(0, 1);
   for (i = 0; i < NBRPLAYERS+1; i += 2)
      printf("%5d     ", players[i].bank+players[i+1].bank);
   /* -- gather the players' cards into the discard pile -- */
   for (p = 0; p < NBRPLAYERS; p++)   {
      cd = players[p].phand;
      for (i = 0; i < players[p].cards; i++)
         discards[nextdisc++] = *cd++;
   }
   /* ---- if display stops on every hand, read a key ---- */
   if (stopping)
      stopper = getch();
   cleargame();
   discs();
   ++hands;
   if (display)   {
      cursor(40,0);
      printf("HANDS: %d ", hands);
   }
}
/* ---- test to see if a player should split the hand ---- */
int split(int p)
{
   int a, b;
   a = players[p].phand[0].value + 1;
   b = players[dealer].phand[0].value + 1;
   if (b > 10)
      b = 10;
   if (a == players[p].phand[1].value + 1)   {
      switch (a)   {
         case 1:   return TRUE;
         case 2:
         case 3:   return (b > 1 && b < 8);
         case 4: return (b == 5 || b == 6);
         case 5: return FALSE;
         case 6:   return (b > 1 && b < 7);
         case 7:   return (b > 1 && b < 8);
         case 8: return TRUE;
         case 9: return !(b == 7 || b == 10 || b == 1);
         case 10: return FALSE;
      }
   }
   return FALSE;
}
/* -------- display the discards pile count -------- */
void discs(void)
{
   if (display)      {
      cursor(20,0);
      printf("DISCARDS: %d   ", nextdisc);
   }
}
/* ---- test if a player has blackjack or went bust ---- */
int bj_bust(int p)
{
   int rtn;
   if ((rtn = (players[p].cards == 2 && handvalue(p) == 21))
         == TRUE)
      stat(p, "*BJ*");      /* player has blackjack */
   else if ((rtn = (handvalue(p) > 21)) == TRUE)
      stat(p, "BUST");        /* player went bust     */
   return rtn;
}
/* ---- player strategy (true = hit, false = stand) ---- */
int pstrategy(int p)
{
   int b, h;
   /* ---- smart player watches dealers up card ---- */
   b = players[dealer].phand[0].value+1;
   h = handvalue(p);
   if (players[p].mode == 'D')
      return 0;
   if (players[p].cards == 2 &&
      players[p].phand[0].value+1 == 1 ||
      players[p].phand[1].value+1 == 1)   {
      switch (h)   {
         case 3:
         case 4:   if (b == 5 || b == 6)
                  doubledown(p);
               return TRUE;
         case 5:
         case 6:   if (b > 3 && b < 7)
                  doubledown(p);
               return TRUE;
         case 7: if (b > 2 && b < 7)
                  doubledown(p);
               return TRUE;
         case 8: if (b > 3 && b < 7)
                  doubledown(p);
               if (b == 2 || b == 7 || b == 8)
                  return FALSE;
               return TRUE;
         default: break;
      }
   }
   switch (h)   {
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:      return TRUE;
      case 9:      if (b > 2 && b < 7)
                  doubledown(p);
               return TRUE;
      case 10:   if (b > 1 && b < 10)
                  doubledown(p);
               return TRUE;
      case 11:   if (b != 1)
                  doubledown(p);
               return TRUE;
      case 12:   return !(b > 3 && b < 7);
      case 13:
      case 14:
      case 15:
      case 16:   return !(b > 1 && b < 7);
      case 17:
      case 18:
      case 19:
      case 20:
      case 21:   return FALSE;
   }
   return FALSE;
}
/* ----------- double down the hand ---------- */
void doubledown(int p)
{
   players[p].mode = 'D';
   stat(p, "DBDN");
}
/* --------- the dealer strategy ---------- */
int dstrategy(int p)
{
   /* - dealer hits on 16 or below, stands on 17 or above - */
   return (handvalue(p) < 17);
}
/* ------- test if a hand wins or loses -------- */
void winlose(int p)
{
   /* --- doubled-down hand bets twice as much --- */
   if (players[p].mode == 'D')
      players[p].bet *= 2;
   /* ---- value > 21 is a bust ---- */
   if (handvalue(p) > 21)
      lost(p);
   /* - blackjack wins if dealer does not have blackjack - */
   else if (handvalue(p) == 21 && players[p].cards == 2 &&
         !(handvalue(dealer) == 21 &&
         players[dealer].cards == 2))
      won(p);
   /* ----- value greater than dealer wins ----- */
   else if (handvalue(p) > handvalue(dealer))
      won(p);
   /* ----- if dealer busts, player wins ----- */
   else if (handvalue(dealer) > 21)
      won(p);
   /* -- if dealer's hand > player's hand, player loses -- */
   else if (handvalue(p) < handvalue(dealer))
      lost(p);
   /* ---- tied hand (push) if none of the above --- */
   else
      push(p);
   /* ------ reset bet for doubled-down hand ------- */
   if (players[p].mode == 'D')
      players[p].bet /= 2;
}
/* -------- compute the value of a hand ------ */
int handvalue(int pl)
{
   CARD *hd;
   int vl = 0, cd, aces = 0;

   hd = players[pl].phand;      /* point to 1st card in hand */
   while (hd->suit)   {
      cd = hd->value+1;      /* value of the card         */
      if (cd > 10)         /* jack, queen, king = 10    */
         cd = 10;
      if (cd == 1)   {      /* ace = 11                  */
         cd = 11;
         aces++;            /* count aces in the hand    */
      }
      vl += cd;            /* accumulate hand value     */
      hd++;               /* point to next card        */
   }
   while (vl > 21 && aces--)   /* adjust for aces if > 21   */
      vl -= 10;            /* ace = 1                   */
   return vl;
}
/* ------ the player won the hand :-) -------- */
void won(int p)
{
   players[p].bank += players[p].bet + players[p].pushing;
   players[p].pushing = 0;
   players[dealer].bank -= players[p].bet;
   post(p, "WIN ");
}
/* -------- the player lost the hand :-( --------- */
void lost(int p)
{
   players[p].bank -= players[p].bet + players[p].pushing;
   players[p].pushing = 0;
   players[dealer].bank += players[p].bet;
   post(p, "LOSS");
}
/* ------- the player tied :-| -------- */
void push(int p)
{
   players[p].pushing = players[p].bet;
   post(p, "PUSH");
}
/* ------- post the WIN/LOSS/PUSH ------ */
void post(int p, char *s)
{
   if (display)   {
      cursor(1+p*5, 24);
      printf("%s", s);
   }
}
/* -------- get the next card from the shoe ------ */
void nextcard(CARD *cd)
{
   if (nextshoe == inshoe)   {
      shuffle();            /* time to reshuffle */
      bury();               /* bury one          */
   }
   *cd = shoe[nextshoe++];
   (cd+1)->suit = FALSE;
   shoes();
}
/* --------- shuffle the discards into the shoe -------- */
void shuffle(void)
{
   int cdp, nd;

   if (display)    {
      cursor(0,0);
      printf("SHUFFLE");
   }
   randomize();
   nd = nextdisc;
   for (nextshoe = 0; nextshoe < nd; nextshoe++)   {
      cdp = random(nextdisc);
      shoe[nextshoe] = discards[cdp];
      while (cdp < nextdisc)   {
         discards[cdp] = discards[cdp+1];
         cdp++;
      }
      --nextdisc;
   }
   discs();
   inshoe = nextshoe;
   nextshoe = 0;
   if (display)    {
      cursor(0,0);
      printf("       ");
   }
}
/* ---------- bury the first card ----------- */
void bury(void)
{
   CARD cd[2];

   nextcard(cd);
    discards[nextdisc++] = *cd;
   if (display)   {
      card(cd[0].suit, cd[0].value, 1, 16);
      cursor(1, 15);
      printf("BURIED");
      if (stopping)
         stopper = getch();
      nocardframe(1, 16);
      cursor(1, 15);
      printf("      ");
   }
}
/* ----- display the number of cards left in the shoe ----- */
void shoes(void)
{
   if (display)      {
      cursor(10, 0);
      printf("SHOE: %d ", inshoe-nextshoe);
   }
}
/* ------- display the hand and the player's money ------ */
void showhand(int pl)
{
   if (display)   {
      cursor(1+pl*5, 3);
      printf("%d", handvalue(pl));
      hand(players[pl].phand, pl*CARDWIDTH, 4);
   }
}
/* --------- display a hand -------- */
void hand(CARD *lst, int x, int y)
{
   while (lst->suit)   {
      card(lst->suit, lst->value, x++,y);
      lst++;
      y += 2;
   }
}
/* ---------- display a card ---------- */
void card(int sut, int val, int x, int y)
{
   cardframe(x, y);
   cursor(x+1, y+1);
   printf("   \b\b\b");
   printf("%s%c", vals[val], sut);
}
/* ---------- display the card frame -------- */
void cardframe(int x, int y)
{
   int y1;

   cursor(x, y);
   printf(topline);
   for (y1 = y+1; y1 < y+CARDHEIGHT-1; y1++)   {
      cursor(x,y1);
      printf(midline);
   }
   cursor(x,y1);
   printf(botline);
}
/* --------- clear the game display ---------- */
void cleargame(void)
{
   int i;

   for (i = 0; i < NBRPLAYERS; i++)   {
      if (players[i].playing == TRUE)   {
         if (display)   {
            cursor(1+i*5, 3);
            printf("  ");
            nohand(players[i].phand, i*CARDWIDTH, 4);
         }
         players[i].cards = 0;
         players[i].playing = !(i & 1);
         stat(i, "    ");
         post(i, "    ");
      }
   }
}
/* ----- display a null hand to erase the hand ----- */
void nohand(CARD *lst, int x, int y)
{
   while (lst->suit)   {
      nocardframe(x++,y);
      y += 2;
      lst++;
   }
}
/* ----- null card frame ------------ */
void nocardframe(int x, int y)
{
   int y1;

   for (y1 = y; y1 < y+CARDHEIGHT; y1++)   {
      cursor(x,y1);
      printf("     ");
   }
}
/* ---- print the status *BJ*, BUST, DBLD, SPLT, etc. ---- */
void stat(int p, char *s)
{
   if (display)   {
      cursor(p*CARDWIDTH, 2);
      printf(s);
   }
}
