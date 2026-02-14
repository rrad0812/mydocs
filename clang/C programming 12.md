# C programiranje 12

## Jedan je rođen svakog minuta

Al Stevens, juli '89

Sada je vreme da se projekat SMALLCOM stavi u bačvu i pusti da odleži. Neki od vas ga koriste i šalju komentare, sugestije i probleme. Projekat je trajao deset meseci, a noviji čitaoci smatraju da je potrebno da povrate probleme kako bi nadoknadili zaostatak. Možda ću ažurirati projekat u budućoj kolumni i dodati mu još funkcija. Za sada je, međutim, godišnji odmor pred nama i došlo je vreme da otkrijemo svoj skriveni hedonizam putovanjem na ostrva i tabele prilika.

Rodni Dengerfild kaže da je otišao u Las Vegas automobilom od 25.000 dolara, a kući se vratio autobusom od 200.000 dolara. Otišao sam u Portoriko da napravim sistem poreza na dohodak i vratio se kući sa programom za simulaciju Blackjack-a.

San Huan, Portoriko ima kockarnice. Moj prijatelj Brzi Eddie Dwier živi tamo, bavi se portorikanskom verzijom poreske uprave danju, a noću švrlja po kockarnicama. Naoružan knjigom o tome kako da pobedi u Blackjack-u, planira prevremeno penzionisanje. Međutim, uz sreću, on će se samo zabaviti trošeći (gubeći) ali ne mnogo novca. Obratite pažnju na operativnu reč - sreća.

Nisam kockar, ali me privlači izazovan problem. Odlučio sam da testiram Fast Eddie's Blackjack sisteme bez rizika od sopstvenog novca. Ima li boljeg načina od kompjuterske simulacije u C programu? Program u [Listing 1](#listing-1) je "bj.c", i modelira igru i neke od strategija koje podučavaju samoproglašeni majstori. Moji zaključci o održivosti ovih strategija mogu se videti u rezultatima programa. Na duge staze, ne možete pobediti. Neće se svi složiti.

Bilo je vremena kada ste mogli pronaći igru Blackjack u nekim kockarnicama koje su koristile jedan špil. Ako ste bili u stanju da mentalno savladate sve strategije, mogli biste dosledno da pobeđujete. Kazina su to naučila i promenila neka pravila, jedno od njih je broj špilova u igri. Sada koriste šest špilova, a strategije, čak i ako možete da ih izvedete besprekorno, što niko ne može, ne funkcionišu. Brzi Eddie i dalje ima dnevni posao.

Svi kazina imaju znakove koji zabranjuju upotrebu personalnih računara i drugih elektronskih pomagala. Znakovi su tu da vas ubede da "sistemi" mogu da rade, da vas nateraju da pokušate, da vas navedu da igrate, da dobijete svoj novac. Možete uzeti svu elektronsku pomoć koju želite. I dalje bi izgubili.

Postoje tri osnovne strategije koje se reklamiraju kako biste postali pobednik Blackjack-a. Prva strategija uključuje posmatranje vaše ruke i dilerove karte. Na osnovu grafikona (koji se nalazi u Fast Eddiejevom priručniku) odlučujete da li ćete udariti ili stajati. Ako su vaše prve dve karte istog apoena, drugi grafikon vam govori da li treba da podelite ruku. Kada dobijete drugu kartu, treća karta vam govori da li treba da udvostručite ruku. Trik je u tome da zapamtite sva tri grafikona i da ih odmah primenite tokom brzog ritma dogovora. Da stvar bude komplikovana, liste se razlikuju u zavisnosti od toga da li igrate u Nevadi, Atlantik Sitiju ili na Karibima.

Druga strategija uključuje brojanje karata. Vodite računa o dve grupe karata. Prva grupa je dobra grupa i sastoji se od karata lica, desetica i asova. Druga grupa je loša. Sastoji se od svih ostalih karata, dvojke do devetke. Morate da pazite na sve karte koje su podeljene vama, dileru i svim ostalim igračima. Morate mentalno da izračunate tekuću ravnotežu dve grupe da biste znali koji je odnos dobrih i loših karata ostao u nepodeljenom delu špila. Što je više dobrih karata, veće su vaše šanse za pobedu. Što je više loših karata, veća je verovatnoća da ćete izgubiti. Kada odnos dođe do određenog nivoa u vašu korist, povećavate svoju opkladu. Kada odnos ode dovoljno protiv vas, prestajete da igrate. Odnos koji primenjujete je funkcija ukupnog broja karata u igri. Kako igra napreduje, dodajete jednu na ukupni broj svaki put kada vidite dobru kartu. Oduzimate jednu svaki put kada vidite lošu kartu. Ovaj tekući zbir primenjujete na ukupan broj karata da biste izračunali odnos dobrih i loših karata preostalih u špilu. To je suština brojanja karata. Više nemam pojedinosti jer je Brzi Edi vodio knjigu kada sam otišao iz San Huana. Još uvek pokušava.

Treća strategija se zove upravljanje novcem. Svoje opklade menjate na osnovu nedavnog odnosa pobeda/gubitaka. Ovo nikada nisam razumeo, i nisam mogao da ga simuliram kako treba. Brzi Edi i ja imamo prijatelja koji u to žarko veruje. Poslednji put kada je bio u San Huanu, pozajmio je novac da se vrati kući.

Simulirao sam obe prve dve strategije neko vreme. Međutim, samo prvi je uključen u bj.c, jer sam zaboravio da vratim algoritam za brojanje kartica u Sjedinjene Države. Bez obzira na to, brojanje karata nije imalo merljiv uticaj na ishod modela osim ako je dogovor bila igra sa jednim špilom, nešto što više ne možete naći u velikim kockarnicama. Ako imate algoritam za brojanje kartica za koji mislite da bi mogao da funkcioniše, možete ga dodati programu.

Ali ako dozvolite svim vašim simuliranim igračima da broje karte, svi ispadaju kada odnos postane previše nepovoljan i igra se zaustavlja.

Imajte na umu da tehnike koje praktikuju kockari i koje simulira bj.c zahtevaju savršenu koncentraciju i skoro fotografsko pamćenje. Oni koji koriste metode insistiraju na tome da se uz praksu mogu savladati neophodne veštine, i to bi moglo biti tačno. Ali kada učenik ne uspe da zaradi novac, nastavnici obično krive učenikovu nesposobnost da pravilno primeni procedure nego metode. Čini se da je istina da metode ne rade. Kompjuter ne pravi nijednu od tih grešaka (osim ako, naravno, greške nisu u kodu), a iako igra besprekorno, ni računar ne može da pobedi.

## BJ.C program

Program bj.c je napisan uglavnom u generičkom ANSI C-u. Preveo sam ga sa Turbo C 2.0. Program koristi dva makroa koji pretpostavljaju da je ANSI.SYS drajver instaliran na računaru. Ova dva makroa su "clr_scrn" i "cursor". Ako koristite drugi sistem, moraćete da promenite ove makroe da biste obrisali ekran i pozicionirali kursor sa protokolima vašeg sistema.

Postoje dve globalne varijable koje možete promeniti da biste modifikovali igru. Jedan je IGRAČI, koji definiše koliko je igrača u igri. Drugi je ŠPILOVI, koji definiše koliko špilova igra koristi. Koristio sam mnogo komplikovaniju verziju programa u San Huanu, a možete dodati funkcije koje je imao ako želite. Dozvoljava mi da odaberem veličinu špila i broj igrača tokom vremena rada. Mogao sam da izaberem stolicu i da se igram zajedno.

Program koristi IBM PC grafički skup znakova za prikaz kartica na ekranu. Ako vaš terminal ne podržava ove znakove, uklonite naredbu `#define IBMPC` i program će koristiti ASCII znakove za predstavljanje kartica.

Između dele u igri Blackjack, karte su podeljene između „cipele“, kutije koja drži nepodeljene karte i „odbacivanja“, hrpe karata koje su već korišćene. Kada cipeli ponestane karata, diler ponovo meša karte i vraća ih u cipelu. Videćete oba termina u celom programu.

Program koristi typedef za KARTICU, koja sadrži vrednost (kec, dvojka, trej ... kralj) i boju. Odelo nije relevantno za simulaciju, ali sam ga uključio da bi program bio realističniji i zanimljiviji. Postoji niz KARTICA koje se nazivaju cipela i jedna koja se zove odbacivanje.

PLAYER typedef sadrži sve o igraču, uključujući niz KARTA u trenutnoj ruci, iznos novca u banci igrača, iznos na koji igrač kladi, pokazivač na funkciju strategije pogodak/stoji i neke druge operativne varijable koje govore programu šta igrač radi na datom mestu u dogovoru. Postoji niz IGRAČA po imenu igrači. Niz ima dva PLAYER unosa za svakog stvarnog igrača. Drugi unos se ostavlja po strani za ono vreme kada igrač odluči da podeli ruku. Poslednji ulazak IGRAČA je diler.

Kada pokrenete program, on prikazuje svaku ruku koja se deli i rezultat. Prikazuje se banka svakog igrača. Možete pokrenuti program tako da se zaustavi nakon svake ruke da biste mogli da pogledate rezultate, ili možete da ga pokrenete neprekidno bez intervencije. Možete pokrenuti program da zaobiđete ekrane, prikazujući samo banke. Ovo pokreće simulaciju mnogo brže i dobija iste rezultate.

Svaki igrač i diler počinju sa bankovnom vrednošću od nula. Kada igrač pobedi, banka igrača se povećava za iznos opklade, a banka dilera se smanjuje za isti iznos. Kada igrač izgubi, dešava se obrnuto. Negativna banka znači da igrač gubi. Pozitivna banka znači da igrač pobeđuje.

Igrač može da "udvostruči" ruku nakon druge karte. To znači da igrač udvostručuje opkladu, ali ne može dobiti više od jedne dodatne karte. Igrač može podeliti ruku ako su prve dve karte iste vrednosti. Na primer, ako dobijete dve petice, možete ih odigrati kao dve nezavisne ruke sa istom opkladom na svaku. Možete udvostručiti jednu ili obe podeljene ruke. Možete osvojiti jednu ruku, obe ili nijednu.

Ako je ruka igrača jednaka sa dilerom, igrač dobija, ali ne ceo iznos. U teoriji, dobijate polovinu onoga što ste uložili. Ali pošto se žetoni ne isporučuju u apoenima koji su podjednako deljivi sa dva, izjednačenje se naziva „push“. Vaša opklada ostaje ista, a da biste je zadržali, morate da osvojite i sledeću ruku.

Postoji nešto u Blackjack-u što se zove "osiguranje" što nikada nisam razumeo. Nije u programu bj.c. Kritioci mojih zaključaka mogu ukazati na ovaj nedostatak ako to pomaže njihovim argumentima.

Mešanje se vrši izračunavanjem slučajnog indeksa u odbačenim brojevima, pomeranjem kartice na ofsetu u cipelu i pomeranjem svih sledećih odbacivanja za jednu poziciju naniže. Ova procedura se nastavlja sve dok sve karte ne budu u cipeli. Kada program počne, sve karte su u odbačenim spremnicima za prvo mešanje. Svaki put kada se špil meša, prva karta je „zakopana“, odnosno premeštena na gomilu za odbacivanje.

Dilereva strategija hit/stand je jednostavna. Pravila kažu da diler mora da pogodi ako je vrednost ruke 16 ili manja i da stoji ako je vrednost ruke 17 ili više. Dileri ne mogu podeliti ruku. Funkcija dstrategi implementira strategiju dilera.

Igrači nemaju ograničeja nametnuta dilerima, a suština simulacije je u dve funkcije koje se zovu split i pstrategi. Ove funkcije odlučuju šta će igrač da uradi sa rukom i dizajnirane su da koriste strategije tri grafikona date u kjizi Fasta Edija za kazina na Karibima.

Podela odluka se zasniva na tome koje su vaše identične karte i koja je karta dilera. Izjava svitch u funkciji split donosi odluku.

Funkcija pstrategi odlučuje da li da udvostruči ili ne i da li da primi udarac ili ne. Gleda na vrednost ruke igrača i dilerove karte. Ako je bilo koja od prve dve karte as, strategija treće karte je drugačija nego inače. Ove izjave za prebacivanje su zasnovane na grafikonima.

Možete pokrenuti bj.c program i izvući svoje zaključke. Često će igrač imati dugu sreću. Često ćete videti da diler gubi na duže vreme. Ali ako pustite program da radi dovoljno dugo, diler dobija veliku pobedu, a igrači gube mnogo.

Ovomesečna kolumna je bila zabavna, ali bi mogla izazvati kontroverzu. Računarska simulacija je zaključila da ne možete stalno da pobeđujete u Blackjack-u. Neki ljudi ne žele da veruju u to. Postoji dominantna osobina karaktera uobičajena kod većine kockara. Ne žele da veruju da je pobeda stvar sreće, poraz u funkciji kvota, a sistemi ne funkcionišu. Svako ko sumnja u ove argumente je dobrodošao da pronađe nedostatke u mom modelu i ispravi ih. Ali ako su, s druge strane, toliko sigurni da grešim, previše su zauzeti osvajanjem novca da bi se petljali sa beznačajnim kompjuterskim programom.

Od onih koji napišu ili pošalju CompuServe poruku da se ne slažu sa ovim zaključcima se traži da ograniče svoje kritike na tačnost koda i stepen do kojeg misle da simulacija odražava stvarni svet. Ne zanimaju me priče o uspehu za stolovima ili specifikacije za Blackjack sisteme koji garantovano rade. Mnogo je knjiga i seminara o tome kako pobediti Blackjack stolove. Ima ih skoro isto kao i knjiga, kurseva kaseta za učenje kod kuće i seminara o tome kako se obogatiti u nekretninama. To vas tera da se zapitate gde je pravi novac. Ako sistemi funkcionišu, moglo bi se zapitati, kako to da su njihovi promoteri u poslu sa seminarima i knjigama?

Kod u bj.c je izbačen za one od vas koji bi želeli da ga isprobaju i možda ga podese. Sumnjam da ćete ikada uspeti da pobedite. Ako to uradite, sumnjam da ćete ikada moći sami da prenesete njegovu veštinu na stolove. Program me je ubedio da se držim dalje od Blackjack stolova. Gledao sam kako stalno gubi simulirani novac za svoje simulirane igrače, i stajao sam u kockarnicama i gledao kako stvarni ljudi gube pravi novac. Ako program može da vas ubedi na isti način na koji je ubedio mene, onda je to javni servis i služio je svojoj svrsi.

Potrošite svoj novac umesto da vidite Vejna Njutna.

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
