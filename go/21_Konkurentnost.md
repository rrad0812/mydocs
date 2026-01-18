[[Generici]](20_Generici.md) [[Sadržaj]](toc.md) [[Gorutine]](22_Gorutine.md)

# Konkurentnost

Konkurentnost je jedna od najmoćnijih karakteristika Go jezika. Konkurentnost, po definiciji, je sposobnost razlaganja računarskog programa ili algoritma na pojedinačne delove, koji se mogu izvršavati nezavisno. Konačni ishod konkurentnog programa je isti kao i kod programa koji je izvršan sekvencijalno.

Koristeći konkurentnost, možemo postići iste rezultate za manje vremena, čime se povećavaju ukupne performanse i efikasnost naših programa.

### Konkurentnost naspram paralelizma

Mnogi ljudi mešaju konkurentnost sa paralelizmom jer oba koncepta donekle podrazumevaju istovremeno izvršavanje koda, ali to su dva potpuno različita koncepta.

*Zadatak konkurencije je pokretanja i upravljanja višestrukim izračunavanjima u isto vreme, dok je zadatak paralelizam pokretanja višestrukih izračunavanja istovremeno.* 

Jednostavan citat Roba Pajka prilično sumira sve: *"Konkurentnost je rad `sa` mnogo stvari odjednom. Paralelizam je rad mnogo stvari odjednom."*

Ali konkurentnost u Gou je više od same sintakse. Da bismo iskoristili moć Goa, prvo moramo da razumemo kako Go pristupa konkurentnom izvršavanju koda. 

Go se oslanja na model konkurentnosti koji se zove `CSP (Communicating Sequential Processes` - komunikacija sekvencijalnih procesa).

### Komunikacija sekvencijalnih procesa (CSP)

Komunikacija sekvencijalnih procesa (CSP) je model koji je predstavio Toni Hoar 1978. godine, a koji opisuje interakcije između konkurentnih procesa. Napravio je proboj u računarstvu, posebno u oblasti konkurentnosti.

Jezici poput Go i Erlang su bili veoma inspirisani konceptom komunikacije sekvencijalnih procesa (CSP).

Konkurencija je teška, ali CSP nam omogućava da damo bolju strukturu našem konkurentnom kodu i pruža model za razmišljanje o konkurentnosti na način koji to čini malo lakšim. Ovde su konkurentni procesi nezavisni i komuniciraju deljenjem kanala između njih.

Kasnije u kursu ćemo naučiti kako Golang to implementira koristeći `gorutine` i `kanale`.

### Osnovni koncepti

##### Trka podataka

Trka podataka nastaje kada procesi moraju konkuretno da pristupe istom resursu. Na primer, jedan proces čita, dok drugi istovremeno piše u potpuno isti resurs.

*Uslovi trke*

Do trke dolazi kada vreme ili redosled događaja utiče na ispravnost dela koda.

##### Zastoj

Zastoj se javlja kada su svi procesi blokirani dok čekaju jedan drugog i program ne može dalje da se izvršava.

*Kofmanovi uslovi*

Postoje četiri uslova, poznata kao `Kofmanovi uslovi`, i svi oni moraju biti zadovoljeni da bi došlo do zastoja.

- *Uzajamno isključivi*

	Kontinuirani proces drži barem jedan resurs u bilo kom trenutku, što ga čini nedeljivim.

- *Drži i čeka*

	Kontinuirani proces drži resurs i čeka na dodatni resurs.

- *Bez mogućnosti konkurencije*

	Resurs koji drži konkurentni proces ne može biti oduzet od strane OS. Može ga osloboditi samo proces koji ga drži.

- *Kružno čekanje*

	Proces čeka resurs koji drži drugi proces, koji čeka resurs koji drži treći proces, i tako dalje, sve dok poslednji proces ne čeka resurs koji drži prvi proces. Dakle, formira se kružni lanac čekanja na resurse.

##### Žive brave

`Livelock` procesi su procesi koji aktivno izvršavaju konkurentne operacije, ali ove operacije ne čine ništa da pomeraju stanje programa napred.

##### Gladovanje

Do gladovanja dolazi kada je proces lišen neophodnih resursa i nije u stanju da završi svoju funkciju.

Gladovanje može da se desi zbog zastoja ili neefikasnih algoritama za zakazivanje procesa. Da bismo rešili problem gladovanja, moramo da primenimo bolje algoritme za raspodelu resursa koji osiguravaju da svaki proces dobije svoj pravedan deo resursa.

[[Generici]](20_Generici.md) [[Sadržaj]](toc.md) [[Gorutine]](22_Gorutine.md)
