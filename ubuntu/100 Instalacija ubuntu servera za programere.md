# Instalacija programerskog Ubuntu servera

- Instaliraj `Ubuntu server` na virtuelnu ili fizičku mašinu Ubuntu 64 OS, sa
  najmanje 2GB Ram-a i 1 procesor-om.

- Pri instalaciji obavezno čekiraj `OpenSSH` server.

- Ažuriraj izvore paketa i podigni na poslednje verzije:
  
  ```sh
  sudo apt update && sudo apt upgrade -y
  ```

- Aktiviraj `firewall` i dozvoli `OpenSSH`
  
  ```sh
  sudo ufw enable
  sudo ufw allow OpenSSH
  ```

- Ako je NAT-ovana mreža (VM) obavezno NAT-uj `guest port:22` na recimo
  `localhost:2022`.

- Iako ne traži reboot

  ```sh
  sudo shutdown -r now
  ```

- Proveri da li je git instaliran sa git --version, ako nije instaliraj sa:
  
  ```sh
  sudo apt install git -y
  ```

- Instaliraj MidnightCommander:

  ```sh
  sudo apt install mc -y
  ```

- Promeni vremensku zonu, uključi sinhronizaciju vremena

  ```sh
  sudo timedatectl set-timezone Europe/Belgrade
  sudo timedatectl set-ntp on
  ```

- Promeni locale

  ```sh
  sudo dpkg-reconfigure locales
  ```

- Dodaj nove `locale sr_RS@latin UTF-8` i postavi ih za default.

- Reboot

  ```sh
  sudo shutdown -r now
  ```

- Generiši par SSH ključeva za pristup serveru

  ```sh
  ssh-keygen -t rsa -b 4096 -C "rrad0812@gmail.com"
  ```

  Pri generisanju obavezno unesi `passpharse`. Za pristup serveru iskopiraj
  javni ključ. Za GitHub iskopiraj javni ključ.

- Instalacija PostgreSQL servera na Ubuntu server

  ```sh
  # Instaliraj neophodne pakete
  sudo apt install postgresql

  # Proveri status servisa servera:
  systemctl status postgresql

  # Proveri interfejs i port na kome server sluša:
  ss -nlt

  # Start/restart/stop server servisa:
  sudo systemctl start postgresql
  sudo systemctl restart postgresql
  sudo systemctl stop postgresql

  # Onemogući/omogući start servisa pri pokretanju OS-a:
  sudo systemctl disable postgresql
  sudo systemctl enable postgresql

  # Poveži se sa bazom podataka:
  # Na localhost-u, lozinka je neophodna
  psql -h localhost -d DB-name -U DB-User

  # Na udaljenom hostu, lozinka je neophodna
  psql -h server-ip-adresa -d DB-name -U DB-user

  # Obezbedi podrazumevanu PostgreSQL bazu podataka (postgres):
  # Postavi password postgres korisniku na Ubuntu sistemu
  passwd postgres

  # Promeni na postgres usera
  su – postgres

  # Postavi password u PostgreSQL za podrzumevanog admina - postgres
  psql -c "ALTER USER postgres WITH PASSWORD 'postgres-db-password';"
  
  # Kreiraj novog korisnika, novu bazu, daj prava i promeni vlasništvo:
  # Promeni na postgres korisnika
  su – postgres
  
  # Pozovi klijenta
  psql
  ```

  ```sql
  # Kreiraj novog korisnika
  CREATE USER user-name WITH PASSWORD 'user-password';
  
  # Kreiraj novu bazu podataka
  CREATE DATABASE user-name;
  
  # Dodeli sva prava novokreiranom korisniku na novokreiranoj bazi podataka
  GRANT ALL ON DATABASE user-name TO user-name;
  
  # Promeni vlasnika baze podataka na novokreiranog korisnika
  
  ALTER DATABASE user-name OWNER TO user-name;
  
  # Proveri da li je baza podataka kreirana
  \l
  ```

  ```sh
  # Konfiguriši mogućnost daljinskog pristupa
  # Ažuriraj datoteku postgresql.conf, tako da podrazumevani pristup samo sa localhost-a
  sudo nano /etc/postgresql/14/main/postgresql.conf
  listen_addresses = 'localhost'
  
  # Promeniš u potrebni
  listen_address = '192.168.1.0/24'
  
  # Ili za pristup sa svih adresa
  listen_address = '*'
  
  # Konfiguriši globalne korisničke dozvole
  # Ažuriraj datoteku pg_hba.conf
  sudo nano /etc/postgresql/14/main/pg_hba.conf
  
  #host #db #users #listen to #auth
  local all postgres peer
  local all all peer
  host all all 127.0.0.1/32 scram-sha-
  host all all ::1/128 scram-sha-
  ```

  Za info vidi <https://www.postgresql.org/docs/current/auth-pg-hba-conf.html>.
  
  ```sh
  # Ponovo pokreni server baze podataka da bi primenio promene:
  sudo systemctl restart postgresql

  # Ako pristupaš udaljeno, otvori na firewall-u 5432 port
  sudo ufw allow 5432

  # Izvezi podatke iz PostgreSQL servera
  pg_dump DB_NAME > OUTPUT_FILE

  # Uvezi podataka u PostgreSQL server
  psql DB_NAME < INPUT_FILE # baza podataka mora biti unapred kreirana

  # Napravi rezervnu kopiju automatski preko Cron JOB-a
  # Promeni na postgres korisnika
  su – postgres

  # Napravi backups dir
  mkdir -p ~/backups

  # Uredi crontab (ovo nije sigurno da može postgres da uradi - probati)
  crontab -e

  # Dodaj na kraj crontab-a (m, h, dom, mon, dow, command)
  0 0 * * * pg_dump -U postgres dbname > 
  ~/backups/dbname-$(date +\%d.\%m.\%y).bak
  ```

  Za info vidi <https://www-linode-com.translate.goog/docs/guides/schedule-tasks-with-cron>

  ```sh
  # Proveri rezervnu kopiju
  # Promeni na postgres korisnika
  su – postgres

  # Napravi logs dir
  mkdir -p ~/logs

  # Uredi crontab
  crontab -e

  # Izmeni backup liniju radi dodavanja logova grešaka
  0 0 * * * pg_dump -U postgres dbname >
  ~/backups/dbname-$(date +\%d.\%m.\%y).bak 2>
  ~/logs/dbname-$(date +\%d.\%m.\%y).bak.log
  ```
