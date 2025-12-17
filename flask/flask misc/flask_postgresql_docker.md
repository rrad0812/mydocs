
# Flask App Development 101

## Kick-starting Your Own Web App Adventure

Flask is one of my favorite frameworks. I love it maybe the most (Maybe because I love Python). It is light and simple, but very powerful. It is perfect for any type of project. From a project prototype to API service, and also large projects.

But the Flask characteristic that I love the most is Freedom.

Flask does not need any predefined project structure or configs. You can literally design your application in whatever way you want.

Although this freedom is wonderful, it may create a specific disadvantage: How can I initiate my Flask project? A situation that many people who first start a Flask app face that makes them a bit confused.

In this post, I will explain how I usually initiate my Flask projects.

### Kreiranje projekta i virtuelne okoline

- **Create a project directory**

  The first step is to create a project directory for our new application. We name our project `flask-cool-app`.

  ```sh
  mkdir flask-cool-app
  ```

- **Create Python virtual environment**

  Inside the application directory, run:
  
  ```sh
  python -m venv .venv
  ```
  
  The name of the virtual env directory is `.venv`
  
  Now we need to enable it. Run (Linux):
  
  ```sh
  :~$  source .venv/bin/activate
  ```

- **Installing the Flask**

  Run:
  
  ```sh
  (.venv):~$  pip install flask
  ```
  
  **Important Note**: After this step, you can run the `flask init` command to initiate a basic project template. However, here we want to do it from scratch with freedom!
  
## Application Factory
  
In Flask, applications are Python packages. So the first step to creating our application is to make a directory. Our application name is `best_app`.

- Inside the `flask-cool-app` directory run:

  ```sh
  cd /path_to/flask-cool-app
  mkdir best_app
  ```

- Now inside the `best_app`, create `__init__.py` to make the `best_app` a package.

  This init script also has another usage for us: **application factory**.

  This script will contain a function name `create_app()` that initiates our application and make it ready to serve.

  Inside the `__init__.py` add this code:

  ```py
  from flask import Flask
  
  def create_app():    
      app = Flask(__name__)        
      app.config.from_mapping(
          SECRET_KEY = "My_Secret_Key"
      )     
      
      return app
  ```

  **Note**: Remember to use an actual `SECRET_KEY` and never share it with anybody!

Nice! Now we have our application ready!

Now if you run these commands, you can see that your application is accessible on <http://localhost:5000>

```sh
cd /path_to/flask-cool-app
export FLASK_APP=best_app
flask run
```

## Modules

In a Flask application (package), you can have multiple modules. These modules can perform different aspects of your application such as Auth module to handle authentication. Generally, dividing your application into some small modules helps to:

- Separation of concern. Make it easier to maintain
- Disable/Enable some features without affecting the others
- Let others write modules to add functionality to the application without worrying
  about changing the core modules.

In this post, our project will have two modules: Hello and Goodbye.

- Inside the `best_app` create a new directory name `modules`.

  ```sh
  cd /path_to/flask-cool-app/best_app
  mkdir modules
  ```

- Now inside this modules directory, create three scripts:

  - `__init__.py` : to make the modules importable like a package. Will be empty.
  - `hello.py` : The first module
  - `goodbye.py` : The second module

- Now let’s write the “hello” module.

  `hello.py`
  
  ```py
  from flask import Blueprint
  
  blueprint = Blueprint('hello', __name__, url_prefix='/hello')
  
  @blueprint.route("/say", methods=["GET"])
  def say_hello():
      return "Hello!"
  ```

  and, goodbye.py:
  
  ```py
  from flask import Blueprint
  
  blueprint = Blueprint('bye', __name__, url_prefix='/bye')
  
  @blueprint.route("/say", methods=["GET"])
  def say_bye():
      return "Goodbye!"
  ```

  **Notes**
  - Each module has a function to say Hello and Goodbye.
  - Both of the use Blueprint. The advantage of defining a blueprint is that we can
    completely separate the URL config for our modules. As you see both of them have the same route “/say” but this makes no conflict since they are registering themselves with different blueprints.
  
- The next step is that we need to register our modules when creating the 
  application. Here is the added part in the `create_app()` function in our application factory (`__init__.py`)

  ```py
  from flask import Flask
  from best_app.modules import hello, goodbye
  
  def create_app():    
      app = Flask(__name__)        
      app.config.from_mapping(
          SECRET_KEY = "My_Secret_Key"
      )     
      
      app.register_blueprint(hello.blueprint)
      app.register_blueprint(goodbye.blueprint)
  
      return app
  ```

- Now re-run the Flask Dev server. After this, you can call the modules’ functions 
  like:

  ```sh
  http://localhost:5000/hello/say
  http://localhost:5000/bye/say
  ```

## Load config variables

The last step for this post is to define the config for our application. Configs variable can help us to prepare our app for different setups and also let others customize it if needed.

To define config, we create a new Python file name config.py in the best_app directory

class CoolConfig(object):
    
    MY_ENV_VAR = "XY"

This config loads the needed variable for us.

Now we need to load this config in our application factory function (__init__.py)

from flask import Flask
from best_app.modules import hello, goodbye
from best_app.config import CoolConfig

def create_app():    
    app = Flask(__name__)        
    app.config.from_mapping(
        SECRET_KEY = "My_Secret_Key"
    )     
    
    app.config.from_object(CoolConfig)    

    app.register_blueprint(hello.blueprint)
    app.register_blueprint(goodbye.blueprint)

    return app

Let’s use it! we change the say_hello() function in the Hello module to this:

from flask import current_app

@blueprint.route("/say", methods=["GET"])
def say_hello():
    return "Hello {}!".format(current_app.config.get('MY_ENV_VAR'))

Now, if you call http://localhost:5000/hello/say, you will see the message “Hello XY!” (remember to re-run the dev server)

Note: As an alternative approach, You can also use a .env file to load the environment variables.

Congrats! We initiated our Flask app!

In the future, I will do my best to explain further steps such as Database configuration, models, and dockerizing the Flask app. Let’s see!

This is the link to the source code on my GitHub: https://github.com/Pooya-Oladazimi/flask-cool-app

This post was originally published on my blog: https://www.polaz.net/flask-app-development-101-kickstarting-your-own-web-app-adventure/

## Flask App Postgres Database Initialization

**Step-by-Step Guide with Models**
Most applications we develop need Relational Databaes(s) to store different types of application data such as user data and application states. No matter which framework or programming language we use, the general steps for initiating a relational database are pretty much the same:

    Create a database
    Design the database schema and relations
    Use the framework data abstraction layer to implement the database model and needed queries
    Run database Migration to apply the designed schema to the target database.

In the last episode, I wrote about how to initiate a Flask app. Here I will show how to create a database schema in Flask.

Note: I suggest looking at the last episode since we are using the same source code.

Let’s start!
Create a Database and config the connection

In this setup, we use Postgres as our database engine. I do not go into the Postgres installation, but if you want to install it, you can look at it here. (Debian)

After the installation, first, we need to create a database user. Log in to Postgres and run this:

:$ sudo -u postgres psql 
> CREATE USER cool_user WITH PASSWORD '1234';

The database user is cool_user and the password is 1234.

Then we need to create the database. Inside the Postgres shell, run:

> CREATE DATABASE cool_db;

The last step is to grant the needed permission to the user cool_user for the database cool_db:

> GRANT ALL PRIVILEGES ON DATABASE cool_db TO cool_user;

Note: This grants all the permissions. In a real setup, just give the needed permissions for security reasons.

After setting up the database, we need to config our Flask app.

In the config.py script, we have to introduce two new variables:

    SQLALCHEMY_DATABASE_URI: This is the database connection setting. The format is: postgresql://DATABASE_USER:PASSWORD@DATABASE_HOST_NAME:DATABASE_PORT/DATABASE_NAME
    SQLALCHEMY_TRACK_MODIFICATIONS: SQLAlchemy tracks modifications to database objects and emits signals to notify the application of changes. Enabling this may have performance overhead for large applications. Choose carefully.

This is the config.py

class CoolConfig(object):
    SQLALCHEMY_DATABASE_URI = "postgresql://cool_user:1234@localhost:5432/cool_db"
    SQLALCHEMY_TRACK_MODIFICATIONS = False

Note: Our database is running on localhost. 5432 is the Postgres desfault port.

Before we implement our database schema, we need to import the database object into our app to use it. To do this, create a new script name databse.py in your app directly and create the object:

from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

This way we make the database object accessible globally in our app.
Implement the Schema: Flask Model

Before we implement our schema, we need to model it in our mind. In this example, we want to model the cars that are purchased by users:

    We need two tables: users and cars
    users table columns are: id (primary key), username, created_at (user registration date), role (user roles in our system that are student, teacher, and employee)
    cars table columns are: id (primary key), model (car name), and owner_id (who bought it, a user id)
    Each user can buy one-to-many cars

With this knowledge, we create our Flask app models. Models are:

    Python Classes
    Data abstraction layer
    Save us from writing complex SQL queries
    We use them to define database schema
    We also use them for all the functions that interact with the data layer.

In the app directory, create a new directory name models. We put our models here.
Get Pooya Oladazimi’s stories in your inbox

Join Medium for free to get updates from this writer.

Then inside this directory create a script name user.py. Put this code in it:

from best_app.database import db

class User(db.Model):
    __tablename__ = 'users'

    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    username = db.Column(db.String(), unique=True, nullable=False)    
    created_at = db.Column(db.Date, nullable=False)
    role = db.Column(db.String(), default="employee")    

    __table_args__ = (
        db.CheckConstraint(role.in_(['student', 'teacher', 'employee']), name='role_types'),      
    )


    def __init__(self, username, created_at, role):
        self.username = username        
        self.created_at = created_at        
        self.role = role
    
    def register_user_if_not_exist(self):        
        db_user = User.query.filter(User.username == self.username).all()
        if not db_user:
            db.session.add(self)
            db.session.commit()
        
        return True
    
    def get_by_username(username):        
        db_user = User.query.filter(User.username == username).first()
        return db_user

    def __repr__(self):
        return f"<User {self.username}>"

    This is our model class for the users’ table
    Pay attention that we first imported the db object that we created before in the database.py
    First, we define our table name using __tablename__
    Then, we introduce our table columns which are class properties
    We also create special constraints for the column role using __table_args__. (The constrain’s name is arbitrary)
    After that is the class constructor
    Then, we implement two functions for user registration and selecting a user based on the given username. Notice that you do not need any SQL query.
    In the end, is the model string representation if we want to print a user object for instance.

Now we implement the cars table model. In the same models directory, create a new script name car.py and put this in that:

from best_app.database import db
from best_app.models.user import User
from sqlalchemy import ForeignKeyConstraint

class Car(db.Model):
    __tablename__ = 'cars'

    id = db.Column(db.Integer, primary_key=True, autoincrement=True)     
    model = db.Column(db.String(), nullable=False)
    owner_id = db.Column(db.Integer, nullable=False)    
    
    __table_args__ = (        
        ForeignKeyConstraint([owner_id], [User.id], ondelete='NO ACTION'),        
    )


    def __init__(self, model, owner_id):
        self.model = model
        self.owner_id = owner_id      
    
    def to_dict(self):
        return {
            'model': self.model,
            'owner': self.owner_id            
        }
    
    def buy_car(self):
        record = Car.query.filter(Car.id == self.id).first()
        if not record:
            db.session.add(self)
            db.session.commit()
        
        return True

    def get_user_cars(user_id):
        records = Car.query.filter(Car.owner_id == user_id).all()
        return [record.to_dict() for record in records] 

    def __repr__(self):
        return f"<Car {self.model}>"

The section is almost exactly like the UserModel. Except:

    We have a new constraint here: ForeignKey. This is the user id that indicated who bought this car. The ondelete policy tells the database what should happen when a user is deleted. Here we tell it to do nothing. But we can also change to other policies such as cascade that also deletes all the cars related to the user that got deleted. Read more
    We also implement a to_dict function. This function transforms a SQLAlchemy object into a dictionary. Why? because SQLAlchemy objects are not JSON serializable. Therefore, we run into problems if we want to return a list of cars to the application client in JSON format. Besides, we can control what to expose from this table.
    After this, we implement two functions to buy a car and return the list of all purchased cars by a user.

Now, our models are ready. However, we still have no tables in our Postgres database. We need some final steps.
Database Initiation and Migration

The last step is to use our models and initiate the database tables. To do this we use the Flask Migration library. We also need the Postgres Database Adopter for Python. Activate the Flask app Python virtual environment and run:

(.venv) > pip install Flask-Migrate
(.venv) > pip install psycopg2

After this, we need to do three important things:

    initiate the db object for our app
    Register our models (User and Car)
    Load the migration object for Flask

We do all these steps inside our application factory:

from flask import Flask
from best_app.modules import hello, goodbye
from best_app.config import CoolConfig
from flask_migrate import Migrate
from best_app.database import db


def create_app():    
    app = Flask(__name__)        
    app.config.from_mapping(
        SECRET_KEY = "My_Secret_Key"
    )     
    
    app.config.from_object(CoolConfig)    
    
    # Database related part
    db.init_app(app)
    from best_app.models.user import User
    from best_app.models.car import Car
    migrate = Migrate(app, db)

    app.register_blueprint(hello.blueprint)
    app.register_blueprint(goodbye.blueprint)

    return app

The last step is to create our tables. Run this in the command line (venv enabled)

(.venv) > export FLASK_APP=best_app
(.venv) > flask db init
(.venv) > flask db migrate
(.venv) > flask db upgrade

Notes:

    You need to run db init only the first time initiating your database.
    the migrate command creates a migration script (SQL version of your model) inside your application.
    The upgrade command is the one that actually runs the migrations on your database.
    Every time you change your schema, you need to run the migrate and upgrade commands.

And that’s it! Our database is ready to use!

You can find the source code here: https://github.com/Pooya-Oladazimi/flask-cool-app

Hope it will be useful for you!

This post was originally published on my blog: https://www.polaz.net/flask-app-postgres-database-initialization-step-by-step-guide-with-models/

The End.

# Dockerizing Flask App with Postgres

A Step-by-Step Guide

In the previous episodes about Flask, I explained how to initiate your Flask application, and also how to set up a Postgres database for it. In this episode, I will show you how to dockerize your Flask application and its database.

Before we start, I suggest taking a look at the previous episodes since we are using the same source code and application:

- Flask App Postgres Database Initialization: Step-by-Step Guide with Models

Most applications we develop need Relational Databaes(s) to store different types of application data such as user data…

medium.com
Flask App Development 101: Kick-starting Your Own Web App Adventure
Flask is one of my favorite frameworks. I love it maybe the most (Maybe because I love Python). It is light and simple…

medium.com

Note: we use docker and docker-compose in this post. You need to install them in case you did not already:

https://docs.docker.com/engine/install/

https://docs.docker.com/compose/install/

Let’s start!
Dockerfile for the App

The first step is to create a Docker file for our application. A docker file acts as a blueprint for our application container.

Create a new file name Dockerfile in your application’s root directory. Then follow the steps in this section to implement it.

First, we set our base image (python:3.9-slim). We also update the Debian package manager and install gcc package. (Some of the Python extensions require gcc)

Note: “-y” option is essential since Debian asks for consent before installing a package. This option says yes to that automatically.

FROM python:3.9-slim

RUN apt-get -q -y update 
RUN apt-get install -y gcc

The next step is to define some environment variables and the working directory. The working directory is the path that your docker file commands execute in. We also set our container username to use later.

ENV USERNAME=cool-app
ENV WORKING_DIR=/home/cool-app

WORKDIR ${WORKING_DIR}

After that, we copy the needed directories and files to the app container.

Note: Create the file service_entrypoint.sh in your app root directory and leave it empty. I explain why we need it and what to put inside it later in this post.

COPY best_app best_app
COPY requirements.txt .
COPY service_entrypoint.sh .

The next step is to create the container user (with the username we defined before) and gives it the needed permissions.

Note: It is not recommended to run your app in the container as the root user. Always create an application user.

    First, we create the user and the group
    Then, we give the new user suitable access permissions for the working directory.
    After, we switch to the new user.
    At last, we add a new path to our Debian path list. (pip needs this for installations)

RUN groupadd ${USERNAME} && \
    useradd -g ${USERNAME} ${USERNAME}

RUN chown -R ${USERNAME}:${USERNAME} ${WORKING_DIR}
RUN chmod -R u=rwx,g=rwx ${WORKING_DIR}

USER ${USERNAME}
ENV PATH "$PATH:/home/${USERNAME}/.local/bin"

Then, we install the needed Python packages with pip.

    We upgrade the pip first to the latest version
    Then we install our packages
    We also add our Flask application as an environment variable. FLASK_APP is an environment variable used in Flask to specify the name of the Python module that contains the Flask application.
    Finally, we make our service_entrypoint (that we defined before) executable.

RUN pip install --upgrade pip
RUN pip install -r requirements.txt

ENV FLASK_APP=best_app
RUN chmod +x service_entrypoint.sh

In the end:

    We open port 5000 in our container. This is the port that we run our Flask app on.
    We initiate our migration scripts using the init command for Flask.
    Finally, we run the service_entrypoint.sh script that completes our app running. (check the next section)

EXPOSE 5000
RUN flask db init

ENTRYPOINT [ "./service_entrypoint.sh" ]

Our Dockerfile is ready now!
Service Entrypoint

Sometimes, our Dockerfile cannot run our service. The reason is that sometimes our service demands some initialization configuration that is not available when we are building our container.
Get Pooya Oladazimi’s stories in your inbox

Join Medium for free to get updates from this writer.

In these situations, we usually use a bash script so-called Entrypoint to do the job for us.

Do you remember the service_entrypoint.sh file that you created earlier? Put this script inside it:

#!/bin/bash

sleep 10
flask db migrate
flask db upgrade 
waitress-serve --port 5000 --call 'best_app:create_app'

tail -f /dev/null

What does it do?

    It creates the needed database tables based on our Flask models and migrations
    Then, it serves our app with the waitress package. Waitress is a Python Web Server Gateway Interface that we use for deploying our application in production mode. https://docs.pylonsproject.org/projects/waitress/en/stable/index.html

Note1: You can use other Python Web Server Gateway Interfaces also.

Note2: Never run your Flask app with the Flask built-in dev server.

Note3: The last line “tail -f /dev/null” is essential to keep the service running. If you miss it, the docker engine brings down the container after finishing the script successfully.
Docker Compose

The last step in dockerizing our app is to write the docker-compose file. This is the way we run our Flask app and the Postgres database in one virtual network as two separate services.

To do this, create a new YAML file in your project directory named docker-compose.yml

First, we add the Postgres Service:

version: '3.0'
services:
  db:
    image: postgres
    container_name: db
    restart: always
    ports:
      - 5123:5432
    env_file:
      - .db.env    
    volumes:
      - ./pData:/var/lib/postgresql/data
    healthcheck:      
      test: ["CMD-SHELL", "pg_isready -U $${POSTGRES_USER} -d $${POSTGRES_DB} -t 1"]
      interval: 10s
      timeout: 10s
      retries: 10
      start_period: 10s

    The service name is db. This name is important since we need it later.
    We map the Postgres default port 5432 to the localhost 5123. This part is optional. You need it if you want to check the Postgres container from your host server.
    Very Important: We bind the Postgres data directory to the pData directory in our root directory. (Docker creates this automatically). This way we persistent our database. Without this, you will lose the data if you bring down the container.
    We also need a health check. We will use it in the next section for writing the Flask app part. The reason is our Flask app requires the database to be up and ready in order to run.

Next and last, we write the Flask app service part in the docker-compose:

best_app:
    container_name: best-app
    build:
      context: .
      dockerfile: Dockerfile
    ports:
      - 8008:5000    
    depends_on:
      db:
        condition: service_healthy    
    links: 
        - db

    Our container name is best-app
    We map port 5000 of our container (the one we used for running Waitress) to the host 8008 (you can choose other ports also)
    We state that our service depends on the db service. This will check based on the health check condition we set for the Postgres container
    We also link our service to the service db. This way our service can connect to Postgres.

Hint: You can define a named network and run both services on that instead of using a link

Now that our docker-composer is ready, we need one more last step to run our service.
Last step and run

The last step is to change the Postgres connection setting in our Flask app config.

We were using localhost as the host for Postgres. We need to change that to db (our Postgres service name in the docker network).

Why? Because localhost would be our Flask app container which does not host any Postgres.

SQLALCHEMY_DATABASE_URI = "postgresql://cool_user:1234@db:5432/cool_db"

Everything is ready and we can run our dockerize service. To do this, navigate to your app root directory (where docker-compose.yml is) and run:

sudo docker-compose up --build

Congrats! Our services are up and running!

You can find the source code for this writing here: https://github.com/Pooya-Oladazimi/flask-cool-app/tree/master

This post was originally published on my blog: https://www.polaz.net/dockerizing-flask-app-with-postgres-a-step-by-step-guide/

The End.