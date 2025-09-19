
# Jam.py - How to

## Content

- [How Jam.py works internally?](#how-jampy-works-internally)
  - [Saving and loading of the task tree](#saving-and-loading-of-the-task-tree)
  - [Application builder](#application-builder)
  - [Web application](#web-application)
- [How to text search by completly phrase?](#how-to-text-search-by-completly-phrase)
- [How to find detail record?](#how-to-find-detail-record)
- [How to create a edit form without access to a view form?](#how-to-create-a-edit-form-without-access-to-a-view-form)
- [How to expose Jam.py as as web services?](#how-to-expose-jampy-as-as-web-services)
- [How to access edit form when access rigth is on view form only?](#how-to-access-edit-form-when-access-right-is-on-view-form-only)
- [How to hide some menu options from general users, and unhide the same for other?](#how-to-hide-some-menu-options-from-general-users-and-unhide-the-same-for-other)
- [Are users transferred when using app import and export functions?](#are-users-transferred-when-using-app-import-and-export-functions)
- [How to do with Stored Procedure?](#how-to-do-with-stored-procedure)
- [How to create dependent drop-down lists?](#how-to-create-dependent-drop-down-lists)
- [How to do with barcode?](#how-to-do-with-barcode)
- [How to do with the detail item?](#how-to-do-with-the-detail-item)
- [How to create a text area control as part of catalog?](#how-to-create-a-text-area-control-as-part-of-catalog)
- [How to change the item_type of the catalog to detail_item?](#how-to-change-the-item_type-of-the-catalog-to-detail_item)
- [How to do with batches process?](#how-to-do-with-batches-process)
- [How to do with asynchronous/synchronous request to server?](#how-to-do-with-asynchronoussynchronous-request-to-server)
- [How to do with details?](#how-to-do-with-details)
- [How to do a multiselect filter in user interface or OR filter?](#how-to-do-a-multiselect-filter-in-user-interface-or-or-filter)
- [How to do with conversion ods document?](#how-to-do-with-conversion-ods-document)
- [How to do with strong validation?](#how-to-do-with-strong-validation)
- [How to automated Jam.py export?](#how-to-automated-jampy-export)
- [How to enable users to change their password?](#how-to-enable-users-to-change-their-password)
- [How to integrate help tab?](#how-to-integrate-help-tab)
- [How to do with virtual tables?](#how-to-do-with-virtual-tables)
- [How to create a static home page in jam.py?](#how-to-create-a-static-home-page-in-jampy)
- [How to do with strong validation II?](#how-to-do-with-strong-validation-ii)
- [How to do with registreted form?](#how-to-do-with-registreted-form)

## How `Jam.py` works internally

### Saving and loading of the task tree

[Content](#content)

The library is based on a task tree. The task tree is defined in the `items.py` module. The ancestor class for tree elements is `AbstractItem`. It specifies:

- the relationship between the elements,
- the immediate owner - `owner`,
- the list of child elements - `items` and
- the root element of the tree – `task`.

Each element has:

- the `get_info` and
- the `set_info`

methods. The `get_info` method returns information about the element and its children in a format that allows it to be saved using the `json` library as text. The result of `get_info` of the task can be saved to a file or sent to the client to build a tree in the browser. The task's `set_info` method is used to build a tree from the information provided by the `get_info` method.

The classes in the `server_classes.py` module are mostly derived from the classes declared in the `items.py` module. The `ServerDataset` class in same module inherits from `Dataset` class from the `dataset.py` module, in which code for a dataset (list of records) is. The SQL class in `sql.py` module, contains queries to get a list of records and save data.

### Application builder

[Content](#content)

The Application Builder project is located in the `Builder` folder of the distribution. This project is used to create a new version of App Builder using an older version. The project has everything needed to develop the Builder further.

The application is started as any other `Jam.py` application. The development process is similar to the development of a regular `Jam.py` application, except that all server code must be located in the `task` Server module. All functions in Server module that are called from client code using the `server` method should be registered in the `register_events` function at the end of the module using the `register` method, and server item events should be defined in this function.

After making and testing changes, the `Prepare files` button is used. The application will create necessary files in the `jam_files` folder.
If the folder does not exist, it will create one. Content of this folder should be copied to the `jam` folder of the `Jam.py` distribution package.

### Web application

[Content](#content)

When a web application is created with `wsgi.py`, the builder Task tree is created – the admin object. The admin loads the task using `get_info` method from the `builder_structure`.info file that was saved when the `Prepare files` button was clicked. This is done in the `admin.py` module that is located in the admin folder of the package. The task tree of the project is created when the first request is received by the web application. The `task.py` module from the `admin` folder of the package contains the code that creates the project Task tree. It uses the admin to read the data from `admin.sqlite` database. To speed up the process the information from corresponding tables is loaded to dictionaries.

## How to text search by completly phrase?

[Content](#content)

The search-input on the `view` form matches in `CONTAINS_ALL` mode, which splits the search string up into words using space delimiters.

Using a `Filter` form input set to `CONTAINS` mode does not split the string, and so can be used to search for a phrase. However, if I wanted to search for multiple phrases, I would need multiple Filter form inputs for the same field.

This works fine, but I thought there might be a simpler way.

I made some small changes to the `Jam.py` source code to alter the behaviour of the CONTAINS_ALL match, so that it handles words and phrases through a single input. Now if I enter a search string like:   `thank you "very much" for "asking me"`   it finds records with the words thank, you, for, and the phrases very much, asking me,  which is the behaviour I want.

If anyone is interested, here are the changes (shown in red):

To `jam.py` - change behaviour of CONTAINS_ALL:

```py
import shlex

def where_clause(self, query, db_module=None):
    if db_module is None:
        db_module = self.task.db_module
    conditions = []
    filters = query['__filters']
    deleted_in_filters = False
    if filters:
        for field_name, filter_type, value in filters:
            if not value is None:
                field = self._field_by_name(field_name)
                if field_name == self._deleted_flag:
                    deleted_in_filters = True
                if filter_type == consts.FILTER_CONTAINS_ALL:
                    try:
                        values = shlex.split(value
                    except:
                        values = value.split()

                    for val in values:
                        conditions.append(self._get_condition(field, consts.FILTER_CONTAINS, val, db_module))
```

To jam.js - change field text highlighting to match:

```js
function highlight(text, search) {
    var i = 0,
        result = text,
        substr,
        start,
        str,
        strings,
        pos,
        p = [];
    if (search) {
        text += '';
    // ***
        //strings = search.toUpperCase().split(' ')
        var regexp = /[^\s"]+|"([^"]*)"/gi;
        var strings = [];
        var cap_srch = search.toUpperCase();
        do {
            var match = regexp.exec(cap_srch);
            if (match != null)
            {
                strings.push(match[1] ? match[1] : match[0]);
            }
        } while (match != null);
    // ***
```

## How to find detail record?

[Content](#content)

I wrote the code to find the track in the invoice. It should be placed in the `invoice.invoice_table.js`.

```js
function on_view_form_created(item) {
    item.add_view_button('Find').click(function() {
        find_record(item);
    });
}        

function find_record(item) {
    let copy = task.invoice_table.copy({handlers: false});
    copy.open({fields: ['track'], open_empty: true});

    copy.edit_options.title = 'Find';
    copy.edit_options.history_button = false;

    copy.on_edit_form_created = function(c) {
        c.edit_form.find('#ok-btn')
            .text('Find')
            .off('click.task')
            .on('click', function() {
                try {
                    c.post();
                    item.locate('track', c.track.value);
                    c.cancel_edit();
                }
                finally {
                    c.edit();
                }
            });
    };
    copy.on_edit_form_keyup= function(c, e) {
        if (e.keyCode === 13 && e.ctrlKey) {
            e.preventDefault();
            return true;
        }
    };
    copy.append_record();
}    
```

## How to create a edit form without access to a view form?

[Content](#content)

You have to open an item first. For example, if you want to add a new record it can be done this way:

```js
task.customers.open(
  { open_empty: true },
  function() {
    task.customers.append_record();
  }
);
```

To edit a record with id field value equal 12:

```js
task.customers.open(
  { where: {id: 12} },
  function() {
    task.customers.edit_record();
  }
);
```

## How to expose Jam.py as as web services?

[Content](#content)

There is an `on_ext_request` event on the server side.

This event is triggered when the server receives a post request, which can be sent from other applications or services.

This request can be processed and can return any data that you need.

## How to access edit form when access right is on view form only?

[Content](#content)

You can use `create_edit_form` method insted of `edit_record` when `can_edit` returns false.

To close form use `close_edit_form`.

## How to hide some menu options from general users, and unhide the same for other?

[Content](#content)

The dynamic menu is created in the `on_page_loaded` event handler in the task client module:

```js
task.each_item(function(group) {
  var li,
      ul;
  if (group.visible) {
    li = $('<li class="dropdown">
             <a class="dropdown-toggle" data-toggle="dropdown" href="#">' +   
               group.item_caption + 
               ' <b class="caret"> </b></a></li>');
    $("#menu").append(li);
    if (group.items.length) {
      ul = $('<ul class="dropdown-menu">'); 
      li.append(ul);
      group.each_item(function(item) {
        if (item.visible) {
          ul.append($('<li>')
          .append($('<a class="item-menu" href="#">' + item.item_caption + '</a>')
          .data('item', item)));                    
        }
      });
    }
  }
});
```

replace:

```js
if (item.visible) {
```

with:

```js
if (item.visible && item.can_view()) {
```

to exclude the items that user can not view.

## Are users transferred when using app import and export functions?

[Content](#content)

Users are not transfered, only roles.

To get information about item's access rights use `can_view`, `can_create`, `can_edit` and `can_delete` methods. This method are available on the client and on the server.

The `task` item on the client have `user_info` attribute with  the following attributes: `user_id`, `role_id`, `user_name`, `role_name`.

On the server each item have `session` property that have the `user_info` attribute. Use it to get `user_id`, `role_id`, `user_name`, `role_name` information.

## How to do with Stored Procedure?

[Content](#content)

Stored procedures on the backend MariaDB database of my application take care of some status flags in the data across various tables to make it easier for users to see what is happening.

I am not sure where in the code to fire them and how the syntax will work and how to refresh data on the page once they are updated.

Questions:

1. Must the `call sp_update_statusflags` stored procedure be executed in
   the `on_apply` server side event? A Python `execute` class seems to be the backend syntax for executing "non-SELECT" sql (i.e. not returning any data), but I get an error.

2. If I wanted to update a `PrintedYN` boolean in the sql database when
   clicking on a report button, how would I be able to update the flag from the client code (either SQL or framework client or server calls could work)?

3. Also if one would want to run a stored procedure from a report
   parameters form - how could one do that please?
  
## How to create dependent drop-down lists?

[Content](#content)

You can do this for a lookup field for which lookup item is set by writing the `on_field_select_value` event handler:

<http://jam-py.com/docs/refs/client/item/on_field_select_value.html>  

and using set_where method:

<http://jam-py.com/docs/refs/client/item/m_set_where.html>

For a lookup field for which lookup values are set you can change lookup_values attribute of the field.

For example, if you add the following code to the Tracks client module in the demo:

```js
function on_after_scroll(item) {
  if (item.rec_no % 2 === 0) {
    item.media_type.lookup_values = [
      [1, "MPEG audio file"],
      [2, "Protected AAC audio file"], 
      [3, "Protected MPEG-4 video file"], 
      [4, "Purchased AAC audio file"], 
      [5, "AAC audio file"]                
    ];
  } else {
    item.media_type.lookup_values = [
      [1, "MPEG audio file"],
      // [2, "Protected AAC audio file"], 
      [3, "Protected MPEG-4 video file"], 
      // [4, "Purchased AAC audio file"], 
      [5, "AAC audio file"]                
    ];
  }
}
```

There will be 5 items in the list for even rows and 3 for odd ones. You must change `lookup_values` before inputs are created.

## How to do with barcode?

[Content](#content)

I added the "price" field to the "products" and "qty" table, "price" and "total" fields to "barcodes."

I deleted all the code from the barcodes server module and rewritten the client module as follows:

```js
function on_view_form_shown(item) {
  var input = item.view_form.find('#barcode');

  input.focus()
    .on( "keydown", function(e) {
      var text = input.val(),
      products = task.products.copy();
    
      if (e.which === 13 && text) {
        products.set_where({barcode: text});
        products.open();
    
        if (products.record_count()) {
          item.insert();
          item.barcode.value = input.val();
          item.product_name.value = products.name.value;
          item.price.value = products.price.value;
          item.post();
          setTimeout(function(){ 
            $('.dbtable.barcodes .inner-table').focus(); 
          }, 100); 
        } else {
          item.warning('Product with barcode ' + text + ' not found');
        }
        input.val('');
      }
    });
}

function init_table(item, options) {
  options.editable = true;
  options.editable_fields = ['qty'];
  options.dblclick_edit = false;
}

function on_after_post(item) {
  item.apply();
}

function on_field_changed(field, lookup_item) {
  var item = field.owner;
  if (field.field_name === 'qty') {
    item.total.value = item.price.value * field.value;
    setTimeout( function() {item.view_form.find('#barcode').focus();},100);
  }
}
```

When enter key is pressed I look for a record in the products with the barcode.

If it exists I set values to the barcodes fields and set focus to the table, otherwise I show a message.

I remove apply method, insted of it I written `on_after_post` event handler that calls it.

The table is created in the `on_view_form_created` event handler in the task client module when a div with `class view-table` exists in the view html template. Before creating it checks if `init_table` function is defined in the item client module and if it is, it calls it and passes parameters used to create table.

So I defined init_table to change the options passed to the create_table module:

<http://jam-py.com/docs/refs/client/item/m_create_table.html>

There are other ways. But it is the simplest in this case.

When focus is on the table you can input the qty field value. Then `on_field_changed` event is triggered. The "total" field value is calculated and focus is changed to the barcode input.

## How to do with the detail item?

[Content](#content)

To work with the detail you must open it. You can do it in the on_after_scroll event handler:

```js
var ScrollTimeOut;

function on_after_scroll(item) {
  clearTimeout(ScrollTimeOut);
  ScrollTimeOut = setTimeout(
    function() {
      item.invoice_table.open(true);
    },
    100
  );
}
```

or in the on_edit_form_created:

```js
function on_edit_form_created(item) {
  item.invoice_table.open.open();
```

## How to create a text area control as part of catalog?

[Content](#content)

Try to do the following in the client module of the catalog:

```js
function on_edit_form_created(item) {
    item.fld.data_type = task.consts.BLOB
}
```

where fld is the name of the field.

## How to change the item_type of the catalog to detail_item?

[Content](#content)

Try to change `type_id` field in the `sys_items` table of admin.sqlite to 12, then in manual node in the builder set `mater_id` and `master_rec_id` fields, and you must change the `parent` field to 4.

## How to do with batches process?

[Content](#content)

The on_before_open event is triggered before the open method sends the request to the server to fetch data.

The on_after_open event is triggered when data is received and we can work with it.

## How to do with asynchronous/synchronous request to server?

[Content](#content)

The code bellow sends an asynchronous request to the server (true means asynchronous).

And then before the request is executed and the client receives data you start the loop.

```js
contracts.open(true);
contracts.each(function(c_rec) { 
  // …
}
```

Instead, you must do (asynchronous):

```js
contracts.open(function(contracts)  {
  contracts.each(function(c_rec) { 
    // …
  }
}
```

Or (synchronous):

```js
contracts.open();
contracts.each(function(c_rec) { 
  // …
}
```

## How to do with details?

[Content](#content)

I have a catalog with two associated detail views, I have created some buttons which I would like to use to switch between the two details views.  Alternatively, is it possible to have the detail views in tabs?

I plan to view more than one detal in tabs.

Now you can use the following code in the item client module do switch between details with buttons:

```js
function on_view_form_created(item) {
  item.add_view_button('Detail2').click(function(){
    set_visible_detail(item, item.detail2);
  });
  item.add_view_button('Detail1').click(function(){
    set_visible_detail(item, item.detail1);
  });
  item.table_options.height = $(window).height() - $('body').height() - 270;
  set_visible_detail(item, item.detail1);
}

function set_visible_detail(item, detail) {
  item.visible_detail = detail;
  detail.create_table(item.view_form.find('.view-detail'), {height: 250});
  detail.open(true);
}

var ScrollTimeOut;

function on_after_scroll(item) {
  clearTimeout(ScrollTimeOut);
  ScrollTimeOut = setTimeout(
    function() {
      item.visible_detail.open(true);
    },
    100
  );
}
```

Please, uncheck view_detail attribute in the item's view form dialog.

## How to do a multiselect filter in user interface or OR filter?

[Content](#content)

My records have a status field (which is a lookup list).

My users would like to be able to look at records in one of two statuses.
I have tried using an IN filter but that doesn't seem to work.
Forgot to add, I can get it working in Javascript, but not via UI.

The following works:

```js
task.projects.open({where: {status__in: [4,5]}});
```

## How to do with conversion .ods document?

[Content](#content)

When manually running a command line to convert an ods document on the command line as "root" there is no issue, but "www-data" the apache 2.4 webserver user, does not have a shell or a temporary directory defined for security purposes (if I undertand correctly).

The challenge is that when the script in the framework executes the headless conversion command there is a workaround that can help and appears to be implemented in the `jam.py` framework 4.0.80.

As root:

```shell
sudo -u www-data soffice --headless --convert-to pdf test.ods
```

result in:

```shell
[Java framework] Error in function createSettingsDocument (elements.cxx).

javaldx failed!
Warning: failed to read path from javaldx
```

The framework ( `server_classes.py` ) also applies the following on *nix OSses a prefix `"export HOME= /tmp "` to the command before the headless convert command, but from the command line it produces the following error:

```shell
sudo -i -u www-data export HOME=/tmp && soffice --headless --convert-to pdf test.ods
```

will result in:

```shell
This account is currently not available.
```

Of course this results in only receiving .ods formatted report files at present.

I think a lot of this problems are not necessarily related to jam-py. For example, one can decide to run Apache infront of python app, which might solve the issue with permissions.

This can be simple config like:

```shell
cat prox2.conf

<VirtualHost *:80>
  ProxyPreserveHost On

  ProxyPass / http://127.0.0.1:8080/
  ProxyPassReverse / http://127.0.0.1:8080/

  RewriteEngine On
  RewriteRule .* - [E=SERVER_PORT:%{SERVER_PORT},NE]
  RequestHeader set X-Forwarded-Port %{SERVER_PORT}e
</VirtualHost>
```

I can now get the `pdf`, `xls`, `html`, and `ods` as I would from the local python wsgi server.

I also added two parameters to the frameworks's conversion code and it seemed to solve the problem with Ubuntu www-data's access to headless conversion: ,`'--invisible'`, `'--norestore'`.

This worked on `Jam.py` 4.0.80 and Ubuntu 16.04 : ( `server_classes.py` ).

```py
def convert_report(self, report, ext):
  converted = False
  with self.conversion_lock:
    try:
      from subprocess import Popen, STDOUT, PIPE

      if os.name == "nt":
        import _winreg
        regpath = "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\
                   App Paths\\soffice.exe"
        root = _winreg.OpenKey(_winreg.HKEY_LOCAL_MACHINE, regpath)
        s_office = _winreg.QueryValue(root, "")
      else:
        s_office = "soffice"
        convertion = Popen([
          s_office, 
          '--headless',
          '--invisible',   
          '--norestore', 
          '--convert-to', 
          ext, 
          report.report_filename, 
          '--outdir', 
          os.path.join(self.work_dir, 'static', 'reports') ],
          stderr=STDOUT,stdout = PIPE)#, shell=True)
        out, err = convertion.communicate()
        converted = True
    except Exception as e:
      print(e)
  return converted
```

I created the `on_convert_report` event to make it possible to send a report to some server on this or remote computer to convert it. You can write some simple server to do it if you need.

## How to do with strong validation?

[Content](#content)

For a request, that after the validation fall in the field focus remain in the field, you can use the following code:

Instead of `on_field_validate(field)`, you should write your `check_valid_field(field)` method

```js
check_valid_field(field) {
  …
}
```

After that, we define validation for each field in `on_edit_before_shown` event.

```js
function on_edit_form_shown(item) {
  item.each_field( function(field) {
    var input = item.edit_form.find('input.' + field.field_name);

    input.blur( function(e) {
      var err;

      if ($(e.relatedTarget).attr('id') !== "cancel-btn") {
        err = check_field_value(field);
        if (err) {
          item.alert_error(err);
          input.focus();             
        }
      }
    });
  });
}
```

And of course in the end, let's do validation of the complete form, on the `on_before_post` event

```js
function on_before_post(item) {
  item.each_field(function(field) {
    var err = check_field_value(field);
    if (err) {
      throw err;
    }
  });
}
```

## How to automated Jam.py export?

[Content](#content)

Below is a short script that allows you to automate the backup the config of your `Jam.py` app.

```py
import requests
import json
import os

# change these variables - or better yet, store in a config file
login = "admin"
password = "superstrongpassword"
url = 'http://my.jam-py.app'
downloadfolder = r'C:\Config backups'

# login
session = requests.Session()
data = json.dumps(["login",0,0,[login,password],time.time()*1000])
headers = {
 'Content-Type' : 'application-json'
}
r = session.post(url + '/api', data=data, headers=headers)

# run the export task
data = json.dumps(["server",0,0,["server_export_task",[1,url]],time.time()*1000])
r = session.post(url + '/api', data=data)

# get export pointer
pth = r.json()['result']['data'][0]
r = session.get(pth)

# write down export file
open(os.path.join(
  downloadfolder, 
  os.path.basename(pth)), 'wb').write(r.content
)
```

## How to enable users to change their password?

[Content](#content)

You should created a catalog Users that store information about users and their logins and passwords, which can be viewed and changed only by admin. And lookup list Roles that have the same ids and role names that roles in the builder.

In the task server module I defined on_login event handler:

```py
def on_login(task, login, password, ip, session_uuid):
  users = task.users.copy(handlers=False)
  
  users.set_where(login=login, password=password)
  users.open()

  if users.record_count() == 1:
    return {
      'user_id': users.id.value,
      'user_name': users.name.value,
      'role_id': users.role.value,
      'role_name': users.role.display_text
    }      
```

Now when you set safe mode to true in the builder, the app will use information from users to login.

To change password I added a menu in the `index.html`:

```html
<li id="pass"><a href="#">Change password</a></li>
```

and in the task client module I use it this way:

```js
$("#menu-right #pass a").click(function(e) {
  e.preventDefault();
  task.change_password.open({open_empty: true});
  task.change_password.append_record();
});
```

`change_password` is a new catalog with virtual table set to `true`. It is used to display `old_password` and `new_password` fields.

In the `change_password` client module there is the following code:

```js
function on_edit_form_created(item) {
  item.edit_form.find("#ok-btn")
  .off('click.task')
  .on('click', function() { change_password(item) });
}

function change_password(item) {
  item.post();
  item.server('change_password', [item.old_password.value, item.new_password.value], 
    function(res) {
      if (res) {
        item.warning('Password has been changed. <br> The application will be 
          reloaded.', function() {
            task.logout();
            location.reload(); 
          }
        );
      }
    }
  );
}

function on_field_changed(field, lookup_item) {
    var item = field.owner;

    if (field.field_name === 'old_password') {
        item.server('check_old_password', [field.value], function(error) {
            if (error) {
                item.alert_error(error);    
            }
        });
    }
}
```

It reassigns OK button click event to `change_password` function the functions send requests to the server to check old password (server module):

```py
def check_old_password(item, old_password):
    user_id = item.session['user_info']['user_id']
    users = item.task.users.copy(handlers=False)

    users.set_where(id=user_id)
    users.open()

    if users.record_count() == 1 and users.password.value == old_password:
        return 

    else:
        return 'Invalid password'            
```

and change password:

```py
def change_password(item, old_password, new_password):
    user_id = item.session['user_info']['user_id']
    users = item.task.users.copy(handlers=False)
    users.set_where(id=user_id)
    users.open()

    if users.record_count() == 1 and users.password.value == old_password:
        users.edit()
        users.password.value = new_password
        users.post()
        users.apply()
        return True

    else:
        return False
```

They use session to get id of the user that send the requst and find corresponding user.

After changing the password the client reloads.

The `on_login` event handler doesn't influence the login in in the builder.

## How to integrate help tab?

[Content](#content)

```js
$("#menu")
  .append($('<li></li>')
    .append($('<a href="#">Help</a>')
      .click(function(e) { 
        e.preventDefault(); 
        task.message( 
          $('<iframe 
              src="http://..." name="help" style="width:100%; height:100%">  
             </iframe>'), 
          { title: 'Help', 
            margin: 0, 
            width: $(window).width() - 50,                        
            height: $(window).height() – 100
          }
        );
      })
    )
  );
```

## How to do with virtual tables?

[Content](#content)

When you create an item with the `virtual_table` attribute set to `true`, the Admin doesn't create a table in the project database. Such items can be used in the following way:

First of all you have two modules - on the server and on the client. You can use them to defined some functions, this functions could be accessed in other modules of the project. So, for example, if you create a lib item and define a function `do_something` in its client module function

```py
do_something(item) { 
  ...// some code 
} 
```

This function can be accessed in other modules via `task` attribute of the item (you can define any arguments for this functions). For example:

```py
item.task.lib.do_something(item) 
```

This works the same way on the client. Please note, that on the client you have to load the module first, when the `Dynamic JS modules loading` attribute in the project parameters is set.

You can use a default html templates or define new ones, to create forms. This way the dashboard of the demo works.

You can add field to such item and add the records dynamiclly on the client. For example, you can create an email item with name, email and mess fields. Then you can add Send email button to the customers view form and use the following code in the customers client module:

```js
function on_view_form_created(item) { //... some code here   
  item.view_form.find('#email-btn')
    .click(function() {   
      item.task.email.open({open_empty: true}); 
      item.task.email.append_record(); 
    }); 
} 
```

When user clicks this button, a new record will be appended to the email item dataset and an edit form of the email will be created.

You don't have to create inputs, check if values are filled in and so on. I am writing such example in the demo app now.

You as well can add several records to the item dataset. That will be displayed when you create a table using `create_table` method.

```js
item.open({open_empty: true}); 
  for (var i = 0; i < 10; i++) { 
    item.append(); 
    item.some_field.value = 'record '+ i; 
    item.post(); 
} 
```

In the Admin this is used in `view`, `edit`, `order`, `detail` dialogs.

If you have some external database you can create an `virtual_table` item with the same fields as in this database table and the same table name,
and then write the `on_open` event handler in the server module (for sqlite db):

```py
import jam.db.db_modules as db

def on_open(item, params): 
  item = item.copy() 
  res = [] 
  mess = '' 
  sql = item.get_select_statement(params, db.get_db_module(db.SQLITE)) 

  try: connection = sqlite3.connect(db_path) // path to sqlite db 
    cur = connection.cursor() 
    cur.execute(sql) 
    res = cur.fetchall() 

  except Exception, e: 
    print ('%s: on_open error %s') % (item.item_name, e.message) 

  finally: 
    if cur: 
      cur.close() 
      connection.close() 

  return res, mess 
```

This code will generate the sql query for this db table the same way as for project db and return dataset that will be displayed on the client.

You have to create a group for this item without common fields.

## How to do with Oracle and Python3?

[Content](#content)

```shell
sudo apt-get install python3-pip
sudo pip install --upgrade pip
sudo pip install cx_Oracle
```

It might be beneficial to install a virtual env not to disturb your Desktop or Server install with Python 3.

See: <https://stackoverflow.com/questions/10763440/how-to-install-python3-version-of-package-via-pip-on-ubuntu> or similar...

If not using local Oracle, the Oracle Client is needed, ie. see: <https://gist.github.com/hangtwenty/5671566>

After that, test the connection to XE (XE is the ORACLE_SID, HR is the schema) with:

```shell
$ORACLE_HOME/bin/sqlplus HR/HR@//host_or_IP:1521/XE
```

OR with the /etc/tnsnames.ora content:

```shell
XE =
  (DESCRIPTION =
    (ADDRESS = (PROTOCOL = TCP)(HOST = host_or_IP)(PORT = 1521))
    (CONNECT_DATA =
      (SERVER = DEDICATED)
      (SERVICE_NAME = XE)
    )
  )
```

like this:

```shell
$ORACLE_HOME/bin/sqlplus HR/HR@XE
SQL*Plus: Release 11.2.0.4.0 Production on Mon Sep 4 13:09:14 2017
Copyright (c) 1982, 2013, Oracle.  All rights reserved.
Connected to:
Oracle Database 11g Express Edition Release 11.2.0.2.0 - 64bit Production
```

Now we can set `jam.py` to XE as the database.

## How to create a static home page in jam.py?

[Content](#content)

Every professional, cloud based business application need to have a static homepage which will load after user log in to the system.

The homepage may contain various elements, such as welcome message, quick video tutorial or just a navigation panel to access different parts of the solution. In this article I will describe how to create a static landing page in jam-py.

First you need to create your content and put it inside the `index.html` as a home_page-template.

Sample home page template with a header text is displayed below:

```html
<div class="home_page-view">
  <h2>Welcome stranger - Enjoy our App!</h2>
</div>
```

Insert the above code between the other templates in jam-py `index.html`.

The next step is to create a new item view. Click on Journals in the project tree view.

Click “New” button to add a new journal. Ensure that the value in the “Name” field is “home_page”. This is important as it needs to match the class name in the template we have created (line 1: `class=”home_page-view”`).

Of course you can name it differently but these two things needs to match, otherwise no content will be displayed.

As we don’t want to create any database table – check the `Virtual Table` checkbox as shown on the image below, then save.

To test your project, open it in user view and select “Home Page” from the journals menu.

Your home page content defined in the `index.html` should be displayed.

The last step is to set this page to load on default when user starts the application.

To do this you need to click “Task” in the project tree then edit “Client Module”.

In the `on_page_loaded` event, at the end of its content replace the following line of code:

```js
$('#menu.item-menu:first').click();
```

with this:

```js
task.home_page.view($("#content"));
```

The above line is telling the system to open the "home_page" journal we have just created, when system is loaded.

## How to add a icon to a button?

[Content](#content)

You must add a button in the html template in the index.html file. For example:

```html
<button type="button" id="ok-btn" class="btn"> <i class="icon-ok"></i> </button>
```

The icons you can use see there:

<http://getbootstrap.com/2.3.2/base-css.html#icons>

After that you find this button using its id in the corresponding form and assign an event handler to it using JQuery:

```js
function on_view_form_created(item) { 
  item.view_form.find('#ok-btn')
    .click(function() { 
      item.append_record(); 
    }) 
} 
```

I plan to create in future actions that can be displayed as buttons or menu items. But now you must do it this way.

## How to do with strong validation II?

[Content](#content)

```js
function on_edit_form_shown(item) {
    item.each_field( function(field) {
        var input = item.edit_form.find('input.' + field.field_name);

        input.blur( function(e) {
            var err;

            if ($(e.relatedTarget).attr('id') !== "cancel-btn") {
                err = check_field_value(field);
                if (err) {
                    item.alert_error(err);
                    input.focus();             
                }
            }
        });
    });
} 


function on_before_post(item) {
    item.each_field(function(field) {
        var err = check_field_value(field);
        if (err) {
            throw err;
        }
    });
}
```

## How to do with registreted form?

[Content](#content)

Start server and type in the browser

<http://127.0.0.1:8080/regist.html>

```html
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <title>`Jam.py` demo</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link rel="icon" href="static/img/j.png" type="image/png">

        <link href="jam/css/bootstrap.css" rel="stylesheet">            <!--do not modify-->
        <link href="jam/css/bootstrap-responsive.css" rel="stylesheet">
        <link href="jam/css/bootstrap-modal.css" rel="stylesheet">
        <link href="jam/css/jam.css" rel="stylesheet">                  <!--do not modify-->
    </head>
    <body>
        <div class="modal fade in" style="top: 50%; margin-top: -150px; width: 500px;">
            <div class="modal-header">
                <h4 class="modal-title">Register</h4>
            </div>
            <form id="login-form" target="dummy" class="form-horizontal" style="margin: 0;">
                <div class="control-group">
                    <label class="control-label" for="name">Name</label>
                    <div class="controls">
                        <input type="text" id="name" placeholder="Login">
                    </div>
                </div>
                <div class="control-group">
                    <label class="control-label" for="login">Login</label>
                    <div class="controls">
                        <input type="text" id="login" placeholder="Login">
                    </div>
                </div>
                <div class="control-group">
                    <label class="control-label" for="password1">Password</label>
                    <div class="controls">
                        <input type="password" id="password1" placeholder="Password" autocomplete="on">
                    </div>
                </div>
                <div class="control-group">
                    <label class="control-label" for="password2">Repeat password</label>
                    <div class="controls">
                        <input type="password" id="password2" placeholder="Repeat password" autocomplete="on">
                    </div>
                </div>
                <div class="alert alert-success" style="margin: 0; display: none">
                    You have been successfully registered
                </div>
                <div class="alert alert-error" style="margin: 0; display: none">
                </div>
                <div class="form-footer">
                    <input type="button" class="btn expanded-btn pull-right" id="register-btn" value="OK" tabindex="3">
                </div>
            </form>
        </div>

        <script src="jam/js/jquery.js"></script>
        <script>
        $(document).ready(function(){

            function register(name, login, password) {
                $.ajax({
                    url: "ext/register",
                    type: "POST",
                    contentType: "application/json;charset=utf-8",
                    data: JSON.stringify([name, login, password]),
                    success: function(response, textStatus, jQxhr) {
                        if (response.result.data) {
                            show_alert(response.result.data);
                        }
                        else {
                            $("div.alert-success").show();
                            setTimeout(
                                function() {
                                    window.location.href = "index.html";
                                },
                                2000
                            );
                        }
                    },
                    error: function(jqXhr, textStatus, errorThrown) {
                        console.log(errorThrown);
                    }
                });
            }
            function show_alert(message) {
                $("div.alert-error")
                    .text(message)
                    .show();
            }

            $('input').focus(function() {
                $("div.alert").hide();
            });

            $("#register-btn").click(function() {
                var name = $("#name").val(),
                    login = $("#login").val(),
                    password1 = $("#password1").val(),
                    password2 = $("#password2").val();
                if (!name) {
                    show_alert('Name is not specified');
                }
                else if (!login) {
                    show_alert('Login is not specified');
                }
                else if (!password1) {
                    show_alert('Password is not specified');
                }
                else if (password1 !== password2) {
                    show_alert('Passwords do not match');
                }
                else {
                    register(name, login, password1)
                }
            })
        })
        </script>

    </body>
</html>
```

The the register form will appear, fill in the fields and click ok, the javascript will send to the server the ajax post request with url: `ext/register` with params `name`, `login`, `password`,

When server receives the request starting with `ext/` it triggers the `on_ext_request` event.

In the task server module there is `on_ext_request` event handler:

```py
def on_ext_request(task, request, params):
    reqs = request.split('/')
    if reqs[2] == 'register':
        name, login, password = params
        users = task.users.copy(handlers=False)
        users.set_where(login=login)
        users.open()
        if users.record_count():
            return 'There is a user with this login'
        users.append()
        users.name.value = name
        users.login.value = login
        users.password.value = password
        users.role.value = 2
        users.post()
        users.apply()
```

It ckecks if there is `register` in url and then looks if there is no user with the login and then register him.

If you register as

```shell
login: dan
password: 11111
```

you will be able to view and create and edit users and their passwords.

There is a way to register users in `sys_users` table in the admin.sqlite.
Comment the `on_login` and `on_ext_request` and add the following code:

```py
def on_ext_request(task, request, params):
    reqs = request.split('/')
    if reqs[2] == 'register':
        name, login, password = params
        users = task.app.admin.sys_users.copy(handlers=False)
        users.set_where(f_login=login)
        users.open()
        if users.record_count():
            return 'There is a user with this login'
        users.append()
        users.f_name.value = name
        users.f_login.value = login
        users.f_password.value = password
        users.f_role.value = 2
        users.post()
        users.apply()
```

This way new user will be registered in the admin. The work with admin tables is not documented yet.

[Content](#content)
