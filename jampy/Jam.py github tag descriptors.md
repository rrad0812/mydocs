
# Jam.py V5 github tag descriptors

## V5.4.136

- `distutils` `copy_tree` replaced with `shutil` `copy_tree` for python versions where `distutils`
  is not supported

## V5.4.135

- Greek translation updated

## V5.4.134

- Updated translations into Greek and Serbian, added Bosnian translation.

## V5.4.133

- `on_param_changed` event added
- `selections` mechamizm modified, `selections` are stored in order they were added
- `detail virtual tables` bug fixed

## V5.4.129

- report `hide_columns` method bug fixed

## V5.4.127

- `on_apply` bug when `pk` field name is not `'id'` fixed
- `on_upload` bug on windows os due to `':'` in file name fixed

## V5.4.126

- task `create_menu` method corrected

## V5.4.125

- `create_menu_item` bug fixed

## V5.4.124

- `Privileges` button added to reports

## V5.4.123

- toipc to docs added: `How to create a custom menu`
- task's `create_menu` method corrected

## V5.4.122

- `export` prject to metadata file bug fixed.
- `relative source file path` changed to `absolute path`

## V5.4.121

- `Docs syntax errors` reported by Drazen Babic fixed

## V5.4.120

- Changes from unofficial V5.4.117 release added connection parameter added to the open server
  method:

  ```py
  open(self, options=None, expanded=None, fields=None, where=None,
    order_by=None, open_empty=False, params=None, offset=None, limit=None,
    funcs=None, group_by=None, safe=False, connection=None)
  ```

  If the connection is set to `None`, the application creates a connection, executes an SQL SELECT query using it, and then closes it, otherwise it uses the connection to execute the query.

- Bug when deleting a record with details from imported database fixed.

## V5.4.119

- `Range of records` can be selected in the jam.py tables using `ShiftKey`

## V5.4.118

- in App builder help badge added to Field Editor Dialog
- when manual mode is set in App builder changing item name won't change item table name

## V5.4.117

- Builder bug when selecting search field in View form dialog fixed

## V5.4.116

- `Boolean filter` bug fixed

## V5.4.115

- bug when `sequence names` were deleted when importing metadata from database not supporting
  sequences to database that supports them fixed bug
- when `sorting` dataset on `date` and `datetime` fields fixed
- `auto_page_scroll` table option added

## V5.4.114

- `apply_delta` inside `on_apply` bug fixed

## V5.4.113

- `File upload` bugs fixed.
- Greek language translation upgraded

## V5.4.112

- `column headers` not shown in details in edit form bug fixed

## V5.4.111

- `Typeahead` is implemented for `lookup lists`

## V5.4.110

- The `history` fields are `sanitized` now

## V5.4.109

- The work on `sanitizing of field` values is completed.
- The `TextArea` attribute is added the Interface Tab of the Edit Form Dialog for TEXT fields
- The `Do not sanitize` attribute is added the Interface Tab of the Edit Form Dialog.
- The `Accept` attribute of the Interface Tab of the Edit Form Dialog for FILE fields is required
  now. Uploaded files are checked on the server against this attribute.
- The `Upload file extensions` attribute is added to the Project parameters that defines file types  
  that are allowed to be uploaded by the task upload method.
- The `expanded options` is added to the `add_edit_button` and `add_view_button` methods.

## V5.4.108

- Fixed bug when choosing language when first launching `builder.html`

## V5.4.107

- To project parameters added `upload_file_ext` parameter. This parameter defines file extensions
  that could be uploaded to server by using task upload method.

## V5.4.106

- Bug when search characters not showing bold on table fixed

## V5.4.105

- Field values are `sanitized` now
- For `image` type fields, the uploaded file is `validated`

## V5.4.104

- `Session cookie` is set with `HttpOnly` flag `True` to protect against XSS attack.

## V5.4.103

- `Upload file` vulnerabilitie fixed

## V5.4.102

- `Typeahead` bug in table column editor fixed

## V5.4.101

- Brazilian-Portuguese Translation added and Portuguese Translation updated, thanks to Maxwell Morais
- For Lookup lists `limitation` on values to positive numbers removed
- Bug with saving editable fields in tables fixed
- Check for reserved words when importing legacy database tables added
- Report cells can now interpret new line character `'n'`

## V5.4.100

- Bug with view_details in view_options fixed.
- Undefined in the title of task edit form corrected.
- When web app tries to pass object value to client string 'Object is passed'.

## V5.4.99

- Added support for `enum` fields.

## V5.4.98

- Bugs related to `import of tables` fixed.

## V5.4.97

- Cookies `samesite` attrubute has `Lax` value now.

## V5.4.96

- delta `client_changes` attribute added

## V5.4.95

- Bug with `detail permissions` on client fixed

## V5.4.94

- `display_text` property big fixed

## V5.4.93

- `Create menu` method of the task corrected, `empty` groups are not displayed now

## V5.4.92

- Bug related to SQLAlchemy upgrade fixed

## V5.4.91

- Support from Python3.8 added
- Werkzeug library upgraded
- SQLAlchemy library upgraded

## V5.4.90

- Italian Translation added

## V5.4.89

- If you want to turn off Application builder, place `empty` builder.html file to project folder. It
  will be loaded by browser insted of `builder.html` from package.

## V5.4.88

- Rule for files in project folder changed in wsgi.py. Only `html` file are accessible now.

## V5.4.87

- Turkish translation added

## V5.4.86

- `create_menu` method of task improved, user permissions are taken into account, menu separator can
  be added by adding an empty string, function can be specifyed that will be executed when menu item clicked.

  ```js
  let menu = [
          ['First',  [task.invoices, task.customers]],
          {'Second': [task.details, task.catalogs, '', task.reports]},
          {Third: [task.details, {Params: function() {alert('params clicked')}}]},
          {Fourth: [task.task.analytics, {'Artists list': [task.artists]}]},
          [task.reports],
          task.reports
      ];
  task.create_menu($("#menu"), $("#content"), {
      custom_menu: menu,
      splash_screen: '<h1 class="text-center">Jam.py Demo Application</h1>',
      view_first: true,
      create_single_group: false,
      create_group_for_single_item: false
  });
  ```

## V5.4.85

- Custom_menu options added to create_menu method of the task.

  ```js
  let menu = [
          ['First',  [task.invoices, task.customers]],
          ['Second', [task.journals, task.catalogs]],
          [task.reports]
      ];
  
  task.create_menu($("#menu"), $("#content"), {
      custom_menu: menu,
      splash_screen: '<h1 class="text-center">Jam.py Demo Application</h1>',
      view_first: true,
      create_single_group: false,
      create_group_for_single_item: false
  });
  ```

## V5.4.74

- Fixed bug when creating a new `report parameter`

## V5.4.73

- Bug that occurs when item is created after `history` item was created fixed.
- Table pagination bug fixed.

## V5.4.72

- MSSQL, Oracle bugs fixed.

## V5.4.71

- Upgrade to werkzeug 0.15.5 release windows bug fixed.

## V5.4.70

- Werkzeug library upgraded to the version 0.15.5
- `editing record` in modeless mode bug fixed

## V5.4.69

**Library**:

- Werkzeug library upgraded to the version 0.15.4
- `common.py` module rewritten, consts object created
- `adm_server.py` module removed
- `admin` folder is created with modules
  - `admin.py` - application builder server side module
  - `task.py` - loading of task from admin.sqlite database
  - `export_metadata.py`
  - `import_metadata.py`
- `builder folder` added to package that contains Application Builder project that is used to create
  Jam.py Application Builder, see `read.me` file in the folder.
- `task` loading accelerated
- `import` of metadata rewritten
- `import` of metadata accelerated
- `permissions` property added
- `logging` created (currently under development and not documented yet)
- `edit` method on the client and server checks now if item state is in edit mode and if it is does
  nothing
- `round` methods are corrected on the client and server, value of currency fields are rounded
  before they are assigned
- `inline editing` is now available for any items (not details only)
- `inline editing` of lookup fields, list fields, date and datetime inputs reworked, bugs fixed
- `fixed columns` of tables bugs fixed
- `tables striped` option added
- `search input` is focused now by `Ctrl-F`, `Escape` returns focus to the table
- `enable_controls` redraws controls now, no need to call `update_controls` method
- lot of bugs fixed

**Application builder**:

- a `link` to the form-related documentation page has been added to the application Builder form headers

**Documetation**:

- `search` bug fixed
- topics related to the server `on_apply` and `on_open` events rewritten
- new topic added `How to prohibit changing record`

## V5.4.61

**Application Builder**:

- Lookup list bug fixed

## V5.4.60

- Synchronization of parameters and reloading of the task tree when metadata changes for web
  applications running on parallel processes reworked.
- Import of metadata reworked.
- Created the ability to import metadata from the migration folder when the server is restarted.
- Migration to another database is available now. See `How to migrate to another database`
- `virtual_table` is now a `read-only` property on the client and server. For an item which
  `virtual_table` property is `true`, calling the `open` method creates an empty data set, and calling the `apply` method does nothing.
- When `importing` a table the `virtual_table` attribute id read only now.
- `title_line_count` option is added to the `table_options` specifies the number of lines of text
  displayed in a title row, if it is 0, the height of the row is determined by the contents of the
  title cells.  
  It can be set in Application Builder.

## V5.4.57

**Library**:  

- `Record locking` bug, when PostgreSQL, MSSQL or Firebird database is used, fixed
- To use `record locking` for items for which you defined `on_apply` event handler you must change.
  - Add the `connection` parameter, create a `cursor` and use the `cursor` to execute sql queries.
  - Otherwise the record locking won’t work.

  For example, the code:

  ```py
  def on_apply(item, delta, params):
      tracks_sql = []
      delta.update_deleted()
      for d in delta:
          for t in d.invoice_table:
              if t.rec_inserted():
                  sql = "UPDATE DEMO_TRACKS SET TRACKS_SOLD = COALESCE(TRACKS_SOLD, 0) + \
                  %s WHERE ID = %s" % \
                  (t.quantity.value, t.track.value)
              elif t.rec_deleted():
                  sql = "UPDATE DEMO_TRACKS SET TRACKS_SOLD = COALESCE(TRACKS_SOLD, 0) - \
                  (SELECT QUANTITY FROM DEMO_INVOICE_TABLE WHERE ID=%s) WHERE ID = %s" % \
  ```

  must be changed to

  ```py
  def on_apply(item, delta, params, connection):
      with item.task.lock('invoice_saved'):
          cursor = connection.cursor()
          delta.update_deleted()
          for d in delta:
              for t in d.invoice_table:
                  if t.rec_inserted():
                      sql = "UPDATE DEMO_TRACKS SET TRACKS_SOLD = COALESCE(TRACKS_SOLD, 0) + \
                      %s WHERE ID = %s" % \
                      (t.quantity.value, t.track.value)
                  elif t.rec_deleted():
                      sql = "UPDATE DEMO_TRACKS SET TRACKS_SOLD = COALESCE(TRACKS_SOLD, 0) - \
                      (SELECT QUANTITY FROM DEMO_INVOICE_TABLE WHERE ID=%s) WHERE ID = %s" % \
                      (t.id.value, t.track.value)
                  elif t.rec_modified():
                      sql = "UPDATE DEMO_TRACKS SET TRACKS_SOLD = COALESCE(TRACKS_SOLD, 0) - \
                      (SELECT QUANTITY FROM DEMO_INVOICE_TABLE WHERE ID=%s) + %s WHERE ID = %s" % \
                      (t.id.value, t.quantity.value, t.track.value)
                  cursor.execute(sql)
  ```

## V5.4.56

**Library**:

- `Record locking` is avaliable
- task creation in wsgi.py modified to avoid `'project have not been created yet’` message
- report parameters `display_text bug fixed

**Demo application**:

- `on_apply` event handler in Invoices server module modified

## V5.4.55

**Library**:

- `show_hints` and `hint_fields` attributes can be added to the `table_options` or `options parameter` of the `create_table` method.
- `refresh_record` method restore positions of detail records

**Documentation**:

- Form events rewriten
- Some topics from Jam.py FAQ are moved to How to

## V5.4.54

**Library**:

- MSSQL bug when selecting tables for import fixed
- delta `old_value` property code modified (not documented yet)

**Documentation**:

- `Authentication section` added to `How to`

## V5.4.53

**Documentation**:

- Latest docs chnges
- How to section bug fixed
- `How to lock a record so that users cannot edit it at the same time` topic removed - other
  algorithm will be used

## V5.4.52

**Library**:

- `on_login` event changed
- `generate_password_hash` and `check_password_hash` mathods added
- `tables resizing` bug for `numneric` fields fixed
- `tables with freezed cols` bugs fixed
- details bug when `renaming copy` fixed
- minor bugs fixed

## V5.4.48

- Bugs related to moving to SQLAlchemy and tables with `virtual scroll` are fixed.

## V5.4.40

- Some bugs fixed
- `deployment section` added to `how to` in docs

## V5.4.39

**Library**:

- Jam.py now uses SQLAlchemy connection poll
- when image field `read_only` attribute is set user can not change the image by double-clicking on
  it
- Some bugs fixed

**Documentation**:

- `How to lock a record so that users cannot edit it at the same time` topic added

## V5.4.37

**Library**:

- Jam.py can now be deployed on PythonAnywhere. See `How to deploy project on PythonAnywhere`
- Directory of the project can be passed to the `create_application` function now (jam/wsgi.py
  module).
- `Multiprocessing connection pool` parameter removed from `Project parameters`
- Bugs related to processing of `keyboard events` by forms fixed
- Some bugs fixed

**Documentation**:

- `How to` section created. That section will contain code examples that can be useful to quickly
  accomplish common tasks.

## V5.4.36

**Library**:

- tables reworked, they now support virtual scrolling.
- some bugs fixed

**Application Builder**:

- Search added for items.  

**Demo application**:

- User registration implemented

## V5.4.31

- Bug when reading `index.html` file fixed.
  `Index.html` must have a `unicode` encoding.
- German translation updated.
- Bug when Dashbord are opened fixed in Demo.
- Users item added to demo.
- `on_login` event handler in task server module that uses Users item to login is written (commented) and changing of password implemented. Uncomment `on_login` to see how it works. Description is here <https://groups.google.com/forum/#!topic/jam-py/Obkv5d3yT8A>
