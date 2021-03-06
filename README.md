# snaplet-redson

This is a snaplet for Haskell [Snap][] framework which provides
create-read-update-delete operations for any JSON objects using Redis
storage. JSON objects are mapped to Redis hashes. Role-based permissions
system is supported.

Primary notion of Redson is a model, or a form, which is a named
collection of fields with certain extra annotations. Every filled form
(«instance of model») has unique ID when stored in Redis.

Models must be defined prior to usage with special syntax, which may
also be requested by client code to build interface for models (using
JavaScript templating, for example).

Transparent mode is supported in which model definitions are not
checked, but permissions engine is unavailable as well. Certain
transformations may be applied to model definition by server and served
to client (see «served models»).

## Models

Prior to reading and writing form data, the server reads form (model)
definition from JSON files stored in directory set by
`models-directory` setting (defaults to `resources/models/`), unless
`transparent-mode` setting is set to `true` (see below).

### Form definition example

(As described in [The SCP Foundation](http://scp-wiki.wikidot.com/)
archives)

```json
{
    "name": "SCP",
    "canCreate": ["front"],
    "canRead": true,
    "canUpdate": ["front", "back", "manager"],
    "canDelete": ["manager"],
    "fields": [
        {
            "name": "code",
            "meta": {
                "label": "Code"
            },
            "canWrite": ["front", "back"]
        },
        {
            "name": "title",
            "meta": {
                "label": "Title"
            },
            "canWrite": ["front", "back"]
        },
        {
            "name": "foundAt",
            "meta": {
                "label": "Original location"
            },
            "canWrite": ["front", "back"]
        },
        {
            "name": "dangerClass",
            "meta": {
                "label": "Danger class",
                "dictionaryName": "DangerClasses",
                "default": "lev0"
            },
            "type": "dictionary",
            "canRead": ["front"],
            "canWrite": ["back"]
        },
        {
            "name": "conditions",
            "meta": {
                "label": "Special Containment Procedures"
            },
            "type": "textarea",
            "canWrite": ["back"]
        },
        {
            "name": "description",
            "type": "textarea",
            "meta": {
                "label": "Description"
            },
            "canWrite": ["back"]
        }
    ]
}
```

Valid keys for form and fields are listed below. Every field is
required unless explicitly stated otherwise.

### Model name

Consider a model is stored in `scp.js` file; we define model name as
the filename without extension (`scp`). Model name is used to
construct the URL to access forms built from this model.

### Valid form definition keys

1.  `name`

    Internal name of model. Must match the basename of JSON file model
    definition is stored in.

2.  `title`

    Human-readable form title.

3.  `canCreate` / `canUpdate` / `canRead` / `canDelete` (*optional*)

    Each field lists roles having access to specific CRUD methods for
    model. Boolean values may be used instead of role lists as well,
    with `false` meaning «none of the roles» and `true` meaning «all
    the roles». Empty list is the same as `false`.

    Implementation follows these rules:

    - `canCreate` implies `canUpdate` permission.

    Note that per-field permissions are still checked after whole-form
    ones succeed.

    If field is not included in model definition, `false` is implied.

    Attempt to violate permissions results in 403 Forbidden error.

4.  `fields`

    List of field objects, where every field object has keys listed
    below.

5.  `applications`

    List of application objects which target many fields at once (see
    below).

### Valid field definition keys

1.  `name`

    Field object must include `name` key which is the internal name of
    the field. Name is alphanumeric string.

    `class` cannot be used as a field name.

2.  `type` (*optional*)

    Type system currently does not impose any checks on server side.
    Following values are proposed for `type`:

    -   `text`,
    -   `textarea`,
    -   `checkbox`,
    -   `dictionary`,
    -   `reference`

    Default field type is `text`

    Client uses type data to properly render field contents.

    If type of field is `reference`, then field holds a number of
    references to another form instances. The client may provide
    interface to edit or view the subform. The actual value provided
    for the form field with `reference` type should be comma-separated
    list of `<formname>:<instanceid>` strings, where `<formname>` is
    the name of form to which the reference is stored and
    `<instanceid>` is the id of specific instance of that form.

3.  `canRead` / `canWrite` (*optional*)

    Per-field permissions have the same syntax as per-field per-form
    permissions. Only readable fields are served to client and only
    writable are allowed to be changed by client.

    Implementation generally follows these rules:

    -   If field is unreadable, then client must not render it.

    -   If field is not writable by user, then form input element must
        be inactive.

    -   `canWrite` implies `canRead`.

    -   Client does not attempt to write inaccessible fields to server
        and does not expect unreadable fields in server response.

4.  `groupName` (*optional*)

    If `groupName` key is present, its values must be equal to name of
    one of the complex field groups as described in group fields file.
    When server processes model description, such fields are spliced
    into list of fields as described for that group (see below).

5.  `index` (*optional*)

    Boolean which instructs server to create or update exact reverse
    index on that field on create/update/delete operations.

    Redis key used is `<modelname>:<field>:<value>`, value stored is a
    set of instance ids with that value for indexed field.

6.  `indexCollate` (*optional*)

    If field has index=true, then indexCollate instructs Redson whether
    or not to perform string cleaning (removing punctuation, space,
    lower-case conversion) prior to saving index values and performing
    an ad-hoc search against them. Defaults to false. Set this to true
    if you want to search by names or other user-provided strings.

    This flag may be removed in the upcoming releases.

7.  `meta` (*optional*)

    A hash of arbitary keys and values which are **not** treated by
    server in any way. These may be used by a client for field-specific
    behaviour or templating.

    Suggested meta annotations:

    1.  `default`

        Specifies the default value of field. (Will possibly move
        out of meta if server will enforce default values upon
        creation some day).

    2.  `dictionaryName`

        When field type is `dictionary`, `dictionaryName` key
        must be present in field description as well. Client should
        provide certain mapping between stored field contents and
        displayed value using the name of dictionary.

    3.  `invisible`

        Boolean field to indicate that field should not be rendered
        on UI (or not handled by client at all).

    4.  `label`

        Human-readable field label.

    5.  `readonly`

        Boolean field to indicate that client must render field as
        disabled.

    6.  `required`

        Boolean field to indicate that field is required. Client may
        provide custom checks in UI for such fields. (Will possibly
        move out of meta)

### Group fields

A group of fields (complex field) with distinctive name may be shared
across several models. Valid complex fields must be defined in a file
set by `field-groups-file` setting ("resources/field-groups.json"),
which must contain a JSON hash where keys are group names and values are
fields in respective group.

```json
{
    "address": [
        {
            "name": "city",
            "meta": {
                "label": "City"
            }
        },
        {
            "name": "zip",
            "meta": {
                "label": "ZIP / Postal code"
            }
        },
        {
            "name": "address",
            "meta": {
                "label": "Address"
            },
            "type": "textarea"
        }
    ]
}

```

Using `groupName` in field description is not allowed (no recursive
complex fields).

### Permissions

Per-field permissions (set in `canRead` and `canWrite` field
properties) are checked prior to writing any data to Redis or sending
response to client (unless `transparent-mode` is `true`).
Implementation currently follows these rules:

-   No unreadable fields are sent to client on READ methods;
-   Attempt to perform any operation without being logged in results in
    401 Unauthorized error.
-   Attempt to perform any operation on unknown model results in 404 Not
    Found error;
-   Attempt to create or update instances with unwritable fields will be
    rejected with 403 Forbidden.

### Field applications

It's possible to change certain annotations for many fields in model
with one instruction. `application` key of form definition contains a
list of application objects. Every application object may contain
`canRead`, `canWrite` and `meta` keys with same syntax as in
fields. Additionally, `targets` keys must be present. If `targets`
is a list of field names, then new values for `canRead`, `canWrite`
are set for matched fields. `true` value of `targets` matches every
field. `meta` values from application and matched field are merged,
with meta keys from application having precedence over field meta.

Example (set new label and foo meta, new permissions for all fields):

```json
{
    "targets": true,
    "meta": {
        "label": "Renamed label",
        "foo": "bar"
    },
    "canRead": ["changed_role"],
    "canWrite": false
}
```

Example (change label of "foo" field):

```json
{
    "targets": ["foo"],
    "meta": {
        "label": "Foo field"
    }
}
```

### Served models

Client may request stripped form description by sending this request:

```
GET /<modelname>/model/
```

Server takes several steps to serve the model.

1.  Permissions processing

    Response will contain original description but without fields
    unreadable by current user. canWrite field property will be set to
    boolean value for every form field, indicating whether the current
    user can write to this field. Whole-form permissions will be set to
    booleans as well, indicating whether the current user has specific
    permissions.

2.  Meta bags

    `meta` for every field as served as-is without any changes.

3.  Group splicing

    Every field `f` with `groupName` annotation is spliced into list
    of actual group members in served model, and `groupName` property
    is attached to every field in splice result, with value equal to
    name of group. Name of every field in group is **prepended** with
    `f_`, where `f` is the name of original field which was spliced
    into group. Client may use this data to recognize fields from the
    same group and render them specifically.

    *Example*:

    Assuming group `bar` has fields named `f1`, `f2` and `f3`,
    and model has field with group splice annotation:

    ```
    "name": "foo",
    "groupName": "bar"
    ```

    then `foo` will be **spliced into** fields named `foo~f1~`,
    `foo~f2~` and `foo~f3~`, and `groupName` for all these fields
    will be set to `bar`.

4.  Applications

    Applications are performed (in sequence following the order they're
    listed in model definition) **after** group splicing, which means
    applications may be used to override default field annotations set
    for group members in `field-groups-file`.

5.  Index fields list caching

    Served form will also contain `indices` field which is a list of
    index fields of model.

6.  Readable models

    Client may also request list of readable models from

    ```
    GET /_models/
    ```

## CRUD

We implement generic CRUD for our forms using Redson snaplet, which is
best explained by its routes (assuming they're installed in top-level
snaplet under `/_` URL; prefix may be changed using `nestSnaplet` in
parent initializer).

All interactions with server use JSON objects as primary format. JSON
objects are mapped to Redis hashes stored under `<modelname>:<id>`
key. Fresh ID's are provided by using `global:<modelname>:<id>` key
which is `INCR`-ed after every new form instance is created in Redis.

No schema checking is performed by server, but permissions engine will
disallow writing arbitary models and fields to server.

CRUD mapping to HTTP methods is implemented in Redson as expected by
[Backbone][]:

```
CREATE → POST /<modelname>
READ → GET /<modelname>/<instanceid>/
UPDATE → PUT /<modelname>/<instanceid>/
DELETE → DELETE /<modelname>/<instanceid>/
```

Redson snaplet is parametrized by `AuthManager` snaplet lens (usually
from top-level application). All methods implemented by Redson require
the user to be logged in, 401 Unauthorized HTTP error response is
issued otherwise.

### Server interface by example

Assume we're using `scp.js` model given above.

1.  CREATE

    Server request:

    ```
    curl localhost:8000/_/scp/ -X POST -d "{\"title\":\"Able\", \"code\":\"076\", \"class\":\"Keter\"}"
    ```

    What server did in Redis:

    ```
    incr global:scp:id
    ```

    (24 is returned)

    ```
    hmset scp:24 code 076 title Able class Keter
    ```

    Server response:

    ```
    {"code":"076","id":"24","title":"Able","class":"Keter"}
    ```

    (note the `id` field which is returned by server after Redis was
    updated with new form instance. Backbone stores new instance id upon
    receiving server response and uses it in further server requests for
    saving updated model instance)

2.  READ

    Server request:

    ```
    curl localhost:8000/_/scp/24/ -X GET
    ```

    Server response:

    ```
    {"code":"076","title":"Able","class":"Keter"}
    ```

    Redis command used:

    ```
    hgetall scp:24
    ```

3.  UPDATE

    What is sent to server:

    ```
    curl localhost:8000/_/scp/24/ -X PUT -d "{\"title\":\"Able\", \"code\":\"076-2\", \"class\":\"Keter\", \"description\":\"Really nasty thing\"}"
    ```

    Server response is 204 (success, No content) in case the instance
    previously existed and 404 if not.

    Note that the all model fields are sent to server (this may be
    improved for efficiency).

4.  DELETE

    Server request:

    ```
    curl localhost:8000/_/scp/24/ -X DELETE
    ```

    Redis deletes the key:

    ```
    del scp:24
    ```

    Server response contains JSON of instance before deletion:

    ```
    {"code":"076-2","title":"Able","description":"Really nasty thing","class":"Keter"}
    ```

## Search

Search interface for model `<modelname>` is available under
`/_/<modelname>/search` access point via GET method. `canRead` form
permission is required to search for instances.

Accepted parameters are:

-   key-value pairs where keys are index fields of model and values are
    search terms;

-   `limit` parameter which sets maximum number of items served;

-   `matchType=p` or `matchType=s` for prefix search or substring search
    of value in index field (prefix search is faster);

-   `searchType=and` or `searchType=or` which indicates if all search
    terms must match or just any of them.

-   `fields=f1,f2,f3` which is a list of fields which must be
    extracted from every matched instance and served in response.

Response is a list of JSON objects for matched instances. If `fields`
is provided, then response is a list of arrays instead, where every
array contains values of specified fields in instance (in order given
by `fields` parameter; if value is not present then null is used).

No per-field read permissions are checked.

Currently search is implemented using slow `keys` Redis command, and
should be considered an ad-hoc solution only.

## Extra features

### Timeline

There's an extra entity stored in Redis for every model called timeline,
which is a list with id's of instances stored in DB (in order of
creation).

`/_/scp/timeline/` serves JSON list of last N (currently 10) timeline
items for model "scp":

```
curl localhost:8000/_/scp/timeline/ -X GET
["39","38","37","36","35","34","33","32","31","30"]
```

If instance is removed from Redis, corresponding timeline entry is
removed as well.

Client front-end uses timeline to show links to fresh instances.

canRead model permission is required to access model timeline.

### WebSockets notifications

`/_/<modelname>/events/` provides instance creation/deletion
notifications through WebSockets interface. Events are transmitted to
clients in JSON format with fields `event`, `model` and `id`,
where `event` is either `create` or `delete`. No permissions are
checked currently when accessing events.

## Snapless operation

The package provides `Snap.Snaplet.Redson.Snapless.*` modules:

- CRUD — low-level operation with Redis DB (on commit level, where
  commit is a list of key-value pairs for named hash); operations
  support index updates so use this for tools which need to fiddle
  with Redson indices;

- Metamodel — model definition parsing;

- Loader — load models from filesystem locations, splicing groups;
  served models are provided using this module.

## Redis interface

We use redis bindings provided by snaplet-redis package. Pool size
numbers are yet to be tuned.

## Setup

Following config options are recognized by Redson:

- `models-directory` (`"resources/models"`): directory which contains
  model definitions to be read by Redson.

- `field-groups-file` (`"resources/field-groups.json"`): file which
  contains descriptions of usable complex field groups.

- `transparent-mode` (`false`): when true, no permissions checking is
  performed. Redson acts in «transparent mode» allowing to store and
  retrieve any JSON data. Any model may be written to.

## To do

### Cache user permissions

Snap.Snaplet.Redson.Snapless.Metamodel coupled with withCheckSecurity
provides permissions checking upon every CRUD operation. Intersecting
user roles and role lists set in form/field permissions should be
performed once when first request from that user is received and cached
for all further requests (models can't be changed without Redson restart
anyways, and restart will be required when new users are added as well).

### External search providers

Might subscribe to model events via socket. Provides lists of matching
instance ids.

### Update inverse references

When instance of model becomes referenced by another instance, inverse
reference should be updated by server.

We already have indices out of the box so we can get this feature for
free.

Perhaps orphan dependent models should be cleared if parent is deleted.

### Factor out Snap.Snaplet.Redson.Util to snap-errors module

### Force default values when creating instance

### Configurable pool size

### WebSockets interface improvement

-   [X] `load-model.js` contains full URI to WebSockets entry point
    (currently hardcoded for `scp` model)
-   [ ] publish events only for respective model under
    `<model>/events` entry point (requires addressing extension
    for PubSub or multiple PubSubs; non-transparent mode only +
    BigBrother role for «all models» event entry point)
-   [ ] check permissions (if we want to serve associated commits in
    event messages, per-model canRead may be not enough (what if
    listener has no access to certain fields and we can't strip commit
    for every listener personally))
-   [ ] possibly use native Redis' publish/subscribe mechanism

### Support search in transparent mode

Currently only index fields of model are searched against, which means
that model definition must be available for searching (this also
required redundant `name` field in form definition), while in fact
redisSearch can be implemented using ModelName only. External providers
must support name-only operation as well.

[snap]: http://snapframework.com/

[backbone]: http://backbonejs.org/
