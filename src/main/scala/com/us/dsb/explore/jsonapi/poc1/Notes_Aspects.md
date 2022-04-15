

to be assimilated:
- chain(s) of dependencies between things:
  - (dependencies across things (e.g., JSON subtrees) that seem somewhat
    independent)
  - type-metadata chain:  to datatypes, from entity types derived from
    query--primary data's type or types, included data's type(s), maybe related
    data's type(s) in relationships); depends on "fields"
  - included-data chain:  to included resource objects, from resource ID objects
    in primary data (including relationships) and from other included objects
    - (depends on "included" and "fields")
    - (how does that map to SQL (without querying for each referring object)?)
- multi-entity-type relationships (e.g., groups members (e.g., users, computers,
  other groups, if as one relationship)
  - entity type handled fine in "type" member
  - need some kind of union entity type (e.g., "user or computer or group")
  - how would query map to SQL and return resource objects of different types?
    - multiple single-join queries?
    - one query using multiple joins getting different tables' data into
      separate groups of columns, UNIONED, with each row mapped to entity type
      per which column group filled in
    - multiple joins?
  - probably just reserve "syntax" to represent union types (e.g., entity
    type names can map to either regular entity type or union entity type)
- What was relationships choice re avoiding "user" vs. "users" in UI re
  references?
  - In resource objects, avoid using "data" (which lists resource identifier
    objects (with type and ID, needing lookup)) and do use "links.related"
    (which gets full resource objects)
  - (probably don't provide "links.self" or even way to get relationship itself
    as primary data) (is getting relationship ever needed for read-only access?)

- mapping to DB queries (~collected)
  - adding customer account ID (not in logical entity model)
  - attributes:  normally to columns, but maybe to expressions
  - entity collections:  normally to tables, but maybe to table expressions
    (e.g., JOINs)
  - filtering:  narrows down WHERE clause
    - What about filtering via exists/forall on relationships?  Just
      SQL subexpression, or change to form of query?
  - relationships:
    - ... "simple" vs. association table ...
    - ... ("included", JOIN stuff, etc.)
  - entity counts

- error handling:
  - general
  - JSON:API-specific (e.g,. "errors")
- HTTP server connection:
  - general
  - Don't _manually_ create _multiple_ HTTP-framework "routes" for available
    paths:
    - Create one ~high-level route, get path tail, interpret path segments.
    - Maybe, use metadata to create multiple routes:
      - (If framework can do something useful when given individual routes/paths
        (showing what's available? better logging?))
- design, decision, and implementation order:
  - Identify higher-priority features (e.g., features/cases/combinations needed
    to replace (chunks of) current functionality, plus known-wantd new ones.)
    - e.g., all entities of type; paging; current filtering, sorting;
    - Identify risk areas re prototyping, etc.
  - Identify "fancier" features (beyond parity with current support and
    know-required new support).
    - Identify likelihood of "fancier" features.
  - Re fancier features, determine what design/details can be deferred vs. what
    needs to be planned for (e.g., reserve "syntax" for possible future
    features/cases/etc.)
- links:  which query parameters propagate and not?
- filtering and visibility to UI?
  - How soon will UI support editing queries?  Will UI need to parse current
   filtering condition from response (e.g., to start from something other than
   UI's query editor (e.g., pre-built filter string) but then let user adjust
   from there?)?
- assim: avoid "user" <--> "users" if path segment uses something else (not
  plural) to indicate collection (e.g., "/collByType=user"?
  - 
 
  

Semi-sorted:
- Likely feature-level increments:
  - Relationships and dependent things:  Likely increments:
    - Start with no relationships (not used in current non-graph UI/back end).
    - Add minimal[*] access to related entities:  Relationship objects, with
     "related" links usable to get related entities as primary data.
     [* Minimum such that client doesn't need to map "user" to "users" (type
     name vs. corresponding  segment/parameter "users").]
   - Defer fancier features until/unless needed:
     - compound documents ("include" query parameter and "included" member")
     - multi-hop relationship access (e.g, "/users/123/rel1/rel2")
  - Type metadata:
    - Data types:
      - Start with at physical vs. logical type.
      - Maybe defer multi-level types.
  - Paging:
    - Start with minimum for UI to drive paging:
      - Accept query parameters (probably just offset and limit).
      - Report resource counts.
      - Maybe pass parsed/decoded paging parameters in metadata.
    - Likely defer first/prev/next/last links, etc.
  - Field selection:
    - Start with "fields" only for primary-data type.  (There are no resource
       objects of other entity types unless/until relationships.)  No submember
        syntax.
    - Likely defer submember syntax unless/until needed.
    - Defer multiple types until/unless relationships.


- High-level query types:
  - Support like:  all users, user with specific ID (for all entity types)
  - Maybe support traversing relationships (e.g., all domains related via
    users' containingDomain relationship from users)
    - Q:  Confirm:  Different from filtering domain collection via some
      expression detecting relationships from users?
    - ??:  assimilate SQL-mapping aspects as for "include"(?)
    - ??:  assimilate single-/multi-hop aspects
    - 
- Base query syntax in URL path:
  - (Depends on (how many) types of high-level queryies supported.)
  - Candidate:  Initial simple-segments draft:
    -
      - /users                          - all users
      - /users/123                      - user 123
      - /users/123/someRel              - user 123 -> someRel -> related entities
      - /users/123/relationship/someRel - similar but relationships itself
      - /users/123/rel1/rel2...         - (maybe) chained ...
    - (Has some irregularities/limitations.)
  - Candidate:  Segments like "<step type>=<step param>":
    - e.g.:
      - /users                             - all users
      - /users/id=123                      - user 123
      - /users/id=123/related=someRel      - user 123  -> someRel -> related entities
      - /users/related=someRel             - all users -> someRel -> related entities
      - /users/id=123/relationship=someRel - similar but relationships itself
      - ...
    - Explicit syntax allows easier ~co-existence of different cases
      (note /users/rel=someRel and /users/id=123).
    - Specifics are TBD (including whether first segment has step type too).
  - Other candidates are possible.
  - Incrementality:  Even if we start with  /users/123, we could can move to
    /users/byId=123 and/users/other=other by treating unrecognized-prefix
    segment  as ID.
  - See more in
    https://rationemllc.atlassian.net/wiki/spaces/CA/pages/3123085352/URL+Paths+request+cases+path+forms+related+aspects)

  
  
- Response metadata (returnd in API responses):
  - Type metadata:
    - Entity-type metadata:
      - List/describe fields (attributes and relationship).
        - So UI knows what is can page to "fields" query parameter.
      - (Reserve "syntax" for union types re multi-entity-type relationships.)
    - Datatype metadata:
      - At least phystical and logical types (e.g., "string" and "entityName").
      - Maybe chain of subtyping, like "string" -> "entityNane" -> "userName".
        - (UI uses most-specific one it knows.  E.g., map types to HTML element
          classes; whichever classes CSS covers affect rendering.)
      - "Obvious" types:  string, int/long, float/double, boolean, etc.
      - Enumeration types:
        - List enumerators, UI can help user select values for filter expressions.
        - Q:  Ordered by enumerator name or by order given somewhere?  (See
          pseudo-submembers note.)
      - Timestamps:
        - Maybe JSON object carrying both milliseconds offset and timestampk
          string as currently.
          - Maybe simply ~hard-coded (defined) that way.
          - Maybe instance of class of types for JSON object structures? (Can
            probably allow for adding that later.)
          - (See pseudo-submember note.)
      - Possibly use submember syntax to refer to (psuedo-)members to request
        ~variations:
        - e.g., in "fields": "created.epochMillis" vs. "created.stringForm" (vs.
          just "created" to get JSON object with both)
        - e.g., in "sort": "enumAttr.logicalOrder" vs. "enumAttr.stringOrder"
          (vs. just "enumAttr" to use enumeration type's default order)


  - Instance metadata:
    - Entity counts:
      - Query total (without paging; after filtering).
        - (For, e.g., showing number of pages.)
      - Entity/relationship list total (before filtering).
        - (As we currently show.)

  - Response metadata's locations:
   - Some in top-level "meta" member
   - Maybe some in deeper "meta" members.  (Anything likely?)


- "Model" metadata:
  - (The model-specific metadata defining our logical entities and mappings
    to the data store. "driver metadata"?  "control ..."?  "master ..."?
    "mapping ..."? )
  - Contains metadata selected from to make response metadata (e.g., response's
    specfic entity type(s)), e.g., entity types, data types.
  - Contains mappings to/from data store:
    - E.g., an entity type's corresponding table (or table expression/etc.).
    - E.g., an attribute's corresponding column (or other expression/etc.).
    - E.g., a relationship's corresponding query components (joined tables,
      joining columns, maybe other SQL fragments).
     
- Mapping between data store and JSON:API response:
  - (See model metadata.)
  - 




- Relationships:
  - Mapping from logical relationships to database representation:
    - Represented in model metadata (e.g., SQL fragments, code fragments)
    - Expected database representations:
      - Simple:  Foreign key in relationship source table to primary key in
        target table.
      - Association table:  Association table with foreign keys to source
        and target tables.
      - Other?  (Maybe something complex behind the scenes, like table
        expression instead of direct table reference?)
    - Minimum level for access to related objects, without client's needing to
     know "user"-to-"users" correspondence:  Support relationship objects
     having "related" links usable to get related entities as primary data.

- Query parameter aspects:
  - Paging:
    - assimilate:  threshold for paging by default?
    - assimilate:  any hard limits on page size/limit?
    - assimilate:  any metadata reporting default/hard limits?
    - (Not for single-entity queries.)
    - For:  primary data.
    -  Q: What about "included" and relationship collections?  (Probably 
       deferred/dropped anyway.)
    - Accept which forms: 1. offset and limit, 2. page size and number, or
      3. either (or 4. somethine else)?
      - if page size and number/offset, client can't get arbitrary range (e.g.,
        page size 10, want 8 items in range 17 to 24)
      - if offset and limit, might complicate generating first/prev/next/last
        links, especially if client sends offset that is not multiple of limit
        or changes limit
        - maybe just ignore irregularities (Prev then Next isn't always the
          "identity" traversal)
    - Provide first/prev/next/last links:
      - Only when paged?  (That's a way to tell UI whether paged, if needed.)
      - Re spec's "When available":
        - Links "first" and "last" probably always available. (Can to go to
          first/last even if already there.)
        - Is availability of "prev" and "next" clear?
    
    - TBD:  support UI's display of paging state and available actions
      - (e.g., state like current page number and/or offsets, and actions like
        "page 1", "page 2", "page 3" links)
      - probably have entity count in metadata (top-level only? relationships'
        too?)
      - probably have current page/offset/etc. numbers in metadata

  - Field selection:
    - Basic/regular (as specified by JSON:API):
      - Select by simple field names.
      - Select specific attributes and/or relationhips.
      - Have default attributes (and/or relationships):
        - Probably API simply defaults to all attributes (and no
          relationships)--UI can have its own defaults.
        - But what about having different defaults for different predefined
          filterings?  All on UI side?
      - (Per entity type.  Affects all resource objects, regardliness of
        where (main primary data, primary data's related ovbjects, "included's
        objects).)
    - Possibly, allow pseudo-submember references:
      - To select members of values that are JSON objects (e.g., if timestamps
        carry both milliseconds numbrer and string).


  - Sorting:
    - (Primary data only.  Not order in "included" or in relationship objects.)
    - Support basic sorting (by attributes only, one or multiple,
      ascending/descending; most datatype ordering obvious--enumerations by
      value string or some listing order?).
    - Presumably don't support sorting by related entities (e.g.,
      ...?sort=someRel/someAttr), at least not for a while.

  - Filtering:
    - *TBD*
    - see https://rationemllc.atlassian.net/wiki/spaces/CA/pages/3116040208/Proposed+Filtering)
    

  - Included data:
    - (Depends on supporting relationships.)
    - TBD:  Supported?  How much?
      - From just one relationships or multiple?
        - (Just one seems arbitrary limitation from usage side, but possibly
          simpler to implement.)
      - One-hop or multiple-hop?
      - (Any other levels/features?)
      - How do multiple hops map to SQL queries?
    - No duplicate resource objects, even if entity is multiply referenced.
