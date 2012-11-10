
-type url() :: string().
-type http_verb() :: get|head|put|post|delete|options.

-record(field, {name :: atom(),
                type :: text | password | hidden | textarea | file |
                        image | submit | reset | button | checkbox | radio |
                        url | email | search | number | range | color |
                        date | time | datetime | 'datetime-local' | month | week,
                value :: [string()],
                content,
                options = [] :: [string()],
                attributes = [] :: [{atom(),string()}]}).

-record(form, {action :: url(),
               method = get :: http_verb(),
               name :: undefined|string(),
               id :: undefined|string(),
               content_type :: string(),
               fields :: [#field{}]}).

-record(link, {type :: a | frame | link,
               id :: undefined|string(),
               url :: url(),
               text :: string()}).
