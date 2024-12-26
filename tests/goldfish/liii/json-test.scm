(import (srfi srfi-78)
        (liii json))

(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文"))) 

