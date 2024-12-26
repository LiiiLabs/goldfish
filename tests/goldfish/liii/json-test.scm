(import (liii check)
        (liii json))

(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文"))) 

(check (json-ref '(("age" . 18)) "age") => 18)

(check-report)

