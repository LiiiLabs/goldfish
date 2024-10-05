(import (liii check)
        (liii base64))

(check (utf8->string (base64-encode (string->utf8 "a"))) => "YQ==")

(check-report)

